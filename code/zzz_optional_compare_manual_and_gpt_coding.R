library(tidyverse)

manual_path <- "data/df_advice_manual_coding.csv"
model_path <- "data/df_advice_gpt5.csv"
output_path <- "figures/manual_gpt5_comparison.csv"

if (!file.exists(manual_path)) {
  stop("Missing required file: ", manual_path)
}
if (!file.exists(model_path)) {
  stop("Missing required file: ", model_path)
}

manual <- read_csv(manual_path, show_col_types = FALSE)
model <- read_csv(model_path, show_col_types = FALSE)

if (!("mix_and_match" %in% names(manual))) {
  stop("`mix_and_match` column was not found in manual file.")
}

start_idx <- match("mix_and_match", names(manual))
candidate_cols <- names(manual)[start_idx:length(names(manual))]
label_cols <- intersect(candidate_cols, names(model))

if (length(label_cols) == 0) {
  stop("No overlapping label columns found from `mix_and_match` onward.")
}

join_keys_priority <- c(
  "participant_code",
  "chain_code",
  "generation",
  "treatment_appeal",
  "feedback_message"
)
join_keys <- intersect(join_keys_priority, intersect(names(manual), names(model)))

if (length(join_keys) == 0) {
  stop("No common key columns available to align rows between files.")
}

safe_divide <- function(numerator, denominator) {
  if_else(denominator > 0, numerator / denominator, NA_real_)
}

to_binary <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  x_num <- dplyr::coalesce(x_num, 0)
  as.integer(x_num > 0)
}

paired <- manual %>%
  select(all_of(c(join_keys, label_cols))) %>%
  rename_with(~ paste0("manual_", .x), all_of(label_cols)) %>%
  inner_join(
    model %>%
      select(all_of(c(join_keys, label_cols))) %>%
      rename_with(~ paste0("model_", .x), all_of(label_cols)),
    by = join_keys
  )

if (nrow(paired) == 0) {
  stop("No overlapping rows after joining manual and model data.")
}

metrics_for_column <- function(df, col_name) {
  m <- to_binary(df[[paste0("manual_", col_name)]])
  p <- to_binary(df[[paste0("model_", col_name)]])

  n11 <- sum(m == 1 & p == 1, na.rm = TRUE)
  n10 <- sum(m == 1 & p == 0, na.rm = TRUE)
  n01 <- sum(m == 0 & p == 1, na.rm = TRUE)
  n00 <- sum(m == 0 & p == 0, na.rm = TRUE)
  n <- n11 + n10 + n01 + n00

  positive_union <- n11 + n10 + n01
  negative_union <- n00 + n10 + n01

  tibble(
    label = col_name,
    n = n,
    n11 = n11,
    n10 = n10,
    n01 = n01,
    n00 = n00,
    accuracy = safe_divide(n11 + n00, n),
    positive_match_jaccard = safe_divide(n11, positive_union),
    negative_match_jaccard = safe_divide(n00, negative_union)
  )
}

per_label <- map_dfr(label_cols, ~ metrics_for_column(paired, .x))

overall_counts <- per_label %>%
  summarise(
    n = sum(n),
    n11 = sum(n11),
    n10 = sum(n10),
    n01 = sum(n01),
    n00 = sum(n00)
  )

overall_row <- overall_counts %>%
  transmute(
    label = "OVERALL",
    n = n,
    n11 = n11,
    n10 = n10,
    n01 = n01,
    n00 = n00,
    accuracy = safe_divide(n11 + n00, n),
    positive_match_jaccard = safe_divide(n11, n11 + n10 + n01),
    negative_match_jaccard = safe_divide(n00, n00 + n10 + n01)
  )

results <- bind_rows(per_label, overall_row) %>%
  mutate(
    across(
      c(accuracy, positive_match_jaccard, negative_match_jaccard),
      ~ round(.x, 2)
    )
  )

output_dir <- dirname(output_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(results, output_path)

cat("Manual file:", manual_path, "\n")
cat("Model file:", model_path, "\n")
cat("Joined rows:", nrow(paired), "\n")
cat("Label columns compared:", length(label_cols), "\n")
cat("Saved metrics to:", output_path, "\n")
