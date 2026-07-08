library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
    mutate(
        generation = as.integer(generation),
        generation_c = as.integer(generation_c),
        period = as.integer(period),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) %>%
    filter(treatment_appeal == "high_appeal")

df_advice <- read_csv("data/df_advice_manual_coding.csv", show_col_types = FALSE)

df_advice_categories <- df_advice %>%
    select(participant_code, mix_and_match:other) %>%
    rename_with(~ paste0("selected_", .), -participant_code) %>%
    rename(selected_advice_author = participant_code)

participant_metrics <- df_long %>%
    group_by(participant_code, chain_code, treatment_appeal, generation, selected_feedback_author) %>%
    arrange(period, .by_group = TRUE) %>%
    summarise(
        generation_c = first(generation_c),
        within_individual_solution_distance = mean_pairwise_distance(grid_state_flatten),
        .groups = "drop"
    ) %>%
    mutate(
        generation_c2 = generation - 2,
        chain_code = factor(chain_code),
        selected_advice_author = selected_feedback_author
    ) %>%
    left_join(df_advice_categories, by = "selected_advice_author") %>%
    filter(generation > 1) %>%
    mutate(
        selected_mix_and_match = coalesce(as.numeric(selected_mix_and_match), 0)
    )

set.seed(42)

model_mix_additive <- lmer(
    within_individual_solution_distance ~ selected_mix_and_match + generation_c2 + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

model_mix_interaction <- lmer(
    within_individual_solution_distance ~ selected_mix_and_match * generation_c2 + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

table6 <- bind_rows(
    extract_fixed_effects(model_mix_additive, "Within-individual distance: additive"),
    extract_fixed_effects(model_mix_interaction, "Within-individual distance: interaction")
)

table6 <- add_model_spacer_rows(table6)

write_csv(table6, "output/supplementary_table6.csv")
write_simple_html_table(table6, "output/supplementary_table6.html")
message("Wrote output/supplementary_table6.csv and output/supplementary_table6.html")
