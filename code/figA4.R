library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

# Cache parsed solution vectors so repeated strings are converted only once.
solution_cache <- new.env(parent = emptyenv())

solution_vector <- function(solution) {
  if (is.na(solution) || solution == "") {
    return(rep(NA_integer_, 27))
  }
  # Reuse cached vector when this solution string has already been parsed.
  if (exists(solution, envir = solution_cache, inherits = FALSE)) {
    return(get(solution, envir = solution_cache, inherits = FALSE))
  }
  tokens <- strsplit(solution, "-", fixed = TRUE)[[1]]
  vec <- as.integer(strsplit(paste(tokens, collapse = ""), "", fixed = TRUE)[[1]])
  # Store parsed vector in cache for future lookups.
  assign(solution, vec, envir = solution_cache)
  vec
}

mean_pairwise_distance <- function(solutions) {
  if (length(solutions) < 2) {
    return(NA_real_)
  }
  mat <- do.call(rbind, lapply(solutions, solution_vector))
  mean(as.numeric(dist(mat, method = "manhattan")))
}

df_long <- read_csv("data/df_long.csv") %>%
  select(all_of(c(
    "participant_code", "chain_code", "treatment_appeal", "generation", "period",
    "grid_state_flatten"
  ))) %>%
  mutate(
    generation = as.integer(generation),
    period = as.integer(period),
    treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
  )


participant_metrics <- df_long %>%
  group_by(participant_code, chain_code, treatment_appeal, generation) %>%
  arrange(period, .by_group = TRUE) %>%
  summarise(
    within_individual_solution_distance = mean_pairwise_distance(grid_state_flatten),
    .groups = "drop"
  ) %>%
  mutate(chain_code = factor(chain_code))


individual_exploration_summary <- participant_metrics %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    n_participants = n(),
    within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
    within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
    within_distance_se = within_distance_sd / sqrt(n_participants),
    .groups = "drop"
  )

treatment_level_summary <- participant_metrics %>%
  group_by(treatment_appeal) %>%
  summarise(
    n_participants = n(),
    within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
    within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
    within_distance_se = within_distance_sd / sqrt(n_participants),
    .groups = "drop"
  )

FigA4A <- ggplot(
  treatment_level_summary,
  aes(
    x = treatment_appeal,
    y = within_distance_mean,
    color = treatment_appeal
  )
) +
  geom_col(aes(fill = treatment_appeal), width = 0.7, alpha = 0.9, linewidth = 0.3) +
  geom_errorbar(
    aes(
      ymin = within_distance_mean - within_distance_se,
      ymax = within_distance_mean + within_distance_se
    ),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 17),
    breaks = seq(0, 17, by = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Treatment",
    y = "Within-individual solution distance",
    color = NULL
  ) +
  base_theme

FigA4B <- ggplot(
  individual_exploration_summary,
  aes(
    x = generation,
    y = within_distance_mean,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  geom_linerange(
    aes(
      ymin = within_distance_mean - within_distance_se,
      ymax = within_distance_mean + within_distance_se
    ),
    linewidth = 0.4
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(individual_exploration_summary$generation))) +
  scale_y_continuous(
    limits = c(0, 17),
    breaks = seq(0, 17, by = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Generation",
    y = "Within-individual solution distance",
    color = NULL
  ) +
  base_theme

FigA4 <- (FigA4A + FigA4B) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")

ggsave(
  "figures/figA4.png",
  FigA4,
  width = 10.2,
  height = 4.6,
  dpi = 300
)
