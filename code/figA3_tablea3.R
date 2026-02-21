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

solution_distance <- function(solution_a, solution_b) {
  if (is.na(solution_a) || is.na(solution_b) || solution_a == "" || solution_b == "") {
    return(NA_real_)
  }
  sum(abs(solution_vector(solution_a) - solution_vector(solution_b)))
}

df_long <- read_csv("data/df_long.csv") %>%
  select(all_of(c(
    "participant_code", "chain_code", "treatment_appeal", "generation", "period",
    "selected_feedback_grid_state_flatten", "grid_state_flatten"
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
    inherited_solution = first(selected_feedback_grid_state_flatten),
    first_trial_solution = grid_state_flatten[period == 1][1],
    transmitted_solution = grid_state_flatten[period == 6][1],
    copied_first_trial = first_trial_solution == inherited_solution,
    distance_inherited_first_trial = if_else(
      generation[1] >= 2,
      solution_distance(inherited_solution, first_trial_solution),
      NA_real_
    ),
    distance_inherited_transmitted = if_else(
      generation[1] >= 2,
      solution_distance(inherited_solution, transmitted_solution),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    chain_code = factor(chain_code),
    copied_first_trial = if_else(generation >= 2, copied_first_trial, NA)
  )



social_influence_summary <- participant_metrics %>%
  filter(generation %in% 2:4) %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    n_participants = n(),
    copy_probability_mean = mean(as.numeric(copied_first_trial), na.rm = TRUE),
    copy_probability_sd = sd(as.numeric(copied_first_trial), na.rm = TRUE),
    copy_probability_se = copy_probability_sd / sqrt(n_participants),
    inherited_to_first_trial_distance_mean = mean(distance_inherited_first_trial, na.rm = TRUE),
    inherited_to_first_trial_distance_sd = sd(distance_inherited_first_trial, na.rm = TRUE),
    inherited_to_first_trial_distance_se = inherited_to_first_trial_distance_sd / sqrt(n_participants),
    inherited_to_transmitted_distance_mean = mean(distance_inherited_transmitted, na.rm = TRUE),
    inherited_to_transmitted_distance_sd = sd(distance_inherited_transmitted, na.rm = TRUE),
    inherited_to_transmitted_distance_se = inherited_to_transmitted_distance_sd / sqrt(n_participants),
    .groups = "drop"
  )

treatment_level_summary <- participant_metrics %>%
  filter(generation %in% 2:4) %>%
  group_by(treatment_appeal) %>%
  summarise(
    n_participants = n(),
    copy_probability_mean = mean(as.numeric(copied_first_trial), na.rm = TRUE),
    copy_probability_sd = sd(as.numeric(copied_first_trial), na.rm = TRUE),
    copy_probability_se = copy_probability_sd / sqrt(n_participants),
    inherited_to_first_trial_distance_mean = mean(distance_inherited_first_trial, na.rm = TRUE),
    inherited_to_first_trial_distance_sd = sd(distance_inherited_first_trial, na.rm = TRUE),
    inherited_to_first_trial_distance_se = inherited_to_first_trial_distance_sd / sqrt(n_participants),
    inherited_to_transmitted_distance_mean = mean(distance_inherited_transmitted, na.rm = TRUE),
    inherited_to_transmitted_distance_sd = sd(distance_inherited_transmitted, na.rm = TRUE),
    inherited_to_transmitted_distance_se = inherited_to_transmitted_distance_sd / sqrt(n_participants),
    .groups = "drop"
  )

FigA3A <- ggplot(
  treatment_level_summary,
  aes(
    x = treatment_appeal,
    y = copy_probability_mean,
    color = treatment_appeal
  )
) +
  geom_col(aes(fill = treatment_appeal), width = 0.7, alpha = 0.9, linewidth = 0.3) +
  geom_errorbar(
    aes(
      ymin = copy_probability_mean - copy_probability_se,
      ymax = copy_probability_mean + copy_probability_se
    ),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Treatment",
    y = "Prob. copying demonstrator at trial 1",
    color = NULL
  ) +
  base_theme

FigA3B <- ggplot(
  social_influence_summary,
  aes(
    x = generation,
    y = copy_probability_mean,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  geom_linerange(
    aes(
      ymin = copy_probability_mean - copy_probability_se,
      ymax = copy_probability_mean + copy_probability_se
    ),
    linewidth = 0.4
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(social_influence_summary$generation))) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Generation",
    y = "Probab. copying demonstrator at trial 1",
    color = NULL
  ) +
  base_theme

FigA3C <- ggplot(
  treatment_level_summary,
  aes(
    x = treatment_appeal,
    y = inherited_to_first_trial_distance_mean,
    color = treatment_appeal
  )
) +
  geom_col(aes(fill = treatment_appeal), width = 0.7, alpha = 0.9, linewidth = 0.3) +
  geom_errorbar(
    aes(
      ymin = inherited_to_first_trial_distance_mean - inherited_to_first_trial_distance_se,
      ymax = inherited_to_first_trial_distance_mean + inherited_to_first_trial_distance_se
    ),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 15),
    breaks = seq(0, 15, by = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Treatment",
    y = "Distance: inherited to first trial",
    color = NULL
  ) +
  base_theme

FigA3D <- ggplot(
  social_influence_summary,
  aes(
    x = generation,
    y = inherited_to_first_trial_distance_mean,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  geom_linerange(
    aes(
      ymin = inherited_to_first_trial_distance_mean - inherited_to_first_trial_distance_se,
      ymax = inherited_to_first_trial_distance_mean + inherited_to_first_trial_distance_se
    ),
    linewidth = 0.4
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(social_influence_summary$generation))) +
  scale_y_continuous(
    limits = c(0, 15),
    breaks = seq(0, 15, by = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Generation",
    y = "Distance: inherited to first trial",
    color = NULL
  ) +
  base_theme

FigA3E <- ggplot(
  treatment_level_summary,
  aes(
    x = treatment_appeal,
    y = inherited_to_transmitted_distance_mean,
    color = treatment_appeal
  )
) +
  geom_col(aes(fill = treatment_appeal), width = 0.7, alpha = 0.9, linewidth = 0.3) +
  geom_errorbar(
    aes(
      ymin = inherited_to_transmitted_distance_mean - inherited_to_transmitted_distance_se,
      ymax = inherited_to_transmitted_distance_mean + inherited_to_transmitted_distance_se
    ),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 15),
    breaks = seq(0, 15, by = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Treatment",
    y = "Distance: inherited to transmitted",
    color = NULL
  ) +
  base_theme

FigA3F <- ggplot(
  social_influence_summary,
  aes(
    x = generation,
    y = inherited_to_transmitted_distance_mean,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  geom_linerange(
    aes(
      ymin = inherited_to_transmitted_distance_mean - inherited_to_transmitted_distance_se,
      ymax = inherited_to_transmitted_distance_mean + inherited_to_transmitted_distance_se
    ),
    linewidth = 0.4
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(social_influence_summary$generation))) +
  scale_y_continuous(
    limits = c(0, 15),
    breaks = seq(0, 15, by = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Generation",
    y = "Distance: inherited to transmitted",
    color = NULL
  ) +
  base_theme

FigA3 <- (FigA3A + FigA3C + FigA3E) /
  (FigA3B + FigA3D + FigA3F) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c("A", "C", "E", "B", "D", "F"))) &
  theme(legend.position = "none")

ggsave(
  "figures/figA3.png",
  FigA3,
  width = 16,
  height = 9,
  dpi = 300
)



#
participant_metrics %>%
  arrange(treatment_appeal, chain_code, generation) %>%
  filter(generation >= 2) %>%
  group_by(treatment_appeal) %>%
  summarise(
    copy_probability = mean(as.numeric(copied_first_trial), na.rm = TRUE),
    distance_inherited_first_trial_mean = mean(distance_inherited_first_trial, na.rm = TRUE),
    distance_inherited_first_trial_sd = sd(distance_inherited_first_trial, na.rm = TRUE),
    distance_inherited_transmitted_mean = mean(distance_inherited_transmitted, na.rm = TRUE),
    distance_inherited_transmitted_sd = sd(distance_inherited_transmitted, na.rm = TRUE),
    n_participants = n()
  )

# test for difference in copy probability between treatments using lmer with chain_code as random intercept, and treatment_appeal as fixed effect
library(lme4)
participant_metrics_test <- participant_metrics %>%
  mutate(copied_first_trial_num = as.numeric(copied_first_trial))

# Table A3a pooled models
copy_prob_model_treatment <- lmer(
  copied_first_trial_num ~ treatment_appeal + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

copy_prob_model_additive <- lmer(
  copied_first_trial_num ~ treatment_appeal + generation + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

copy_prob_model_interaction <- lmer(
  copied_first_trial_num ~ treatment_appeal * generation + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

# Table A3b pooled models
distance_first_trial_model_treatment <- lmer(
  distance_inherited_first_trial ~ treatment_appeal + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

distance_first_trial_model_additive <- lmer(
  distance_inherited_first_trial ~ treatment_appeal + generation + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

distance_first_trial_model_interaction <- lmer(
  distance_inherited_first_trial ~ treatment_appeal * generation + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

# Table A3c pooled models
distance_transmitted_model_treatment <- lmer(
  distance_inherited_transmitted ~ treatment_appeal + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

distance_transmitted_model_additive <- lmer(
  distance_inherited_transmitted ~ treatment_appeal + generation + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

distance_transmitted_model_interaction <- lmer(
  distance_inherited_transmitted ~ treatment_appeal * generation + (1 | chain_code),
  data = participant_metrics_test,
  REML = FALSE
)

library(sjPlot)

out_a <- tempfile(fileext = ".html")
out_b <- tempfile(fileext = ".html")
out_c <- tempfile(fileext = ".html")

print(
  tab_model(
    copy_prob_model_treatment,
    copy_prob_model_additive,
    copy_prob_model_interaction,
    show.ci = FALSE,
    show.se = TRUE,
    show.re.var = FALSE,
    dv.labels = c("Pooled (Treatment)", "Pooled (Treatment + generation)", "Pooled (Treatment × generation)"),
    title = "Table A3a) Copy probability",
    file = out_a
  )
)

print(
  tab_model(
    distance_first_trial_model_treatment,
    distance_first_trial_model_additive,
    distance_first_trial_model_interaction,
    show.ci = FALSE,
    show.se = TRUE,
    show.re.var = FALSE,
    dv.labels = c("Pooled (Treatment)", "Pooled (Treatment + generation)", "Pooled (Treatment × generation)"),
    title = "Table A3b) Distance: inherited to first trial",
    file = out_b
  )
)

print(
  tab_model(
    distance_transmitted_model_treatment,
    distance_transmitted_model_additive,
    distance_transmitted_model_interaction,
    show.ci = FALSE,
    show.se = TRUE,
    show.re.var = FALSE,
    dv.labels = c("Pooled (Treatment)", "Pooled (Treatment + generation)", "Pooled (Treatment × generation)"),
    title = "Table A3c) Distance: inherited to transmitted",
    file = out_c
  )
)

combined_out <- "figures/table_a3.html"
combined_lines <- c(
  readLines(out_a, warn = FALSE),
  "<br/>",
  readLines(out_b, warn = FALSE),
  "<br/>",
  readLines(out_c, warn = FALSE)
)
writeLines(combined_lines, combined_out)

file.remove(out_a, out_b, out_c)
