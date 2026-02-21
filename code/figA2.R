library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

pairwise_agreement <- function(values) {
  if (length(values) < 2) {
    return(NA_real_)
  }
  pairs <- combn(values, 2, simplify = FALSE)
  mean(vapply(pairs, function(x) as.numeric(x[1] == x[2]), numeric(1)))
}

df_long <- read_csv("data/df_long.csv") %>%
  select(all_of(c(
    "participant_code", "chain_code", "treatment_appeal", "generation", "period",
    "selected_feedback_author"
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
    inherited_author = first(selected_feedback_author),
    .groups = "drop"
  )

agreement_chain <- participant_metrics %>%
  filter(generation %in% 2:4) %>%
  group_by(chain_code, treatment_appeal, generation) %>%
  summarise(
    agreement_selected_demonstrator = pairwise_agreement(inherited_author),
    .groups = "drop"
  )

agreement_pooled_summary <- agreement_chain %>%
  group_by(treatment_appeal) %>%
  summarise(
    n_chain_generation = n(),
    n_chains = n_distinct(chain_code),
    agreement_mean = mean(agreement_selected_demonstrator, na.rm = TRUE),
    agreement_sd = sd(agreement_selected_demonstrator, na.rm = TRUE),
    agreement_se = agreement_sd / sqrt(n_chain_generation),
    .groups = "drop"
  )

agreement_generation_summary <- agreement_chain %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    n_chain_generation = n(),
    agreement_mean = mean(agreement_selected_demonstrator, na.rm = TRUE),
    agreement_sd = sd(agreement_selected_demonstrator, na.rm = TRUE),
    agreement_se = agreement_sd / sqrt(n_chain_generation),
    .groups = "drop"
  )

FigA2A <- ggplot(
  agreement_pooled_summary,
  aes(x = treatment_appeal, y = agreement_mean, fill = treatment_appeal)
) +
  geom_col(width = 0.65, alpha = 0.95) +
  geom_errorbar(
    aes(ymin = agreement_mean - agreement_se, ymax = agreement_mean + agreement_se),
    width = 0.15,
    linewidth = 0.45
  ) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = "Treatment",
    y = "Agreement on selected demonstrator\n",
    fill = NULL
  ) +
  base_theme +
  theme(legend.position = "none")

FigA2B <- ggplot(
  agreement_generation_summary,
  aes(
    x = generation,
    y = agreement_mean,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  geom_linerange(
    aes(ymin = agreement_mean - agreement_se, ymax = agreement_mean + agreement_se),
    linewidth = 0.4
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(agreement_generation_summary$generation))) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = "Generation",
    y = "Agreement on selected demonstrator",
    color = NULL
  ) +
  base_theme +
  theme(legend.position = "top")

FigA2 <- (FigA2A + FigA2B) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top")

ggsave(
  "figures/figA2.png",
  FigA2,
  width = 10,
  height = 5,
  dpi = 300
)
