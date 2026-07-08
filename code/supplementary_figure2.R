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

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
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
# print(agreement_chain, n = Inf)
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
  ) %>%
  mutate(generation_f = factor(generation))

FigA2A <- ggplot(
  agreement_chain,
  aes(
    x = treatment_appeal, y = agreement_selected_demonstrator,
    color = treatment_appeal
  )
) +
  geom_errorbar(
    data = agreement_pooled_summary,
    aes(
      x = treatment_appeal,
      ymin = agreement_mean - agreement_se,
      ymax = agreement_mean + agreement_se,
      color = I(treatment_colors_dark[treatment_appeal])
    ),
    inherit.aes = FALSE,
    width = 0.2,
    linewidth = 0.7
  ) +
  geom_point(
    data = agreement_pooled_summary,
    aes(x = treatment_appeal, y = agreement_mean, color = treatment_appeal),
    inherit.aes = FALSE,
    size = 3
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = "Treatment",
    y = "Agreement on selected demonstrator\n",
    color = NULL
  ) +
  base_theme +
  theme(legend.position = "none")

FigA2B <- ggplot(
  agreement_generation_summary,
  aes(
    x = generation_f,
    y = agreement_mean,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_line(linewidth = 0.6) +
  geom_errorbar(
    aes(ymin = agreement_mean - agreement_se, ymax = agreement_mean + agreement_se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2,
    linewidth = 0.7
  ) +
  geom_point(size = 2.8) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(breaks = as.character(sort(unique(agreement_generation_summary$generation)))) +
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

fig.width <- 10
fig.height <- 5

ggsave(
  "output/supplementary_figure2.png",
  FigA2,
  width = fig.width,
  height = fig.height,
  dpi = 300
)
ggsave(
  "output/supplementary_figure2.pdf",
  FigA2,
  width = fig.width,
  height = fig.height,
  device = grDevices::cairo_pdf
)
# ggsave(
#   "output/supplementary_figure2.svg",
#   FigA2,
#   width = fig.width,
#   height = fig.height,
#   device = svglite::svglite
# )
