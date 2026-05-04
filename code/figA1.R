library(tidyverse)
library(kableExtra)
library(lme4)
library(lmerTest)
library(knitr)
library(sjPlot)
library(dplyr)
library(ggplot2)
library(scales)
library(purrr)
library(jsonlite)
library(patchwork)
source("code/_helpers.R")

# get data
df_long <- read_csv("data/df_long.csv")


df_long <- df_long %>%
  mutate(net_payoff = 72 + 80 * plants_treated - cost)
# set theme
theme_set(theme_bw())

df_long_feedbacks <- df_long %>%
  filter(period == 6)

df_long_feedbacks_apps <- df_long_feedbacks %>%
  select(participant_code, treatment_appeal, generation, grid_state, plants_treated)

################

df_long_feedbacks_apps <- df_long_feedbacks_apps %>%
  mutate(tmp = map(grid_state, count_blues_apps_safe)) %>%
  unnest_wider(tmp)

## Plants treated
FigA1A <- df_long_feedbacks_apps %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    mean = mean(plants_treated),
    sd = sd(plants_treated),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  ggplot(aes(x = generation, y = mean, color = treatment_appeal, group = interaction(treatment_appeal, generation))) +
  geom_line(aes(group = treatment_appeal), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(size = 3) +
  ## line for optimum payoff
  # geom_hline(yintercept = 9, linetype = "dashed", color="green", size=1, alpha=0.5) +
  # geom_hline(yintercept = 7, linetype = "dashed", color="orange", size=1, alpha=0.5) +
  ## integers in x axis
  scale_x_continuous(breaks = seq(1, 4, 1)) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  labs(x = "Generation", y = "Plants treated") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )

# ggsave("figures/figA1A.png", plot = FigA1A, width = 5, height = 5, dpi = 300)


## Blue Nutrients
FigA1B <- df_long_feedbacks_apps %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    mean = mean(n_blues),
    sd = sd(n_blues),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  ggplot(aes(x = generation, y = mean, color = treatment_appeal, group = interaction(treatment_appeal, generation))) +
  geom_line(aes(group = treatment_appeal), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(size = 3) +
  ## line for optimum payoff
  # geom_hline(yintercept = 9, linetype = "dashed", color="green", size=1, alpha=0.5) +
  # geom_hline(yintercept = 7, linetype = "dashed", color="orange", size=1, alpha=0.5) +
  ## integers in x axis
  scale_x_continuous(breaks = seq(1, 4, 1)) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  labs(x = "Generation", y = "Number of blue nutrients") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )

# ggsave("figures/figA1B.png", plot = FigA1B, width = 5, height = 5, dpi = 300)

## N nutrients

FigA1C <- df_long_feedbacks_apps %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    mean = mean(n_apps),
    sd = sd(n_apps),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  ggplot(aes(x = generation, y = mean, color = treatment_appeal, group = interaction(treatment_appeal, generation))) +
  geom_line(aes(group = treatment_appeal), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(size = 3) +
  ## line for optimum payoff
  # geom_hline(yintercept = 9, linetype = "dashed", color="green", size=1, alpha=0.5) +
  # geom_hline(yintercept = 7, linetype = "dashed", color="orange", size=1, alpha=0.5) +
  ## integers in x axis
  scale_x_continuous(breaks = seq(1, 4, 1)) +
  scale_y_continuous(breaks = seq(1, 16, 1), limits = c(9, 16)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  labs(x = "Generation", y = "Number of nutrients") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )

# ggsave("figures/figA1C.png", plot = FigA1C, width = 5, height = 5, dpi = 300)

FigA1 <- wrap_plots(FigA1A, FigA1B, FigA1C) +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "A")

ggsave("figures/figA1.png", plot = FigA1, width = 10.5, height = 4, dpi = 300)
