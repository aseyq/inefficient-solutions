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
df_long <- read_csv("data/processed/df_long_processed.csv")

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
  mutate(se = sd / sqrt(n), generation_f = factor(generation)) %>%
  ggplot(aes(x = generation_f, y = mean, color = treatment_appeal, group = treatment_appeal)) +
  geom_violin(
    data = df_long_feedbacks_apps %>% mutate(generation_f = factor(generation)),
    aes(
      x = generation_f,
      y = plants_treated,
      fill = treatment_appeal,
      group = interaction(generation_f, treatment_appeal)
    ),
    inherit.aes = FALSE,
    position = position_dodge(width = 0.45),
    width = 1,
    alpha = 0.18,
    color = NA,
    linewidth = 0.0,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_line(aes(group = treatment_appeal), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(size = 3) +
  ## integers in x axis
  scale_x_discrete(breaks = as.character(1:4)) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  scale_fill_manual(labels = treatment_names, values = treatment_colors, guide = "none") +
  labs(x = "Generation", y = "Plants treated") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )



## Blue Nutrients
FigA1B <- df_long_feedbacks_apps %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    mean = mean(n_blues),
    sd = sd(n_blues),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n), generation_f = factor(generation)) %>%
  ggplot(aes(x = generation_f, y = mean, color = treatment_appeal, group = treatment_appeal)) +
  geom_violin(
    data = df_long_feedbacks_apps %>% mutate(generation_f = factor(generation)),
    aes(
      x = generation_f,
      y = n_blues,
      fill = treatment_appeal,
      group = interaction(generation_f, treatment_appeal)
    ),
    inherit.aes = FALSE,
    position = position_dodge(width = 0.45),
    width = 1,
    alpha = 0.18,
    color = NA,
    linewidth = 0.0,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_line(aes(group = treatment_appeal), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(size = 3) +
  ## line for optimum payoff
  ## integers in x axis
  scale_x_discrete(breaks = as.character(1:4)) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  scale_fill_manual(labels = treatment_names, values = treatment_colors, guide = "none") +
  labs(x = "Generation", y = "Number of blue nutrients") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )


## N nutrients

FigA1C <- df_long_feedbacks_apps %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    mean = mean(n_apps),
    sd = sd(n_apps),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n), generation_f = factor(generation)) %>%
  ggplot(aes(x = generation_f, y = mean, color = treatment_appeal, group = treatment_appeal)) +
  geom_violin(
    data = df_long_feedbacks_apps %>% mutate(generation_f = factor(generation)),
    aes(
      x = generation_f,
      y = n_apps,
      fill = treatment_appeal,
      group = interaction(generation_f, treatment_appeal)
    ),
    inherit.aes = FALSE,
    position = position_dodge(width = 0.45),
    width = 1,
    alpha = 0.18,
    color = NA,
    linewidth = 0.0,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_line(aes(group = treatment_appeal), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(size = 3) +
  ## integers in x axis
  scale_x_discrete(breaks = as.character(1:4)) +
  scale_y_continuous(breaks = seq(1, 16, 1), limits = c(9, 16)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  scale_fill_manual(labels = treatment_names, values = treatment_colors, guide = "none") +
  labs(x = "Generation", y = "Number of nutrients") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )

FigA1 <- wrap_plots(FigA1A, FigA1B, FigA1C) +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "A")

fig.width <- 14
fig.height <- 4

ggsave("output/supplementary_figure1.png", plot = FigA1, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure1.pdf", plot = FigA1, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure1.svg", plot = FigA1, width = fig.width, height = fig.height, device = svglite::svglite)
