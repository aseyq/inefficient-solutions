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

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

count_blues_apps <- function(x) {
  if (is.null(x) || is.na(x) || x == "") {
    return(list(n_blues = NA_integer_, n_apps = NA_integer_))
  }
  
  grid <- jsonlite::fromJSON(
    x,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  
  # grid is 3 lists (rows), each containing 3 cells; flatten one level to 9 cells
  cells <- unlist(grid, recursive = FALSE)
  
  getv <- function(cell, nm) as.integer(cell[[nm]] %||% 0L)
  
  b <- sum(vapply(cells, getv, integer(1), nm = "blue"))
  y <- sum(vapply(cells, getv, integer(1), nm = "yellow"))
  r <- sum(vapply(cells, getv, integer(1), nm = "red"))
  
  list(n_blues = b, n_apps = b + y + r)
}

# If any rows contain malformed JSON, this will return NA instead of erroring
count_blues_apps_safe <- purrr::possibly(count_blues_apps,
                                         otherwise = list(n_blues = NA_integer_, n_apps = NA_integer_))

df_long_feedbacks_apps <- df_long_feedbacks_apps %>%
  mutate(tmp = map(grid_state, count_blues_apps_safe)) %>%
  unnest_wider(tmp)

## Plants treated
FigA1A <- df_long_feedbacks_apps %>% 
  group_by(treatment_appeal, generation)  %>%
  summarise(mean = mean(plants_treated),
            sd = sd(plants_treated),
            n = n(),
            .groups = "drop"
  )  %>%
  mutate(se = sd/sqrt(n))  %>%
  ggplot(aes(x=generation, y=mean, color=treatment_appeal, group=interaction(treatment_appeal,generation))) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.2) +
  geom_line(aes(group=treatment_appeal), linewidth = 0.5) +
  ## line for optimum payoff
  #geom_hline(yintercept = 9, linetype = "dashed", color="green", size=1, alpha=0.5) +
  #geom_hline(yintercept = 7, linetype = "dashed", color="orange", size=1, alpha=0.5) +
  ## integers in x axis
  scale_x_continuous(breaks = seq(1, 4, 1)) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  labs(x = "Generation", y = "Plants treated") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "none")

# ggsave("figures/figA1A.png", plot = FigA1A, width = 5, height = 5, dpi = 300)


## Blue Nutrients
FigA1B <- df_long_feedbacks_apps %>% 
  group_by(treatment_appeal, generation)  %>%
  summarise(mean = mean(n_blues),
            sd = sd(n_blues),
            n = n(),
            .groups = "drop"
  )  %>%
  mutate(se = sd/sqrt(n))  %>%
  ggplot(aes(x=generation, y=mean, color=treatment_appeal, group=interaction(treatment_appeal,generation))) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.2) +
  geom_line(aes(group=treatment_appeal), linewidth = 0.5) +
  ## line for optimum payoff
  #geom_hline(yintercept = 9, linetype = "dashed", color="green", size=1, alpha=0.5) +
  #geom_hline(yintercept = 7, linetype = "dashed", color="orange", size=1, alpha=0.5) +
  ## integers in x axis
  scale_x_continuous(breaks = seq(1, 4, 1)) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  labs(x = "Generation", y = "Number of blue nutrients") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "top")

# ggsave("figures/figA1B.png", plot = FigA1B, width = 5, height = 5, dpi = 300)

## N nutrients

FigA1C <- df_long_feedbacks_apps %>% 
  group_by(treatment_appeal, generation)  %>%
  summarise(mean = mean(n_apps),
            sd = sd(n_apps),
            n = n(),
            .groups = "drop"
  )  %>%
  mutate(se = sd/sqrt(n))  %>%
  ggplot(aes(x=generation, y=mean, color=treatment_appeal, group=interaction(treatment_appeal,generation))) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.2) +
  geom_line(aes(group=treatment_appeal), linewidth = 0.5) +
  ## line for optimum payoff
  #geom_hline(yintercept = 9, linetype = "dashed", color="green", size=1, alpha=0.5) +
  #geom_hline(yintercept = 7, linetype = "dashed", color="orange", size=1, alpha=0.5) +
  ## integers in x axis
  scale_x_continuous(breaks = seq(1, 4, 1)) +
  scale_y_continuous(breaks = seq(1, 16, 1), limits = c(9, 16)) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  labs(x = "Generation", y = "Number of nutrients") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "none")

# ggsave("figures/figA1C.png", plot = FigA1C, width = 5, height = 5, dpi = 300)

FigA1 <- wrap_plots(FigA1A, FigA1B, FigA1C) +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = 'A')

ggsave("figures/figA1.png", plot = FigA1, width = 10.5, height = 3.5, dpi = 300)
