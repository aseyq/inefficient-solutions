library(tidyverse)
library(kableExtra)
library(lme4)
library(lmerTest)
library(knitr)
library(sjPlot)
library(dplyr)
library(ggplot2)
library(scales)
source("code/_helpers.R")

# get data
df_long <- read_csv("data/df_long.csv")  

df_long <- df_long %>% 
  mutate(net_payoff = 72 + 80 * plants_treated - cost)
# set theme
theme_set(theme_bw())


#######################

df_long_feedbacks <- df_long %>% 
  filter(period == 6)

common_sols_la <- df_long_feedbacks %>% 
  filter(treatment_appeal == "low_appeal") %>%
  group_by(grid_state_flatten, grid_state, treatment_appeal, cost, net_payoff) %>%
  summarise(n = n(), .groups = "drop") %>% 
  arrange(desc(n)) 

common_sols_ha <- df_long_feedbacks %>%
  filter(treatment_appeal == "high_appeal") %>%
  group_by(grid_state_flatten, grid_state, treatment_appeal, cost, net_payoff) %>%
  summarise(n = n(), .groups = "drop") %>% 
  arrange(desc(n))


make_cum_df <- function(df, label, denom = sum(df$n)) {
  df %>%
    ungroup() %>%                 
    arrange(desc(n)) %>%
    mutate(
      treatment = label,
      rank = seq_len(n()),        
      cum_n = cumsum(n),
      cum_prop = .data$cum_n / denom
    ) %>%
    select(all_of(c("treatment", "rank", "n", "cum_n", "cum_prop")))
}

cum_ha <- make_cum_df(common_sols_ha, "high_appeal")
cum_la <- make_cum_df(common_sols_la, "low_appeal")

cum_all <- bind_rows(cum_ha, cum_la)%>%
  filter(n != 1) # Keep only configs that appear at least twice

Fig4 <- ggplot(cum_all, aes(x = rank, y = cum_prop, colour = treatment, group = treatment)) +
  geom_line(linewidth = 1) +
  geom_point(aes(fill = "white"), shape = 21, size = 7, stroke = 1.5) +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_colour_manual(
    labels = treatment_names,
    values = treatment_colors,
    guide  = guide_legend(
      title = NULL,
      override.aes = list(shape = 16, size = 4)  # filled dots in legend
    )
  ) +
  scale_fill_identity() +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(1, 14), breaks = 1:14) +
  labs(x = "Most common transmitted solutions (ranked by n)", y = "Cumulative share") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top",
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("figures/fig4.png", plot = Fig4, width = 8, height = 6, dpi = 300)

