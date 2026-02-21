library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

df_long <- read_csv("data/df_long.csv") %>%
  mutate(
    generation = as.integer(generation),
    period = as.integer(period),
    net_payoff = 72 + 80 * plants_treated - cost,
    treatment_appeal = factor(treatment_appeal, levels = c("low_appeal", "high_appeal")),
    chain_code = as.character(chain_code)
  )

chain_order <- df_long %>%
  distinct(treatment_appeal, chain_code) %>%
  mutate(
    chain_facet = paste0(
      if_else(treatment_appeal == "low_appeal", "Low", "High"),
      " appeal · Chain ",
      chain_code
    )
  )

df_chain_scores <- df_long %>%
  filter(period == 6) %>%
  left_join(chain_order, by = c("treatment_appeal", "chain_code")) %>%
  mutate(chain_facet = as.character(chain_facet))

chain_slopes <- df_chain_scores %>%
  group_by(treatment_appeal, chain_code, chain_facet) %>%
  summarise(
    slope = coef(lm(net_payoff ~ generation))[2],
    .groups = "drop"
  ) %>%
  arrange(treatment_appeal, desc(slope), chain_code) %>%
  mutate(
    chain_no = row_number(),
    chain_label = paste0("Chain ", chain_no)
  )

chain_label_map <- setNames(chain_slopes$chain_label, chain_slopes$chain_facet)
chain_no_labeller <- as_labeller(chain_label_map)

df_chain_scores <- df_chain_scores %>%
  left_join(
    chain_slopes %>% select(treatment_appeal, chain_code, chain_no, chain_label),
    by = c("treatment_appeal", "chain_code")
  ) %>%
  mutate(
    chain_facet = factor(chain_facet, levels = chain_slopes$chain_facet)
  )

chain_generation_average <- df_chain_scores %>%
  group_by(treatment_appeal, chain_facet, generation) %>%
  summarise(mean_score = mean(net_payoff, na.rm = TRUE), .groups = "drop")

plot_score_treatment <- function(treatment_value) {
  data_t <- df_chain_scores %>%
    filter(treatment_appeal == treatment_value)

  avg_t <- chain_generation_average %>%
    filter(treatment_appeal == treatment_value)

  ggplot(
    data_t,
    aes(x = generation, y = net_payoff, color = treatment_appeal)
  ) +
    geom_point(
      size = 1.9,
      alpha = 0.8
    ) +
    geom_line(
      data = avg_t,
      aes(x = generation, y = mean_score, group = chain_facet),
      inherit.aes = FALSE,
      color = "#2E7D32",
      linewidth = 1.0,
      alpha = 0.95
    ) +
    facet_wrap(
      ~chain_facet,
      ncol = 10,
      labeller = chain_no_labeller
    ) +
    scale_color_manual(
      values = treatment_colors,
      breaks = c("low_appeal", "high_appeal"),
      labels = treatment_names[c("low_appeal", "high_appeal")]
    ) +
    scale_x_continuous(breaks = sort(unique(df_chain_scores$generation))) +
    labs(
      title = treatment_names[[treatment_value]],
      x = "Generation",
      y = "Score of transmitted solutions",
      color = NULL
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 9),
      strip.background = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
    )
}

FigA10a <- (plot_score_treatment("low_appeal") / plot_score_treatment("high_appeal")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# ggsave(
#   "figures/figA10a.png",
#   FigA10a,
#   width = 14,
#   height = 9,
#   dpi = 300
# )

df_chain_cost <- df_long %>%
  filter(period == 6) %>%
  left_join(chain_order, by = c("treatment_appeal", "chain_code")) %>%
  mutate(chain_facet = as.character(chain_facet))

df_chain_cost <- df_chain_cost %>%
  left_join(
    chain_slopes %>% select(treatment_appeal, chain_code, chain_no, chain_label),
    by = c("treatment_appeal", "chain_code")
  ) %>%
  mutate(
    chain_facet = factor(chain_facet, levels = chain_slopes$chain_facet)
  )

chain_generation_average_cost <- df_chain_cost %>%
  group_by(treatment_appeal, chain_facet, generation) %>%
  summarise(mean_cost = mean(cost, na.rm = TRUE), .groups = "drop")

plot_cost_treatment <- function(treatment_value) {
  data_t <- df_chain_cost %>%
    filter(treatment_appeal == treatment_value)

  avg_t <- chain_generation_average_cost %>%
    filter(treatment_appeal == treatment_value)

  ggplot(
    data_t,
    aes(x = generation, y = cost, color = treatment_appeal)
  ) +
    geom_point(
      size = 1.9,
      alpha = 0.8
    ) +
    geom_line(
      data = avg_t,
      aes(x = generation, y = mean_cost, group = chain_facet),
      inherit.aes = FALSE,
      color = "#C62828",
      linewidth = 1.0,
      alpha = 0.95
    ) +
    facet_wrap(
      ~chain_facet,
      ncol = 10,
      labeller = chain_no_labeller
    ) +
    scale_color_manual(
      values = treatment_colors,
      breaks = c("low_appeal", "high_appeal"),
      labels = treatment_names[c("low_appeal", "high_appeal")]
    ) +
    scale_x_continuous(breaks = sort(unique(df_chain_cost$generation))) +
    labs(
      title = treatment_names[[treatment_value]],
      x = "Generation",
      y = "Cost of transmitted solutions",
      color = NULL
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 9),
      strip.background = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
    )
}

FigA10b <- (plot_cost_treatment("low_appeal") / plot_cost_treatment("high_appeal")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# ggsave(
#   "figures/figA10b.png",
#   FigA10b,
#   width = 14,
#   height = 9,
#   dpi = 300
# )

# Combined figure
FigA10 <- wrap_plots(
  wrap_elements(FigA10a),
  wrap_elements(FigA10b),
  ncol = 1
) +
  plot_annotation(tag_levels = "A")

ggsave(
  "figures/figA10.png",
  FigA10,
  width = 12,
  height = 15,
  dpi = 300
)
