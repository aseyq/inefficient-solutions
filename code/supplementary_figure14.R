library(tidyverse)
library(patchwork)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

base_theme <- make_base_theme()

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
  mutate(
    generation = as.integer(generation),
    period = as.integer(period),
    treatment_appeal = factor(treatment_appeal, levels = c("low_appeal", "high_appeal")),
    chain_code = as.character(chain_code)
  )

chain_order_path <- "data/processed/chain_order.csv"
if (!file.exists(chain_order_path)) {
  stop("Missing required file: ", chain_order_path, ". Run code/_make_chain_order.R first.")
}
chain_order <- read_csv(chain_order_path, show_col_types = FALSE)

df_chain_scores <- df_long %>%
  filter(period == 6) %>%
  left_join(chain_order, by = c("treatment_appeal", "chain_code")) %>%
  mutate(
    chain_label = factor(paste("Chain", chain_rank), levels = paste("Chain", 1:20))
  )

chain_generation_average <- df_chain_scores %>%
  group_by(treatment_appeal, chain_label, generation) %>%
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
      aes(x = generation, y = mean_score, group = chain_label),
      inherit.aes = FALSE,
      color = "#2E7D32",
      linewidth = 1.0,
      alpha = 0.95
    ) +
    facet_wrap(
      ~chain_label,
      ncol = 10
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

FigA14a <- (plot_score_treatment("low_appeal") / plot_score_treatment("high_appeal")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# ggsave(
#   "figures/figA14a.png",
#   FigA14a,
#   width = 14,
#   height = 9,
#   dpi = 300
# )

df_chain_cost <- df_long %>%
  filter(period == 6) %>%
  left_join(chain_order, by = c("treatment_appeal", "chain_code")) %>%
  mutate(
    chain_label = factor(paste("Chain", chain_rank), levels = paste("Chain", 1:20))
  )

chain_generation_average_cost <- df_chain_cost %>%
  group_by(treatment_appeal, chain_label, generation) %>%
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
      aes(x = generation, y = mean_cost, group = chain_label),
      inherit.aes = FALSE,
      color = "#C62828",
      linewidth = 1.0,
      alpha = 0.95
    ) +
    facet_wrap(
      ~chain_label,
      ncol = 10
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

FigA14b <- (plot_cost_treatment("low_appeal") / plot_cost_treatment("high_appeal")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# ggsave(
#   "figures/figA14b.png",
#   FigA14b,
#   width = 14,
#   height = 9,
#   dpi = 300
# )

# Combined figure
FigA14 <- wrap_plots(
  wrap_elements(FigA14a),
  wrap_elements(FigA14b),
  ncol = 1
) +
  plot_annotation(tag_levels = "A")

fig.width <- 12
fig.height <- 15

ggsave(
  "output/supplementary_figure14.png",
  FigA14,
  width = fig.width,
  height = fig.height,
  dpi = 300
)
ggsave(
  "output/supplementary_figure14.pdf",
  FigA14,
  width = fig.width,
  height = fig.height,
  device = grDevices::cairo_pdf
)
ggsave(
  "output/supplementary_figure14.svg",
  FigA14,
  width = fig.width,
  height = fig.height,
  device = svglite::svglite
)
