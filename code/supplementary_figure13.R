library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(grid)
library(jsonlite)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

df_long <- read_csv("data/processed/df_long_processed.csv", show_col_types = FALSE)

# Compute per-color nutrient proportions via parse_grid_safe
df_long <- df_long %>%
  mutate(tmp = map(grid_state, parse_grid_safe)) %>%
  unnest_wider(tmp) %>%
  mutate(
    n_total     = n_blue + n_yellow + n_red,
    prop_blue   = if_else(n_total > 0, n_blue / n_total, NA_real_),
    prop_yellow = if_else(n_total > 0, n_yellow / n_total, NA_real_),
    prop_red    = if_else(n_total > 0, n_red / n_total, NA_real_)
  )

# Chain ordering from code/_make_chain_order.R
chain_order_path <- "data/processed/chain_order.csv"
if (!file.exists(chain_order_path)) {
  stop("Missing required file: ", chain_order_path, ". Run code/_make_chain_order.R first.")
}
chain_order <- read_csv(chain_order_path, show_col_types = FALSE)

# --- Data preparation ---
df_plot <- df_long %>%
  filter(period <= 6, generation <= 4) %>%
  arrange(treatment_appeal, chain_code, generation, participant_code, period) %>%
  group_by(treatment_appeal, chain_code, generation) %>%
  mutate(individual_id = as.numeric(factor(participant_code))) %>%
  ungroup() %>%
  left_join(chain_order, by = c("treatment_appeal", "chain_code")) %>%
  mutate(y = (chain_num_plot - 1) * 6 + individual_id)

# y-axis: one label per chain at the vertical midpoint of its block
y_scale <- df_plot %>%
  group_by(chain_num_plot, chain_rank) %>%
  summarise(y_mid = mean(y), .groups = "drop") %>%
  mutate(label = paste("Chain", chain_rank))

# --- Palettes ---
blues <- colorRampPalette(brewer.pal(9, "Blues"))(100)
yellows <- colorRampPalette(c("white", "#ffcc00"))(100)
reds <- colorRampPalette(brewer.pal(9, "Reds"))(100)

# --- Plot function ---
# limits:   passed to scale_fill_gradientn; defaults to c(0, 1) for proportions
# y_breaks/y_labels: optional chain labels for the y-axis
base_plot <- function(data, fill_var, palette, title, limits = c(0, 1),
                      y_breaks = NULL, y_labels = NULL) {
  ggplot(data, aes(x = period_in_chain, y = y, fill = .data[[fill_var]])) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colours = palette,
      limits = limits,
      name = title
    ) +
    facet_wrap(
      ~treatment_appeal,
      scales = "free_y",
      labeller = as_labeller(treatment_names)
    ) +
    geom_vline(xintercept = c(6.5, 12.5, 18.5), linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
    {
      if (!is.null(y_breaks)) scale_y_continuous(breaks = y_breaks, labels = y_labels)
    } +
    labs(
      x = NULL,
      y = NULL
    ) +
    annotation_custom(
      grob = textGrob(
        "Trial",
        gp = gpar(fontsize = 10),
        y = unit(-1.5, "lines")
      ),
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = -Inf
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12),
      panel.grid = element_blank(),
      axis.text.y = if (is.null(y_labels)) element_blank() else element_text(size = 7),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(size = 10),
      plot.margin = margin(10, 5, 25, 5)
    ) +
    geom_text(
      data = data.frame(
        period_in_chain = c(3.5, 9.5, 15.5, 21.5),
        label = paste("Generation", 1:4),
        y = Inf
      ),
      aes(x = period_in_chain, y = y, label = label),
      vjust = 2,
      size = 2,
      inherit.aes = FALSE
    )
}

# --- Create panels ---
p1 <- base_plot(df_plot, "prop_blue", blues, "Proportion of\nblue nutrients", y_breaks = y_scale$y_mid, y_labels = y_scale$label)
p2 <- base_plot(df_plot, "prop_yellow", yellows, "Proportion of\nyellow nutrients", y_breaks = y_scale$y_mid, y_labels = y_scale$label)
p3 <- base_plot(df_plot, "prop_red", reds, "Proportion of\nred nutrients", y_breaks = y_scale$y_mid, y_labels = y_scale$label)

FigA13 <- p1 / p2 / p3 +
  plot_annotation(tag_levels = "A")

fig.width <- 10
fig.height <- 14

ggsave("output/supplementary_figure13.png", plot = FigA13, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure13.pdf", plot = FigA13, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure13.svg", plot = FigA13, width = fig.width, height = fig.height, device = svglite::svglite)
