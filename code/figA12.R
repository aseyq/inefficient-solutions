library(tidyverse)
library(grid)
library(jsonlite)
source("code/_helpers.R")

df_long <- read_csv("data/df_long.csv", show_col_types = FALSE)

# Compute n_nutrients (sum of all nutrient applications) via parse_grid_safe
df_long <- df_long %>%
  mutate(tmp = map(grid_state, parse_grid_safe)) %>%
  unnest_wider(tmp) %>%
  mutate(n_nutrients = n_blue + n_yellow + n_red)

# Chain ordering from figA10 (sorted by blue-solution status across generations)
chain_order <- read_csv("data/processed/chain_order.csv", show_col_types = FALSE)

df_plot <- df_long %>%
  arrange(treatment_appeal, chain_code, generation, participant_code, period) %>%
  group_by(treatment_appeal, chain_code, generation) %>%
  mutate(individual_id = as.numeric(factor(participant_code))) %>%
  ungroup() %>%
  left_join(chain_order, by = c("treatment_appeal", "chain_code")) %>%
  mutate(y = (chain_num_plot - 1) * 4 + individual_id)

# y-axis: one label per chain at the vertical midpoint of its block
y_scale <- df_plot %>%
  group_by(chain_num_plot, chain_rank) %>%
  summarise(y_mid = mean(y), .groups = "drop") %>%
  mutate(label = paste("Chain", chain_rank))

FigA12 <- df_plot %>%
  ggplot(aes(x = period_in_chain, y = y, fill = n_nutrients)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("grey90", "grey60", "#000000"),
    name = "Number of\nnutrients used"
  ) +
  facet_wrap(
    ~treatment_appeal,
    scales = "free_y",
    labeller = as_labeller(treatment_names)
  ) +
  geom_vline(xintercept = c(6.5, 12.5, 18.5), linetype = "dashed") +
  scale_x_continuous(
    breaks = c(1, 6, 12, 18, 24),
    labels = c(0, 6, 12, 18, 24)
  ) +
  scale_y_continuous(breaks = y_scale$y_mid, labels = y_scale$label) +
  labs(
    x = NULL,
    y = NULL
  ) +
  annotation_custom(
    grob = textGrob(
      "Trial",
      gp = gpar(fontsize = 10),
      y = unit(-2, "lines")
    ),
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = -Inf
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 7),
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
    vjust = 2.25,
    size = 2,
    inherit.aes = FALSE
  )

ggsave("figures/figA12.png", plot = FigA12, width = 10, height = 6, dpi = 300)
