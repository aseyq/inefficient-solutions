library(tidyverse)
source("code/_helpers.R")

df_long <- read_csv("data/df_long_nopay.csv")

df_cumulative <- df_long %>%
    arrange(treatment_appeal, generation, period_in_chain) %>%
    group_by(treatment_appeal) %>%
    mutate(
        n_unique = map_int(
            period_in_chain,
            ~ n_distinct(grid_state_flatten[period_in_chain <= .x])
        )
    ) %>%
    ungroup()

# Add generation variable (1 for 1-6, 2 for 7-12, etc.)
df_cumulative <- df_cumulative %>%
    mutate(generation = (period_in_chain - 1) %/% 6 + 1)

# Plot
supplementary_figure16 <- ggplot(df_cumulative, aes(x = period_in_chain, y = n_unique, color = treatment_appeal, group = interaction(generation, treatment_appeal))) +
    geom_line() +
    geom_point(size = 2) +
    geom_abline(slope = 60, intercept = 0, linewidth = 0.4, linetype = "dashed", color = "gray40") +

    # Vertical dotted lines
    geom_vline(xintercept = c(6.5, 12.5, 18.5), linetype = "dotted", color = "gray40") +

    # Generation labels
    annotate("text", x = 3.5, y = 1500, label = "Generation 1", size = 4) +
    annotate("text", x = 9.5, y = 1500, label = "Generation 2", size = 4) +
    annotate("text", x = 15.5, y = 1500, label = "Generation 3", size = 4) +
    annotate("text", x = 21.5, y = 1500, label = "Generation 4", size = 4) +

    # Color and label customization
    scale_color_manual(
        labels = treatment_names,
        values = treatment_colors,
        name = NULL
    ) +

    # Axis formatting
    scale_x_continuous(breaks = 1:24) +
    scale_y_continuous(breaks = seq(0, 1500, 500)) +

    # Layout and theme
    labs(
        x = "Trial in chain",
        y = "Number of Unique Solutions"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )

# Save plot
fig.height <- 5
fig.width <- 6

ggsave("output/supplementary_figure16.png", plot = supplementary_figure16, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure16.pdf", plot = supplementary_figure16, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure16.svg", plot = supplementary_figure16, width = fig.width, height = fig.height, device = svglite::svglite)
