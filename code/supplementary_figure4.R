library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
    select(all_of(c(
        "participant_code", "chain_code", "treatment_appeal", "generation", "period",
        "grid_state_flatten"
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
        within_individual_solution_distance = mean_pairwise_distance(grid_state_flatten),
        .groups = "drop"
    ) %>%
    mutate(chain_code = factor(chain_code))

participant_metrics_plot <- participant_metrics %>%
    filter(is.finite(within_individual_solution_distance)) %>%
    mutate(generation_f = factor(generation))

individual_exploration_summary <- participant_metrics %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        n_participants = n(),
        within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
        within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
        within_distance_se = within_distance_sd / sqrt(n_participants),
        .groups = "drop"
    ) %>%
    mutate(generation_f = factor(generation))

y_limit_upper <- ceiling(max(
    participant_metrics_plot$within_individual_solution_distance,
    individual_exploration_summary$within_distance_mean + individual_exploration_summary$within_distance_se,
    na.rm = TRUE
))

if (!is.finite(y_limit_upper)) {
    y_limit_upper <- 1
}

y_breaks <- scales::breaks_pretty(n = 8)(c(0, y_limit_upper))

individual_exploration_treatment_summary <- participant_metrics_plot %>%
    group_by(treatment_appeal) %>%
    summarise(
        n_participants = n(),
        within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
        within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
        within_distance_se = within_distance_sd / sqrt(n_participants),
        .groups = "drop"
    )

FigA4A <- ggplot(
    individual_exploration_treatment_summary,
    aes(
        x = treatment_appeal,
        y = within_distance_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = participant_metrics_plot,
        aes(
            x = treatment_appeal,
            y = within_individual_solution_distance,
            fill = treatment_appeal
        ),
        inherit.aes = FALSE,
        width = 0.9,
        alpha = 0.18,
        color = NA,
        linewidth = 0.0,
        trim = FALSE,
        na.rm = TRUE
    ) +
    geom_errorbar(
        aes(
            ymin = within_distance_mean - within_distance_se,
            ymax = within_distance_mean + within_distance_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(labels = treatment_names) +
    scale_y_continuous(
        limits = c(0, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Treatment",
        y = "Within-individual solution distance",
        color = NULL
    ) +
    base_theme

FigA4B <- ggplot(
    individual_exploration_summary,
    aes(
        x = generation_f,
        y = within_distance_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = participant_metrics_plot,
        aes(
            x = generation_f,
            y = within_individual_solution_distance,
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
    geom_line(linewidth = 0.6) +
    geom_errorbar(
        aes(
            ymin = within_distance_mean - within_distance_se,
            ymax = within_distance_mean + within_distance_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(individual_exploration_summary$generation)))) +
    scale_y_continuous(
        limits = c(0, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Within-individual solution distance",
        color = NULL
    ) +
    base_theme

FigA4 <- (FigA4A + FigA4B) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "none")

fig.width <- 10.2
fig.height <- 4.6

ggsave("output/supplementary_figure4.png", FigA4, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure4.pdf", FigA4, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure4.svg", FigA4, width = fig.width, height = fig.height, device = svglite::svglite)
