library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

dir.create("output", showWarnings = FALSE, recursive = TRUE)

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
    mutate(
        generation = as.integer(generation),
        generation_c = as.integer(generation_c),
        period = as.integer(period),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) %>%
    filter(treatment_appeal == "high_appeal")

df_advice <- read_csv("data/df_advice_manual_coding.csv", show_col_types = FALSE)

df_advice_categories <- df_advice %>%
    select(participant_code, mix_and_match:other) %>%
    rename_with(~ paste0("selected_", .), -participant_code) %>%
    rename(selected_advice_author = participant_code)

participant_metrics <- df_long %>%
    group_by(participant_code, chain_code, treatment_appeal, generation, selected_feedback_author) %>%
    arrange(period, .by_group = TRUE) %>%
    summarise(
        generation_c = first(generation_c),
        within_individual_solution_distance = mean_pairwise_distance(grid_state_flatten),
        .groups = "drop"
    ) %>%
    mutate(
        generation_c2 = generation - 2,
        chain_code = factor(chain_code),
        selected_advice_author = selected_feedback_author
    ) %>%
    left_join(df_advice_categories, by = "selected_advice_author") %>%
    filter(generation > 1) %>%
    mutate(
        selected_mix_and_match = coalesce(as.numeric(selected_mix_and_match), 0),
        selected_mix_and_match_group = factor(
            selected_mix_and_match,
            levels = c(0, 1),
            labels = c("All others", "Mix & Match")
        )
    )

plot_metrics <- participant_metrics %>%
    filter(
        is.finite(within_individual_solution_distance),
        !is.na(generation),
        !is.na(selected_mix_and_match_group)
    )

mix_generation_summary <- plot_metrics %>%
    group_by(generation, selected_mix_and_match_group) %>%
    summarise(
        n_participants = n(),
        within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
        within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
        within_distance_se = within_distance_sd / sqrt(n_participants),
        .groups = "drop"
    )

group_colors <- c(
    "All others" = "#404142",
    "Mix & Match" = "#B5179E"
)

y_limit_lower <- 0
y_limit_upper <- max(
    plot_metrics$within_individual_solution_distance,
    mix_generation_summary$within_distance_mean + mix_generation_summary$within_distance_se,
    na.rm = TRUE
)
y_limit_upper <- ceiling(y_limit_upper * 20) / 20
if (!is.finite(y_limit_upper) || y_limit_upper <= y_limit_lower) {
    y_limit_upper <- 1
}
y_breaks <- pretty(c(y_limit_lower, y_limit_upper), n = 7)
y_breaks <- y_breaks[y_breaks >= y_limit_lower & y_breaks <= y_limit_upper]

mix_group_summary <- plot_metrics %>%
    group_by(selected_mix_and_match_group) %>%
    summarise(
        n_participants = n(),
        within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
        within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
        within_distance_se = within_distance_sd / sqrt(n_participants),
        .groups = "drop"
    )

FigA9A <- ggplot(
    mix_group_summary,
    aes(
        x = selected_mix_and_match_group,
        y = within_distance_mean,
        color = selected_mix_and_match_group,
        group = selected_mix_and_match_group
    )
) +
    geom_violin(
        data = plot_metrics,
        aes(
            x = selected_mix_and_match_group,
            y = within_individual_solution_distance,
            fill = selected_mix_and_match_group
        ),
        inherit.aes = FALSE,
        width = 0.9,
        alpha = 0.18,
        color = NA,
        linewidth = 0,
        trim = FALSE,
        na.rm = TRUE
    ) +
    geom_errorbar(
        aes(
            ymin = within_distance_mean - within_distance_se,
            ymax = within_distance_mean + within_distance_se
        ),
        width = 0.2,
        linewidth = 0.4,
        color = "black"
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_colors) +
    scale_y_continuous(
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    coord_cartesian(ylim = c(y_limit_lower, y_limit_upper)) +
    labs(
        x = "Selected advice",
        y = "Within-individual solution distance",
        color = NULL
    ) +
    base_theme

FigA9B <- ggplot(
    plot_metrics,
    aes(
        x = generation,
        y = within_individual_solution_distance,
        group = interaction(generation, selected_mix_and_match_group),
        fill = selected_mix_and_match_group
    )
) +
    geom_violin(
        alpha = 0.18,
        color = NA,
        linewidth = 0,
        width = 1,
        trim = FALSE,
        position = position_dodge(width = 0.45)
    ) +
    geom_line(
        data = mix_generation_summary,
        aes(
            x = generation,
            y = within_distance_mean,
            color = selected_mix_and_match_group,
            group = selected_mix_and_match_group
        ),
        linewidth = 0.6
    ) +
    geom_errorbar(
        data = mix_generation_summary,
        aes(
            x = generation,
            y = within_distance_mean,
            ymin = within_distance_mean - within_distance_se,
            ymax = within_distance_mean + within_distance_se
        ),
        width = 0.2,
        linewidth = 0.4,
        color = "black"
    ) +
    geom_point(
        data = mix_generation_summary,
        aes(
            x = generation,
            y = within_distance_mean,
            color = selected_mix_and_match_group
        ),
        size = 2.8
    ) +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_colors) +
    scale_x_continuous(breaks = sort(unique(mix_generation_summary$generation))) +
    scale_y_continuous(
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    coord_cartesian(ylim = c(y_limit_lower, y_limit_upper)) +
    labs(
        x = "Generation",
        y = "Within-individual solution distance",
        color = NULL
    ) +
    base_theme

FigA9 <- (FigA9A + FigA9B) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "none")

fig.width <- 10
fig.height <- 4.6

ggsave("output/supplementary_figure9.png", FigA9, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure9.pdf", FigA9, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure9.svg", FigA9, width = fig.width, height = fig.height, device = svglite::svglite)
