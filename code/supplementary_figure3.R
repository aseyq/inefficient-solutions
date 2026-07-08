library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

dir.create("output", showWarnings = FALSE, recursive = TRUE)

df_long <- read_csv("data/processed/df_long_processed.csv")
participant_metrics <- build_social_influence_participant_metrics(df_long)

set.seed(42)

participant_metrics_g2_4 <- participant_metrics %>%
    filter(generation %in% 2:4) %>%
    mutate(generation_f = factor(generation))

participant_metrics_first <- participant_metrics_g2_4 %>%
    filter(is.finite(distance_inherited_first_trial))

participant_metrics_trans <- participant_metrics_g2_4 %>%
    filter(is.finite(distance_inherited_transmitted))

social_influence_summary <- participant_metrics %>%
    filter(generation %in% 2:4) %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        n_participants = n(),
        copy_probability_mean = mean(as.numeric(copied_first_trial), na.rm = TRUE),
        copy_probability_sd = sd(as.numeric(copied_first_trial), na.rm = TRUE),
        copy_probability_se = copy_probability_sd / sqrt(n_participants),
        inherited_to_first_trial_distance_mean = mean(distance_inherited_first_trial, na.rm = TRUE),
        inherited_to_first_trial_distance_sd = sd(distance_inherited_first_trial, na.rm = TRUE),
        inherited_to_first_trial_distance_se = inherited_to_first_trial_distance_sd / sqrt(n_participants),
        inherited_to_transmitted_distance_mean = mean(distance_inherited_transmitted, na.rm = TRUE),
        inherited_to_transmitted_distance_sd = sd(distance_inherited_transmitted, na.rm = TRUE),
        inherited_to_transmitted_distance_se = inherited_to_transmitted_distance_sd / sqrt(n_participants),
        .groups = "drop"
    ) %>%
    mutate(generation_f = factor(generation))

treatment_level_summary <- participant_metrics %>%
    filter(generation %in% 2:4) %>%
    group_by(treatment_appeal) %>%
    summarise(
        n_participants = n(),
        copy_probability_mean = mean(as.numeric(copied_first_trial), na.rm = TRUE),
        copy_probability_sd = sd(as.numeric(copied_first_trial), na.rm = TRUE),
        copy_probability_se = copy_probability_sd / sqrt(n_participants),
        inherited_to_first_trial_distance_mean = mean(distance_inherited_first_trial, na.rm = TRUE),
        inherited_to_first_trial_distance_sd = sd(distance_inherited_first_trial, na.rm = TRUE),
        inherited_to_first_trial_distance_se = inherited_to_first_trial_distance_sd / sqrt(n_participants),
        inherited_to_transmitted_distance_mean = mean(distance_inherited_transmitted, na.rm = TRUE),
        inherited_to_transmitted_distance_sd = sd(distance_inherited_transmitted, na.rm = TRUE),
        inherited_to_transmitted_distance_se = inherited_to_transmitted_distance_sd / sqrt(n_participants),
        .groups = "drop"
    )

distance_limit_upper <- ceiling(max(
    participant_metrics_first$distance_inherited_first_trial,
    participant_metrics_trans$distance_inherited_transmitted,
    social_influence_summary$inherited_to_first_trial_distance_mean + social_influence_summary$inherited_to_first_trial_distance_se,
    social_influence_summary$inherited_to_transmitted_distance_mean + social_influence_summary$inherited_to_transmitted_distance_se,
    na.rm = TRUE
))

if (!is.finite(distance_limit_upper)) {
    distance_limit_upper <- 1
}

distance_breaks <- scales::breaks_pretty(n = 8)(c(0, distance_limit_upper))

FigA3A <- ggplot(
    treatment_level_summary,
    aes(
        x = treatment_appeal,
        y = copy_probability_mean,
        color = treatment_appeal
    )
) +
    geom_errorbar(
        aes(
            ymin = copy_probability_mean - copy_probability_se,
            ymax = copy_probability_mean + copy_probability_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 3) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_x_discrete(labels = treatment_names) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Treatment",
        y = "Prob. copying demonstrator at trial 1",
        color = NULL
    ) +
    base_theme

FigA3B <- ggplot(
    social_influence_summary,
    aes(
        x = generation,
        y = copy_probability_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_line(linewidth = 0.6) +
    geom_errorbar(
        aes(
            ymin = copy_probability_mean - copy_probability_se,
            ymax = copy_probability_mean + copy_probability_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_x_continuous(breaks = sort(unique(social_influence_summary$generation))) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Probab. copying demonstrator at trial 1",
        color = NULL
    ) +
    base_theme

distance_first_treatment_summary <- participant_metrics_first %>%
    group_by(treatment_appeal) %>%
    summarise(
        n_participants = n(),
        distance_mean = mean(distance_inherited_first_trial, na.rm = TRUE),
        distance_se = sd(distance_inherited_first_trial, na.rm = TRUE) / sqrt(n_participants),
        .groups = "drop"
    )

FigA3C <- ggplot(
    distance_first_treatment_summary,
    aes(
        x = treatment_appeal,
        y = distance_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = participant_metrics_first,
        aes(
            x = treatment_appeal,
            y = distance_inherited_first_trial,
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
            ymin = distance_mean - distance_se,
            ymax = distance_mean + distance_se,
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
        limits = c(0, distance_limit_upper),
        breaks = distance_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Treatment",
        y = "Distance: inherited to first trial",
        color = NULL
    ) +
    base_theme

FigA3D <- ggplot(
    social_influence_summary,
    aes(
        x = generation_f,
        y = inherited_to_first_trial_distance_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = participant_metrics_first,
        aes(
            x = generation_f,
            y = distance_inherited_first_trial,
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
            ymin = inherited_to_first_trial_distance_mean - inherited_to_first_trial_distance_se,
            ymax = inherited_to_first_trial_distance_mean + inherited_to_first_trial_distance_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(social_influence_summary$generation)))) +
    scale_y_continuous(
        limits = c(0, distance_limit_upper),
        breaks = distance_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Distance: inherited to first trial",
        color = NULL
    ) +
    base_theme

distance_trans_treatment_summary <- participant_metrics_trans %>%
    group_by(treatment_appeal) %>%
    summarise(
        n_participants = n(),
        distance_mean = mean(distance_inherited_transmitted, na.rm = TRUE),
        distance_se = sd(distance_inherited_transmitted, na.rm = TRUE) / sqrt(n_participants),
        .groups = "drop"
    )

FigA3E <- ggplot(
    distance_trans_treatment_summary,
    aes(
        x = treatment_appeal,
        y = distance_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = participant_metrics_trans,
        aes(
            x = treatment_appeal,
            y = distance_inherited_transmitted,
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
            ymin = distance_mean - distance_se,
            ymax = distance_mean + distance_se,
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
        limits = c(0, distance_limit_upper),
        breaks = distance_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Treatment",
        y = "Distance: inherited to transmitted",
        color = NULL
    ) +
    base_theme

FigA3F <- ggplot(
    social_influence_summary,
    aes(
        x = generation_f,
        y = inherited_to_transmitted_distance_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = participant_metrics_trans,
        aes(
            x = generation_f,
            y = distance_inherited_transmitted,
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
            ymin = inherited_to_transmitted_distance_mean - inherited_to_transmitted_distance_se,
            ymax = inherited_to_transmitted_distance_mean + inherited_to_transmitted_distance_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(social_influence_summary$generation)))) +
    scale_y_continuous(
        limits = c(0, distance_limit_upper),
        breaks = distance_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Distance: inherited to transmitted",
        color = NULL
    ) +
    base_theme

FigA3 <- (FigA3A + FigA3C + FigA3E) /
    (FigA3B + FigA3D + FigA3F) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = list(c("A", "C", "E", "B", "D", "F"))) &
    theme(legend.position = "none")

fig.width <- 16
fig.height <- 9

ggsave("output/supplementary_figure3.png", FigA3, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure3.pdf", FigA3, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure3.svg", FigA3, width = fig.width, height = fig.height, device = svglite::svglite)
