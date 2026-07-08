library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

clean_prefix <- function(x) {
    x %>%
        stringr::str_replace(
            regex("^\\s*the field yields a higher output when\\s*", ignore_case = TRUE),
            ""
        ) %>%
        stringr::str_squish()
}

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
    select(all_of(c("period", "generation", "treatment_appeal", "feedback_message")))

df_msg <- df_long %>%
    filter(period == 6) %>%
    mutate(
        generation = as.integer(generation),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) %>%
    select(generation, treatment_appeal, feedback_message)

df_msg_metrics <- df_msg %>%
    mutate(
        cleaned_feedback = clean_prefix(feedback_message),
        feedback_length_chars = nchar(cleaned_feedback)
    )

df_msg_metrics_plot <- df_msg_metrics %>%
    filter(is.finite(feedback_length_chars)) %>%
    mutate(generation_f = factor(generation))

text_summary <- df_msg_metrics %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        n_participants = n(),
        mean_feedback_length = mean(feedback_length_chars, na.rm = TRUE),
        sd_feedback_length = sd(feedback_length_chars, na.rm = TRUE),
        se_feedback_length = sd_feedback_length / sqrt(n_participants),
        .groups = "drop"
    ) %>%
    mutate(generation_f = factor(generation))

y_limit_upper <- ceiling(max(
    df_msg_metrics_plot$feedback_length_chars,
    text_summary$mean_feedback_length + text_summary$se_feedback_length,
    na.rm = TRUE
))

if (!is.finite(y_limit_upper)) {
    y_limit_upper <- 1
}

y_breaks <- scales::breaks_pretty(n = 8)(c(0, y_limit_upper))

text_treatment_summary <- df_msg_metrics_plot %>%
    group_by(treatment_appeal) %>%
    summarise(
        n_participants = n(),
        mean_feedback_length = mean(feedback_length_chars, na.rm = TRUE),
        sd_feedback_length = sd(feedback_length_chars, na.rm = TRUE),
        se_feedback_length = sd_feedback_length / sqrt(n_participants),
        .groups = "drop"
    )

FigA6A <- ggplot(
    text_treatment_summary,
    aes(
        x = treatment_appeal,
        y = mean_feedback_length,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = df_msg_metrics_plot,
        aes(
            x = treatment_appeal,
            y = feedback_length_chars,
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
            ymin = mean_feedback_length - se_feedback_length,
            ymax = mean_feedback_length + se_feedback_length,
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
        y = "Advice length (characters)",
        color = NULL
    ) +
    base_theme

FigA6B <- ggplot(
    text_summary,
    aes(
        x = generation_f,
        y = mean_feedback_length,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = df_msg_metrics_plot,
        aes(
            x = generation_f,
            y = feedback_length_chars,
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
            ymin = mean_feedback_length - se_feedback_length,
            ymax = mean_feedback_length + se_feedback_length,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(text_summary$generation)))) +
    scale_y_continuous(
        limits = c(0, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Advice length (characters)",
        color = NULL
    ) +
    base_theme

FigA6 <- (FigA6A + FigA6B) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "none")

fig.width <- 10.2
fig.height <- 4.6

ggsave("output/supplementary_figure6.png", FigA6, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure6.pdf", FigA6, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure6.svg", FigA6, width = fig.width, height = fig.height, device = svglite::svglite)
