library(tidyverse)
library(patchwork)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

semantic_dir <- "data/processed"

base_theme <- make_base_theme()

required_files <- c(
    file.path(semantic_dir, "semantic_similarity_by_chain_generation.csv"),
    file.path(semantic_dir, "semantic_similarity_by_chain.csv"),
    file.path(semantic_dir, "semantic_similarity_by_treatment_generation.csv"),
    file.path(semantic_dir, "semantic_similarity_by_treatment.csv")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
    stop(
        paste0(
            "Missing semantic similarity file(s): ",
            paste(missing_files, collapse = ", "),
            ". Run the semantic preprocessing script first."
        )
    )
}

semantic_chain <- read_csv(file.path(semantic_dir, "semantic_similarity_by_chain.csv"), show_col_types = FALSE) %>%
    mutate(
        treatment_appeal = if_else(stringr::str_ends(chain_code, "H"), "high_appeal", "low_appeal"),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )

semantic_chain_tg <- read_csv(file.path(semantic_dir, "semantic_similarity_by_chain_generation.csv"), show_col_types = FALSE) %>%
    mutate(
        generation = as.integer(generation),
        generation_f = factor(generation),
        treatment_appeal = if_else(stringr::str_ends(chain_code, "H"), "high_appeal", "low_appeal"),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )

semantic_tg <- read_csv(file.path(semantic_dir, "semantic_similarity_by_treatment_generation.csv"), show_col_types = FALSE) %>%
    mutate(
        generation = as.integer(generation),
        generation_f = factor(generation),
        se_similarity = sd_similarity / sqrt(pmax(n_pairs, 1)),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )

semantic_treat <- read_csv(file.path(semantic_dir, "semantic_similarity_by_treatment.csv"), show_col_types = FALSE) %>%
    mutate(
        se_similarity = sd_similarity / sqrt(pmax(n_pairs, 1)),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )

y_limit_lower <- 0

y_limit_upper <- ceiling(max(
    semantic_chain$mean_similarity,
    semantic_chain_tg$mean_similarity,
    semantic_treat$mean_similarity,
    semantic_tg$mean_similarity,
    na.rm = TRUE
) * 20) / 20

if (!is.finite(y_limit_upper) || y_limit_upper <= y_limit_lower) {
    y_limit_upper <- y_limit_lower + 0.1
}

y_breaks <- scales::breaks_pretty(n = 8)(c(y_limit_lower, y_limit_upper))

FigA7A <- ggplot(
    semantic_treat,
    aes(x = treatment_appeal, y = mean_similarity, color = treatment_appeal)
) +
    geom_violin(
        data = semantic_chain,
        aes(
            x = treatment_appeal,
            y = mean_similarity,
            fill = treatment_appeal
        ),
        inherit.aes = FALSE,
        width = 0.7,
        alpha = 0.18,
        color = NA,
        linewidth = 0,
        trim = FALSE,
        na.rm = TRUE
    ) +
    geom_point(size = 3) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names) +
    scale_x_discrete(labels = treatment_names) +
    scale_y_continuous(
        limits = c(y_limit_lower, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Treatment",
        y = "Mean cosine similarity (SBERT)",
        fill = NULL
    ) +
    base_theme +
    theme(legend.position = "none")

FigA7B <- ggplot(
    semantic_tg,
    aes(
        x = generation_f,
        y = mean_similarity,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = semantic_chain_tg,
        aes(
            x = generation_f,
            y = mean_similarity,
            fill = treatment_appeal,
            group = interaction(generation_f, treatment_appeal)
        ),
        inherit.aes = FALSE,
        position = position_dodge(width = 0.45),
        width = 1,
        alpha = 0.18,
        color = NA,
        linewidth = 0,
        trim = FALSE,
        na.rm = TRUE
    ) +
    geom_point(size = 2.8) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(semantic_tg$generation)))) +
    scale_y_continuous(
        limits = c(y_limit_lower, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Mean cosine similarity (SBERT)",
        color = NULL
    ) +
    base_theme

FigA7 <- (FigA7A + FigA7B) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "none")

fig.width <- 10.2
fig.height <- 4.8

ggsave("output/supplementary_figure7.png", FigA7, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure7.pdf", FigA7, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure7.svg", FigA7, width = fig.width, height = fig.height, device = svglite::svglite)
