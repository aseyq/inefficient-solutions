library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

compute_chain_diversity <- function(group_df) {
    mat <- do.call(rbind, lapply(group_df$transmitted_solution, solution_vector))
    dist_mat <- as.matrix(dist(mat, method = "manhattan", diag = TRUE, upper = TRUE))
    chains <- as.character(group_df$chain_code)
    unique_chains <- unique(chains)

    centroid_mat <- t(vapply(
        unique_chains,
        function(chain_id) colMeans(mat[chains == chain_id, , drop = FALSE]),
        numeric(ncol(mat))
    ))
    centroid_dist_mat <- as.matrix(dist(centroid_mat, method = "manhattan", diag = TRUE, upper = TRUE))
    rownames(centroid_dist_mat) <- unique_chains
    colnames(centroid_dist_mat) <- unique_chains

    map_dfr(unique_chains, function(chain_id) {
        within_idx <- which(chains == chain_id)
        between_idx <- which(chains != chain_id)
        other_chains <- setdiff(unique_chains, chain_id)

        within_chain_diversity <- if (length(within_idx) >= 2) {
            within_mat <- dist_mat[within_idx, within_idx, drop = FALSE]
            mean(within_mat[upper.tri(within_mat)])
        } else {
            NA_real_
        }

        between_chain_diversity <- if (length(between_idx) >= 1) {
            mean(dist_mat[within_idx, between_idx, drop = FALSE])
        } else {
            NA_real_
        }

        between_chain_centroid_diversity <- if (length(other_chains) >= 1) {
            mean(centroid_dist_mat[chain_id, other_chains])
        } else {
            NA_real_
        }

        tibble(
            chain_code = chain_id,
            within_chain_diversity = within_chain_diversity,
            between_chain_diversity = between_chain_diversity,
            between_chain_centroid_diversity = between_chain_centroid_diversity
        )
    })
}

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
        transmitted_solution = grid_state_flatten[period == 6][1],
        .groups = "drop"
    )

cultural_convergence_chain <- participant_metrics %>%
    select(chain_code, treatment_appeal, generation, transmitted_solution) %>%
    group_by(treatment_appeal, generation) %>%
    group_modify(~ compute_chain_diversity(.x)) %>%
    ungroup() %>%
    mutate(
        chain_code = factor(chain_code),
        generation_f = factor(generation)
    )

cultural_convergence_summary <- cultural_convergence_chain %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        n_chains = n(),
        within_chain_diversity_mean = mean(within_chain_diversity, na.rm = TRUE),
        within_chain_diversity_sd = sd(within_chain_diversity, na.rm = TRUE),
        within_chain_diversity_se = within_chain_diversity_sd / sqrt(n_chains),
        between_chain_diversity_mean = mean(between_chain_diversity, na.rm = TRUE),
        between_chain_diversity_sd = sd(between_chain_diversity, na.rm = TRUE),
        between_chain_diversity_se = between_chain_diversity_sd / sqrt(n_chains),
        between_chain_centroid_diversity_mean = mean(between_chain_centroid_diversity, na.rm = TRUE),
        between_chain_centroid_diversity_sd = sd(between_chain_centroid_diversity, na.rm = TRUE),
        between_chain_centroid_diversity_se = between_chain_centroid_diversity_sd / sqrt(n_chains),
        .groups = "drop"
    ) %>%
    mutate(generation_f = factor(generation))

y_limit_upper <- ceiling(max(
    cultural_convergence_chain$within_chain_diversity,
    cultural_convergence_chain$between_chain_diversity,
    cultural_convergence_summary$within_chain_diversity_mean + cultural_convergence_summary$within_chain_diversity_se,
    cultural_convergence_summary$between_chain_diversity_mean + cultural_convergence_summary$between_chain_diversity_se,
    na.rm = TRUE
))

if (!is.finite(y_limit_upper)) {
    y_limit_upper <- 1
}

y_breaks <- scales::breaks_pretty(n = 8)(c(0, y_limit_upper))

FigA5A <- ggplot(
    cultural_convergence_summary,
    aes(
        x = generation_f,
        y = within_chain_diversity_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = cultural_convergence_chain,
        aes(
            x = generation_f,
            y = within_chain_diversity,
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
            ymin = within_chain_diversity_mean - within_chain_diversity_se,
            ymax = within_chain_diversity_mean + within_chain_diversity_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(cultural_convergence_summary$generation)))) +
    scale_y_continuous(
        limits = c(0, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Within-chain distance",
        color = NULL
    ) +
    base_theme

FigA5B <- ggplot(
    cultural_convergence_summary,
    aes(
        x = generation_f,
        y = between_chain_diversity_mean,
        color = treatment_appeal,
        group = treatment_appeal
    )
) +
    geom_violin(
        data = cultural_convergence_chain,
        aes(
            x = generation_f,
            y = between_chain_diversity,
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
            ymin = between_chain_diversity_mean - between_chain_diversity_se,
            ymax = between_chain_diversity_mean + between_chain_diversity_se,
            color = I(treatment_colors_dark[treatment_appeal])
        ),
        width = 0.2,
        linewidth = 0.7
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = treatment_colors, labels = treatment_names) +
    scale_fill_manual(values = treatment_colors, labels = treatment_names, guide = "none") +
    scale_x_discrete(breaks = as.character(sort(unique(cultural_convergence_summary$generation)))) +
    scale_y_continuous(
        limits = c(0, y_limit_upper),
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Generation",
        y = "Between-chains distance",
        color = NULL
    ) +
    base_theme

FigA5 <- (FigA5A + FigA5B) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "top")

fig.width <- 8
fig.height <- 5

ggsave("output/supplementary_figure5.png", FigA5, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure5.pdf", FigA5, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure5.svg", FigA5, width = fig.width, height = fig.height, device = svglite::svglite)
