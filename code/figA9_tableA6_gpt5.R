library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(sjPlot)
source("code/_helpers.R")

base_theme <- make_base_theme()

# Cache parsed solution vectors so repeated strings are converted only once.
solution_cache <- new.env(parent = emptyenv())

solution_vector <- function(solution) {
    if (is.na(solution) || solution == "") {
        return(rep(NA_integer_, 27))
    }
    if (exists(solution, envir = solution_cache, inherits = FALSE)) {
        return(get(solution, envir = solution_cache, inherits = FALSE))
    }
    tokens <- strsplit(solution, "-", fixed = TRUE)[[1]]
    vec <- as.integer(strsplit(paste(tokens, collapse = ""), "", fixed = TRUE)[[1]])
    assign(solution, vec, envir = solution_cache)
    vec
}

mean_pairwise_distance <- function(solutions) {
    if (length(solutions) < 2) {
        return(NA_real_)
    }
    mat <- do.call(rbind, lapply(solutions, solution_vector))
    mean(as.numeric(dist(mat, method = "manhattan")))
}

df_long <- read_csv("data/df_long.csv") %>%
    mutate(
        generation = as.integer(generation),
        period = as.integer(period),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) %>%
    filter(treatment_appeal == "high_appeal")

df_advice <- read_csv("data/df_advice_gpt5.csv", show_col_types = FALSE)

df_advice_categories <- df_advice %>%
    select(participant_code, mix_and_match:other) %>%
    rename_with(~ paste0("selected_", .), -participant_code) %>%
    rename(selected_advice_author = participant_code)

participant_metrics <- df_long %>%
    group_by(participant_code, chain_code, treatment_appeal, generation, selected_feedback_author) %>%
    arrange(period, .by_group = TRUE) %>%
    summarise(
        within_individual_solution_distance = mean_pairwise_distance(grid_state_flatten),
        .groups = "drop"
    ) %>%
    mutate(
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

mix_treatment_summary <- participant_metrics %>%
    group_by(selected_mix_and_match_group) %>%
    summarise(
        n_participants = n(),
        within_distance_mean = mean(within_individual_solution_distance, na.rm = TRUE),
        within_distance_sd = sd(within_individual_solution_distance, na.rm = TRUE),
        within_distance_se = within_distance_sd / sqrt(n_participants),
        .groups = "drop"
    )

mix_generation_summary <- participant_metrics %>%
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

FigA9A <- ggplot(
    mix_treatment_summary,
    aes(
        x = selected_mix_and_match_group,
        y = within_distance_mean,
        color = selected_mix_and_match_group
    )
) +
    geom_col(aes(fill = selected_mix_and_match_group), width = 0.7, alpha = 0.9, linewidth = 0.3) +
    geom_errorbar(
        aes(
            ymin = within_distance_mean - within_distance_se,
            ymax = within_distance_mean + within_distance_se
        ),
        width = 0.15,
        linewidth = 0.4,
        color = "black"
    ) +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_colors) +
    scale_y_continuous(
        limits = c(0, 12),
        breaks = seq(0, 12, by = 1),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        x = "Selected advice",
        y = "Within-individual solution distance",
        color = NULL
    ) +
    base_theme

FigA9B <- ggplot(
    mix_generation_summary,
    aes(
        x = generation,
        y = within_distance_mean,
        color = selected_mix_and_match_group,
        group = selected_mix_and_match_group
    )
) +
    geom_point(size = 2.8) +
    geom_line(linewidth = 0.6) +
    geom_linerange(
        aes(
            ymin = within_distance_mean - within_distance_se,
            ymax = within_distance_mean + within_distance_se
        ),
        linewidth = 0.4
    ) +
    scale_color_manual(values = group_colors) +
    scale_x_continuous(breaks = sort(unique(mix_generation_summary$generation))) +
    scale_y_continuous(
        limits = c(0, 12),
        breaks = seq(0, 12, by = 1),
        expand = expansion(mult = c(0, 0.05))
    ) +
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

ggsave(
    "figures/figA9_gpt5.png",
    FigA9,
    width = 10.2,
    height = 4.6,
    dpi = 300
)

model_tableA6_mix <- lmer(
    within_individual_solution_distance ~ selected_mix_and_match + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

model_tableA6_mix_additive <- lmer(
    within_individual_solution_distance ~ selected_mix_and_match + generation + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

model_tableA6_mix_interaction <- lmer(
    within_individual_solution_distance ~ selected_mix_and_match * generation + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

print(
    tab_model(
        model_tableA6_mix,
        model_tableA6_mix_additive,
        model_tableA6_mix_interaction,
        show.ci = FALSE,
        show.se = TRUE,
        show.re.var = FALSE,
        dv.labels = c("Mix & Match", "Mix & Match + generation", "Mix & Match × generation"),
        title = "Table A6) High-appeal: within-individual distance by selecting Mix & Match advice",
        file = "figures/table_a6_gpt5.html"
    )
)
message("Error bar on Fig A9 GPT5 comparison gets out of the fixed range. I am keeping it to compare it with the original Fig9 that we put in the paper. Can update range for both before publication.")
