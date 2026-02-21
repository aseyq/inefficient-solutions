library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)

# Table A4 outcomes (participant-level exploration metrics)

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

# --- Panel B models: within-individual solution distance
model_treatment_within <- lmer(
    within_individual_solution_distance ~ treatment_appeal + (1 | chain_code),
    data = participant_metrics
)

model_additive_within <- lmer(
    within_individual_solution_distance ~ generation + treatment_appeal + (1 | chain_code),
    data = participant_metrics
)

model_interaction_within <- lmer(
    within_individual_solution_distance ~ generation * treatment_appeal + (1 | chain_code),
    data = participant_metrics
)

# Write single table for Table A4
out <- "figures/table_a4.html"

print(
    tab_model(
        model_treatment_within,
        model_additive_within,
        model_interaction_within,
        show.ci = FALSE,
        show.se = TRUE,
        show.re.var = FALSE,
        dv.labels = c("Treatment only", "Treatment + generation", "Treatment × generation"),
        title = "Table A4) Within-individual solution distance",
        file = out
    )
)
