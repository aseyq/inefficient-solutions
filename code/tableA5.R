library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
source("code/_helpers.R")

# Table A5 outcomes (chain-level cultural convergence metrics)

compute_chain_diversity <- function(group_df) {
    mat <- do.call(rbind, lapply(group_df$transmitted_solution, solution_vector))
    dist_mat <- as.matrix(dist(mat, method = "manhattan", diag = TRUE, upper = TRUE))
    chains <- as.character(group_df$chain_code)
    unique_chains <- unique(chains)

    map_dfr(unique_chains, function(chain_id) {
        within_idx <- which(chains == chain_id)
        between_idx <- which(chains != chain_id)

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

        tibble(
            chain_code = chain_id,
            within_chain_diversity = within_chain_diversity,
            between_chain_diversity = between_chain_diversity
        )
    })
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
        transmitted_solution = grid_state_flatten[period == 6][1],
        .groups = "drop"
    )

cultural_convergence_chain <- participant_metrics %>%
    select(chain_code, treatment_appeal, generation, transmitted_solution) %>%
    group_by(treatment_appeal, generation) %>%
    group_modify(~ compute_chain_diversity(.x)) %>%
    ungroup() %>%
    mutate(chain_code = factor(chain_code))

# Three models per outcome: treatment-only, additive (treatment + generation),
# and interaction (treatment x generation).

# --- Within-chain distance
model_treatment_within <- lmer(
    within_chain_diversity ~ treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain
)

model_additive_within <- lmer(
    within_chain_diversity ~ generation + treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain
)

model_interaction_within <- lmer(
    within_chain_diversity ~ generation * treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain
)

# --- Between-chains distance
model_treatment_between <- lmer(
    between_chain_diversity ~ treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain
)

model_additive_between <- lmer(
    between_chain_diversity ~ generation + treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain
)

model_interaction_between <- lmer(
    between_chain_diversity ~ generation * treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain
)

# Write single table for Table A5
out <- "figures/table_a5.html"

print(
    tab_model(
        model_treatment_within,
        model_additive_within,
        model_interaction_within,
        model_treatment_between,
        model_additive_between,
        model_interaction_between,
        show.ci = FALSE,
        show.se = TRUE,
        show.re.var = FALSE,
        dv.labels = c(
            "Within-chain distance",
            "Within-chain distance",
            "Within-chain distance",
            "Between-chains distance",
            "Between-chains distance",
            "Between-chains distance"
        ),
        file = out
    )
)
