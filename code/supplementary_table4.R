library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

compute_chain_diversity <- function(group_df) {
    mat <- do.call(rbind, lapply(group_df$transmitted_solution, solution_vector))
    dist_mat <- as.matrix(dist(mat, method = "manhattan", diag = TRUE, upper = TRUE))
    chains <- as.character(group_df$chain_code)
    unique_chains <- unique(chains)

    purrr::map_dfr(unique_chains, function(chain_id) {
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

df_long <- read_csv("data/processed/df_long_processed.csv") %>%
    select(all_of(c(
        "participant_code", "chain_code", "treatment_appeal", "generation", "generation_c", "period",
        "grid_state_flatten"
    ))) %>%
    mutate(
        generation = as.integer(generation),
        generation_c = as.numeric(generation_c),
        period = as.integer(period),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )

participant_metrics <- df_long %>%
    group_by(participant_code, chain_code, treatment_appeal, generation, generation_c) %>%
    arrange(period, .by_group = TRUE) %>%
    summarise(
        transmitted_solution = grid_state_flatten[period == 6][1],
        .groups = "drop"
    )

cultural_convergence_chain <- participant_metrics %>%
    select(chain_code, treatment_appeal, generation, generation_c, transmitted_solution) %>%
    group_by(treatment_appeal, generation, generation_c) %>%
    group_modify(~ compute_chain_diversity(.x)) %>%
    ungroup() %>%
    mutate(chain_code = factor(chain_code))

set.seed(42)

model_additive_within <- lmer(
    within_chain_diversity ~ generation_c + treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain,
    REML = FALSE
)

model_interaction_within <- lmer(
    within_chain_diversity ~ generation_c * treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain,
    REML = FALSE
)

model_additive_between <- lmer(
    between_chain_diversity ~ generation_c + treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain,
    REML = FALSE
)

model_interaction_between <- lmer(
    between_chain_diversity ~ generation_c * treatment_appeal + (1 | chain_code),
    data = cultural_convergence_chain,
    REML = FALSE
)

table4 <- bind_rows(
    extract_fixed_effects(model_additive_within, "Within-chain distance: additive"),
    extract_fixed_effects(model_interaction_within, "Within-chain distance: interaction"),
    extract_fixed_effects(model_additive_between, "Between-chains distance: additive"),
    extract_fixed_effects(model_interaction_between, "Between-chains distance: interaction")
)

table4 <- add_model_spacer_rows(table4)

write_csv(table4, "output/supplementary_table4.csv")
write_simple_html_table(table4, "output/supplementary_table4.html")
message("Wrote output/supplementary_table4.csv and output/supplementary_table4.html")
