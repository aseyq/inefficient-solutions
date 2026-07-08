library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

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
        within_individual_solution_distance = mean_pairwise_distance(grid_state_flatten),
        .groups = "drop"
    ) %>%
    mutate(chain_code = factor(chain_code))

set.seed(42)

model_additive_within <- lmer(
    within_individual_solution_distance ~ generation_c + treatment_appeal + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

model_interaction_within <- lmer(
    within_individual_solution_distance ~ generation_c * treatment_appeal + (1 | chain_code),
    data = participant_metrics,
    REML = FALSE
)

table3 <- bind_rows(
    extract_fixed_effects(model_additive_within, "Within-individual distance: additive"),
    extract_fixed_effects(model_interaction_within, "Within-individual distance: interaction")
)

table3 <- add_model_spacer_rows(table3)

write_csv(table3, "output/supplementary_table3.csv")
write_simple_html_table(table3, "output/supplementary_table3.html")
message("Wrote output/supplementary_table3.csv and output/supplementary_table3.html")
