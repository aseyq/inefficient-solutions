library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

# get data
df_long <- read_csv("data/processed/df_long_processed.csv")


df_long <- df_long %>%
    filter(period == 6) %>%
    mutate(
        generation = as.integer(generation),
        generation_c = as.numeric(generation_c),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )

df_nutrients <- df_long %>%
    select(participant_code, treatment_appeal, generation, generation_c, chain_code, plants_treated, grid_state) %>%
    mutate(tmp = map(grid_state, count_blues_apps_safe)) %>%
    unnest_wider(tmp)

set.seed(42)

# --- Table A1a) Plants treated (Fig A1A)
model_additive_plants <- lmer(
    plants_treated ~ generation_c + treatment_appeal + (1 | chain_code),
    data = df_nutrients,
    REML = FALSE
)

model_interaction_plants <- lmer(
    plants_treated ~ generation_c * treatment_appeal + (1 | chain_code),
    data = df_nutrients,
    REML = FALSE
)

# --- Table A1b) Blue nutrients (Fig A1B)
model_additive_blues <- lmer(
    n_blues ~ generation_c + treatment_appeal + (1 | chain_code),
    data = df_nutrients,
    REML = FALSE
)

model_interaction_blues <- lmer(
    n_blues ~ generation_c * treatment_appeal + (1 | chain_code),
    data = df_nutrients,
    REML = FALSE
)

# --- Table A1c) Total nutrients (Fig A1C)
model_additive_total <- lmer(
    n_apps ~ generation_c + treatment_appeal + (1 | chain_code),
    data = df_nutrients,
    REML = FALSE
)

model_interaction_total <- lmer(
    n_apps ~ generation_c * treatment_appeal + (1 | chain_code),
    data = df_nutrients,
    REML = FALSE
)

table_a1_results <- bind_rows(
    extract_fixed_effects(model_additive_plants, "A. Plants treated: additive"),
    extract_fixed_effects(model_interaction_plants, "A. Plants treated: interaction"),
    extract_fixed_effects(model_additive_blues, "B. Blue nutrients: additive"),
    extract_fixed_effects(model_interaction_blues, "B. Blue nutrients: interaction"),
    extract_fixed_effects(model_additive_total, "C. Total nutrients: additive"),
    extract_fixed_effects(model_interaction_total, "C. Total nutrients: interaction")
)

table_a1_results <- add_model_spacer_rows(table_a1_results)

write_csv(table_a1_results, "output/supplementary_table1.csv")
write_simple_html_table(table_a1_results, "output/supplementary_table1.html")
message("Wrote output/supplementary_table1.csv and output/supplementary_table1.html")
