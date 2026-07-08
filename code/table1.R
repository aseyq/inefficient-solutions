library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

# get data
df_long <- read_csv("data/processed/df_long_processed.csv")


df_long <- df_long %>%
    mutate(
        generation = as.integer(generation),
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) %>%
    filter(period == 6)

set.seed(42)

# A) Scores
model_low <- lmer(
    net_payoff ~ generation_c + (1 | chain_code),
    data = df_long %>% filter(treatment_appeal == "low_appeal"),
    REML = FALSE
)

model_high <- lmer(
    net_payoff ~ generation_c + (1 | chain_code),
    data = df_long %>% filter(treatment_appeal == "high_appeal"),
    REML = FALSE
)

model_interaction <- lmer(
    net_payoff ~ treatment_appeal * generation_c + (1 | chain_code),
    data = df_long,
    REML = FALSE
)

# B) Cost
model_low_cost <- lmer(
    cost ~ generation_c + (1 | chain_code),
    data = df_long %>% filter(treatment_appeal == "low_appeal"),
    REML = FALSE
)

model_high_cost <- lmer(
    cost ~ generation_c + (1 | chain_code),
    data = df_long %>% filter(treatment_appeal == "high_appeal"),
    REML = FALSE
)

model_interaction_cost <- lmer(
    cost ~ treatment_appeal * generation_c + (1 | chain_code),
    data = df_long,
    REML = FALSE
)

table1 <- bind_rows(
    extract_fixed_effects(model_low, "A. Scores: low appeal"),
    extract_fixed_effects(model_high, "A. Scores: high appeal"),
    extract_fixed_effects(model_interaction, "A. Scores: interaction"),
    extract_fixed_effects(model_low_cost, "B. Cost: low appeal"),
    extract_fixed_effects(model_high_cost, "B. Cost: high appeal"),
    extract_fixed_effects(model_interaction_cost, "B. Cost: interaction")
)

table1 <- add_model_spacer_rows(table1)

write_csv(table1, "output/table1.csv")
write_simple_html_table(table1, "output/table1.html")
message("Wrote output/table1.csv and output/table1.html")
