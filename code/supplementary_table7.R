library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

# get data from no-pay experiment
df_long <- read_csv("data/df_long_nopay.csv") |>
    mutate(
        generation = as.integer(generation),
        generation_c = generation - 1,
        net_payoff = 72 + 80 * plants_treated - cost,
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) |>
    filter(period == 6)

# A) Scores
model_low <- lmer(
    net_payoff ~ generation_c + (1 | chain_code),
    data = df_long |> filter(treatment_appeal == "low_appeal"),
    REML = FALSE
)

model_high <- lmer(
    net_payoff ~ generation_c + (1 | chain_code),
    data = df_long |> filter(treatment_appeal == "high_appeal"),
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
    data = df_long |> filter(treatment_appeal == "low_appeal"),
    REML = FALSE
)

model_high_cost <- lmer(
    cost ~ generation_c + (1 | chain_code),
    data = df_long |> filter(treatment_appeal == "high_appeal"),
    REML = FALSE
)

model_interaction_cost <- lmer(
    cost ~ treatment_appeal * generation_c + (1 | chain_code),
    data = df_long,
    REML = FALSE
)

supplementary_table7 <- bind_rows(
    extract_fixed_effects(model_low, "A Scores - Low-appeal only"),
    extract_fixed_effects(model_high, "A Scores - High-appeal only"),
    extract_fixed_effects(model_interaction, "A Scores - Pooled (Treatment x generation_c)"),
    extract_fixed_effects(model_low_cost, "B Cost - Low-appeal only"),
    extract_fixed_effects(model_high_cost, "B Cost - High-appeal only"),
    extract_fixed_effects(model_interaction_cost, "B Cost - Pooled (Treatment x generation_c)")
)

supplementary_table7 <- add_model_spacer_rows(supplementary_table7)

write_csv(supplementary_table7, "output/supplementary_table7.csv")
write_simple_html_table(supplementary_table7, "output/supplementary_table7.html")
