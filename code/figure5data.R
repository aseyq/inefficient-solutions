library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(sjPlot)

# get data
df_long <- read_csv("data/processed/df_long_processed.csv")

df_long <- df_long %>%
    filter(period == 6)

df_totals <- df_long %>%
    group_by(treatment_appeal) %>%
    summarise(n_treatment = n(), .groups = "drop")

most_common_solutions <- df_long %>%
    group_by(grid_state_flatten, treatment_appeal, cost, net_payoff) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n)) %>%
    group_by(treatment_appeal) %>%
    slice(1:2) %>%
    left_join(df_totals, by = "treatment_appeal") %>%
    mutate(p_transmission = sprintf("%.3f", n / n_treatment))

most_common_solutions %>%
    write_csv("output/figure5data.csv")
