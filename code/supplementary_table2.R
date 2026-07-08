library(tidyverse)
library(lme4)
library(lmerTest)
source("code/_helpers.R")

dir.create("output", showWarnings = FALSE, recursive = TRUE)

df_long <- read_csv("data/processed/df_long_processed.csv")
participant_metrics <- build_social_influence_participant_metrics(df_long)

set.seed(42)

participant_metrics_test <- participant_metrics %>%
    filter(generation >= 2) %>%
    mutate(
        copied_first_trial_num = as.numeric(copied_first_trial),
        generation_c2 = generation - 2
    )


participant_metrics_test %>%
    group_by(treatment_appeal) %>%
    summarise(
        mean_copied_first_trial = mean(copied_first_trial_num, na.rm = TRUE),
        sd_copied_first_trial = sd(copied_first_trial_num, na.rm = TRUE),
        mean_distance_inherited_first_trial = mean(distance_inherited_first_trial, na.rm = TRUE),
        sd_distance_inherited_first_trial = sd(distance_inherited_first_trial, na.rm = TRUE),
    )

# A) Copy probability
copy_prob_model_additive <- lmer(
    copied_first_trial_num ~ treatment_appeal + generation_c2 + (1 | chain_code),
    data = participant_metrics_test,
    REML = FALSE
)

copy_prob_model_interaction <- lmer(
    copied_first_trial_num ~ treatment_appeal * generation_c2 + (1 | chain_code),
    data = participant_metrics_test,
    REML = FALSE
)

# B) Distance inherited to first trial
distance_first_trial_model_additive <- lmer(
    distance_inherited_first_trial ~ treatment_appeal + generation_c2 + (1 | chain_code),
    data = participant_metrics_test,
    REML = FALSE
)

distance_first_trial_model_interaction <- lmer(
    distance_inherited_first_trial ~ treatment_appeal * generation_c2 + (1 | chain_code),
    data = participant_metrics_test,
    REML = FALSE
)

# C) Distance inherited to transmitted
distance_transmitted_model_additive <- lmer(
    distance_inherited_transmitted ~ treatment_appeal + generation_c2 + (1 | chain_code),
    data = participant_metrics_test,
    REML = FALSE
)

distance_transmitted_model_interaction <- lmer(
    distance_inherited_transmitted ~ treatment_appeal * generation_c2 + (1 | chain_code),
    data = participant_metrics_test,
    REML = FALSE
)

table2 <- bind_rows(
    extract_fixed_effects(copy_prob_model_additive, "A. Copy probability: additive"),
    extract_fixed_effects(copy_prob_model_interaction, "A. Copy probability: interaction"),
    extract_fixed_effects(distance_first_trial_model_additive, "B. Distance first trial: additive"),
    extract_fixed_effects(distance_first_trial_model_interaction, "B. Distance first trial: interaction"),
    extract_fixed_effects(distance_transmitted_model_additive, "C. Distance transmitted: additive"),
    extract_fixed_effects(distance_transmitted_model_interaction, "C. Distance transmitted: interaction")
)

table2 <- add_model_spacer_rows(table2)

write_csv(table2, "output/supplementary_table2.csv")
write_simple_html_table(table2, "output/supplementary_table2.html")
message("Wrote output/supplementary_table2.csv and output/supplementary_table2.html")
