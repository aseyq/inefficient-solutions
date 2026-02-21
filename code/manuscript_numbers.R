library(tidyverse)
library(lme4)
library(lmerTest)
library(kableExtra)

df_long <- read_csv("data/df_long.csv")

df_chain_selections <- df_long %>%
    group_by(chain_code, generation, participant_code) %>%
    slice(1) %>%
    arrange(chain_code, generation) %>%
    select(
        treatment_appeal,
        chain_code,
        generation,
        participant_code,
        selected_feedback_author,
        selected_feedback_score,
        feedback_score
    ) %>%
    group_by(chain_code, generation) %>%
    mutate(max_feedback_score = max(feedback_score)) %>%
    ungroup()

df_chain_max_score_available <- df_chain_selections %>%
    group_by(chain_code, generation) %>%
    slice(1) %>%
    select(chain_code, generation, max_feedback_score_to_select = max_feedback_score) %>%
    mutate(generation = generation + 1) %>%
    filter(generation <= 4)

df_chain_selections2 <- df_chain_selections %>%
    left_join(df_chain_max_score_available, by = c("chain_code", "generation")) %>%
    select(-max_feedback_score) %>%
    mutate(selected_feedback_ismax = selected_feedback_score == max_feedback_score_to_select) %>%
    filter(generation > 1) %>%
    mutate(
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    )


selection_probability_by_treatment <- df_chain_selections2 %>%
    group_by(treatment_appeal) %>%
    summarise(prop_selected_max = mean(selected_feedback_ismax), .groups = "drop")

print(selection_probability_by_treatment)


selection_prob_model <- glm(
    selected_feedback_ismax ~ treatment_appeal,
    data = df_chain_selections2,
    family = binomial(link = "logit")
)

model_coef <- summary(selection_prob_model)$coefficients
treatment_row <- "treatment_appeallow_appeal"

if (!treatment_row %in% rownames(model_coef)) {
    stop("Treatment coefficient not found. Check treatment_appeal coding.")
}

beta <- unname(model_coef[treatment_row, "Estimate"])
p_value <- unname(model_coef[treatment_row, "Pr(>|z|)"])
ci <- suppressMessages(confint.default(selection_prob_model, parm = treatment_row, level = 0.95))

selection_prob_test <- tibble(
    term = treatment_row,
    beta = beta,
    ci_lower_95 = ci[1],
    ci_upper_95 = ci[2],
    p_value = p_value
)

# print(selection_prob_test)


df_chain_selections2 <- df_chain_selections2 %>%
    mutate(selected_feedback_ismax_num = as.numeric(selected_feedback_ismax))

selection_prob_lmer <- lmer(
    selected_feedback_ismax_num ~ treatment_appeal + (1 | chain_code),
    data = df_chain_selections2,
    REML = FALSE
)

lmer_coef <- summary(selection_prob_lmer)$coefficients
lmer_treatment_row <- "treatment_appeallow_appeal"

if (!lmer_treatment_row %in% rownames(lmer_coef)) {
    stop("LMER treatment coefficient not found. Check treatment_appeal coding.")
}

lmer_beta <- unname(lmer_coef[lmer_treatment_row, "Estimate"])
lmer_p_value <- unname(lmer_coef[lmer_treatment_row, "Pr(>|t|)"])
lmer_ci_all <- suppressMessages(confint(selection_prob_lmer, method = "Wald", level = 0.95))
lmer_ci <- lmer_ci_all[lmer_treatment_row, ]

selection_prob_lmer_test <- tibble(
    term = lmer_treatment_row,
    beta = lmer_beta,
    ci_lower_95 = lmer_ci[1],
    ci_upper_95 = lmer_ci[2],
    p_value = lmer_p_value
)

print(selection_prob_lmer_test)

# Combined manuscript table: logit and lmer side-by-side in one HTML output
avg_high <- selection_probability_by_treatment %>%
    filter(treatment_appeal == "high_appeal") %>%
    pull(prop_selected_max)

avg_low <- selection_probability_by_treatment %>%
    filter(treatment_appeal == "low_appeal") %>%
    pull(prop_selected_max)

table_manu1 <- bind_rows(
    selection_prob_test %>% mutate(model = "Logit"),
    selection_prob_lmer_test %>% mutate(model = "LMER (LPM)")
) %>%
    transmute(
        model,
        term,
        avg_low,
        avg_high,
        beta,
        ci_lower_95,
        ci_upper_95,
        p_value
    )

table_manu1 %>%
    mutate(
        across(c(avg_low, avg_high, beta, ci_lower_95, ci_upper_95, p_value), ~ round(.x, 3))
    ) %>%
    kbl(
        format = "html",
        col.names = c(
            "Model",
            "Term",
            "Avg low-appeal",
            "Avg high-appeal",
            "Beta",
            "CI lower (95%)",
            "CI upper (95%)",
            "p-value"
        ),
        caption = "Selection probability difference by treatment: logit and LMER"
    ) %>%
    kable_styling(full_width = FALSE) %>%
    save_kable("figures/table_manuscript_values.html")
