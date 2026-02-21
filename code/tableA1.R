library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(sjPlot)


# get data
df_long <- read_csv("data/df_long.csv")
# df_wide <- read_csv("data/df_wide.csv")

df_long <- df_long %>%
  mutate(
    net_payoff = 72 + 80 * plants_treated - cost,
    treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
  ) %>%
  filter(period == 6)

# low-appeal only
model_low <- lmer(
  net_payoff ~ generation + (1 | chain_code),
  data = df_long %>% filter(treatment_appeal == "low_appeal")
)

# high-appeal only
model_high <- lmer(
  net_payoff ~ generation + (1 | chain_code),
  data = df_long %>% filter(treatment_appeal == "high_appeal")
)

# treatment x generation
model_interaction <- lmer(
  net_payoff ~ treatment_appeal * generation + (1 | chain_code),
  data = df_long
)


out_a <- tempfile(fileext = ".html")
out_b <- tempfile(fileext = ".html")


print(tab_model(model_low, model_high, model_interaction,
  show.ci = FALSE,
  show.se = TRUE,
  show.re.var = FALSE,
  dv.labels = c(
    "Low-Appeal only",
    "High-Appeal only",
    "Pooled (Treatment × generation)"
  ),
  title = "A) Scores",
  file = out_a
))


## Cost models
# low-appeal only
model_low_cost <- lmer(
  cost ~ generation + (1 | chain_code),
  data = df_long %>% filter(treatment_appeal == "low_appeal")
)

# high-appeal only
model_high_cost <- lmer(
  cost ~ generation + (1 | chain_code),
  data = df_long %>% filter(treatment_appeal == "high_appeal")
)

# treatment x generation
model_interaction_cost <- lmer(
  cost ~ treatment_appeal * generation + (1 | chain_code),
  data = df_long
)

# Table with standard errors
print(tab_model(model_low_cost, model_high_cost, model_interaction_cost,
  show.ci = FALSE,
  show.se = TRUE,
  show.re.var = FALSE,
  dv.labels = c(
    "Low-Appeal only",
    "High-Appeal only",
    "Pooled (Treatment × generation)"
  ),
  title = "B) Cost",
  file = out_b
))

combined_out <- "figures/table_a1.html"
combined_lines <- c(
  readLines(out_a, warn = FALSE),
  "<br/>",
  readLines(out_b, warn = FALSE)
)
writeLines(combined_lines, combined_out)

file.remove(out_a, out_b)
