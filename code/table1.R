library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(sjPlot)


# get data
df_long <- read_csv("data/df_long.csv")  
# df_wide <- read_csv("data/df_wide.csv")  

df_long <- df_long %>% 
  mutate(net_payoff = 72 + 80 * plants_treated - cost)  %>% 
  filter(period == 6)


# LOW-Appeal only
model_low <- lmer(net_payoff ~ generation + (1 | chain_code), 
                  data = df_long %>% filter(treatment_appeal == "low_appeal"))

# HIGH-Appeal only
model_high <- lmer(net_payoff ~ generation + (1 | chain_code), 
                   data = df_long %>% filter(treatment_appeal == "high_appeal"))

# POOLED model with interaction
model_pooled <- lmer(net_payoff ~ generation * treatment_appeal + (1 | chain_code), 
                     data = df_long)


print(tab_model(model_low, model_high, model_pooled,
          show.ci = FALSE,
          show.se = TRUE,
          show.re.var = FALSE,
          dv.labels = c("Low-Appeal only", "High-Appeal only", "Pooled"),
          title = "A) Scores",
          file = "figures/model_scores_table.html"))


## Cost models
# LOW-Appeal only
model_low_cost <- lmer(cost ~ generation + (1 | chain_code), 
                       data = df_long %>% filter(treatment_appeal == "low_appeal"))

# HIGH-Appeal only
model_high_cost <- lmer(cost ~ generation + (1 | chain_code), 
                        data = df_long %>% filter(treatment_appeal == "high_appeal"))

# POOLED model with interaction
model_pooled_cost <- lmer(cost ~ generation * treatment_appeal + (1 | chain_code), 
                          data = df_long)

# Table with standard errors
print(tab_model(model_low_cost, model_high_cost, model_pooled_cost,
          show.ci = FALSE,
          show.se = TRUE,
          show.re.var = FALSE,
          dv.labels = c("Low-Appeal only", "High-Appeal only", "Pooled"),
          title = "B) Cost",
        file = "figures/model_cost_table.html"))
