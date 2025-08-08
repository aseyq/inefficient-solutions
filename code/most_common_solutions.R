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

df_totals<- df_long  %>% 
    group_by(treatment_appeal)  %>% 
    summarise(n_treatment = n())  


most_common_solutions <- df_long  %>% 
    group_by(grid_state_flatten, treatment_appeal, cost, net_payoff)  %>% 
    summarise(n = n())  %>% 
    arrange(desc(n))   %>% 
    group_by(treatment_appeal)  %>%
    slice(1:2)  %>% 
    left_join(df_totals, by = "treatment_appeal")  %>%
    mutate(p_transmission = round(n/n_treatment,2)) 
    # save it to figures

print(most_common_solutions)
most_common_solutions %>%
    write_csv("figures/most_common_solutions.csv")

glimpse(df_long)
