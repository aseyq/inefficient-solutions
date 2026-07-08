library(tidyverse)

# Instead of repeating the same code in multiple scripts, we process
# the data once and save it to a new file. This script may be run once
# to generate the processed data file, which is then used in other scripts.

df <- read_csv("data/df_long.csv") %>%
    mutate(net_payoff = 72 + 80 * plants_treated - cost) %>%
    mutate(generation_c = generation - 1)

write_csv(df, "data/processed/df_long_processed.csv")
