library(tidyverse)
source("code/_helpers.R")
# This is a helper script for plots A10, A11, A12, A13, and A14, for them to use the same
# chain ordering Chains sorted by mean score (points) in the final generation, within each treatment.
# This makes it easier to compare patterns across chains.


df_long <- read_csv("data/df_long.csv", show_col_types = FALSE)

chain_avg_score <- df_long %>%
    filter(generation == max(generation)) %>%
    group_by(treatment_appeal, chain_code) %>%
    summarise(mean_score = mean(points, na.rm = TRUE), .groups = "drop")

chain_order <- chain_avg_score %>%
    arrange(treatment_appeal == "low_appeal", desc(mean_score), chain_code) %>%
    mutate(chain_num = row_number()) %>%
    # chain_num_plot: within-treatment rank reversed so that chain_num_plot=1 is
    # the worst chain (plotted at the bottom) and the highest value is the best
    # chain (plotted at the top).
    group_by(treatment_appeal) %>%
    mutate(
        chain_num_plot = n() - row_number() + 1, # 1=worst (bottom), n=best (top)
        chain_rank     = row_number() # 1=best (top), n=worst (bottom)
    ) %>%
    ungroup() %>%
    select(treatment_appeal, chain_code, chain_num, chain_num_plot, chain_rank)

write_csv(chain_order, "data/processed/chain_order.csv")
message("Saved ", nrow(chain_order), " chains to data/processed/chain_order.csv")
