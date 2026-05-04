library(tidyverse)
library(jsonlite)
library(patchwork)
source("code/_helpers.R")

# -- Data ----------------------------------------------------------------------
df_long   <- read_csv("data/df_long.csv",                 show_col_types = FALSE)
df_advice <- read_csv("data/df_advice_manual_coding.csv", show_col_types = FALSE)

# -- Transmitted solutions (period 6) with nutrient counts --------------------
df_trans <- df_long %>%
    filter(period == 6) %>%
    select(participant_code, treatment_appeal, generation, chain_code, grid_state, plants_treated) %>%
    mutate(tmp = map(grid_state, parse_grid_safe)) %>%
    unnest_wider(tmp) %>%
    mutate(
        is_optimal   = n_blue == 9 & n_yellow == 0 & n_red == 0 & plants_treated == 9,
        is_only_blue = n_blue > 0  & n_yellow == 0 & n_red == 0 & !is_optimal,
        is_blue_pred = n_blue > (n_yellow + n_red) & !is_optimal & !(n_blue > 0 & n_yellow == 0 & n_red == 0)
    )

# -- Chain numbering: sorted by blue status at gen 4, then 3, then 2, then 1 --
chain_status_by_gen <- df_trans %>%
    mutate(
        gen_status = case_when(
            is_optimal   ~ 1L,
            is_only_blue ~ 2L,
            is_blue_pred ~ 3L,
            TRUE         ~ 4L
        )
    ) %>%
    group_by(treatment_appeal, chain_code, generation) %>%
    summarise(gen_status = min(gen_status, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = generation, values_from = gen_status,
                names_prefix = "gen", values_fill = 4L)

chain_nums <- chain_status_by_gen %>%
    arrange(treatment_appeal == "low_appeal", gen1, gen2, gen3, gen4, chain_code) %>%
    mutate(chain_num = row_number())

# -- Favor Blue advice flag per chain - generation ----------------------------
advice_flags <- df_advice %>%
    group_by(chain_code, generation) %>%
    summarise(has_favor_blue = any(blue_prioritize == 1, na.rm = TRUE), .groups = "drop")

# -- Panel A: stacked bar chart - blue solution categories -------------------
bar_counts <- df_trans %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        `Optimal solution` = sum(is_optimal,   na.rm = TRUE),
        `Only blue`        = sum(is_only_blue, na.rm = TRUE),
        `Blue predominant` = sum(is_blue_pred, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_longer(c(`Optimal solution`, `Only blue`, `Blue predominant`), names_to = "type", values_to = "n") %>%
    mutate(type = factor(type, levels = c("Blue predominant", "Only blue", "Optimal solution")))

FigA10 <- ggplot(bar_counts, aes(x = factor(generation), y = n, fill = type)) +
    geom_col(position = "stack", width = 0.7) +
    facet_wrap(~treatment_appeal, labeller = as_labeller(treatment_names)) +
    scale_fill_manual(values = c("Optimal solution" = "#08306b", "Only blue" = "#1a6faf", "Blue predominant" = "#74b9d4")) +
    scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = expansion(mult = c(0, 0.02))) +
    labs(x = "Generation", y = "Number of solutions", fill = NULL) +
    make_base_theme() + theme_bw() +
    theme(legend.position = "top", panel.grid = element_blank())

# -- Panel B: tile plot - blue status per chain - generation ------------------
tile_data <- df_trans %>%
    group_by(chain_code, treatment_appeal, generation) %>%
    summarise(
        optimal   = any(is_optimal,   na.rm = TRUE),
        only_blue = any(is_only_blue, na.rm = TRUE),
        pred_blue = any(is_blue_pred, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    left_join(chain_nums,   by = c("treatment_appeal", "chain_code")) %>%
    left_join(advice_flags, by = c("chain_code", "generation")) %>%
    mutate(
        blue_status = case_when(
            optimal   ~ "Optimal solution",
            only_blue ~ "Only blue",
            pred_blue ~ "Predominantly blue",
            TRUE      ~ "Neither"
        ),
        blue_status = factor(blue_status, levels = c("Optimal solution", "Only blue", "Predominantly blue", "Neither")),
        chain_label = factor(
            paste("Chain", chain_num),
            levels = paste("Chain", sort(unique(chain_num), decreasing = TRUE))
        )
    )

FigA10_tile <- ggplot(tile_data, aes(x = factor(generation), y = chain_label, fill = blue_status)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_point(
        data  = ~ filter(.x, has_favor_blue),
        aes(color = "Favor Blue advice"),
        size = 1.5, shape = 16
    ) +
    facet_wrap(~treatment_appeal, labeller = as_labeller(treatment_names), scales = "free_y", nrow = 1) +
    scale_fill_manual(
        values = c("Optimal solution" = "#08306b", "Only blue" = "#1a6faf", "Predominantly blue" = "#74b9d4", "Neither" = "#e8e8e8"),
        breaks = c("Optimal solution", "Only blue", "Predominantly blue")
    ) +
    scale_color_manual(
        values = c("Favor Blue advice" = "#00aaff"),
        name   = NULL,
        guide  = guide_legend(override.aes = list(size = 2))
    ) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    labs(x = "Generation", y = "Chain", fill = NULL) +
    make_base_theme() + theme_bw() +
    theme(legend.position = "top", axis.text.y = element_text(size = 7), panel.grid = element_blank())

# -- Combine and save ----------------------------------------------------------
combined_a10 <- (FigA10 / FigA10_tile) +
    plot_layout(heights = c(1, 1.6)) +
    plot_annotation(tag_levels = "A")

ggsave("figures/figA10.png", combined_a10, width = 8.5, height = 9, dpi = 300)
