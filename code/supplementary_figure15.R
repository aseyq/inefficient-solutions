library(tidyverse)
library(patchwork)
source("code/_helpers.R")

# get data from no-pay experiment
df_long_nopay <- read_csv("data/df_long_nopay.csv")

theme_set(theme_bw())

df_long_feedbacks <- df_long_nopay %>%
    mutate(
        generation = as.integer(generation),
        generation_c = generation - 1,
        treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal")),
        net_payoff = 72 + 80 * plants_treated - cost
    ) %>%
    filter(period == 6)

panel_margin <- margin(6, 8, 6, 8)

figA15_netpayoff_summary <- df_long_feedbacks %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        mean = mean(net_payoff),
        sd = sd(net_payoff),
        n = n(),
        .groups = "drop"
    ) %>%
    mutate(
        se = sd / sqrt(n),
        generation_f = factor(generation)
    )

figA15_cost_summary <- df_long_feedbacks %>%
    group_by(treatment_appeal, generation) %>%
    summarise(
        mean = mean(cost),
        sd = sd(cost),
        n = n(),
        .groups = "drop"
    ) %>%
    mutate(
        se = sd / sqrt(n),
        generation_f = factor(generation)
    )

### Net payoff of transmitted solutions
FigA15A <- figA15_netpayoff_summary %>%
    ggplot(aes(x = generation_f, y = mean, color = treatment_appeal, group = treatment_appeal)) +
    geom_violin(
        data = df_long_feedbacks %>% mutate(generation_f = factor(generation)),
        aes(
            x = generation_f,
            y = net_payoff,
            fill = treatment_appeal,
            group = interaction(generation_f, treatment_appeal)
        ),
        inherit.aes = FALSE,
        position = position_dodge(width = 0.45),
        width = 1,
        alpha = 0.18,
        color = NA,
        linewidth = 0.0,
        trim = FALSE,
        na.rm = TRUE
    ) +
    geom_line(linewidth = 0.5) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
        width = 0, linewidth = 0.9
    ) +
    geom_point(size = 4) +
    scale_color_manual(labels = treatment_names, values = treatment_colors) +
    scale_fill_manual(labels = treatment_names, values = treatment_colors, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), breaks = seq(500, 885, 50)) +
    coord_cartesian(ylim = c(500, 775)) +
    labs(x = "Generation", y = "Score of transmitted solutions") +
    guides(color = guide_legend(title = NULL), linewidth = "none") +
    theme_classic() +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.margin = panel_margin
    )

### Cost of transmitted solutions
FigA15B <- figA15_cost_summary %>%
    ggplot(aes(x = generation_f, y = mean, color = treatment_appeal, group = treatment_appeal)) +
    geom_violin(
        data = df_long_feedbacks %>% mutate(generation_f = factor(generation)),
        aes(
            x = generation_f,
            y = cost,
            fill = treatment_appeal,
            group = interaction(generation_f, treatment_appeal)
        ),
        inherit.aes = FALSE,
        position = position_dodge(width = 0.45),
        width = 1,
        alpha = 0.18,
        color = NA,
        linewidth = 0.0,
        trim = FALSE,
        na.rm = TRUE
    ) +
    geom_hline(yintercept = 98, linetype = "dotted", color = "black", linewidth = 0.5, alpha = .5) +
    geom_line(linewidth = 0.5) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = I(treatment_colors_dark[treatment_appeal])),
        width = 0, linewidth = 0.9
    ) +
    geom_point(size = 4) +
    scale_color_manual(labels = treatment_names, values = treatment_colors) +
    scale_fill_manual(labels = treatment_names, values = treatment_colors, guide = "none") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Generation", y = "Cost of transmitted solutions") +
    guides(color = guide_legend(title = NULL), linewidth = "none") +
    theme_classic() +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.margin = panel_margin
    )

supplementary_figure15 <- wrap_plots(FigA15A, FigA15B) +
    plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(
        plot.margin = margin(0, 20, 0, 20),
        plot.tag = element_text(size = 16, face = "bold"),
        legend.position = "top",
        legend.justification = "center",
        legend.box.just = "center"
    )

fig.width <- 12
fig.height <- 5

ggsave("output/supplementary_figure15.png", supplementary_figure15, width = fig.width, height = fig.height, dpi = 300)
ggsave("output/supplementary_figure15.pdf", supplementary_figure15, width = fig.width, height = fig.height, device = grDevices::cairo_pdf)
ggsave("output/supplementary_figure15.svg", supplementary_figure15, width = fig.width, height = fig.height, device = svglite::svglite)
