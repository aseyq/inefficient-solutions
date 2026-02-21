library(tidyverse)
library(ggrepel)

input_path <- "data/df_advice_manual_coding.csv"
if (!file.exists(input_path)) {
    stop("Missing required file: ", input_path)
}

category_labels <- c(
    "mix_and_match" = "Mix & Match",
    "shape_based" = "Plant Shape Based",
    "color_based" = "Plant Color Based (Other)",
    "spread" = "Even out/Spread nutrients",
    "mix_nutrients" = "Mix Nutrients",
    "mismatch" = "Mix & Mismatch",
    "spatial" = "Spatial",
    "useall" = "Use All/Most Nutrients",
    "other" = "Other",
    "yellow_prioritize" = "Favor Yellow",
    "blue_prioritize" = "Favor Blue",
    "red_prioritize" = "Favor Red",
    "pure_nutrients" = "Pure Nutrients",
    "time" = "Time-Based",
    "less_per_plant" = "Less per Plant",
    "more_per_plant" = "More per Plant",
    "variation" = "Variation",
    "noinfo" = "No Information"
)

category_colors_base <- c(
    "mix_and_match" = "#B5179E",
    "shape_based" = "#7B2CBF",
    "color_based" = "#3300ff",
    "spread" = "#20ce01",
    "mix_nutrients" = "#E56B6F",
    "mismatch" = "#FB8500",
    "spatial" = "#09b030",
    "useall" = "#c95101",
    "other" = "#4B5563",
    "yellow_prioritize" = "#d69e19",
    "blue_prioritize" = "#0008ff",
    "red_prioritize" = "#DC2626",
    "pure_nutrients" = "#1D3557",
    "time" = "#6D28D9",
    "less_per_plant" = "#2A9D8F",
    "more_per_plant" = "#E76F51",
    "variation" = "#A21CAF",
    "oth" = "#8A5A44",
    "noinfo" = "#6B7280"
)

df_advice <- read_csv(input_path, show_col_types = FALSE)

if ("n" %in% names(df_advice)) {
    category_start <- which(names(df_advice) == "n") + 1
    plot_categories <- names(df_advice)[category_start:ncol(df_advice)]
} else {
    non_category_cols <- c(
        "participant_code", "chain_code", "generation", "treatment_appeal",
        "feedback_message", "cleaned_feedback"
    )
    plot_categories <- setdiff(names(df_advice), non_category_cols)
}

if (length(plot_categories) == 0) {
    stop("No category columns found in input file.")
}

missing_color_measures <- setdiff(plot_categories, names(category_colors_base))
if (length(missing_color_measures) > 0) {
    fallback_colors <- scales::hue_pal()(length(missing_color_measures))
    names(fallback_colors) <- missing_color_measures
    category_colors <- c(category_colors_base, fallback_colors)
} else {
    category_colors <- category_colors_base
}

category_labels_present <- setNames(
    recode(plot_categories, !!!category_labels, .default = plot_categories),
    plot_categories
)

df_advice <- df_advice %>%
    mutate(
        generation = as.integer(generation),
        across(all_of(plot_categories), ~ coalesce(as.numeric(.x), 0))
    )

manual_summary <- df_advice %>%
    select(all_of(c("generation", "treatment_appeal", plot_categories))) %>%
    pivot_longer(cols = all_of(plot_categories), names_to = "measure", values_to = "value") %>%
    group_by(generation, treatment_appeal, measure) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(treatment_appeal))

manual_summary <- manual_summary %>%
    group_by(treatment_appeal, measure) %>%
    filter(max(value, na.rm = TRUE) > 0) %>%
    ungroup()

label_summary <- manual_summary %>%
    group_by(treatment_appeal, measure) %>%
    filter(value > 0) %>%
    filter(generation == max(generation[value > 0], na.rm = TRUE)) %>%
    mutate(measure_label = recode(measure, !!!category_labels, .default = measure)) %>%
    ungroup()

FigA8 <- ggplot(manual_summary, aes(x = generation, y = value, color = measure)) +
    geom_line(linetype = 2) +
    geom_point() +
    facet_wrap(
        treatment_appeal ~ .,
        labeller = as_labeller(c("high_appeal" = "High Appeal", "low_appeal" = "Low Appeal"))
    ) +
    geom_label_repel(
        data = label_summary,
        aes(label = measure_label),
        size = 2.75,
        hjust = 0,
        show.legend = FALSE,
        fill = scales::alpha("white", 0.6),
        label.size = 0.1,
        nudge_x = 0.2,
        direction = "y",
        segment.color = "gray20",
        segment.linetype = "dotted",
        segment.alpha = 0.4,
        min.segment.length = 0,
        box.padding = 0.00,
        point.padding = 0.1,
        force = 1,
        force_pull = 1,
        max.iter = 20000,
        seed = 1,
        max.overlaps = Inf
    ) +
    scale_color_manual(values = category_colors, labels = category_labels_present) +
    scale_x_continuous(breaks = 1:4) +
    labs(
        x = "Generation",
        y = "Advice content",
        color = "Advice"
    ) +
    theme_bw() +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(5.5, 80, 5.5, 5.5)
    ) +
    expand_limits(x = c(1, 5.7), y = c(0, 40)) +
    coord_cartesian(clip = "off") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

ggsave("figures/figA8.png", plot = FigA8, width = 11.5, height = 6, dpi = 300)
