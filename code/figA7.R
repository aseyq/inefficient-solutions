library(tidyverse)
library(patchwork)
source("code/_helpers.R")

semantic_dir <- "data/processed"

base_theme <- make_base_theme()

required_files <- c(
  file.path(semantic_dir, "semantic_similarity_by_treatment_generation.csv"),
  file.path(semantic_dir, "semantic_similarity_by_treatment.csv")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing semantic similarity file(s): ",
      paste(missing_files, collapse = ", "),
      ". Run python code/figA7_calclulate_semantic.py first."
    )
  )
}

semantic_tg <- read_csv(file.path(semantic_dir, "semantic_similarity_by_treatment_generation.csv"), show_col_types = FALSE) %>%
  mutate(
    generation = as.integer(generation),
    treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
  )

semantic_treat <- read_csv(file.path(semantic_dir, "semantic_similarity_by_treatment.csv"), show_col_types = FALSE) %>%
  mutate(treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal")))

FigA7A <- ggplot(
  semantic_treat,
  aes(x = treatment_appeal, y = mean_similarity, fill = treatment_appeal)
) +
  geom_col(width = 0.65, alpha = 0.95) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  expand_limits(y = 0.5) +
  labs(
    x = "Treatment",
    y = "Mean cosine similarity (SBERT)",
    fill = NULL
  ) +
  base_theme +
  theme(legend.position = "none")

FigA7B <- ggplot(
  semantic_tg,
  aes(
    x = generation,
    y = mean_similarity,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(semantic_tg$generation))) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  expand_limits(y = 0.5) +
  labs(
    x = "Generation",
    y = "Mean cosine similarity (SBERT)",
    color = NULL
  ) +
  base_theme

FigA7 <- (FigA7A + FigA7B) +
  plot_layout(widths = c(1.3, 1.0), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top")

ggsave(
  "figures/figA7.png",
  FigA7,
  width = 10.2,
  height = 4.8,
  dpi = 300
)
