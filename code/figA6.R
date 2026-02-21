library(tidyverse)
library(patchwork)
source("code/_helpers.R")

base_theme <- make_base_theme()

clean_prefix <- function(x) {
  x %>%
    stringr::str_replace(
      regex("^\\s*the field yields a higher output when\\s*", ignore_case = TRUE),
      ""
    ) %>%
    stringr::str_squish()
}

df_long <- read_csv("data/df_long.csv") %>%
  select(all_of(c("period", "generation", "treatment_appeal", "feedback_message")))

df_msg <- df_long %>%
  filter(period == 6) %>%
  mutate(
    generation = as.integer(generation),
    treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
  ) %>%
  select(generation, treatment_appeal, feedback_message)

df_msg_metrics <- df_msg %>%
  mutate(
    cleaned_feedback = clean_prefix(feedback_message),
    feedback_length_chars = nchar(cleaned_feedback)
  )

text_summary <- df_msg_metrics %>%
  group_by(treatment_appeal, generation) %>%
  summarise(
    n_participants = n(),
    mean_feedback_length = mean(feedback_length_chars, na.rm = TRUE),
    sd_feedback_length = sd(feedback_length_chars, na.rm = TRUE),
    se_feedback_length = sd_feedback_length / sqrt(n_participants),
    .groups = "drop"
  )

text_summary_treatment <- df_msg_metrics %>%
  group_by(treatment_appeal) %>%
  summarise(
    n_participants = n(),
    mean_feedback_length = mean(feedback_length_chars, na.rm = TRUE),
    sd_feedback_length = sd(feedback_length_chars, na.rm = TRUE),
    se_feedback_length = sd_feedback_length / sqrt(n_participants),
    .groups = "drop"
  )

FigA6A <- ggplot(
  text_summary_treatment,
  aes(
    x = treatment_appeal,
    y = mean_feedback_length,
    color = treatment_appeal
  )
) +
  geom_col(aes(fill = treatment_appeal), width = 0.7, alpha = 0.9, linewidth = 0.3) +
  geom_errorbar(
    aes(
      ymin = mean_feedback_length - se_feedback_length,
      ymax = mean_feedback_length + se_feedback_length
    ),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_fill_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_discrete(labels = treatment_names) +
  scale_y_continuous(
    limits = c(0, 225),
    breaks = seq(0, 225, by = 25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Treatment",
    y = "Feedback length (characters)",
    color = NULL
  ) +
  base_theme

FigA6B <- ggplot(
  text_summary,
  aes(
    x = generation,
    y = mean_feedback_length,
    color = treatment_appeal,
    group = treatment_appeal
  )
) +
  geom_point(size = 2.8) +
  geom_line(linewidth = 0.6) +
  geom_linerange(
    aes(
      ymin = mean_feedback_length - se_feedback_length,
      ymax = mean_feedback_length + se_feedback_length
    ),
    linewidth = 0.4
  ) +
  scale_color_manual(values = treatment_colors, labels = treatment_names) +
  scale_x_continuous(breaks = sort(unique(text_summary$generation))) +
  scale_y_continuous(
    limits = c(0, 225),
    breaks = seq(0, 225, by = 25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Generation",
    y = "Feedback length (characters)",
    color = NULL
  ) +
  base_theme

FigA6 <- (FigA6A + FigA6B) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")

ggsave(
  "figures/figA6.png",
  FigA6,
  width = 10.2,
  height = 4.6,
  dpi = 300
)
