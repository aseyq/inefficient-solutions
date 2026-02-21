treatment_names <- c(
  "high_appeal" = "High appeal",
  "low_appeal" = "Low appeal"
)

treatment_colors <- c(
  "high_appeal" = "#6A9FCC",
  "low_appeal" = "#C0C0C0"
)

make_base_theme <- function(legend_position = "top") {
  theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = legend_position
    )
}
