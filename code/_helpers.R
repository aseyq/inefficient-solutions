treatment_names <- c(
  "high_appeal" = "High appeal",
  "low_appeal" = "Low appeal"
)

treatment_colors <- c(
  "high_appeal" = "#0058CC",
  "low_appeal" = "#a19f99"
)

# Darker variants of treatment_colors, used for error bars so they stand out
# against the lighter bars/lines/points that share the same hue.
treatment_colors_dark <- c(
  "high_appeal" = "#0058CC",
  "low_appeal"  = "#a19f99"
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

# --- Null-coalescing operator ------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# --- Grid-state JSON parsing -------------------------------------------------
# parse_grid: returns blue / yellow / red counts separately
parse_grid <- function(x) {
  if (is.null(x) || is.na(x) || x == "") {
    return(list(n_blue = NA_integer_, n_yellow = NA_integer_, n_red = NA_integer_))
  }
  cells <- unlist(
    jsonlite::fromJSON(x, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    recursive = FALSE
  )
  getv <- function(cell, nm) as.integer(cell[[nm]] %||% 0L)
  list(
    n_blue   = sum(vapply(cells, getv, integer(1), nm = "blue")),
    n_yellow = sum(vapply(cells, getv, integer(1), nm = "yellow")),
    n_red    = sum(vapply(cells, getv, integer(1), nm = "red"))
  )
}
parse_grid_safe <- purrr::possibly(
  parse_grid,
  otherwise = list(n_blue = NA_integer_, n_yellow = NA_integer_, n_red = NA_integer_)
)

# count_blues_apps: returns total blue count and total nutrient count
count_blues_apps <- function(x) {
  if (is.null(x) || is.na(x) || x == "") {
    return(list(n_blues = NA_integer_, n_apps = NA_integer_))
  }
  cells <- unlist(
    jsonlite::fromJSON(x, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    recursive = FALSE
  )
  getv <- function(cell, nm) as.integer(cell[[nm]] %||% 0L)
  b <- sum(vapply(cells, getv, integer(1), nm = "blue"))
  y <- sum(vapply(cells, getv, integer(1), nm = "yellow"))
  r <- sum(vapply(cells, getv, integer(1), nm = "red"))
  list(n_blues = b, n_apps = b + y + r)
}
count_blues_apps_safe <- purrr::possibly(
  count_blues_apps,
  otherwise = list(n_blues = NA_integer_, n_apps = NA_integer_)
)

# --- Solution vector parsing -------------------------------------------------
# Cache parsed solution vectors so repeated strings are converted only once.
solution_cache <- new.env(parent = emptyenv())

solution_vector <- function(solution) {
  if (is.na(solution) || solution == "") {
    return(rep(NA_integer_, 27))
  }
  if (exists(solution, envir = solution_cache, inherits = FALSE)) {
    return(get(solution, envir = solution_cache, inherits = FALSE))
  }
  tokens <- strsplit(solution, "-", fixed = TRUE)[[1]]
  vec <- as.integer(strsplit(paste(tokens, collapse = ""), "", fixed = TRUE)[[1]])
  assign(solution, vec, envir = solution_cache)
  vec
}

mean_pairwise_distance <- function(solutions) {
  if (length(solutions) < 2) {
    return(NA_real_)
  }
  mat <- do.call(rbind, lapply(solutions, solution_vector))
  mean(as.numeric(dist(mat, method = "manhattan")))
}

solution_distance <- function(solution_a, solution_b) {
  if (is.na(solution_a) || is.na(solution_b) || solution_a == "" || solution_b == "") {
    return(NA_real_)
  }
  sum(abs(solution_vector(solution_a) - solution_vector(solution_b)))
}

build_social_influence_participant_metrics <- function(df_long) {
  df_long %>%
    dplyr::select(dplyr::all_of(c(
      "participant_code", "chain_code", "treatment_appeal", "generation", "period",
      "generation_c", "selected_feedback_grid_state_flatten", "grid_state_flatten"
    ))) %>%
    dplyr::mutate(
      generation = as.integer(generation),
      generation_c = as.integer(generation_c),
      period = as.integer(period),
      treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
    ) %>%
    dplyr::group_by(participant_code, chain_code, treatment_appeal, generation) %>%
    dplyr::arrange(period, .by_group = TRUE) %>%
    dplyr::summarise(
      generation_c = dplyr::first(generation_c),
      inherited_solution = dplyr::first(selected_feedback_grid_state_flatten),
      first_trial_solution = grid_state_flatten[period == 1][1],
      transmitted_solution = grid_state_flatten[period == 6][1],
      copied_first_trial = first_trial_solution == inherited_solution,
      distance_inherited_first_trial = dplyr::if_else(
        generation[1] >= 2,
        solution_distance(inherited_solution, first_trial_solution),
        NA_real_
      ),
      distance_inherited_transmitted = dplyr::if_else(
        generation[1] >= 2,
        solution_distance(inherited_solution, transmitted_solution),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      chain_code = factor(chain_code),
      copied_first_trial = dplyr::if_else(generation >= 2, copied_first_trial, NA)
    )
}

write_boxplot_summary <- function(path, data, value_col, group_col, plot_title, source_desc = NA_character_) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  value <- data[[value_col]]
  group <- data[[group_col]]
  keep <- !is.na(value) & is.finite(value) & !is.na(group)
  value <- value[keep]
  group <- group[keep]

  split_values <- split(value, group, drop = TRUE)

  summary_list <- lapply(names(split_values), function(gname) {
    x <- split_values[[gname]]
    q1 <- as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, type = 7))
    med <- as.numeric(stats::quantile(x, 0.50, na.rm = TRUE, type = 7))
    q3 <- as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, type = 7))
    iqr <- as.numeric(stats::IQR(x, na.rm = TRUE, type = 7))
    whisker_bounds <- c(q1 - 1.5 * iqr, q3 + 1.5 * iqr)
    whisk_stats <- boxplot.stats(x, coef = 1.5, do.conf = FALSE, do.out = TRUE)$stats

    data.frame(
      group_value = gname,
      n = length(x),
      min_raw = min(x, na.rm = TRUE),
      p25 = q1,
      median = med,
      p75 = q3,
      max_raw = max(x, na.rm = TRUE),
      iqr = iqr,
      whisker_lower_bound = whisker_bounds[1],
      whisker_upper_bound = whisker_bounds[2],
      whisker_lower = whisk_stats[1],
      whisker_upper = whisk_stats[5],
      outliers_below = sum(x < whisk_stats[1], na.rm = TRUE),
      outliers_above = sum(x > whisk_stats[5], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  if (length(summary_list) == 0) {
    summary_tbl <- data.frame(
      group_value = character(0),
      n = integer(0),
      min_raw = numeric(0),
      p25 = numeric(0),
      median = numeric(0),
      p75 = numeric(0),
      max_raw = numeric(0),
      iqr = numeric(0),
      whisker_lower_bound = numeric(0),
      whisker_upper_bound = numeric(0),
      whisker_lower = numeric(0),
      whisker_upper = numeric(0),
      outliers_below = integer(0),
      outliers_above = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    summary_tbl <- do.call(rbind, summary_list)
  }

  colnames(summary_tbl)[1] <- group_col
  numeric_cols <- vapply(summary_tbl, is.numeric, logical(1))
  summary_tbl[numeric_cols] <- lapply(summary_tbl[numeric_cols], function(x) round(x, 6))

  source_line <- if (!is.na(source_desc) && nzchar(source_desc)) {
    paste0("Source: ", source_desc)
  } else {
    "Source: (not specified)"
  }

  header <- c(
    paste0("Plot: ", plot_title),
    paste0("Y variable: ", value_col),
    source_line,
    "",
    "Definition used (ggplot2 geom_boxplot / Tukey):",
    "- Centre line: median (50th percentile).",
    "- Box bounds: 25th percentile (Q1) and 75th percentile (Q3).",
    "- IQR: Q3 - Q1.",
    "- Whisker theoretical bounds: Q1 - 1.5*IQR and Q3 + 1.5*IQR.",
    "- Whisker endpoints shown: most extreme observed values within those bounds.",
    "- Min/Max raw: minimum and maximum observed values (including outliers).",
    ""
  )

  table_lines <- capture.output(print(summary_tbl, row.names = FALSE))
  writeLines(c(header, table_lines), con = path, useBytes = TRUE)
  invisible(summary_tbl)
}

extract_fixed_effects <- function(model, model_name) {
  coefs <- as.data.frame(summary(model)$coefficients)
  coefs$Term <- rownames(coefs)

  ci <- as.data.frame(confint(model, parm = "beta_", method = "Wald"))
  ci$Term <- rownames(ci)
  names(ci)[1:2] <- c("ci_low", "ci_high")

  coefs |>
    dplyr::left_join(ci, by = "Term") |>
    dplyr::transmute(
      Model = model_name,
      Term = Term,
      `Estimate (b)` = sprintf("%.2f", Estimate),
      SE = sprintf("%.2f", `Std. Error`),
      `95% CI` = paste0("[", sprintf("%.2f", ci_low), ", ", sprintf("%.2f", ci_high), "]"),
      df = sprintf("%.2f", df),
      t = sprintf("%.2f", `t value`),
      p = dplyr::if_else(
        `Pr(>|t|)` < 0.001,
        "<0.001",
        sprintf("%.3f", `Pr(>|t|)`)
      )
    )
}

add_model_spacer_rows <- function(tbl, model_col = "Model") {
  if (nrow(tbl) == 0) {
    return(tbl)
  }

  model_values <- tbl[[model_col]]
  change_points <- which(model_values[-1] != model_values[-length(model_values)])
  block_ends <- c(change_points, nrow(tbl))

  parts <- vector("list", length(block_ends) * 2 - 1)
  start_idx <- 1
  out_idx <- 1

  for (i in seq_along(block_ends)) {
    end_idx <- block_ends[i]
    parts[[out_idx]] <- tbl[start_idx:end_idx, , drop = FALSE]
    out_idx <- out_idx + 1

    if (i < length(block_ends)) {
      spacer <- as.data.frame(as.list(rep("", ncol(tbl))), stringsAsFactors = FALSE)
      names(spacer) <- names(tbl)
      parts[[out_idx]] <- spacer
      out_idx <- out_idx + 1
    }

    start_idx <- end_idx + 1
  }

  dplyr::bind_rows(parts)
}

write_simple_html_table <- function(tbl, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  header_cells <- paste0("<th>", esc(names(tbl)), "</th>", collapse = "")

  row_html <- apply(tbl, 1, function(r) {
    cells <- paste0("<td>", esc(r), "</td>", collapse = "")
    paste0("<tr>", cells, "</tr>")
  })

  html <- c(
    "<table border='1' cellpadding='6' cellspacing='0'>",
    paste0("<thead><tr>", header_cells, "</tr></thead>"),
    "<tbody>",
    row_html,
    "</tbody>",
    "</table>"
  )

  writeLines(html, path)
}
