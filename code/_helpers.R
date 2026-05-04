treatment_names <- c(
  "high_appeal" = "High appeal",
  "low_appeal" = "Low appeal"
)

treatment_colors <- c(
  "high_appeal" = "#6A9FCC",
  "low_appeal" = "#C0C0C0"
)

# Darker variants of treatment_colors, used for error bars so they stand out
# against the lighter bars/lines/points that share the same hue.
treatment_colors_dark <- c(
  "high_appeal" = "#3A6A99",
  "low_appeal"  = "#707070"
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
