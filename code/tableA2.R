library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(jsonlite)
library(purrr)

# get data
df_long <- read_csv("data/df_long.csv")

df_long <- df_long %>%
  filter(period == 6) %>%
  mutate(
    generation = as.integer(generation),
    treatment_appeal = factor(treatment_appeal, levels = c("high_appeal", "low_appeal"))
  )

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

count_blues_apps <- function(x) {
  if (is.null(x) || is.na(x) || x == "") {
    return(list(n_blues = NA_integer_, n_apps = NA_integer_))
  }

  grid <- jsonlite::fromJSON(
    x,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )

  # grid is 3 lists (rows), each containing 3 cells; flatten one level to 9 cells
  cells <- unlist(grid, recursive = FALSE)

  getv <- function(cell, nm) as.integer(cell[[nm]] %||% 0L)

  b <- sum(vapply(cells, getv, integer(1), nm = "blue"))
  y <- sum(vapply(cells, getv, integer(1), nm = "yellow"))
  r <- sum(vapply(cells, getv, integer(1), nm = "red"))

  list(n_blues = b, n_apps = b + y + r)
}

# If any rows contain malformed JSON, this will return NA instead of erroring
count_blues_apps_safe <- purrr::possibly(
  count_blues_apps,
  otherwise = list(n_blues = NA_integer_, n_apps = NA_integer_)
)

df_nutrients <- df_long %>%
  select(participant_code, treatment_appeal, generation, chain_code, plants_treated, grid_state) %>%
  mutate(tmp = map(grid_state, count_blues_apps_safe)) %>%
  unnest_wider(tmp)

out_a <- tempfile(fileext = ".html")
out_b <- tempfile(fileext = ".html")
out_c <- tempfile(fileext = ".html")

# --- Table A2a) Plants treated (Fig A1A)

model_additive_plants <- lmer(
  plants_treated ~ generation + treatment_appeal + (1 | chain_code),
  data = df_nutrients
)

model_interaction_plants <- lmer(
  plants_treated ~ generation * treatment_appeal + (1 | chain_code),
  data = df_nutrients
)

print(
  tab_model(
    model_additive_plants,
    model_interaction_plants,
    show.ci = FALSE,
    show.se = TRUE,
    show.re.var = FALSE,
    dv.labels = c(
      "Pooled (Generation + treatment)",
      "Pooled (Treatment × generation)"
    ),
    title = "Table A2a) Plants treated",
    file = out_a
  )
)

# --- Table A2b) Blue nutrients (Fig A1B)
model_additive_blues <- lmer(
  n_blues ~ generation + treatment_appeal + (1 | chain_code),
  data = df_nutrients
)

model_interaction_blues <- lmer(
  n_blues ~ generation * treatment_appeal + (1 | chain_code),
  data = df_nutrients
)

print(
  tab_model(
    model_additive_blues,
    model_interaction_blues,
    show.ci = FALSE,
    show.se = TRUE,
    show.re.var = FALSE,
    dv.labels = c(
      "Pooled (Generation + treatment)",
      "Pooled (Treatment × generation)"
    ),
    title = "Table A2b) Blue nutrients",
    file = out_b
  )
)

# --- Table A2c) Total nutrients (Fig A1C)
model_additive_total <- lmer(
  n_apps ~ generation + treatment_appeal + (1 | chain_code),
  data = df_nutrients
)

model_interaction_total <- lmer(
  n_apps ~ generation * treatment_appeal + (1 | chain_code),
  data = df_nutrients
)

print(
  tab_model(
    model_additive_total,
    model_interaction_total,
    show.ci = FALSE,
    show.se = TRUE,
    show.re.var = FALSE,
    dv.labels = c(
      "Pooled (Generation + treatment)",
      "Pooled (Treatment × generation)"
    ),
    title = "Table A2c) Total nutrients",
    file = out_c
  )
)

combined_out <- "figures/table_a2.html"
combined_lines <- c(
  readLines(out_a, warn = FALSE),
  "<br/>",
  readLines(out_b, warn = FALSE),
  "<br/>",
  readLines(out_c, warn = FALSE)
)
writeLines(combined_lines, combined_out)

file.remove(out_a, out_b, out_c)
