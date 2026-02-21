# Clear figures output and run all analysis scripts in code/

timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_step <- function(...) message(sprintf("[%s]", timestamp()), " ", ...)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)
  setwd(dirname(script_path))
}

log_step("Starting run_all.R")
log_step("Working directory: ", getwd())

figures_dir <- "figures"
code_dir <- "code"

# Silence readr's column specification chatter across sourced scripts.
options(readr.show_col_types = FALSE)
log_step("Set option readr.show_col_types = FALSE")

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
  log_step("Created missing directory: ", figures_dir)
}

figure_contents <- list.files(
  figures_dir,
  full.names = TRUE,
  recursive = TRUE,
  all.files = TRUE,
  no.. = TRUE
)

if (length(figure_contents) > 0) {
  log_step("Deleting ", length(figure_contents), " existing item(s) from ", figures_dir)
  unlink(figure_contents, recursive = TRUE, force = TRUE)
} else {
  log_step("No existing files to delete in ", figures_dir)
}

scripts <- list.files(code_dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = FALSE)
scripts <- scripts[!grepl("^_", basename(scripts))]
scripts <- sort(scripts)

if (length(scripts) == 0) {
  stop("No R scripts found in code/.")
}

log_step("Found ", length(scripts), " script(s) in ", code_dir)
message("Execution order:")
for (script in scripts) {
  message(" - ", script)
}

failed_scripts <- character(0)
run_start <- Sys.time()

for (script in scripts) {
  script_start <- Sys.time()
  log_step("Running: ", script)
  result <- tryCatch(
    {
      withCallingHandlers(
        suppressPackageStartupMessages(
          source(script, local = .GlobalEnv, echo = FALSE)
        ),
        warning = function(w) {
          if (grepl("was built under R version", conditionMessage(w), fixed = TRUE)) {
            invokeRestart("muffleWarning")
          }
        }
      )
      TRUE
    },
    error = function(e) {
      log_step("ERROR in ", script, ": ", conditionMessage(e))
      FALSE
    }
  )

  if (!result) {
    failed_scripts <- c(failed_scripts, script)
  } else {
    elapsed <- round(as.numeric(difftime(Sys.time(), script_start, units = "secs")), 2)
    log_step("Completed: ", script, " (", elapsed, "s)")
  }
}

if (length(failed_scripts) > 0) {
  total_elapsed <- round(as.numeric(difftime(Sys.time(), run_start, units = "secs")), 2)
  log_step("Run finished with failures after ", total_elapsed, "s")
  stop(
    "One or more scripts failed:\n",
    paste(sprintf(" - %s", failed_scripts), collapse = "\n")
  )
}

total_elapsed <- round(as.numeric(difftime(Sys.time(), run_start, units = "secs")), 2)
log_step("All scripts completed successfully in ", total_elapsed, "s")
