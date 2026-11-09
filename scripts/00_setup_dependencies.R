# NHS Outlook: one-time dependency setup / deliberate package updates
# Run from the project root: source("scripts/00_setup_dependencies.R")
# This project already uses renv; DO NOT run renv::init() again.

if (!file.exists("renv.lock") || !file.exists("renv/activate.R")) {
  stop("Open the NHS Outlook project root first (renv.lock not found).")
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Dependencies of the current George Eliot deep-dive module. These are added
# to the existing lockfile; other pre-existing dependencies remain untouched.
deep_dive_packages <- c(
  "dplyr", "tidyr", "purrr", "stringr", "readr", "readxl",
  "janitor", "lubridate", "ggplot2", "rvest", "xml2", "tibble"
)

# Detect packages used by other first-party R scripts. Avoid accidentally
# scanning nested vendored libraries, renv, cache and large downloaded data.
script_paths <- c("scripts", "R", "analysis", "reports")
script_paths <- script_paths[dir.exists(script_paths)]
r_files <- unique(unlist(lapply(script_paths, function(path) {
  list.files(path, pattern = "\\.[rR]$", recursive = TRUE, full.names = TRUE)
})))
r_files <- r_files[!grepl("(^|/|\\\\)(renv|library|staging|packrat|\\.git)(/|\\\\)", r_files)]

# Calls of library(), require(), and requireNamespace() in project code.
extract_pkgs <- function(file) {
  lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  pattern <- "(?:library|require|requireNamespace)\\s*\\(\\s*['\\\"]?([A-Za-z][A-Za-z0-9.]*)"
  hits <- regmatches(lines, gregexpr(pattern, lines, perl = TRUE))
  hits <- unlist(hits, use.names = FALSE)
  unique(sub(pattern, "\\1", hits, perl = TRUE))
}
auto_detected <- unique(unlist(lapply(r_files, extract_pkgs), use.names = FALSE))
# 'renv' is managed separately. The base/recommended packages don't need install.
base_packages <- rownames(installed.packages(priority = c("base", "recommended")))
packages <- sort(setdiff(unique(c(deep_dive_packages, auto_detected)), c(base_packages, "renv")))
cat("Project packages requested:\n", paste(packages, collapse = ", "), "\n")

# Install against the active renv project library, not the global R library.
# renv::install is safe to rerun and preserves existing installed versions.
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  renv::install(missing, prompt = FALSE)
} else {
  message("All declared packages are available.")
}

# Snapshot the installed project environment. Using type="all" avoids dropping
# packages already present in this project but loaded dynamically in other scripts.
# (This may include some incidental installed packages; review the Git diff.)
renv::snapshot(type = "all", prompt = FALSE)
message("Updated renv.lock. Commit renv.lock and this setup script to Git.")
renv::status()
