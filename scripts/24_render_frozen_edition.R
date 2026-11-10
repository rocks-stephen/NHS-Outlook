# Use a function scope so all temporary files and R options are reliably
# cleaned up on success or on failure, including when sourced in RStudio.
nhs_render_locked_edition <- function() {
source("R/utils.R")
source("R/edition_archive.R")

render_issue_date <- getOption("nhs.outlook.render_issue_date")
render_mode <- getOption("nhs.outlook.render_mode")
# Select original (default) or a locked analytical revision, e.g. "r1".
render_revision <- normalise_outlook_edition_revision(
  getOption("nhs.outlook.render_revision", "")
)
if (is.null(render_issue_date) || is.null(render_mode)) {
  stop(
    "Set options 'nhs.outlook.render_issue_date' and ",
    "'nhs.outlook.render_mode' before rendering a frozen edition."
  )
}
issue_day <- format(as.Date(render_issue_date), "%Y-%m-%d")
render_mode <- tolower(as.character(render_mode))
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
edition_root <- outlook_edition_root(
  issue_date = as.Date(issue_day), publication_mode = render_mode,
  edition_revision = render_revision, project_root = project_root
)
verify_frozen_outlook_edition(edition_root)
edition_manifest <- data.table::fread(
  file.path(edition_root, "edition_manifest.csv"), encoding = "UTF-8"
)
if (nrow(edition_manifest) != 1L ||
    edition_manifest$publication_mode[1L] != render_mode) {
  stop("Frozen edition metadata does not match the requested edition.")
}

render_root <- tempfile("nhs-outlook-frozen-render-")
dir.create(render_root, recursive = TRUE, showWarnings = FALSE)

copy_tree <- function(source, destination) {
  files <- list.files(source, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
  if (!length(files)) return(invisible(TRUE))
  relative <- substring(files, nchar(normalizePath(source, winslash = "/")) + 2L)
  info <- file.info(files)
  for (i in seq_along(files)) {
    target <- file.path(destination, relative[i])
    if (isTRUE(info$isdir[i])) {
      dir.create(target, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(files[i], target, overwrite = TRUE, copy.date = TRUE)) {
        stop("Could not prepare frozen render input: ", files[i])
      }
    }
  }
  invisible(TRUE)
}

copy_tree(file.path(edition_root, "inputs"), render_root)
# Older frozen editions did not capture this rendering dependency. It is
# needed to label the provider thresholds, not to refit forecast models.
# We never alter the immutable edition archive: copy to the temporary render.
render_core_model <- file.path(render_root, "config", "core_model.csv")
if (!file.exists(render_core_model)) {
  current_core_model <- file.path(project_root, "config", "core_model.csv")
  if (!file.exists(current_core_model)) {
    stop("Frozen render is missing config/core_model.csv and current copy is unavailable.")
  }
  dir.create(dirname(render_core_model), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(current_core_model, render_core_model, overwrite = FALSE)) {
    stop("Could not copy config/core_model.csv into temporary frozen render.")
  }
  warning(
    "This older edition did not archive config/core_model.csv; temporary " ,
    "render uses the current file. Compare signal labels with the original " ,
    "edition before publishing. No archived inputs are changed."
  )
}
copy_tree(file.path(project_root, "R"), file.path(render_root, "R"))
dir.create(file.path(render_root, "scripts"), recursive = TRUE, showWarnings = FALSE)
for (script_name in c(
  "11_build_monthly_outlook.R", "17_build_performance_outlook.R",
  "19_export_publication_bundle.R"
)) {
  file.copy(
    file.path(project_root, "scripts", script_name),
    file.path(render_root, "scripts", script_name),
    overwrite = TRUE
  )
}
use_original_templates <- isTRUE(getOption(
  "nhs.outlook.render_use_original_templates", FALSE
))
template_source <- if (use_original_templates) {
  file.path(edition_root, "original_templates", "reports")
} else {
  file.path(project_root, "reports")
}
copy_tree(template_source, file.path(render_root, "reports"))

default_output_directory <- file.path(
  project_root, "output", "rerenders", issue_day, render_mode
)
if (nzchar(render_revision)) {
  default_output_directory <- file.path(
    default_output_directory, "revisions", render_revision
  )
}
output_directory <- getOption(
  "nhs.outlook.render_output_dir", default_output_directory
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

old_wd <- getwd()
old_options <- options(c(
  "nhs.outlook.publication_mode", "nhs.outlook.issue_date",
  "nhs.outlook.publication_status", "nhs.outlook.freeze_edition",
  "nhs.outlook.edition_revision"
))
on.exit({
  setwd(old_wd)
  options(old_options)
  unlink(render_root, recursive = TRUE, force = TRUE)
}, add = TRUE)
setwd(render_root)
options(
  nhs.outlook.publication_mode = render_mode,
  nhs.outlook.issue_date = as.Date(issue_day),
  nhs.outlook.publication_status = edition_manifest$publication_status[1L],
  nhs.outlook.freeze_edition = FALSE,
  nhs.outlook.edition_revision = render_revision
)
# Stage 17 builds the overview and seven core deep dives but not A&E.
# Stage 11 rebuilds A&E from the same frozen inputs, in this temporary folder.
if (identical(render_mode, "forecast")) {
  source("scripts/11_build_monthly_outlook.R", local = new.env(parent = globalenv()))
}
source("scripts/17_build_performance_outlook.R", local = new.env(parent = globalenv()))
if (isTRUE(getOption("nhs.outlook.render_pdf", TRUE))) {
  source("scripts/19_export_publication_bundle.R",
         local = new.env(parent = globalenv()))
}

generated <- list.files(
  file.path(render_root, "output", "releases"),
  full.names = TRUE, recursive = FALSE
)
if (!length(generated)) stop("Frozen-edition render produced no release files.")
copied <- file.copy(
  generated, file.path(output_directory, basename(generated)), overwrite = TRUE
)
if (!all(copied)) stop("One or more frozen-edition outputs could not be copied.")
bundle_directory <- file.path(
  render_root, "output", "publication", issue_day, render_mode
)
if (dir.exists(bundle_directory)) {
  bundle_files <- list.files(bundle_directory, full.names = TRUE)
  if (length(bundle_files)) {
    copied_bundle <- file.copy(
      bundle_files, file.path(output_directory, basename(bundle_files)),
      overwrite = TRUE
    )
    if (!all(copied_bundle)) {
      stop("One or more frozen-edition PDF bundle files could not be copied.")
    }
  }
}
message(
  "Frozen ", render_mode, " edition ", issue_day,
  if (nzchar(render_revision)) paste0(" revision ", render_revision) else "",
  " rendered to ", normalizePath(output_directory, winslash = "/"), "."
)
}

nhs_render_locked_edition()
rm(nhs_render_locked_edition)
