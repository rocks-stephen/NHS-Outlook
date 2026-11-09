source("R/utils.R")
source("R/edition_archive.R")

render_issue_date <- getOption("nhs.outlook.render_issue_date")
render_mode <- getOption("nhs.outlook.render_mode")
if (is.null(render_issue_date) || is.null(render_mode)) {
  stop(
    "Set options 'nhs.outlook.render_issue_date' and ",
    "'nhs.outlook.render_mode' before rendering a frozen edition."
  )
}
issue_day <- format(as.Date(render_issue_date), "%Y-%m-%d")
render_mode <- tolower(as.character(render_mode))
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
edition_root <- file.path(
  project_root, "output", "editions", issue_day, render_mode
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
on.exit(unlink(render_root, recursive = TRUE, force = TRUE), add = TRUE)

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
copy_tree(file.path(project_root, "R"), file.path(render_root, "R"))
dir.create(file.path(render_root, "scripts"), recursive = TRUE, showWarnings = FALSE)
for (script_name in c(
  "17_build_performance_outlook.R", "19_export_publication_bundle.R"
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

output_directory <- getOption(
  "nhs.outlook.render_output_dir",
  file.path(project_root, "output", "rerenders", issue_day, render_mode)
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

old_wd <- getwd()
old_options <- options(c(
  "nhs.outlook.publication_mode", "nhs.outlook.issue_date",
  "nhs.outlook.publication_status", "nhs.outlook.freeze_edition"
))
on.exit({
  setwd(old_wd)
  options(old_options)
}, add = TRUE)
setwd(render_root)
options(
  nhs.outlook.publication_mode = render_mode,
  nhs.outlook.issue_date = as.Date(issue_day),
  nhs.outlook.publication_status = edition_manifest$publication_status[1L],
  nhs.outlook.freeze_edition = FALSE
)
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
  " rendered to ", normalizePath(output_directory, winslash = "/"), "."
)

