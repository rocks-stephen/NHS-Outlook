# Presentation-only promotion of an APPROVED frozen forecast rerender.
# Never writes to output/editions, re-fits a model or issues a new forecast.
# Requires an explicit opt-in, matching edition manifest and nine rendered PDFs.
nhs_publish_approved_frozen_forecast <- function() {
  if (!isTRUE(getOption("nhs.outlook.approve_rerender_publishing", FALSE))) {
    stop("Set options(nhs.outlook.approve_rerender_publishing = TRUE) only after reviewing HTML/PDF.")
  }
  source("R/utils.R", local = TRUE)
  source("R/edition_archive.R", local = TRUE)
  day <- format(as.Date(getOption("nhs.outlook.render_issue_date")), "%Y-%m-%d")
  if (length(day) != 1L || is.na(day)) stop("Set a valid nhs.outlook.render_issue_date.")
  mode <- as.character(getOption("nhs.outlook.render_mode", ""))[1L]
  if (!identical(mode, "forecast")) stop("This helper promotes only forecast rerenders.")
  rev <- normalise_outlook_edition_revision(getOption("nhs.outlook.render_revision", ""))
  rerender <- file.path("output", "rerenders", day, mode)
  if (nzchar(rev)) rerender <- file.path(rerender, "revisions", rev)
  if (!dir.exists(rerender)) stop("Missing reviewed rerender directory: ", rerender)
  frozen <- outlook_edition_root(as.Date(day), "forecast", rev)
  verify_frozen_outlook_edition(frozen)

  overview_manifest_path <- "output/performance/overview_manifest.csv"
  if (!file.exists(overview_manifest_path)) stop("Missing publication manifest; do not promote.")
  manifest <- data.table::fread(overview_manifest_path)
  if (nrow(manifest) != 1L ||
      !identical(as.character(manifest$issue_date[1L]), day) ||
      !identical(manifest$publication_mode[1L], "forecast") ||
      !identical(manifest$publication_status[1L], "pilot")) {
    stop("Current publication manifest is not the matching pilot forecast edition.")
  }
  rows <- data.table::fread("output/performance/overview_metric_rows.csv")
  if (nrow(rows) != 8L || anyDuplicated(rows$metric_id) ||
      !"deep_dive_file" %in% names(rows)) {
    stop("Expected exactly eight current indicator files in the overview rows.")
  }
  detail_files <- unique(as.character(rows$deep_dive_file))
  main_files <- c("nhs-performance-outlook-forecast-latest.html",
                  "nhs-performance-outlook-latest.html")
  html_names <- unique(c(main_files, detail_files))
  html_sources <- file.path(rerender, html_names)
  if (any(!file.exists(html_sources))) {
    stop("Missing rerendered HTML: ", paste(html_names[!file.exists(html_sources)], collapse = ", "))
  }
  # Validate that each deep dive contains the new forecast-target card.
  has_card <- vapply(file.path(rerender, detail_files), function(path) {
    html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    grepl('class="card target-date-card"', html, fixed = TRUE) &&
      grepl("Model-implied chance", html, fixed = TRUE) &&
      !grepl("Not estimated</strong>", html, fixed = TRUE)
  }, logical(1))
  if (!all(has_card)) {
    stop("Missing or unestimated target-card probabilities in: ",
         paste(detail_files[!has_card], collapse = ", "))
  }
  main_html <- paste(readLines(file.path(rerender, main_files[1L]),
                               warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl("milestone-chance", main_html, fixed = TRUE)) {
    stop("Overview probability cells are missing.")
  }
  pdf_sources <- list.files(rerender, pattern = "[.]pdf$", full.names = TRUE)
  if (length(pdf_sources) != 9L) {
    stop("Expected nine forecast PDFs (overview + eight indicators), found ",
         length(pdf_sources), ". Run the frozen renderer with render_pdf = TRUE.")
  }
  # Site builder removes publication_site/forecasts before recreating it.
  # Save historical dated PDF folders, then restore them after rebuilding.
  prior_pdf_dir <- file.path("publication_site", "forecasts")
  old_pdf_folders <- if (dir.exists(prior_pdf_dir)) {
    list.dirs(prior_pdf_dir, full.names = FALSE, recursive = FALSE)
  } else {
    character()
  }
  backup <- tempfile("nhs-outlook-previous-pdfs-")
  dir.create(backup, recursive = TRUE)
  on.exit(unlink(backup, recursive = TRUE, force = TRUE), add = TRUE)
  for (old in old_pdf_folders[old_pdf_folders != day]) {
    src <- file.path(prior_pdf_dir, old)
    if (dir.exists(src) && !file.copy(src, backup, recursive = TRUE)) {
      stop("Unable to back up historical site PDF folder: ", src)
    }
  }
  release_dir <- file.path("output", "releases")
  dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
  target_html <- file.path(release_dir, html_names)
  if (!all(file.copy(html_sources, target_html, overwrite = TRUE))) {
    stop("Could not stage approved HTML in output/releases.")
  }
  publication_dir <- file.path("output", "publication", day, "forecast")
  dir.create(publication_dir, recursive = TRUE, showWarnings = FALSE)
  if (!all(file.copy(pdf_sources,
                     file.path(publication_dir, basename(pdf_sources)),
                     overwrite = TRUE))) {
    stop("Could not stage approved PDFs in output/publication.")
  }
  # Keep the exported publication inventory consistent with any replaced PDFs.
  pdf_manifest_file <- file.path(publication_dir, "publication_manifest.csv")
  if (file.exists(pdf_manifest_file)) {
    pdf_manifest <- data.table::fread(pdf_manifest_file)
    assert_columns(pdf_manifest, c("output_file", "bytes"), "PDF manifest")
    if (nrow(pdf_manifest) != length(pdf_sources) ||
        !setequal(basename(pdf_manifest$output_file), basename(pdf_sources))) {
      stop("The existing publication PDF manifest does not match the rerender bundle.")
    }
    pdf_manifest[, bytes := as.numeric(file.info(file.path(
      publication_dir, basename(output_file)))$size)]
    data.table::fwrite(pdf_manifest, pdf_manifest_file)
  }
  source("R/publish_outlook_website.R", local = TRUE)
  for (old in old_pdf_folders[old_pdf_folders != day]) {
    src <- file.path(backup, old)
    if (dir.exists(src) && !file.copy(src, prior_pdf_dir, recursive = TRUE)) {
      stop("Site updated but could not restore historical PDF folder: ", old)
    }
  }
  message("Approved presentation updates staged and mirrored into website files. ",
          "Review git diff; no git commit or push was performed.")
}

nhs_publish_approved_frozen_forecast()
rm(nhs_publish_approved_frozen_forecast)
