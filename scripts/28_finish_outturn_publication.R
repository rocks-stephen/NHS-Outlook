# Finalize the existing NHS Outlook outturn without rerunning model stages.
# Run from the project root after replacing scripts/19 and R/release_qa.R.
source("R/utils.R")
source("R/edition_archive.R")

overview_path <- "output/performance/overview_manifest.csv"
if (!file.exists(overview_path)) stop("Run script 17 first; overview manifest missing.")
meta <- data.table::fread(overview_path, encoding = "UTF-8")
if (nrow(meta) != 1L || !all(c(
  "issue_date", "publication_mode", "publication_status",
  "edition_revision", "source_forecast_issue_date"
) %in% names(meta))) stop("Overview manifest has incomplete edition metadata.")
if (meta$publication_mode[1L] != "outturn") stop("Expected outturn mode.")
if (meta$publication_status[1L] != "pilot") stop("Expected pilot publication status.")
revision <- normalise_outlook_edition_revision(meta$edition_revision[1L])
root <- outlook_edition_root(meta$issue_date[1L], "outturn", revision)
identity <- paste0(meta$issue_date[1L], " / outturn / ",
                   if (nzchar(revision)) revision else "original")
manifest <- file.path(root, "edition_manifest.csv")
if (!file.exists(manifest)) {
  existing <- if (dir.exists(root)) list.files(
    root, all.files = TRUE, recursive = TRUE, no.. = TRUE
  ) else character()
  if (length(existing)) stop(
    "The archive is partially written for ", identity,
    ". Do not overwrite it. Choose the next unused revision, rerun script 17, then retry."
  )
  message("No archive exists for ", identity,
          "; freezing the current analytical inputs as a new edition.")
  freeze_outlook_edition(
    issue_date = as.Date(meta$issue_date[1L]),
    publication_mode = "outturn",
    publication_status = meta$publication_status[1L],
    source_forecast_issue_date = as.Date(meta$source_forecast_issue_date[1L]),
    edition_revision = revision
  )
}
verify_frozen_outlook_edition(root)
message("Edition inputs and checksum manifest verified: ", root)

# Rebuild only PDFs/manifest and re-run original checks.
source("scripts/19_export_publication_bundle.R")
source("scripts/20_release_qa.R")
message("Publication QA complete. Review the overview PDF before pushing.")
