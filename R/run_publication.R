# Single entry point for a complete NHS Outlook release.
# Override the options below before sourcing when producing an outturn or when
# resuming from a later pipeline stage.

if (is.null(getOption("nhs.outlook.publication_mode"))) {
  options(nhs.outlook.publication_mode = "forecast")
}
if (is.null(getOption("nhs.outlook.issue_date"))) {
  options(nhs.outlook.issue_date = Sys.Date())
}
if (is.null(getOption("nhs.outlook.publication_status"))) {
  options(nhs.outlook.publication_status = "pilot")
}

publication_mode___ <- getOption("nhs.outlook.publication_mode")
publication_issue_date___ <- getOption("nhs.outlook.issue_date")
publication_status___ <- getOption("nhs.outlook.publication_status")

source("scripts/18_run_monthly_refresh.R")

if (identical(tolower(as.character(publication_mode___)), "forecast")) {
  options(
    nhs.outlook.publication_mode = publication_mode___,
    nhs.outlook.issue_date = publication_issue_date___,
    nhs.outlook.publication_status = publication_status___
  )
  source("R/publish_outlook_website.R")
  options(
    nhs.outlook.publication_mode = NULL,
    nhs.outlook.issue_date = NULL,
    nhs.outlook.publication_status = NULL
  )
}

rm(
  publication_mode___, publication_issue_date___, publication_status___,
  envir = .GlobalEnv
)
