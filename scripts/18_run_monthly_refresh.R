stage_numbers <- c(1:17, 19L, 20L)
all_pipeline_scripts <- sprintf("scripts/%02d_%s.R", stage_numbers, c(
  "discover_nhse_sources",
  "download_nhse_sources",
  "import_national",
  "import_provider",
  "qa_stage1",
  "model_national",
  "plot_national",
  "model_provider",
  "plot_provider",
  "validate_stage3_outputs",
  "build_monthly_outlook",
  "discover_core_sources",
  "download_core_sources",
  "import_core_metrics",
  "model_core_metrics",
  "validate_core_metrics",
  "build_performance_outlook",
  "export_publication_bundle",
  "release_qa"
))

missing_scripts <- all_pipeline_scripts[!file.exists(all_pipeline_scripts)]
if (length(missing_scripts)) {
  stop("Monthly refresh cannot start; missing: ", paste(missing_scripts, collapse = ", "))
}

source("R/utils.R")
publication_mode <- nhs_outlook_publication_mode()
publication_issue_date <- nhs_outlook_issue_date()
options(
  nhs.outlook.publication_mode = publication_mode,
  nhs.outlook.issue_date = as.Date(publication_issue_date)
)
message(
  "Publication mode: ", publication_mode,
  "; issue date: ", format(as.Date(publication_issue_date), "%Y-%m-%d"), "."
)

start_stage <- suppressWarnings(as.integer(
  getOption("ae.refresh.start_stage", 1L)
))
options(ae.refresh.start_stage = NULL)
if (length(start_stage) != 1L || is.na(start_stage) ||
    !start_stage %in% stage_numbers) {
  stop(
    "Option 'ae.refresh.start_stage' must be one of: ",
    paste(stage_numbers, collapse = ", "), "."
  )
}
start_position <- match(start_stage, stage_numbers)
pipeline_scripts <- all_pipeline_scripts[
  seq.int(start_position, length(all_pipeline_scripts))
]
if (start_stage > 1L) {
  message("Resuming the monthly refresh at stage ", start_stage, ".")
}

for (script in pipeline_scripts) {
  message("\n--- Running ", script, " ---")
  source(script, local = new.env(parent = globalenv()), chdir = FALSE)
}

message(
  "\nMonthly ", publication_mode,
  " refresh complete. Automated release QA passed; complete the ",
  "manual rows in output/qa/release_signoff.csv before publication. Open ",
  if (publication_mode == "forecast") {
    "output/releases/nhs-performance-outlook-forecast-latest.html."
  } else {
    "output/releases/nhs-performance-outturn-latest.html."
  }
)
options(
  nhs.outlook.publication_mode = NULL,
  nhs.outlook.issue_date = NULL
)
