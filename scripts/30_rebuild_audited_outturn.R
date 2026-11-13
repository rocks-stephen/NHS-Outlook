# Rebuild today's outturn from existing imported data with source-aligned Cat2.
# Does not redownload sources or rerun Stage 1-14 imports.
# Edit the revision option if the selected edition already has a frozen archive.
options(
  nhs.outlook.publication_mode = "outturn",
  nhs.outlook.issue_date = as.Date("2026-11-12"),
  nhs.outlook.edition_revision = "r4",
  nhs.outlook.publication_status = "pilot"
)
source("scripts/14a_align_ambulance_official.R")
source("scripts/15_model_core_metrics.R")
source("scripts/16_validate_core_metrics.R")
source("scripts/17_build_performance_outlook.R")
source("scripts/17b_verify_four_columns.R")
source("scripts/29_audit_official_headlines.R")
# Only run export and release QA when the eight headline checks have passed.
source("scripts/19_export_publication_bundle.R")
source("scripts/20_release_qa.R")
message("Audit and publication QA passed. Review the report before Git push.")
