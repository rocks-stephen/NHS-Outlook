# Rebuild from already imported and modelled 12 November source data.
# Does not repeat stages 1-16; your stage 14/15 Category 2 patch must already be installed.
# If r5 already exists, change it to the next unused revision.
options(nhs.outlook.publication_mode = "outturn",
        nhs.outlook.issue_date = as.Date("2026-11-12"),
        nhs.outlook.edition_revision = "r5",
        nhs.outlook.publication_status = "pilot")
source("scripts/32a_inspect_cat2_incident_counts.R")
source("scripts/17_build_performance_outlook.R")
source("scripts/31_verify_cat2_annual_target.R")
# Check the printed annual probability & sensitivity BEFORE publishing.
# Rebuild bundle and run the existing full release QA once verified.
# source("scripts/19_export_publication_bundle.R")
# source("scripts/20_release_qa.R")
