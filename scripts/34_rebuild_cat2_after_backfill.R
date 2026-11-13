# Use the official England time-series CSV to populate A10/A30 for April-Sep.
# Then remodel core metrics from existing imports; DO NOT rerun stage 14,
# which would overwrite the corrected national_panel.csv.
source("scripts/33_backfill_cat2_official_counts.R")
source("scripts/15_model_core_metrics.R")
source("scripts/16_validate_core_metrics.R")
# Rebuild the existing annual-average outturn; this will inspect FY counts,
# create the revised edition, and verify the forecast/annual chart.
source("scripts/32_rebuild_cat2_annual_outturn.R")
# Review before publication; nothing is uploaded to GitHub by this script.
