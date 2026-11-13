# Run AFTER scripts/14_import_core_metrics.R, BEFORE scripts/15_model_core_metrics.R.
# This fixes any premature decimal-minute rounding in the model's national input,
# but never substitutes the already rounded HH:MM:SS value for the precise mean.
source("R/ambulance_official.R")
if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table is required")

panel_file <- "data-interim/core/national_panel.csv"
if (!file.exists(panel_file)) stop("Run script 14 first; missing: ", panel_file)
panel <- data.table::fread(panel_file, encoding = "UTF-8")
panel[, calendar_month := data.table::as.IDate(as.character(calendar_month))]
stopifnot(all(c("metric_id", "calendar_month", "value") %in% names(panel)))
reference <- nhs_cat2_read_control()
reconciliation <- nhs_cat2_validate_model_values(panel, reference)

for (i in seq_len(nrow(reference))) {
  row <- which(panel$metric_id == "ambulance_cat2" &
               panel$calendar_month == reference$calendar_month[i])
  if (length(row) != 1L) stop("Missing or duplicate Cat2 month in panel.")
  # Numerator is TOTAL MINUTES (not hours), denominator the incident count.
  # Keep the internal aggregation precisely reconciled to the source totals.
  if (all(c("numerator", "denominator") %in% names(panel))) {
    data.table::set(panel, i = row, j = "numerator",
                    value = reference$total_response_hours[i] * 60)
    data.table::set(panel, i = row, j = "denominator",
                    value = reference$incidents[i])
  }
  data.table::set(panel, i = row, j = "value", value = reference$precise_minutes[i])
}

# Required source provenance remains in the imported panel: do not fabricate a
# new URL, SHA-256, or source record. Save an explicit transformation audit.
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
reconciliation[, `:=`(corrected_value_minutes = precise_minutes,
                      published_mean_seconds = published_seconds)]
data.table::fwrite(reconciliation,
                   "output/qa/ambulance_official_source_reconciliation.csv")
data.table::fwrite(panel, panel_file)
message("AmbSYS C2 national data aligned with source totals for ", nrow(reference),
        " month(s). Published observed mean: ",
        nhs_cat2_clock(reference$published_minutes[nrow(reference)]),
        ". Precise modelling mean: ",
        sprintf("%.6f min", reference$precise_minutes[nrow(reference)]), ".")
