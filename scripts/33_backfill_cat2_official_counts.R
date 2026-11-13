# Run from the ae-trust-forecast project root.
# Uses the CURRENT NHS England AmbSYS time-series CSV (A10/A30).
# Optional for offline use:
# options(nhs.cat2.official_ambsys_csv = "C:/.../AmbSYS-to-Oct-2026.csv")
source("R/ambulance_counts_backfill.R")
result <- nhs_cat2_backfill_national()
print(result[, .(calendar_month, incident_count, services,
                 precise_minutes, prior_mean_minutes,
                 disagreement_seconds, applied)])
