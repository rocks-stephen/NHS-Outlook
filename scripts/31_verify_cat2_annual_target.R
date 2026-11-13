# Verify annual target and overlay AFTER script 17, BEFORE PDF export.
library(data.table)
annual <- fread("output/core/ambulance_cat2/annual_target_outlook.csv")
series <- fread("output/core/ambulance_cat2/annual_rolling_12m_projection.csv")
outturn <- fread("output/performance/outturn_metric_rows.csv")
cat2 <- outturn[metric_id == "ambulance_cat2"]
stopifnot(nrow(annual) == 1L, nrow(cat2) == 1L,
          is.finite(annual$financial_year_mean_forecast[1L]),
          is.finite(annual$annual_probability[1L]),
          abs(cat2$projected_target_value[1L] - annual$financial_year_mean_forecast[1L]) < 1e-10,
          abs(cat2$target_probability[1L] - annual$annual_probability[1L]) < 1e-10,
          nrow(series[calendar_month >= "2026-04-01" & calendar_month <= "2027-03-01"]) == 12L)
files <- c("output/releases/nhs-performance-outturn-latest.html",
           "output/releases/ambulance-category-2-outturn-latest.html")
for (f in files) if (!file.exists(f)) stop("Missing ", f)
main <- paste(readLines(files[1L], warn = FALSE), collapse = "\n")
detail <- paste(readLines(files[2L], warn = FALSE), collapse = "\n")
if (!grepl("FY 2026/27", main, fixed = TRUE) ||
    !grepl("New annual-average basis", main, fixed = TRUE) ||
    !grepl("cat2-rolling-observed", detail, fixed = TRUE) ||
    !grepl("cat2-rolling-forecast", detail, fixed = TRUE)) {
  stop("Annual-average summary or chart overlay is absent from outturn HTML")
}
print(annual[, .(observed_months, future_months,
                 observed_year_to_date_minutes, required_mean_remaining_months,
                 financial_year_mean_forecast, annual_probability,
                 probability_rho_zero, probability_rho_090)])
message("Annual Category 2 target, projection and chart verified against current generated rows.")
