# Rebuild a new national Category 2 annual-history edition and dual-view HTML.
# Does not alter national_panel.csv, provider inputs, model fits or GitHub.
# R parses the entire file before evaluating any code.
stopifnot(file.exists("R/ambulance_counts_backfill.R"),
          file.exists("R/ambulance_annual_target.R"),
          file.exists("R/ambulance_cat2_history_chart.R"),
          file.exists("data-interim/core/national_panel.csv"))
source("R/ambulance_counts_backfill.R")
source("R/ambulance_cat2_history_chart.R")
source("R/ambulance_annual_target.R")

nhs_cat2_recover_long_history()
annual_preview <- nhs_cat2_load_annual_series()
rolling <- annual_preview$series[kind == "actual" &
                   is.finite(rolling_12m_minutes)]
if (nrow(rolling) < 12L) {
  stop("Only ", nrow(rolling), " verified observed 12-month means. ",
       "Review output/qa/ambulance_cat2_long_history_audit.csv before publication.")
}
# Regression guard: both published planning milestones must appear in BOTH
# chart views before the release revision is built.
for (spec in list(
  list(view = "annual", column = "rolling_12m_minutes",
       marks = c("cat2-target-point", "cat2-target-late-point")),
  list(view = "monthly", column = "monthly_mean_minutes",
       marks = c("cat2-target-dotted", "cat2-target-late-dotted"))
)) {
  tab <- if (spec$view == "annual") {
    annual_preview$series
  } else {
    panel <- data.table::fread("data-interim/core/national_panel.csv")
    actual <- panel[metric_id == "ambulance_cat2" & is.finite(value), .(
      calendar_month = as.Date(calendar_month),
      monthly_mean_minutes = as.numeric(value), kind = "actual")]
    projected <- annual_preview$series[kind == "forecast", .(
      calendar_month, monthly_mean_minutes, kind)]
    data.table::rbindlist(list(actual, projected))
  }
  svg <- nhs_cat2_chart_svg(tab, spec$column, spec$view)
  for (mark in spec$marks) {
    if (!grepl(mark, svg, fixed = TRUE)) {
      stop("Missing ", mark, " from ", spec$view, " chart")
    }
  }
}
print(annual_preview$summary[, .(observed_year_to_date_minutes,
                                  required_mean_remaining_months,
                                  financial_year_mean_forecast,
                                  annual_probability,
                                  probability_rho_zero,
                                  probability_rho_090)])
message("Verified historical rolling means: ", nrow(rolling), "; first ",
        min(rolling$calendar_month), "; last ", max(rolling$calendar_month))

issue <- as.Date("2026-11-12")
base <- file.path("output", "editions", format(issue, "%Y-%m-%d"),
                  "outturn", "revisions")
prior <- if (dir.exists(base)) {
  basename(list.dirs(base, recursive = FALSE))
} else {
  character()
}
used <- suppressWarnings(as.integer(sub("^r", "", prior[grepl("^r[1-9][0-9]*$", prior)])))
revision <- paste0("r", if (length(used)) max(used) + 1L else 1L)
message("Building a new national-only chart revision: ", revision)
options(nhs.outlook.publication_mode = "outturn",
        nhs.outlook.issue_date = issue,
        nhs.outlook.edition_revision = revision,
        nhs.outlook.publication_status = "pilot",
        nhs.outlook.freeze_edition = TRUE)
source("scripts/17_build_performance_outlook.R")

html_path <- "output/releases/ambulance-category-2-outturn-latest.html"
html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
for (marker in c('data-cat2-choose="annual"',
                 'data-cat2-choose="monthly"',
                 'class="outlook-chart cat2-annual-chart"',
                 'class="outlook-chart cat2-monthly-chart"',
                 'cat2-target-point', 'cat2-target-dotted',
                 'cat2-target-late-point', 'cat2-target-late-dotted',
                 'cat2-toggle-js')) {
  if (!grepl(marker, html, fixed = TRUE))
    stop("Rebuilt HTML missing required marker: ", marker)
}
if (grepl('benchmark 25.0 min', html, fixed = TRUE))
  stop("Provider-page monthly 25-minute benchmark remains; must correct before release")
message("National monthly/annual chart toggle verified. Open: ", html_path)
message("Audit: output/qa/ambulance_cat2_long_history_audit.csv")
message("This script does not export PDF, run full release QA, or publish to GitHub.")
