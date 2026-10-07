source("R/utils.R")
source("R/scrape_nhse.R")
source("R/core_sources.R")

source_config <- data.table::fread("config/core_sources.csv", encoding = "UTF-8")
source_config[, `:=`(
  required = parse_logical_strict(required),
  strict_coverage = parse_logical_strict(strict_coverage)
)]
acquisition_config <- read_key_value_config("config/acquisition.csv")
discovery <- discover_core_sources(source_config, acquisition_config$user_agent)

selected_monthly <- discovery$selected_manifest[selection_mode == "monthly"]
coverage_checks <- selected_monthly[, {
  expected <- data.table::as.IDate(seq(
    as.Date(min(activity_month)), as.Date(max(activity_month)), by = "month"
  ))
  missing <- setdiff(expected, activity_month)
  list(
    selected_files = .N,
    first_month = min(activity_month),
    last_month = max(activity_month),
    missing_months_n = length(missing),
    missing_months = paste(missing, collapse = ";")
  )
}, by = .(metric_id, dataset_role)]
expected_starts <- source_config[
  selection_mode == "monthly",
  .(
    metric_id,
    dataset_role,
    configured_start_month = data.table::as.IDate(start_month),
    strict_coverage
  )
]
coverage_checks <- merge(
  coverage_checks, expected_starts,
  by = c("metric_id", "dataset_role"), all.x = TRUE
)
coverage_checks[, starts_as_configured := first_month == configured_start_month]
dir.create("data-interim/core", recursive = TRUE, showWarnings = FALSE)
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(coverage_checks, "output/qa/core_source_coverage.csv")
if (any(coverage_checks[
  strict_coverage == TRUE,
  missing_months_n > 0L | !starts_as_configured
])) {
  stop(
    "Core source discovery found incomplete configured monthly coverage for: ",
    paste(coverage_checks[
      strict_coverage == TRUE &
        (missing_months_n > 0L | !starts_as_configured), metric_id
    ], collapse = ", "),
    ". Inspect output/qa/core_source_coverage.csv."
  )
}

data.table::fwrite(
  discovery$link_inventory, "data-interim/core/source_link_inventory.csv"
)
data.table::fwrite(
  discovery$selected_manifest, "data-interim/core/source_manifest.csv"
)
data.table::fwrite(
  discovery$discovery_failures,
  "output/qa/core_optional_source_discovery_failures.csv"
)
message(
  "Discovered and selected ", nrow(discovery$selected_manifest),
  " official core-metric source files; ",
  nrow(discovery$discovery_failures), " optional source contract(s) unavailable."
)
