source("R/utils.R")
source("R/national_forecast.R")
source("R/core_import.R")
source("R/core_forecast.R")

manifest_path <- "data-interim/core/downloaded_source_manifest.csv"
if (!file.exists(manifest_path)) {
  stop("Run scripts/13_download_core_sources.R first.")
}
manifest <- data.table::fread(manifest_path, encoding = "UTF-8")
imported <- import_core_metrics(manifest)

dir.create("data-interim/core", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(imported$national, "data-interim/core/national_panel.csv")
data.table::fwrite(imported$provider, "data-interim/core/provider_panel.csv")
data.table::fwrite(
  imported$community_service_band,
  "data-interim/core/community_waits_service_bands.csv"
)
data.table::fwrite(
  imported$community_service_summary,
  "data-interim/core/community_waits_service_summary.csv"
)

coverage <- data.table::rbindlist(list(
  imported$national[, .(
    panel = "national",
    first_month = min(calendar_month),
    last_month = max(calendar_month),
    complete_rows = sum(complete_submission),
    incomplete_rows = sum(!complete_submission),
    entities = data.table::uniqueN(entity_id)
  ), by = metric_id],
  imported$provider[, .(
    panel = "provider",
    first_month = min(calendar_month),
    last_month = max(calendar_month),
    complete_rows = sum(complete_submission),
    incomplete_rows = sum(!complete_submission),
    entities = data.table::uniqueN(entity_id)
  ), by = metric_id]
), use.names = TRUE)
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(coverage, "output/qa/core_import_coverage.csv")

ucr_provider <- imported$provider[metric_id == "ucr_2h"]
ucr_provider_coverage <- if (nrow(ucr_provider)) {
  ucr_provider[, .(
    panel_rows = .N,
    providers_in_panel = data.table::uniqueN(entity_id),
    submitted_provider_rates = sum(complete_submission == TRUE),
    submitted_rates_with_activity_proxy = sum(
      complete_submission == TRUE & is.finite(activity_volume_proxy)
    ),
    submitted_rates_missing_activity_proxy = sum(
      complete_submission == TRUE & !is.finite(activity_volume_proxy)
    ),
    explicit_missing_submissions = sum(complete_submission == FALSE)
  ), by = calendar_month]
} else {
  data.table::data.table(
    calendar_month = data.table::as.IDate(character()), panel_rows = integer(),
    providers_in_panel = integer(), submitted_provider_rates = integer(),
    submitted_rates_with_activity_proxy = integer(),
    submitted_rates_missing_activity_proxy = integer(),
    explicit_missing_submissions = integer()
  )
}
ucr_provider_import_exclusions <- if (nrow(ucr_provider)) {
  ucr_provider[
    complete_submission != TRUE | !is.finite(activity_volume_proxy),
    .(
      metric_id, calendar_month, entity_id, entity_name,
      exclusion_reason = data.table::fifelse(
        complete_submission != TRUE,
        "provider_rate_not_submitted",
        "provider_activity_proxy_missing"
      ),
      source_file
    )
  ]
} else {
  data.table::data.table(
    metric_id = character(), calendar_month = data.table::as.IDate(character()),
    entity_id = character(), entity_name = character(),
    exclusion_reason = character(), source_file = character()
  )
}
data.table::fwrite(
  ucr_provider_coverage, "output/qa/ucr_provider_import_coverage.csv"
)
data.table::fwrite(
  ucr_provider_import_exclusions,
  "output/qa/ucr_provider_import_exclusions.csv"
)

community_provider <- imported$provider[metric_id == "community_18w"]
community_provider_coverage <- if (nrow(community_provider)) {
  community_provider[, .(
    providers_in_panel = data.table::uniqueN(entity_id),
    complete_provider_submissions = sum(complete_submission == TRUE),
    incomplete_or_missing_submissions = sum(complete_submission != TRUE)
  ), by = calendar_month]
} else {
  data.table::data.table(
    calendar_month = data.table::as.IDate(character()),
    providers_in_panel = integer(), complete_provider_submissions = integer(),
    incomplete_or_missing_submissions = integer()
  )
}
community_provider_import_exclusions <- data.table::rbindlist(list(
  community_provider[complete_submission != TRUE, .(
    metric_id, calendar_month, entity_id, entity_name,
    exclusion_reason = "provider_submission_incomplete_or_missing", source_file
  )],
  imported$community_service_summary[
    geography_type == "Organisation" & !nzchar(geography_code), .(
      metric_id = "community_18w", calendar_month,
      entity_id = NA_character_, entity_name = geography_name,
      exclusion_reason = "published_organisation_code_unavailable", source_file
    )
  ]
), use.names = TRUE, fill = TRUE)
data.table::fwrite(
  community_provider_coverage,
  "output/qa/community_provider_import_coverage.csv"
)
data.table::fwrite(
  community_provider_import_exclusions,
  "output/qa/community_provider_import_exclusions.csv"
)
data.table::fwrite(
  imported$cancer_national_reconciliation,
  "output/qa/cancer_national_reconciliation.csv"
)
data.table::fwrite(
  imported$optional_import_failures,
  "output/qa/core_optional_source_import_failures.csv"
)

community_service_coverage <- if (nrow(imported$community_service_band)) {
  imported$community_service_band[, .(
    services = data.table::uniqueN(service_id),
    geography_rows = data.table::uniqueN(source_geography_key),
    waiting_bands = data.table::uniqueN(wait_band),
    reported_cells = sum(cell_status == "reported"),
    suppressed_cells = sum(cell_status == "suppressed"),
    not_submitted_cells = sum(cell_status == "not_submitted"),
    non_numeric_cells = sum(cell_status == "non_numeric")
  ), by = .(calendar_month, geography_type)]
} else {
  data.table::data.table(
    calendar_month = data.table::as.IDate(character()),
    geography_type = character(), services = integer(),
    geography_rows = integer(), waiting_bands = integer(),
    reported_cells = integer(), suppressed_cells = integer(),
    not_submitted_cells = integer(), non_numeric_cells = integer()
  )
}
data.table::fwrite(
  community_service_coverage,
  "output/qa/community_waits_service_import_coverage.csv"
)
data.table::fwrite(
  imported$community_service_summary[, .(
    calendar_month, geography_type, geography_code, geography_name,
    source_geography_key,
    service_group, service_id, service_name, total_waiting_list,
    over_18_weeks_count, within_18_weeks_count,
    within_18_weeks_proportion, published_band_sum,
    reconciliation_gap_count, reconciliation_gap_share,
    reconciliation_tolerance_count, band_schema, band_breakdown_complete,
    complete_submission, reconciliation_status, identity_status, source_file
  )],
  "output/qa/community_waits_service_reconciliation.csv"
)
community_national_total_reconciliation <- if (
  nrow(imported$community_service_summary)
) {
  service_total <- imported$community_service_summary[
    geography_type == "England" & is.finite(total_waiting_list), .(
      service_total_waiting_list = sum(total_waiting_list),
      services_with_total = .N
    ), by = calendar_month
  ]
  headline_total <- imported$national[
    metric_id == "community_18w", .(
      headline_total_waiting_list = denominator
    ), by = calendar_month
  ]
  out <- merge(
    headline_total, service_total, by = "calendar_month", all = TRUE
  )
  out[, `:=`(
    difference = service_total_waiting_list - headline_total_waiting_list,
    reconciles_exactly = is.finite(service_total_waiting_list) &
      is.finite(headline_total_waiting_list) &
      service_total_waiting_list == headline_total_waiting_list
  )]
  out[]
} else {
  data.table::data.table(
    calendar_month = data.table::as.IDate(character()),
    headline_total_waiting_list = numeric(),
    service_total_waiting_list = numeric(), services_with_total = integer(),
    difference = numeric(), reconciles_exactly = logical()
  )
}
data.table::fwrite(
  community_national_total_reconciliation,
  "output/qa/community_waits_national_total_reconciliation.csv"
)

model_config <- data.table::fread("config/core_model.csv", encoding = "UTF-8")
national_history <- data.table::rbindlist(lapply(
  seq_len(nrow(model_config)),
  function(i) {
    config_row <- model_config[i]
    metric <- config_row$metric_id[1L]
    x <- imported$national[
      metric_id == metric & complete_submission == TRUE & is.finite(value)
    ]
    contiguous <- core_contiguous_tail(x)
    minimum_training <- core_config_integer(
      config_row, "minimum_training_months", 12L
    )
    backtest_months <- core_config_integer(config_row, "backtest_months", 6L)
    required <- minimum_training + backtest_months
    data.table::data.table(
      metric_id = metric,
      first_month = if (nrow(x)) {
        min(x$calendar_month)
      } else {
        data.table::as.IDate(NA_character_)
      },
      last_month = if (nrow(x)) {
        max(x$calendar_month)
      } else {
        data.table::as.IDate(NA_character_)
      },
      complete_months = data.table::uniqueN(x$calendar_month),
      latest_consecutive_months = nrow(contiguous),
      minimum_training_months = minimum_training,
      configured_backtest_months = backtest_months,
      required_consecutive_months = required,
      supports_full_backtest = nrow(contiguous) >= required
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(national_history, "output/qa/core_national_history.csv")
message(
  "Imported ", nrow(imported$national), " national and ",
  nrow(imported$provider), " provider panel rows for ",
  data.table::uniqueN(imported$national$metric_id), " core metrics. ",
  sum(national_history$supports_full_backtest), " of ", nrow(national_history),
  " configured national series support their full backtests; ineligible series ",
  "will be reported and omitted at the modelling stage. Community service panel: ",
  nrow(imported$community_service_summary), " derived geography-service-month rows."
)
