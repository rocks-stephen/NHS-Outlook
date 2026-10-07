source("R/utils.R")
source("R/national_forecast.R")
source("R/release_qa.R")

required_files <- c(
  "data-interim/core/national_panel.csv",
  "data-interim/core/provider_panel.csv",
  "data-interim/core/community_waits_service_bands.csv",
  "data-interim/core/community_waits_service_summary.csv",
  "output/core/model_status.csv",
  "output/core/forecast_method_register.csv",
  "output/performance/overview_metric_rows.csv",
  "output/performance/overview_manifest.csv",
  "output/performance/outturn_provider_watch_manifest.csv",
  "output/performance/forecast_method_register.csv",
  "output/qa/stage3_output_checks.csv",
  "output/qa/core_metric_output_checks.csv",
  "output/national/model_comparison_by_horizon.csv",
  "output/national/forecast_method.csv",
  "output/national/release_forecast_archive.csv",
  "output/national/release_forecast_scorecard.csv",
  "config/performance_metrics.csv",
  "config/national_model.csv",
  "config/core_model.csv",
  "config/core_qa_reference_values.csv",
  "config/community_service_qa_reference_values.csv",
  "output/qa/community_waits_national_total_reconciliation.csv",
  "output/qa/ucr_provider_import_coverage.csv",
  "output/qa/ucr_provider_import_exclusions.csv",
  "output/qa/ucr_provider_model_eligibility.csv"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Release QA cannot start; missing: ", paste(missing_files, collapse = ", "), ".")
}

checks <- list()
check_index <- 0L
add_check <- function(section, metric_id, check, severity, passed, detail,
                      evidence_file = NA_character_) {
  check_index <<- check_index + 1L
  checks[[check_index]] <<- release_qa_check(
    section, metric_id, check, severity, passed, detail, evidence_file
  )
}

read_csv <- function(path, date_columns = character()) {
  out <- data.table::fread(path, encoding = "UTF-8")
  for (column in intersect(date_columns, names(out))) {
    out[, (column) := data.table::as.IDate(as.character(get(column)))]
  }
  out[]
}

national <- read_csv("data-interim/core/national_panel.csv", "calendar_month")
provider <- read_csv("data-interim/core/provider_panel.csv", "calendar_month")
model_status <- read_csv("output/core/model_status.csv")
metric_config <- read_csv("config/performance_metrics.csv")
metric_config[, `:=`(
  active = parse_logical_strict(active),
  higher_is_better = parse_logical_strict(higher_is_better),
  provider_signal_enabled = parse_logical_strict(provider_signal_enabled)
)]
included_metrics <- model_status[
  model_status %in% c("included", "included_national_only"), metric_id
]
performance_method_register <- read_csv(
  "output/performance/forecast_method_register.csv"
)
add_check(
  "forecast_method", "ALL", "method_register_covers_published_metrics", "fatal",
  "metric_id" %in% names(performance_method_register) &&
    setequal(
      performance_method_register$metric_id,
      c("ae4h_all", included_metrics)
    ),
  paste(
    nrow(performance_method_register), "method row(s):",
    paste(performance_method_register$metric_id, collapse = ", ")
  ),
  "output/performance/forecast_method_register.csv"
)

# Carry forward the fatal structural checks already completed by stages 10 and 16.
for (existing_path in c(
  "output/qa/stage3_output_checks.csv",
  "output/qa/core_metric_output_checks.csv"
)) {
  existing <- read_csv(existing_path)
  existing_metric_id <- if ("metric_id" %in% names(existing)) {
    existing$metric_id
  } else {
    rep("AE4H", nrow(existing))
  }
  for (i in seq_len(nrow(existing))) {
    add_check(
      "existing_structural_validation", existing_metric_id[i], existing$check[i],
      "fatal", existing$passed[i], existing$detail[i], existing_path
    )
  }
}

# A&E forecast comparison and genuine release-vintage preservation.
ae_comparison_path <- "output/national/model_comparison_by_horizon.csv"
if (file.exists(ae_comparison_path)) {
  ae_comparison <- read_csv(ae_comparison_path)
  ae_config <- read_key_value_config("config/national_model.csv")
  ae_reference <- ae_comparison[
    evaluation_window == "primary_selection" & horizon_months == 1L &
      model == ae_config$reference_model
  ]
  ae_naive <- ae_comparison[
    evaluation_window == "primary_selection" & horizon_months == 1L &
      model == "seasonal_naive"
  ]
  add_check(
    "forecast", "ae4h_all", "reference_model_scored", "fatal",
    nrow(ae_reference) == 1L && is.finite(ae_reference$rmse_pp),
    paste("reference rows", nrow(ae_reference)), ae_comparison_path
  )
  if (nrow(ae_reference) == 1L && nrow(ae_naive) == 1L) {
    ratio <- ae_reference$rmse_pp / ae_naive$rmse_pp
    add_check(
      "forecast", "ae4h_all", "reference_rmse_within_10pct_of_seasonal_naive",
      "warning", ratio <= 1.10,
      paste0("RMSE ratio ", round(ratio, 3L), "."), ae_comparison_path
    )
    bias_share <- abs(ae_reference$bias_pp) / ae_reference$mae_pp
    add_check(
      "forecast", "ae4h_all", "reference_bias_below_half_mae", "warning",
      !is.finite(bias_share) || bias_share <= 0.5,
      paste0("Absolute bias / MAE = ", round(bias_share, 3L), "."), ae_comparison_path
    )
  }
}
ae_archive_path <- "output/national/release_forecast_archive.csv"
ae_scorecard_path <- "output/national/release_forecast_scorecard.csv"
if (file.exists(ae_archive_path) && file.exists(ae_scorecard_path)) {
  ae_consistency <- release_qa_archive_consistency(
    read_csv(ae_archive_path), read_csv(ae_scorecard_path),
    c("forecast_version", "data_through_month", "forecast_month", "model"),
    c("predicted_performance", "lower_80", "upper_80", "lower_95", "upper_95")
  )
  add_check(
    "forecast_vintage", "ae4h_all", "scorecard_retains_archived_forecast", "fatal",
    ae_consistency$passed, ae_consistency$detail, ae_scorecard_path
  )
  ae_scorecard <- read_csv(ae_scorecard_path)
  ae_scored <- ae_scorecard[
    forecast_status == "scored" & is.finite(actual_performance)
  ]
  add_check(
    "forecast_vintage", "ae4h_all", "genuine_release_interval_coverage", "review", NA,
    if (nrow(ae_scored)) paste0(
      nrow(ae_scored), " scored release forecast(s); 80% coverage ",
      round(100 * mean(
        ae_scored$actual_performance >= ae_scored$lower_80 &
          ae_scored$actual_performance <= ae_scored$upper_80
      ), 1L), "%; 95% coverage ",
      round(100 * mean(
        ae_scored$actual_performance >= ae_scored$lower_95 &
          ae_scored$actual_performance <= ae_scored$upper_95
      ), 1L), "%."
    ) else "No genuine release forecast has matured yet.",
    ae_scorecard_path
  )
}

# Import integrity and traceability.
add_check(
  "import", "ALL", "unique_national_metric_month_keys", "fatal",
  !anyDuplicated(national[, .(metric_id, calendar_month)]),
  paste(nrow(national), "national rows"), "data-interim/core/national_panel.csv"
)
add_check(
  "import", "ALL", "unique_provider_metric_entity_month_keys", "fatal",
  !anyDuplicated(provider[, .(metric_id, entity_id, calendar_month)]),
  paste(nrow(provider), "provider rows"), "data-interim/core/provider_panel.csv"
)
complete_rows <- data.table::rbindlist(list(
  national[complete_submission == TRUE], provider[complete_submission == TRUE]
), use.names = TRUE, fill = TRUE)
calendar_month_start <- !is.na(complete_rows$calendar_month) &
  format(as.Date(complete_rows$calendar_month), "%d") == "01"
add_check(
  "import", "ALL", "calendar_months_are_month_starts", "fatal",
  all(calendar_month_start),
  paste(sum(!calendar_month_start), "row(s) are not first-of-month dates."),
  "data-interim/core/national_panel.csv; data-interim/core/provider_panel.csv"
)
source_trace_ok <-
  !is.na(complete_rows$source_file) & nzchar(trimws(complete_rows$source_file)) &
  !is.na(complete_rows$source_url) & nzchar(trimws(complete_rows$source_url)) &
  grepl("^[0-9a-fA-F]{64}$", complete_rows$source_sha256)
add_check(
  "import", "ALL", "complete_rows_have_source_provenance", "fatal",
  all(source_trace_ok),
  paste(sum(!source_trace_ok), "complete row(s) lack file, URL or SHA-256 provenance."),
  "data-interim/core/national_panel.csv; data-interim/core/provider_panel.csv"
)
reconcilable <- complete_rows[
  is.finite(numerator) & is.finite(denominator) & denominator > 0
]
reconciliation_error <- abs(reconcilable$value -
                              reconcilable$numerator / reconcilable$denominator)
add_check(
  "import", "ALL", "rates_reconcile_to_numerator_and_denominator", "fatal",
  !nrow(reconcilable) || all(reconciliation_error <= 1e-8),
  paste(
    nrow(reconcilable), "reconcilable row(s);",
    sum(reconciliation_error > 1e-8), "outside tolerance."
  ),
  "data-interim/core/national_panel.csv; data-interim/core/provider_panel.csv"
)

# Community waits service-by-band panel. Organisation names are retained for
# mapping and descriptive work, but must not silently become provider IDs.
community_band_path <- "data-interim/core/community_waits_service_bands.csv"
community_summary_path <- "data-interim/core/community_waits_service_summary.csv"
community_total_reconciliation_path <-
  "output/qa/community_waits_national_total_reconciliation.csv"
community_band <- read_csv(community_band_path, "calendar_month")
community_summary <- read_csv(community_summary_path, "calendar_month")
community_total_reconciliation <- read_csv(
  community_total_reconciliation_path, "calendar_month"
)
community_headline_available <- nrow(national[
  metric_id == "community_18w" & complete_submission == TRUE
]) > 0L
community_headline_months <- national[
  metric_id == "community_18w" & complete_submission == TRUE,
  sort(unique(calendar_month))
]
community_band_months <- sort(unique(community_band$calendar_month))
community_summary_months <- sort(unique(community_summary$calendar_month))
add_check(
  "community_service_import", "community_18w",
  "service_panel_present_when_headline_is_present", "fatal",
  !community_headline_available ||
    (nrow(community_band) > 0L && nrow(community_summary) > 0L),
  paste(nrow(community_band), "band rows;", nrow(community_summary), "summary rows."),
  community_summary_path
)

# UCR provider percentages are published separately from the referral-activity
# counts used for volume screening. Confirm both are retained and exclusions are
# explicit rather than silently dropping providers.
ucr_provider <- provider[metric_id == "ucr_2h"]
ucr_coverage <- read_csv(
  "output/qa/ucr_provider_import_coverage.csv", "calendar_month"
)
ucr_import_exclusions <- read_csv(
  "output/qa/ucr_provider_import_exclusions.csv", "calendar_month"
)
ucr_model_eligibility <- read_csv(
  "output/qa/ucr_provider_model_eligibility.csv", "data_through_month"
)
add_check(
  "ucr_provider_import", "ucr_2h", "provider_rates_and_activity_proxy_present",
  "fatal",
  nrow(ucr_provider[complete_submission == TRUE]) > 0L &&
    nrow(ucr_provider[
      complete_submission == TRUE & is.finite(activity_volume_proxy)
    ]) > 0L &&
    nrow(ucr_coverage) > 0L,
  paste(
    nrow(ucr_provider[complete_submission == TRUE]), "submitted rate row(s);",
    nrow(ucr_provider[
      complete_submission == TRUE & is.finite(activity_volume_proxy)
    ]), "with activity proxy."
  ),
  "data-interim/core/provider_panel.csv"
)
add_check(
  "ucr_provider_import", "ucr_2h", "provider_import_exclusions_are_explicit",
  "fatal",
  all(c("entity_id", "calendar_month", "exclusion_reason") %in%
        names(ucr_import_exclusions)) &&
    nrow(ucr_coverage) == data.table::uniqueN(ucr_provider$calendar_month),
  paste(nrow(ucr_import_exclusions), "provider-month exclusion row(s)."),
  "output/qa/ucr_provider_import_exclusions.csv"
)
add_check(
  "ucr_provider_model", "ucr_2h", "provider_signal_eligibility_is_explicit",
  "fatal",
  nrow(ucr_model_eligibility) > 0L &&
    all(!is.na(ucr_model_eligibility$signal_eligibility_reason)) &&
    any(ucr_model_eligibility$signal_eligibility_reason == "eligible"),
  paste(
    sum(ucr_model_eligibility$signal_eligibility_reason == "eligible"),
    "eligible;",
    sum(ucr_model_eligibility$signal_eligibility_reason != "eligible"),
    "explicitly excluded."
  ),
  "output/qa/ucr_provider_model_eligibility.csv"
)
add_check(
  "community_service_import", "community_18w",
  "service_panel_covers_every_imported_headline_month", "fatal",
  !community_headline_available ||
    (
      identical(
        as.character(community_band_months),
        as.character(community_headline_months)
      ) &&
        identical(
          as.character(community_summary_months),
          as.character(community_headline_months)
        )
    ),
  paste(
    length(community_headline_months), "headline month(s);",
    length(community_band_months), "band-panel month(s);",
    length(community_summary_months), "summary-panel month(s)."
  ),
  paste(community_band_path, community_summary_path, sep = "; ")
)
if (nrow(community_band)) {
  band_key <- community_band[, .(
    calendar_month, source_geography_key, service_id, wait_band
  )]
  add_check(
    "community_service_import", "community_18w",
    "unique_month_geography_service_band_keys", "fatal",
    !anyDuplicated(band_key), paste(nrow(band_key), "service-band rows."),
    community_band_path
  )
  reported_counts <- community_band[
    cell_status == "reported", waiting_count
  ]
  add_check(
    "community_service_import", "community_18w",
    "reported_waiting_counts_are_non_negative", "fatal",
    all(is.finite(reported_counts) & reported_counts >= 0),
    paste(sum(!is.finite(reported_counts) | reported_counts < 0), "invalid count(s)."),
    community_band_path
  )
  month_bands <- community_band[, .(
    wait_bands = list(unique(wait_band))
  ), by = calendar_month]
  month_bands[, required_schema_present := vapply(wait_bands, function(z) {
    all(c("total", "wait_18_52") %in% z) &&
      ("wait_over_52" %in% z ||
         all(c("wait_52_104", "wait_over_104") %in% z))
  }, logical(1))]
  add_check(
    "community_service_import", "community_18w",
    "each_month_has_required_over_18_band_schema", "fatal",
    all(month_bands$required_schema_present),
    paste(sum(!month_bands$required_schema_present), "month(s) lack required bands."),
    community_band_path
  )
}
if (nrow(community_summary)) {
  summary_key <- community_summary[, .(
    calendar_month, source_geography_key, service_id
  )]
  add_check(
    "community_service_import", "community_18w",
    "unique_month_geography_service_summary_keys", "fatal",
    !anyDuplicated(summary_key), paste(nrow(summary_key), "service summary rows."),
    community_summary_path
  )
  complete_service <- community_summary[complete_submission == TRUE]
  add_check(
    "community_service_import", "community_18w",
    "complete_service_rates_are_bounded_and_reconcile", "fatal",
    all(
      is.finite(complete_service$within_18_weeks_proportion) &
        complete_service$within_18_weeks_proportion >= 0 &
        complete_service$within_18_weeks_proportion <= 1 &
        abs(
          complete_service$within_18_weeks_proportion -
            complete_service$within_18_weeks_count /
              complete_service$total_waiting_list
        ) <= 1e-8
    ),
    paste(nrow(complete_service), "complete derived service rates."),
    community_summary_path
  )
  material_gap <- community_summary[
    reconciliation_status == "material_published_band_gap"
  ]
  add_check(
    "community_service_import", "community_18w",
    "published_wait_band_reconciliation_review", "review", NA,
    paste(
      nrow(material_gap), "service/geography/month row(s) have a published band",
      "sum differing from the total by more than 0.5% or 5 people; this is",
      "reported for review because NHS England notes that weekly bands may not",
      "sum to the total."
    ),
    "output/qa/community_waits_service_reconciliation.csv"
  )
}
if (nrow(community_total_reconciliation)) {
  add_check(
    "community_service_import", "community_18w",
    "england_service_totals_reconcile_to_headline_total", "fatal",
    all(community_total_reconciliation$reconciles_exactly == TRUE),
    paste(
      sum(community_total_reconciliation$reconciles_exactly != TRUE),
      "month(s) do not reconcile exactly."
    ),
    community_total_reconciliation_path
  )
}
community_service_references <- read_csv(
  "config/community_service_qa_reference_values.csv", "calendar_month"
)
for (i in seq_len(nrow(community_service_references))) {
  reference <- community_service_references[i]
  observed <- community_summary[
    calendar_month == reference$calendar_month &
      geography_type == reference$geography_type &
      service_id == reference$service_id
  ]
  difference <- if (nrow(observed) == 1L) max(c(
    abs(observed$total_waiting_list - reference$expected_total_waiting_list),
    abs(observed$over_18_weeks_count - reference$expected_over_18_weeks_count),
    abs(
      observed$within_18_weeks_proportion -
        reference$expected_within_18_weeks_proportion
    )
  )) else Inf
  add_check(
    "community_service_source_reference", "community_18w",
    paste0(
      "service_reference_", reference$service_id, "_",
      reference$calendar_month
    ),
    reference$severity, nrow(observed) == 1L && difference <= reference$tolerance,
    paste0(
      reference$source_note, "; observed rows ", nrow(observed),
      "; maximum absolute difference ", signif(difference, 8L), "."
    ),
    "config/community_service_qa_reference_values.csv"
  )
}
add_check(
  "community_service_import", "community_18w",
  "name_only_organisations_not_used_as_provider_ids", "fatal",
  !any(provider$metric_id == "community_18w"),
  "Organisation-by-service rows remain in the separate unharmonised service panel.",
  community_summary_path
)

optional_path <- "output/qa/core_optional_source_import_failures.csv"
optional_failures <- if (file.exists(optional_path) && file.info(optional_path)$size > 0) {
  read_csv(optional_path)
} else {
  data.table::data.table()
}
if (nrow(optional_failures)) {
  failure_summary <- optional_failures[, .N, by = metric_id]
  for (i in seq_len(nrow(failure_summary))) add_check(
    "import", failure_summary$metric_id[i], "optional_source_files_rejected",
    "warning", FALSE,
    paste(failure_summary$N[i], "source file(s) were rejected; review the detailed reasons."),
    optional_path
  )
}

profile <- release_qa_import_profile(data.table::copy(national))
data.table::fwrite(profile, "output/qa/core_import_profile.csv")
anomalies <- release_qa_import_anomalies(national, optional_failures)
data.table::fwrite(anomalies, "output/qa/core_import_anomalies.csv")
for (i in seq_len(nrow(profile))) {
  add_check(
    "time_series", profile$metric_id[i], "national_series_has_no_internal_gaps",
    "warning", profile$missing_months[i] == 0L,
    if (profile$missing_months[i] == 0L) {
      paste(profile$complete_months[i], "complete monthly observations.")
    } else {
      paste(profile$missing_months[i], "missing month(s):", profile$missing_month_list[i])
    },
    "output/qa/core_import_profile.csv"
  )
  add_check(
    "time_series", profile$metric_id[i], "latest_monthly_change_not_extreme",
    "warning", !isTRUE(profile$latest_change_unusual[i]),
    paste0(
      "Latest change ", signif(profile$latest_monthly_change[i], 5L),
      "; robust z-score ", signif(profile$latest_change_robust_z[i], 4L), "."
    ),
    "output/qa/core_import_profile.csv"
  )
}

# Hand-checked reference observations from official source workbooks.
references <- read_csv("config/core_qa_reference_values.csv", "calendar_month")
for (i in seq_len(nrow(references))) {
  reference <- references[i]
  observed <- national[
    metric_id == reference$metric_id & calendar_month == reference$calendar_month,
    value
  ]
  passed <- length(observed) == 1L && is.finite(observed) &&
    abs(observed - reference$expected_value) <= reference$tolerance
  add_check(
    "source_reference", reference$metric_id,
    paste0("reference_value_", reference$calendar_month),
    reference$severity, passed,
    paste0(
      reference$source_note, "; expected ", signif(reference$expected_value, 12L),
      "; observed ", if (length(observed)) signif(observed[1L], 12L) else "not imported", "."
    ),
    "config/core_qa_reference_values.csv"
  )
}

# Forecast usefulness, vintage integrity and interval monitoring.
for (metric in included_metrics) {
  metric_dir <- file.path("output/core", metric)
  comparison_path <- file.path(metric_dir, "national_model_comparison.csv")
  comparison <- if (file.exists(comparison_path)) read_csv(comparison_path) else {
    data.table::data.table()
  }
  reference <- comparison[model == "reference_ensemble"]
  naive <- comparison[model == "seasonal_naive"]
  add_check(
    "forecast", metric, "reference_model_scored", "fatal",
    nrow(reference) == 1L && is.finite(reference$rmse_native),
    paste("reference rows", nrow(reference)), comparison_path
  )
  if (nrow(reference) == 1L && nrow(naive) == 1L && is.finite(naive$rmse_native)) {
    ratio <- reference$rmse_native / naive$rmse_native
    add_check(
      "forecast", metric, "reference_rmse_within_10pct_of_seasonal_naive",
      "warning", ratio <= 1.10,
      paste0("RMSE ratio ", round(ratio, 3L), "."), comparison_path
    )
    bias_share <- if (reference$mae_native > 0) {
      abs(reference$bias_native) / reference$mae_native
    } else {
      NA_real_
    }
    add_check(
      "forecast", metric, "reference_bias_below_half_mae", "warning",
      !is.finite(bias_share) || bias_share <= 0.5,
      paste0("Absolute bias / MAE = ", round(bias_share, 3L), "."), comparison_path
    )
  }
  reversal_path <- file.path(
    metric_dir, "national_forecast_reversal_diagnostic.csv"
  )
  if (file.exists(reversal_path)) {
    reversal <- read_csv(reversal_path)
    reversal_flag <- nrow(reversal) == 1L &&
      isTRUE(parse_logical_strict(reversal$forecast_reversal_flag)[1L])
    add_check(
      "forecast", metric, "material_recent_trend_reversal", "warning",
      !reversal_flag,
      if (nrow(reversal)) reversal$forecast_reversal_reason[1L] else {
        "Forecast reversal diagnostic was empty."
      },
      reversal_path
    )
  }
  archive_path <- file.path(metric_dir, "national_release_forecast_archive.csv")
  scorecard_path <- file.path(metric_dir, "national_release_forecast_scorecard.csv")
  if (file.exists(archive_path) && file.exists(scorecard_path)) {
    archive <- read_csv(archive_path)
    scorecard <- read_csv(scorecard_path)
    consistency <- release_qa_archive_consistency(
      archive, scorecard,
      c("metric_id", "forecast_version", "data_through_month", "forecast_month", "entity_id", "model")
    )
    add_check(
      "forecast_vintage", metric, "scorecard_retains_archived_forecast", "fatal",
      consistency$passed, consistency$detail, scorecard_path
    )
    coverage <- release_qa_interval_coverage(scorecard)
    add_check(
      "forecast_vintage", metric, "genuine_release_interval_coverage", "review", NA,
      if (coverage$n) paste0(
        coverage$n, " scored release forecast(s); 80% coverage ",
        round(100 * coverage$coverage_80, 1L), "%; 95% coverage ",
        round(100 * coverage$coverage_95, 1L), "%."
      ) else "No genuine release forecast has matured yet.",
      scorecard_path
    )
  }
}

# A compact, deterministic provider sample for human review.
provider_review_parts <- list()
provider_review_index <- 0L
ae_watch_path <- "output/provider/latest_watchlist.csv"
if (file.exists(ae_watch_path)) {
  ae_watch <- read_csv(ae_watch_path)
  ae_candidates <- list(
    favourable = utils::head(ae_watch[
      signal == "sustained_above_trajectory"
    ][order(-six_month_gap_to_trajectory_pp)], 3L),
    adverse = utils::head(ae_watch[
      signal == "sustained_below_trajectory"
    ][order(six_month_gap_to_trajectory_pp)], 3L),
    near_threshold = utils::head(ae_watch[
      signal == "within_sustained_threshold" &
        is.finite(six_month_gap_to_trajectory_pp)
    ][order(-abs(six_month_gap_to_trajectory_pp))], 3L)
  )
  for (group in names(ae_candidates)) if (nrow(ae_candidates[[group]])) {
    provider_review_index <- provider_review_index + 1L
    provider_review_parts[[provider_review_index]] <- ae_candidates[[group]][, .(
      metric_id = "ae4h_all",
      entity_id = analysis_trust_id,
      entity_name = analysis_trust_name,
      review_group = group,
      signal,
      gap_native = six_month_gap_to_trajectory_pp / 100,
      direction_share = six_month_direction_share,
      latest_error_same_direction,
      signal_evidence
    )]
  }
}
core_model_config <- read_csv("config/core_model.csv")
for (metric in model_status[model_status == "included", metric_id]) {
  watch_path <- file.path("output/core", metric, "provider_latest_watchlist.csv")
  if (!file.exists(watch_path)) next
  watch <- read_csv(watch_path)
  if (!nrow(watch)) next
  threshold <- core_model_config[metric_id == metric, signal_materiality_native][1L]
  candidates <- list(
    favourable = utils::head(watch[
      signal == "sustained_favourable"
    ][order(-favourable_gap_native)], 3L),
    adverse = utils::head(watch[
      signal == "sustained_adverse"
    ][order(favourable_gap_native)], 3L),
    near_threshold = utils::head(watch[
      signal == "no_sustained_signal" & is.finite(favourable_gap_native) &
        abs(favourable_gap_native) < threshold
    ][order(-abs(favourable_gap_native))], 3L)
  )
  for (group in names(candidates)) if (nrow(candidates[[group]])) {
    provider_review_index <- provider_review_index + 1L
    provider_review_parts[[provider_review_index]] <- candidates[[group]][, .(
      metric_id,
      entity_id,
      entity_name,
      review_group = group,
      signal,
      gap_native = favourable_gap_native,
      direction_share,
      latest_error_same_direction,
      signal_evidence
    )]
  }
}
provider_review <- if (length(provider_review_parts)) {
  data.table::rbindlist(provider_review_parts, use.names = TRUE, fill = TRUE)
} else {
  data.table::data.table(
    metric_id = character(), entity_id = character(), entity_name = character(),
    review_group = character(), signal = character(), gap_native = numeric(),
    direction_share = numeric(), latest_error_same_direction = logical(),
    signal_evidence = character()
  )
}
data.table::fwrite(provider_review, "output/qa/provider_signal_review_sample.csv")

# Publication completeness, page counts and portrait A4 geometry.
overview_manifest <- read_csv("output/performance/overview_manifest.csv")
manifest_has_mode <- "publication_mode" %in% names(overview_manifest) &&
  nrow(overview_manifest) == 1L &&
  overview_manifest$publication_mode[1L] %in% c("forecast", "outturn")
add_check(
  "publication", "ALL", "publication_mode_is_explicit", "fatal",
  manifest_has_mode,
  if (manifest_has_mode) {
    paste("Publication mode:", overview_manifest$publication_mode[1L])
  } else {
    "Manifest must contain exactly one forecast/outturn publication mode."
  },
  "output/performance/overview_manifest.csv"
)
manifest_is_pilot <- "publication_status" %in% names(overview_manifest) &&
  nrow(overview_manifest) == 1L &&
  overview_manifest$publication_status[1L] == "pilot"
add_check(
  "publication", "ALL", "publication_status_is_pilot", "fatal",
  manifest_is_pilot,
  if (manifest_is_pilot) "Publication status: pilot." else {
    "Overview manifest must identify this publication as pilot."
  },
  "output/performance/overview_manifest.csv"
)
publication_mode <- if (manifest_has_mode) {
  overview_manifest$publication_mode[1L]
} else {
  "invalid"
}
published_snapshot_path <- "output/performance/latest_published_forecast_rows.csv"
add_check(
  "publication", "ALL", "published_forecast_snapshot_exists", "fatal",
  file.exists(published_snapshot_path), published_snapshot_path,
  published_snapshot_path
)
if (file.exists(published_snapshot_path) && manifest_has_mode) {
  published_snapshot <- read_csv(
    published_snapshot_path,
    c("latest_month", "forecast_month", "publication_issue_date")
  )
  snapshot_issue_dates <- unique(as.Date(published_snapshot$publication_issue_date))
  manifest_issue_date <- as.Date(overview_manifest$issue_date[1L])
  if (publication_mode == "forecast") {
    current_rows <- read_csv(
      "output/performance/overview_metric_rows.csv", "forecast_month"
    )
    snapshot_values <- published_snapshot[, .(
      metric_id, forecast_month, forecast_value
    )]
    current_values <- current_rows[, .(
      metric_id, forecast_month, forecast_value
    )]
    snapshot_comparison <- merge(
      snapshot_values, current_values,
      by = c("metric_id", "forecast_month"), all = TRUE,
      suffixes = c("_snapshot", "_current")
    )
    values_match <- nrow(snapshot_comparison) == nrow(published_snapshot) &&
      nrow(snapshot_comparison) == nrow(current_rows) &&
      all(is.finite(snapshot_comparison$forecast_value_snapshot)) &&
      all(is.finite(snapshot_comparison$forecast_value_current)) &&
      all(abs(
        snapshot_comparison$forecast_value_snapshot -
          snapshot_comparison$forecast_value_current
      ) <= 1e-12)
    add_check(
      "publication", "ALL", "forecast_snapshot_matches_published_rows", "fatal",
      length(snapshot_issue_dates) == 1L &&
        identical(snapshot_issue_dates, manifest_issue_date) &&
        values_match,
      paste(
        nrow(published_snapshot), "snapshot row(s); issue date",
        paste(snapshot_issue_dates, collapse = ", ")
      ),
      published_snapshot_path
    )
  } else if (publication_mode == "outturn") {
    source_issue_date <- as.Date(overview_manifest$source_forecast_issue_date[1L])
    add_check(
      "publication", "ALL", "outturn_uses_prior_forecast_snapshot", "fatal",
      length(snapshot_issue_dates) == 1L &&
        identical(snapshot_issue_dates, source_issue_date) &&
        is.finite(as.numeric(source_issue_date)) &&
        source_issue_date < manifest_issue_date,
      paste(
        "forecast issued", source_issue_date, "; outturn issued",
        manifest_issue_date
      ),
      published_snapshot_path
    )
    provider_watch_manifest <- read_csv(
      "output/performance/outturn_provider_watch_manifest.csv",
      c("data_through_month", "actual_month")
    )
    provider_watch_current <- nrow(provider_watch_manifest) > 0L &&
      !anyDuplicated(provider_watch_manifest$metric_id) &&
      all(
        !is.na(provider_watch_manifest$data_through_month) &
          !is.na(provider_watch_manifest$actual_month) &
          provider_watch_manifest$data_through_month ==
            provider_watch_manifest$actual_month
      )
    add_check(
      "publication", "ALL", "outturn_provider_watch_uses_new_release", "fatal",
      provider_watch_current,
      paste(nrow(provider_watch_manifest), "provider-watch detail file(s)."),
      "output/performance/outturn_provider_watch_manifest.csv"
    )
  }
}
publication_manifest_path <- file.path(
  "output/publication",
  overview_manifest$issue_date[1L],
  publication_mode,
  "publication_manifest.csv"
)
add_check(
  "publication", "ALL", "publication_manifest_exists", "fatal",
  file.exists(publication_manifest_path), publication_manifest_path,
  publication_manifest_path
)
if (file.exists(publication_manifest_path)) {
  publication_manifest <- read_csv(publication_manifest_path)
  expected_html <- if (publication_mode == "forecast") {
    current_rows <- read_csv("output/performance/overview_metric_rows.csv")
    unique(c(
      "output/releases/nhs-performance-outlook-forecast-latest.html",
      file.path("output/releases", current_rows$deep_dive_file)
    ))
  } else if (publication_mode == "outturn") {
    provider_watch_manifest <- read_csv(
      "output/performance/outturn_provider_watch_manifest.csv"
    )
    unique(c(
      "output/releases/nhs-performance-outturn-latest.html",
      file.path("output/releases", provider_watch_manifest$output_file)
    ))
  } else {
    character()
  }
  manifested_html <- unique(publication_manifest$source_html)
  add_check(
    "publication", "ALL", "manifest_contains_only_selected_edition",
    "fatal", setequal(expected_html, manifested_html),
    paste("expected", length(expected_html), "HTML source(s); manifest contains", length(manifested_html)),
    publication_manifest_path
  )
  if (publication_mode == "forecast") {
    overview_html_path <-
      "output/releases/nhs-performance-outlook-forecast-latest.html"
    overview_html <- paste(
      readLines(overview_html_path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    current_rows <- read_csv("output/performance/overview_metric_rows.csv")
    expected_metric_ids <- c(
      "ae4h_all", "ambulance_cat2", "rtt_18w", "diagnostics_6w",
      "cancer_62d", "ucr_2h", "community_18w", "talking_therapies_6w"
    )
    detail_links_present <- nrow(current_rows) == 8L && all(vapply(
      current_rows$deep_dive_file,
      function(x) grepl(paste0('href="', x, '"'), overview_html, fixed = TRUE),
      logical(1)
    ))
    add_check(
      "publication", "ALL", "overview_contains_eight_indicators", "fatal",
      nrow(current_rows) == 8L &&
        identical(current_rows$metric_id, expected_metric_ids),
      paste(nrow(current_rows), "indicator row(s):",
            paste(current_rows$metric_id, collapse = ", ")),
      "output/performance/overview_metric_rows.csv"
    )
    add_check(
      "publication", "ALL", "overview_html_displays_pilot", "fatal",
      grepl("Pilot", overview_html, fixed = TRUE),
      "Pilot label is rendered in the underlying main publication HTML.",
      overview_html_path
    )
    add_check(
      "publication", "ALL", "overview_has_eight_web_detail_links", "fatal",
      detail_links_present &&
        grepl('<th class="detail-head">Detail</th>', overview_html, fixed = TRUE),
      paste(sum(vapply(
        current_rows$deep_dive_file,
        function(x) grepl(paste0('href="', x, '"'), overview_html, fixed = TRUE),
        logical(1)
      )), "configured Detail link(s) present."),
      overview_html_path
    )
    add_check(
      "publication", "community_18w",
      "community_detail_is_not_classified_as_rtt", "fatal",
      identical(
        current_rows[metric_id == "community_18w", deep_dive_file],
        "community-18-week-outlook-latest.html"
      ) && identical(
        current_rows[metric_id == "rtt_18w", deep_dive_file],
        "rtt-18-week-outlook-latest.html"
      ),
      "Community and RTT use distinct metric IDs and detailed output paths.",
      "output/performance/overview_metric_rows.csv"
    )
    add_check(
      "publication", "ucr_2h", "ucr_detail_is_in_publication_bundle", "fatal",
      "output/releases/urgent-community-response-outlook-latest.html" %in%
        manifested_html,
      "Two-hour UCR detailed forecast is included.", publication_manifest_path
    )
    add_check(
      "publication", "ambulance_cat2",
      "ambulance_detail_is_in_publication_bundle", "fatal",
      "output/releases/ambulance-category-2-outlook-latest.html" %in%
        manifested_html,
      "Ambulance Category 2 detailed forecast is included.",
      publication_manifest_path
    )
  }
  add_check(
    "publication", "ALL", "manifest_rows_match_publication_mode", "fatal",
    "publication_mode" %in% names(publication_manifest) &&
      "publication_status" %in% names(publication_manifest) &&
      "artifact_role" %in% names(publication_manifest) &&
      nrow(publication_manifest) > 0L &&
      all(publication_manifest$publication_mode == publication_mode) &&
      all(publication_manifest$publication_status == "pilot") &&
      all(publication_manifest$edition == publication_mode) &&
      sum(publication_manifest$artifact_role == "overview") == 1L &&
      if (publication_mode == "outturn") {
        all(publication_manifest$artifact_role %in% c("overview", "provider_watch")) &&
          any(publication_manifest$artifact_role == "provider_watch")
      } else {
        all(publication_manifest$artifact_role %in% c(
          "overview", "indicator_deep_dive"
        ))
      },
    paste(nrow(publication_manifest), publication_mode, "artifact(s)."),
    publication_manifest_path
  )
  for (i in seq_len(nrow(publication_manifest))) {
    html_path <- publication_manifest$source_html[i]
    pdf_path <- publication_manifest$output_file[i]
    html_pages <- release_qa_html_pages(html_path)
    pdf_metadata <- release_qa_pdf_metadata(pdf_path)
    add_check(
      "publication", basename(html_path), "html_declares_a4_portrait", "fatal",
      release_qa_html_is_a4_portrait(html_path), html_path, html_path
    )
    add_check(
      "publication", basename(pdf_path), "pdf_created_and_nonempty", "fatal",
      file.exists(pdf_path) && file.info(pdf_path)$size >= 5000,
      if (file.exists(pdf_path)) paste(file.info(pdf_path)$size, "bytes") else "missing",
      pdf_path
    )
    add_check(
      "publication", basename(pdf_path), "pdf_page_count_matches_html", "fatal",
      html_pages > 0L && pdf_metadata$pages == html_pages,
      paste("HTML pages", html_pages, "; PDF pages", pdf_metadata$pages), pdf_path
    )
    portrait_known <- is.finite(pdf_metadata$width_points) &&
      is.finite(pdf_metadata$height_points)
    add_check(
      "publication", basename(pdf_path), "pdf_is_a4_portrait", "fatal",
      portrait_known &&
        abs(pdf_metadata$width_points - 595.3) <= 3 &&
        abs(pdf_metadata$height_points - 841.9) <= 3,
      paste0(
        "MediaBox ", round(pdf_metadata$width_points, 2L), " x ",
        round(pdf_metadata$height_points, 2L), " points."
      ), pdf_path
    )
  }
}

# Human review remains explicit and is not represented as an automated pass.
manual_checks <- c(
  "Review the overview PDF at 100% print scale for clipping and legibility.",
  if (publication_mode == "forecast") {
    "Review one national deep dive and one provider page at 100% print scale."
  } else {
    paste(
      "Confirm that the outturn scores the forecast edition issued before release",
      "and review one refreshed provider-watch page at 100% print scale."
    )
  },
  "Review rejected optional source files and unusual latest monthly movements.",
  "Review output/qa/provider_signal_review_sample.csv: the three largest favourable and adverse signals and three near-threshold cases.",
  "Confirm commentary, source labels, constitutional standards and planning targets are factual."
)
for (i in seq_along(manual_checks)) add_check(
  "manual_signoff", "ALL", paste0("manual_review_", i), "manual", NA,
  manual_checks[i], "output/qa/release_signoff.csv"
)

signoff <- data.table::rbindlist(checks, use.names = TRUE, fill = TRUE)
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(signoff, "output/qa/release_signoff.csv")
warnings <- signoff[severity == "warning" & passed == FALSE]
data.table::fwrite(warnings, "output/qa/release_qa_warnings.csv")
fatal <- signoff[severity == "fatal" & passed == FALSE]
if (nrow(fatal)) {
  stop(
    "Release QA failed with ", nrow(fatal), " fatal check(s): ",
    paste(fatal[, paste(metric_id, check, sep = "/")], collapse = ", "),
    ". Inspect output/qa/release_signoff.csv."
  )
}
message(
  "Automated release QA passed: ", sum(signoff$severity == "fatal"),
  " fatal checks passed; ", nrow(warnings),
  " warning(s) require review; manual sign-off remains pending."
)
