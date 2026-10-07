qa_national_month <- function(x) {
  required <- c(
    "calendar_month", "type1_attendances_n", "type1_within_4h_n", "type1_over_4h_n",
    "type1_performance", "published_type1_performance", "source_method",
    "ae4h_attendances_n", "ae4h_within_4h_n", "ae4h_over_4h_n",
    "ae4h_performance", "published_ae4h_performance",
    "national_comparability_era", "source_file"
  )
  assert_columns(x, required)
  issues <- data.table::rbindlist(list(
    x[is.na(calendar_month), .(calendar_month, source_file, issue = "missing_month")],
    x[type1_within_4h_n < 0 | type1_over_4h_n < 0,
      .(calendar_month, source_file, issue = "negative_count")],
    x[type1_attendances_n <= 0, .(calendar_month, source_file, issue = "nonpositive_denominator")],
    x[abs(type1_attendances_n - type1_within_4h_n - type1_over_4h_n) > 1e-8,
      .(calendar_month, source_file, issue = "denominator_identity_failure")],
    x[abs(type1_performance - published_type1_performance) > 1e-8,
      .(calendar_month, source_file, issue = "published_percentage_mismatch")],
    x[ae4h_within_4h_n < 0 | ae4h_over_4h_n < 0,
      .(calendar_month, source_file, issue = "all_types_negative_count")],
    x[ae4h_attendances_n <= 0,
      .(calendar_month, source_file, issue = "all_types_nonpositive_denominator")],
    x[abs(ae4h_attendances_n - ae4h_within_4h_n - ae4h_over_4h_n) > 1e-8,
      .(calendar_month, source_file, issue = "all_types_denominator_identity_failure")],
    x[abs(ae4h_performance - published_ae4h_performance) > 1e-8,
      .(calendar_month, source_file, issue = "all_types_published_percentage_mismatch")],
    x[source_method == "monthly_collection" &
        (type1_within_4h_n %% 1 != 0 | type1_over_4h_n %% 1 != 0),
      .(calendar_month, source_file, issue = "fractional_count_in_actual_monthly_era")]
  ), use.names = TRUE, fill = TRUE)
  duplicates <- x[, .N, by = calendar_month][N > 1L]
  expected <- data.table::as.IDate(seq(as.Date(min(x$calendar_month)), as.Date(max(x$calendar_month)), by = "month"))
  gaps <- data.table::data.table(calendar_month = setdiff(expected, x$calendar_month))
  list(issues = issues, duplicates = duplicates, gaps = gaps)
}

qa_provider_month <- function(x) {
  required <- c(
    "calendar_month", "source_org_code", "row_scope", "type1_attendances_n",
    "type1_within_4h_n", "type1_over_4h_n", "type1_reported_performance_denominator_n",
    "type1_performance", "submission_status",
    "ae4h_attendances_n", "ae4h_within_4h_n", "ae4h_over_4h_n",
    "ae4h_reported_performance_denominator_n", "ae4h_performance",
    "ae4h_submission_status", "source_file"
  )
  assert_columns(x, required)
  provider <- x[row_scope == "provider"]
  provider[, zero_net_type1_adjustment :=
    !is.na(type1_attendances_n) &
    type1_attendances_n == 0 &
    !is.na(type1_within_4h_n) &
    !is.na(type1_over_4h_n) &
    type1_within_4h_n + type1_over_4h_n == 0 &
    (type1_within_4h_n < 0 | type1_over_4h_n < 0)
  ]
  provider[, zero_net_ae4h_adjustment :=
    !is.na(ae4h_attendances_n) &
    ae4h_attendances_n == 0 &
    !is.na(ae4h_within_4h_n) &
    !is.na(ae4h_over_4h_n) &
    ae4h_within_4h_n + ae4h_over_4h_n == 0 &
    (ae4h_within_4h_n < 0 | ae4h_over_4h_n < 0)
  ]
  issues <- data.table::rbindlist(list(
    provider[is.na(source_org_code) | !nzchar(source_org_code),
      .(calendar_month, source_org_code, source_file, issue = "missing_org_code")],
    provider[!zero_net_type1_adjustment &
        (type1_attendances_n < 0 | type1_over_4h_n < 0 | type1_within_4h_n < 0),
      .(calendar_month, source_org_code, source_file, issue = "negative_count")],
    provider[!zero_net_type1_adjustment &
        !is.na(type1_over_4h_n) & type1_over_4h_n > type1_attendances_n,
      .(calendar_month, source_org_code, source_file, issue = "breaches_exceed_attendances")],
    provider[
      (!is.na(type1_attendances_n) & type1_attendances_n %% 1 != 0) |
        (!is.na(type1_over_4h_n) & type1_over_4h_n %% 1 != 0) |
        (!is.na(type1_within_4h_n) & type1_within_4h_n %% 1 != 0),
      .(calendar_month, source_org_code, source_file, issue = "fractional_provider_count")
    ],
    provider[submission_status == "submitted" &
        abs(type1_attendances_n - type1_reported_performance_denominator_n) > 1e-8,
      .(calendar_month, source_org_code, source_file, issue = "submitted_denominator_mismatch")],
    provider[submission_status == "submitted" & is.na(type1_performance),
      .(calendar_month, source_org_code, source_file, issue = "submitted_but_performance_missing")],
    provider[!zero_net_ae4h_adjustment &
        (ae4h_attendances_n < 0 | ae4h_over_4h_n < 0 | ae4h_within_4h_n < 0),
      .(calendar_month, source_org_code, source_file, issue = "all_types_negative_count")],
    provider[!zero_net_ae4h_adjustment &
        !is.na(ae4h_over_4h_n) & ae4h_over_4h_n > ae4h_attendances_n,
      .(calendar_month, source_org_code, source_file,
        issue = "all_types_breaches_exceed_attendances")],
    provider[ae4h_submission_status == "submitted" &
        abs(ae4h_attendances_n - ae4h_reported_performance_denominator_n) > 1e-8,
      .(calendar_month, source_org_code, source_file,
        issue = "all_types_submitted_denominator_mismatch")],
    provider[ae4h_submission_status == "submitted" & is.na(ae4h_performance),
      .(calendar_month, source_org_code, source_file,
        issue = "all_types_submitted_but_performance_missing")]
  ), use.names = TRUE, fill = TRUE)
  duplicates <- provider[, .N, by = .(calendar_month, source_org_code)][N > 1L]
  source_anomalies <- provider[zero_net_type1_adjustment | zero_net_ae4h_adjustment, .(
    calendar_month,
    source_org_code,
    type1_attendances_n,
    type1_within_4h_n,
    type1_over_4h_n,
    ae4h_attendances_n,
    ae4h_within_4h_n,
    ae4h_over_4h_n,
    source_file,
    issue = data.table::fcase(
      zero_net_type1_adjustment & zero_net_ae4h_adjustment,
        "published_zero_net_type1_and_all_types_adjustment",
      zero_net_ae4h_adjustment, "published_zero_net_all_types_adjustment",
      default = "published_zero_net_type1_adjustment"
    )
  )]
  list(issues = issues, duplicates = duplicates, source_anomalies = source_anomalies)
}

provider_month_reconciliation <- function(x) {
  submitted <- x[
    row_scope == "provider" & ae4h_submission_status == "submitted" &
      type1_attendances_n > 0
  ]
  submitted[, .(
    submitted_type1_attendances_n = sum(type1_attendances_n),
    submitted_type1_within_4h_n = sum(type1_within_4h_n),
    submitted_type1_over_4h_n = sum(type1_over_4h_n),
    submitting_type1_providers_n = data.table::uniqueN(source_org_code),
    submitted_ae4h_attendances_n = sum(ae4h_attendances_n),
    submitted_ae4h_within_4h_n = sum(ae4h_within_4h_n),
    submitted_ae4h_over_4h_n = sum(ae4h_over_4h_n)
  ), by = calendar_month][
    , type1_performance := submitted_type1_within_4h_n /
      (submitted_type1_within_4h_n + submitted_type1_over_4h_n)
  ][
    , ae4h_performance := submitted_ae4h_within_4h_n /
      (submitted_ae4h_within_4h_n + submitted_ae4h_over_4h_n)
  ][]
}
