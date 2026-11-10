diagnostic_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

diagnostic_pp <- function(x, digits = 1L) {
  paste0(ifelse(x > 0, "+", ""), formatC(x, digits = digits, format = "f"), "pp")
}

diagnostic_count <- function(x) {
  paste0(ifelse(x > 0, "+", ""), format(round(x), big.mark = ","))
}

diagnostic_pct_change <- function(current, comparison, digits = 1L) {
  if (!is.finite(current) || !is.finite(comparison) || comparison == 0) return(NA_real_)
  100 * (current / comparison - 1)
}

diagnostic_metric_labels <- function(metric_id) {
  switch(metric_id,
    diagnostics_6w = list(
      numerator = "People waiting over six weeks", denominator = "Total diagnostic waiting list",
      favourable = "A smaller share is waiting over six weeks."
    ),
    cancer_62d = list(
      numerator = "Treatments within 62 days", denominator = "Treatments in the published cohort",
      favourable = "A larger share began treatment within 62 days."
    ),
    community_18w = list(
      numerator = "People within 18 weeks", denominator = "Total community waiting list",
      favourable = "A larger share of the list is within 18 weeks."
    ),
    rtt_18w = list(
      numerator = "Pathways within 18 weeks", denominator = "Incomplete pathways",
      favourable = "A larger share of incomplete pathways is within 18 weeks."
    ),
    list(numerator = "Within-standard activity", denominator = "Published activity",
         favourable = "The headline rate improved.")
  )
}

build_core_metric_diagnostics <- function(national_panel, metric_row,
                                          output_dir = "output/diagnostics") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metric_id <- metric_row$metric_id[1L]
  metric_id_value <- metric_id
  z <- data.table::copy(national_panel[metric_id == metric_id_value])
  z[, calendar_month := data.table::as.IDate(calendar_month)]
  data.table::setorder(z, calendar_month)
  latest <- z[.N]
  previous <- z[calendar_month == data.table::as.IDate(seq(
    as.Date(latest$calendar_month), by = "-1 month", length.out = 2L
  )[2L])][1L]
  year_ago <- z[calendar_month == data.table::as.IDate(seq(
    as.Date(latest$calendar_month), by = "-1 month", length.out = 13L
  )[13L])][1L]
  rate_scale <- if (metric_row$unit[1L] == "proportion") 100 else 1
  rate_suffix <- if (metric_row$unit[1L] == "proportion") "pp" else ""
  change_label <- function(value) paste0(
    ifelse(value > 0, "+", ""), formatC(value, digits = 1L, format = "f"), rate_suffix
  )
  mom <- if (nrow(previous)) rate_scale * (latest$value - previous$value) else NA_real_
  yoy <- if (nrow(year_ago)) rate_scale * (latest$value - year_ago$value) else NA_real_
  has_counts <- nrow(previous) && all(is.finite(c(
    latest$numerator, latest$denominator, previous$numerator, previous$denominator
  )))
  labels <- diagnostic_metric_labels(metric_id)
  numerator_change <- if (has_counts) latest$numerator - previous$numerator else NA_real_
  denominator_change <- if (has_counts) latest$denominator - previous$denominator else NA_real_
  outside_change <- if (has_counts) {
    (latest$denominator - latest$numerator) -
      (previous$denominator - previous$numerator)
  } else NA_real_
  direction <- if (abs(mom) < metric_row$flat_threshold_native[1L] * rate_scale) {
    "was broadly unchanged"
  } else if ((mom > 0) == isTRUE(metric_row$higher_is_better[1L])) {
    "improved"
  } else {
    "deteriorated"
  }
  explanation <- if (identical(metric_id, "ambulance_cat2")) {
    "Monthly and annual Category 2 response-time changes are reported in minutes."
  } else if (has_counts) {
    paste0(
      metric_row$display_name[1L], " ", direction, " in the latest month. ",
      labels$denominator, " changed by ", diagnostic_count(denominator_change),
      ", while ", labels$numerator, " changed by ",
      diagnostic_count(numerator_change), ". The percentage therefore reflects ",
      "both the size of the cohort and how many were within the standard."
    )
  } else {
    paste0(
      metric_row$display_name[1L], " ", direction,
      " in the latest month. The current imported series does not contain a ",
      "reconciled activity denominator, so this page describes the movement but ",
      "does not attribute it to demand or throughput."
    )
  }
  summary <- data.table::data.table(
    metric_id, latest_month = latest$calendar_month,
    previous_month = if (nrow(previous)) previous$calendar_month else data.table::as.IDate(NA),
    year_ago_month = if (nrow(year_ago)) year_ago$calendar_month else data.table::as.IDate(NA),
    month_change_native = mom, year_change_native = yoy,
    numerator_change = numerator_change, denominator_change = denominator_change,
    outside_standard_change = outside_change, interpretation = explanation
  )
  data.table::fwrite(summary, file.path(output_dir, paste0(metric_id, "_summary.csv")))
  # Ambulance Category 2 has no reconciled activity volume in this input.
  # Show observed changes without boilerplate prose or empty-volume placeholders.
  if (identical(metric_id, "ambulance_cat2")) {
    return(list(
      summary = summary,
      html = "",
      compact_html = paste0(
        '<section class="what-changed"><div class="what-changed-heading">',
        '<span>Recent changes</span></div><div class="what-changed-stats">',
        '<div><span>Monthly change</span><strong>', change_label(mom),
        ' min</strong></div>',
        '<div><span>Annual change</span><strong>', change_label(yoy),
        ' min</strong></div></div></section>'
      )
    ))
  }
  cards <- paste0(
    '<div class="diagnostic-cards">',
    '<div><span>Latest monthly change</span><strong>', change_label(mom),
    '</strong><em>versus ', format(as.Date(previous$calendar_month), "%b %Y"), '</em></div>',
    '<div><span>Change over a year</span><strong>', change_label(yoy),
    '</strong><em>versus ', format(as.Date(year_ago$calendar_month), "%b %Y"), '</em></div>',
    if (has_counts) paste0(
      '<div><span>', diagnostic_escape(labels$denominator), '</span><strong>',
      diagnostic_count(denominator_change), '</strong><em>monthly change</em></div>',
      '<div><span>', diagnostic_escape(labels$numerator), '</span><strong>',
      diagnostic_count(numerator_change), '</strong><em>monthly change</em></div>'
    ) else paste0(
      '<div><span>Underlying volume</span><strong>Not available</strong>',
      '<em>in the validated series</em></div>',
      '<div><span>Interpretation</span><strong>Descriptive</strong>',
      '<em>no causal attribution</em></div>'
    ), '</div>'
  )
  detail <- if (has_counts) paste0(
    '<div class="diagnostic-columns"><section><h3>What moved underneath the rate?</h3>',
    '<div class="diagnostic-row"><span>', diagnostic_escape(labels$denominator),
    '</span><strong>', diagnostic_count(denominator_change),
    '</strong><em>change from the previous month</em></div>',
    '<div class="diagnostic-row"><span>', diagnostic_escape(labels$numerator),
    '</span><strong>', diagnostic_count(numerator_change),
    '</strong><em>change from the previous month</em></div>',
    '<div class="diagnostic-row"><span>Outside the standard</span><strong>',
    diagnostic_count(outside_change),
    '</strong><em>derived change from published totals</em></div></section>',
    '<section><h3>How to read this</h3><p class="diagnostic-copy">',
    diagnostic_escape(labels$favourable),
    ' Counts show scale; the percentage shows the balance between the two.</p></section></div>'
  ) else ""
  list(summary = summary, html = paste0(
    cards, '<p class="diagnostic-interpretation"><strong>What changed?</strong> ',
    diagnostic_escape(explanation), '</p>', detail
  ), compact_html = paste0(
    '<section class="what-changed"><div class="what-changed-heading"><span>What changed?</span><p>',
    diagnostic_escape(explanation), '</p></div><div class="what-changed-stats">',
    '<div><span>Monthly</span><strong>', change_label(mom), '</strong></div>',
    '<div><span>Annual</span><strong>', change_label(yoy), '</strong></div>',
    if (has_counts) paste0(
      '<div><span>', diagnostic_escape(labels$denominator), '</span><strong>',
      diagnostic_count(denominator_change), '</strong></div>'
    ) else paste0(
      '<div><span>Volume breakdown</span><strong>Not available</strong></div>'
    ), '</div></section>'
  ))
}

diagnostic_provider_change <- function(panel, value_column, volume_column,
                                       id_column, name_column, latest_month,
                                       comparison_month) {
  keep <- c(id_column, name_column, "calendar_month", value_column, volume_column)
  z <- data.table::copy(panel[calendar_month %in% c(latest_month, comparison_month), ..keep])
  data.table::setnames(
    z, c(id_column, name_column, value_column, volume_column),
    c("entity_id", "entity_name", "value", "volume")
  )
  current <- z[calendar_month == latest_month, .(
    entity_id, entity_name, current_value = value, current_volume = volume
  )]
  previous <- z[calendar_month == comparison_month, .(
    entity_id, previous_value = value, previous_volume = volume
  )]
  out <- merge(current, previous, by = "entity_id", all = FALSE)
  out <- out[
    is.finite(current_value) & is.finite(previous_value) &
      is.finite(current_volume) & is.finite(previous_volume) &
      current_volume > 0 & previous_volume > 0
  ]
  if (!nrow(out)) return(out)
  out[, `:=`(
    change_pp = 100 * (current_value - previous_value),
    current_weight = current_volume / sum(current_volume),
    previous_weight = previous_volume / sum(previous_volume)
  )]
  out[, contribution_pp := 100 * (
    0.5 * (current_weight + previous_weight) *
      (current_value - previous_value) +
      0.5 * (current_value + previous_value) *
      (current_weight - previous_weight)
  )]
  data.table::setorder(out, contribution_pp)
  out[]
}

diagnostic_breadth_summary <- function(changes) {
  if (!nrow(changes)) return(data.table::data.table())
  national_change <- sum(changes$contribution_pp)
  adverse <- changes$change_pp < 0
  contributors <- changes[sign(contribution_pp) == sign(national_change)]
  concentration <- if (nrow(contributors) && national_change != 0) {
    min(1, sum(utils::head(
      contributors[order(-abs(contribution_pp))]$contribution_pp, 10L
    )) / national_change)
  } else {
    NA_real_
  }
  data.table::data.table(
    providers_compared = nrow(changes),
    providers_deteriorating = sum(adverse),
    share_deteriorating = mean(adverse),
    median_change_pp = stats::median(changes$change_pp),
    national_change_pp = national_change,
    top_10_share_of_national_change = concentration
  )
}

diagnostic_top_provider_rows <- function(changes, higher_is_better = TRUE,
                                         n = 5L) {
  if (!nrow(changes)) return("")
  z <- data.table::copy(changes)
  z[, adverse_score := if (higher_is_better) -contribution_pp else contribution_pp]
  data.table::setorder(z, -adverse_score)
  z <- utils::head(z, n)
  paste(vapply(seq_len(nrow(z)), function(i) paste0(
    '<div class="diagnostic-row"><span>', diagnostic_escape(z$entity_name[i]),
    '</span><strong>', diagnostic_pp(z$change_pp[i]),
    '</strong><em>', diagnostic_pp(z$contribution_pp[i]),
    ' contribution</em></div>'
  ), character(1)), collapse = "")
}

build_ae_diagnostics <- function(national, provider_panel,
                                 output_dir = "output/diagnostics") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  national <- data.table::copy(national)
  provider_panel <- data.table::copy(provider_panel)
  national[, calendar_month := data.table::as.IDate(calendar_month)]
  provider_panel[, calendar_month := data.table::as.IDate(calendar_month)]
  latest_month <- max(national$calendar_month)
  previous_month <- data.table::as.IDate(seq(
    as.Date(latest_month), by = "-1 month", length.out = 2L
  )[2L])
  year_ago <- data.table::as.IDate(seq(
    as.Date(latest_month), by = "-1 month", length.out = 13L
  )[13L])
  latest <- national[calendar_month == latest_month][1L]
  previous <- national[calendar_month == previous_month][1L]
  prior_year <- national[calendar_month == year_ago][1L]
  changes <- diagnostic_provider_change(
    provider_panel[complete_submission == TRUE], "ae4h_performance",
    "ae4h_attendances_n", "analysis_trust_id", "analysis_trust_name",
    latest_month, previous_month
  )
  breadth <- diagnostic_breadth_summary(changes)
  attendance_yoy <- diagnostic_pct_change(
    latest$ae4h_attendances_n, prior_year$ae4h_attendances_n
  )
  within_yoy <- diagnostic_pct_change(
    latest$ae4h_within_4h_n, prior_year$ae4h_within_4h_n
  )
  breaches_yoy <- diagnostic_pct_change(
    latest$ae4h_over_4h_n, prior_year$ae4h_over_4h_n
  )
  performance_yoy <- 100 * (
    latest$ae4h_performance - prior_year$ae4h_performance
  )
  interpretation <- if (attendance_yoy >= 2 && performance_yoy <= -0.5) {
    "Demand was higher than a year earlier and performance weakened: the pattern is consistent with demand rising faster than processing and flow capacity."
  } else if (attendance_yoy < 2 && performance_yoy <= -0.5) {
    "Performance weakened without a material rise in attendance volume. Higher demand alone does not explain the deterioration; processing or downstream flow is the stronger descriptive signal."
  } else if (attendance_yoy >= 2 && performance_yoy > -0.5) {
    "Attendance demand increased, but four-hour performance was broadly maintained or improved, indicating that the additional volume was largely absorbed."
  } else {
    "Neither attendance demand nor four-hour performance moved materially relative to a year earlier."
  }
  summary <- data.table::data.table(
    latest_month, previous_month, year_ago,
    performance_change_month_pp = if (nrow(previous)) 100 * (
      latest$ae4h_performance - previous$ae4h_performance
    ) else NA_real_,
    performance_change_year_pp = performance_yoy,
    attendance_change_year_pct = attendance_yoy,
    within_4h_change_year_pct = within_yoy,
    breaches_change_year_pct = breaches_yoy,
    providers_compared = breadth$providers_compared,
    share_providers_deteriorating = breadth$share_deteriorating,
    median_provider_change_pp = breadth$median_change_pp,
    top_10_share_of_national_change = breadth$top_10_share_of_national_change,
    interpretation = interpretation
  )
  data.table::fwrite(summary, file.path(output_dir, "ae_summary.csv"))
  data.table::fwrite(changes, file.path(output_dir, "ae_provider_contributions.csv"))
  cards <- paste0(
    '<div class="diagnostic-cards">',
    '<div><span>Attendance demand</span><strong>',
    formatC(attendance_yoy, digits = 1L, format = "f"), '%</strong><em>versus ',
    format(as.Date(year_ago), "%b %Y"), '</em></div>',
    '<div><span>Four-hour performance</span><strong>',
    diagnostic_pp(performance_yoy), '</strong><em>year on year</em></div>',
    '<div><span>Seen within four hours</span><strong>',
    formatC(within_yoy, digits = 1L, format = "f"), '%</strong><em>year on year</em></div>',
    '<div><span>Four-hour breaches</span><strong>',
    formatC(breaches_yoy, digits = 1L, format = "f"), '%</strong><em>year on year</em></div>',
    '</div>'
  )
  html <- paste0(
    cards,
    '<p class="diagnostic-interpretation"><strong>Demand or flow?</strong> ',
    diagnostic_escape(interpretation), '</p>',
    '<p class="diagnostic-copy">Attendances describe demand at the front door. ',
    'The number seen within four hours and the number breaching the standard show ',
    'how that demand translated into the published performance rate. They cannot ',
    'on their own distinguish department processes from downstream bed and discharge flow.</p>'
  )
  compact_html <- paste0(
    '<section class="what-changed"><div class="what-changed-heading"><span>What changed?</span><p>',
    diagnostic_escape(interpretation), '</p></div><div class="what-changed-stats">',
    '<div><span>Attendances</span><strong>',
    formatC(attendance_yoy, digits = 1L, format = "f"), '%</strong></div>',
    '<div><span>Seen within 4 hours</span><strong>',
    formatC(within_yoy, digits = 1L, format = "f"), '%</strong></div>',
    '<div><span>Breaches</span><strong>',
    formatC(breaches_yoy, digits = 1L, format = "f"), '%</strong></div>',
    '</div></section>'
  )
  list(summary = summary, providers = changes, html = html,
       compact_html = compact_html)
}

# Calculate RTT within-18-week counts without treating truly missing waits as zero.
# In the full CSV, zero-pathway rows can have blank weekly cells. Those rows
# contribute exactly zero; a positive-total row with a missing weekly band
# remains NA and cannot safely be used in a specialty aggregation.
rtt_diagnostic_within_18_rows <- function(x, week_columns) {
  bands <- as.matrix(x[, ..week_columns])
  within <- rowSums(bands, na.rm = FALSE)
  observed <- rowSums(bands, na.rm = TRUE)
  empty_zero_rows <- is.finite(x$total_all) & x$total_all == 0 &
    !is.finite(within) & is.finite(observed) & observed == 0
  within[empty_zero_rows] <- 0
  within
}

rtt_diagnostic_read_month <- function(row) {
  path <- row$local_path
  temporary_directory <- NULL
  if (tolower(tools::file_ext(path)) == "zip") {
    listing <- utils::unzip(path, list = TRUE)
    csv <- listing$Name[grepl("[.]csv$", listing$Name, ignore.case = TRUE)]
    if (length(csv) != 1L) stop("RTT diagnostic expected one CSV in ", path)
    temporary_directory <- tempfile("rtt-diagnostic-")
    dir.create(temporary_directory)
    utils::unzip(path, files = csv, exdir = temporary_directory)
    path <- file.path(temporary_directory, csv)
    on.exit(unlink(temporary_directory, recursive = TRUE), add = TRUE)
  }
  header <- names(data.table::fread(path, nrows = 0L, encoding = "UTF-8"))
  clean <- clean_names_transparent(header)
  names(clean) <- header
  week_clean <- clean[grepl("^gt_[0-9]{2}_to_[0-9]{2}_weeks_sum_1$", clean)]
  upper <- as.integer(sub(
    "^gt_[0-9]{2}_to_([0-9]{2})_weeks_sum_1$", "\\1", week_clean
  ))
  week_clean <- week_clean[upper <= 18L]
  if (length(week_clean) != 18L) {
    stop("RTT diagnostic source must contain 18 weekly bands ending at week 18.")
  }
  wanted <- c(
    "provider_org_code", "provider_org_name", "rtt_part_type",
    "treatment_function_code", "treatment_function_name", "total_all", week_clean
  )
  if (!all(wanted %in% clean)) {
    stop("RTT diagnostic source schema changed; missing ",
         paste(setdiff(wanted, clean), collapse = ", "))
  }
  original <- names(clean)[match(wanted, clean)]
  x <- data.table::fread(
    path, select = original, encoding = "UTF-8", showProgress = FALSE,
    na.strings = c("", "NA", "N/A", "-")
  )
  data.table::setnames(x, clean_names_transparent(names(x)))
  for (column in c("total_all", week_clean)) {
    data.table::set(x, j = column, value = suppressWarnings(as.numeric(x[[column]])))
  }
  x[, within_18 := rtt_diagnostic_within_18_rows(x, week_clean)]
  x[, calendar_month := data.table::as.IDate(row$activity_month)]
  x[
    !is.na(provider_org_code) & nzchar(trimws(provider_org_code)) &
      !is.na(rtt_part_type) & is.finite(total_all)
  ]
}

build_rtt_diagnostics <- function(download_manifest,
                                  output_dir = "output/diagnostics") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- data.table::copy(download_manifest[
    metric_id == "rtt_18w" & dataset_role == "provider_monthly" &
      selected == TRUE & is_current == TRUE
  ])
  rows[, activity_month := data.table::as.IDate(activity_month)]
  data.table::setorder(rows, activity_month)
  rows <- utils::tail(rows, 2L)
  if (nrow(rows) != 2L) stop("RTT diagnostics require the latest two monthly extracts.")
  raw <- data.table::rbindlist(lapply(seq_len(nrow(rows)), function(i) {
    rtt_diagnostic_read_month(rows[i])
  }), use.names = TRUE, fill = TRUE)
  provider <- raw[treatment_function_code == "C_999", .(
    entity_name = core_latest_nonmissing_character(provider_org_name),
    total = sum(total_all, na.rm = TRUE),
    within_18 = sum(within_18, na.rm = TRUE)
  ), by = .(calendar_month, entity_id = provider_org_code, rtt_part_type)]
  provider_wide <- data.table::dcast(
    provider, calendar_month + entity_id + entity_name ~ rtt_part_type,
    value.var = c("total", "within_18"), fill = 0
  )
  required_parts <- paste0("total_", c("Part_1A", "Part_1B", "Part_2", "Part_2A", "Part_3"))
  for (column in required_parts) if (!column %in% names(provider_wide)) {
    provider_wide[, (column) := 0]
  }
  provider_wide[, `:=`(
    incomplete_total = total_Part_2,
    incomplete_within_18 = within_18_Part_2,
    incomplete_over_18 = total_Part_2 - within_18_Part_2,
    admitted_completions = total_Part_1A,
    nonadmitted_completions = total_Part_1B,
    incomplete_with_dta = total_Part_2A,
    new_rtt_periods = total_Part_3,
    performance = within_18_Part_2 / total_Part_2
  )]
  latest_month <- max(provider_wide$calendar_month)
  previous_month <- min(provider_wide$calendar_month)
  changes <- diagnostic_provider_change(
    provider_wide[
      grepl("^[A-Z][A-Z0-9]{2}$", entity_id) &
        is.finite(performance) & incomplete_total > 0
    ],
    "performance", "incomplete_total", "entity_id", "entity_name",
    latest_month, previous_month
  )
  breadth <- diagnostic_breadth_summary(changes)
  totals <- provider_wide[, lapply(.SD, sum, na.rm = TRUE),
    by = calendar_month,
    .SDcols = c(
      "incomplete_total", "incomplete_within_18", "incomplete_over_18",
      "admitted_completions", "nonadmitted_completions",
      "incomplete_with_dta", "new_rtt_periods"
    )
  ]
  current <- totals[calendar_month == latest_month][1L]
  previous <- totals[calendar_month == previous_month][1L]
  current[, `:=`(
    list_change = incomplete_total - previous$incomplete_total,
    over_18_change = incomplete_over_18 - previous$incomplete_over_18,
    completions = admitted_completions + nonadmitted_completions,
    admitted_completion_change = admitted_completions - previous$admitted_completions,
    nonadmitted_completion_change = nonadmitted_completions - previous$nonadmitted_completions,
    new_rtt_period_change = new_rtt_periods - previous$new_rtt_periods,
    dta_change = incomplete_with_dta - previous$incomplete_with_dta,
    stock_flow_residual = (incomplete_total - previous$incomplete_total) -
      (new_rtt_periods - admitted_completions - nonadmitted_completions)
  )]
  specialty <- raw[
    treatment_function_code != "C_999" & rtt_part_type == "Part_2",
    .(
      specialty_name = core_latest_nonmissing_character(treatment_function_name),
      incomplete_total = sum(total_all, na.rm = TRUE),
      incomplete_within_18 = sum(within_18, na.rm = TRUE)
    ), by = .(calendar_month, specialty_code = treatment_function_code)
  ]
  specialty[, incomplete_over_18 := incomplete_total - incomplete_within_18]
  latest_specialty <- specialty[calendar_month == latest_month]
  previous_specialty <- specialty[calendar_month == previous_month, .(
    specialty_code, previous_over_18 = incomplete_over_18,
    previous_total = incomplete_total
  )]
  specialty_change <- merge(
    latest_specialty, previous_specialty, by = "specialty_code", all = FALSE
  )
  specialty_change[, `:=`(
    over_18_change = incomplete_over_18 - previous_over_18,
    list_change = incomplete_total - previous_total,
    performance = incomplete_within_18 / incomplete_total
  )]
  data.table::setorder(specialty_change, -over_18_change)

  # The descriptive specialty page needs current waiting-list levels, not the
  # matched-month changes used for the RTT stock/flow diagnostic above.
  # Do not silently turn missing elapsed-wait bands into zero.
  specialty_levels <- raw[
    !is.na(treatment_function_code) & nzchar(trimws(treatment_function_code)) &
      treatment_function_code != "C_999" & rtt_part_type == "Part_2" &
      calendar_month == latest_month,
    .(
      specialty_name = core_latest_nonmissing_character(treatment_function_name),
      total_waiting_list = sum(total_all),
      within_18_weeks_count = if (all(is.finite(within_18))) {
        sum(within_18)
      } else {
        NA_real_
      },
      provider_rows = .N,
      unresolved_positive_rows = sum(total_all > 0 & !is.finite(within_18)),
      unresolved_pathways = sum(
        total_all[total_all > 0 & !is.finite(within_18)], na.rm = TRUE
      )
    ), by = .(calendar_month, specialty_code = treatment_function_code)
  ]
  specialty_levels[, `:=`(
    over_18_weeks_count = total_waiting_list - within_18_weeks_count,
    within_18_weeks_proportion = within_18_weeks_count / total_waiting_list
  )]
  specialty_levels[, complete_submission :=
    is.finite(total_waiting_list) & total_waiting_list > 0 &
      is.finite(within_18_weeks_count) & within_18_weeks_count >= 0 &
      within_18_weeks_count <= total_waiting_list
  ]
  data.table::fwrite(
    specialty_levels, file.path(output_dir, "rtt_specialty_levels.csv")
  )
  balance <- current$new_rtt_periods - current$completions
  interpretation <- if (balance > 0) {
    paste0(
      "New RTT periods exceeded admitted and non-admitted completions by ",
      format(round(balance), big.mark = ","),
      ". This is consistent with inflow exceeding recorded outflow; the residual also includes transfers, corrections and reporting differences."
    )
  } else {
    paste0(
      "Recorded completions exceeded new RTT periods by ",
      format(round(abs(balance)), big.mark = ","),
      ". Any list growth despite that balance is therefore concentrated in transfers, corrections or reporting differences."
    )
  }
  summary <- data.table::data.table(
    latest_month, previous_month,
    list_change = current$list_change,
    over_18_change = current$over_18_change,
    new_rtt_periods = current$new_rtt_periods,
    new_rtt_period_change = current$new_rtt_period_change,
    admitted_completions = current$admitted_completions,
    admitted_completion_change = current$admitted_completion_change,
    nonadmitted_completions = current$nonadmitted_completions,
    nonadmitted_completion_change = current$nonadmitted_completion_change,
    incomplete_with_dta = current$incomplete_with_dta,
    dta_change = current$dta_change,
    stock_flow_residual = current$stock_flow_residual,
    providers_compared = breadth$providers_compared,
    share_providers_deteriorating = breadth$share_deteriorating,
    median_provider_change_pp = breadth$median_change_pp,
    top_10_share_of_national_change = breadth$top_10_share_of_national_change,
    interpretation = interpretation
  )
  data.table::fwrite(summary, file.path(output_dir, "rtt_summary.csv"))
  data.table::fwrite(changes, file.path(output_dir, "rtt_provider_contributions.csv"))
  data.table::fwrite(specialty_change, file.path(output_dir, "rtt_specialty_contributions.csv"))
  specialty_rows <- paste(vapply(seq_len(min(6L, nrow(specialty_change))), function(i) {
    z <- specialty_change[i]
    paste0('<div class="diagnostic-row"><span>', diagnostic_escape(z$specialty_name),
           '</span><strong>', format(round(z$over_18_change), big.mark = ","),
           '</strong><em>change in over-18-week pathways</em></div>')
  }, character(1)), collapse = "")
  html <- paste0(
    '<div class="diagnostic-cards">',
    '<div><span>Waiting-list change</span><strong>',
    diagnostic_count(current$list_change), '</strong><em>month on month</em></div>',
    '<div><span>New RTT periods</span><strong>',
    format(round(current$new_rtt_periods), big.mark = ","), '</strong><em>latest month</em></div>',
    '<div><span>Total completions</span><strong>',
    format(round(current$completions), big.mark = ","), '</strong><em>',
    format(round(current$admitted_completions), big.mark = ","), ' admitted</em></div>',
    '<div><span>Over-18-week pathways</span><strong>',
    diagnostic_count(current$over_18_change),
    '</strong><em>change from previous month</em></div>',
    '</div><p class="diagnostic-interpretation"><strong>Inflow or outflow?</strong> ',
    diagnostic_escape(interpretation), '</p>',
    '<section class="diagnostic-balance"><h3>Stock and flow detail</h3>',
    '<div class="diagnostic-row"><span>New RTT periods</span><strong>',
    diagnostic_count(current$new_rtt_period_change),
    '</strong><em>change from previous month</em></div>',
    '<div class="diagnostic-row"><span>Admitted completions</span><strong>',
    diagnostic_count(current$admitted_completion_change),
    '</strong><em>change from previous month</em></div>',
    '<div class="diagnostic-row"><span>Non-admitted completions</span><strong>',
    diagnostic_count(current$nonadmitted_completion_change),
    '</strong><em>change from previous month</em></div>',
    '<div class="diagnostic-row"><span>Incomplete pathways with a DTA</span><strong>',
    diagnostic_count(current$dta_change),
    '</strong><em>change from previous month</em></div></section>',
    '<div class="diagnostic-columns"><section><h3>Specialties adding most over-18-week pathways</h3>',
    specialty_rows, '</section><section><h3>How to read this</h3>',
    '<p class="diagnostic-copy">The specialty rows show where the national change ',
    'in long waits was concentrated. They are not adjusted for specialty size, case ',
    'mix or transfers between providers.</p></section></div>'
  )
  compact_html <- paste0(
    '<section class="what-changed"><div class="what-changed-heading"><span>What changed?</span><p>',
    diagnostic_escape(interpretation), '</p></div><div class="what-changed-stats">',
    '<div><span>Waiting list</span><strong>', diagnostic_count(current$list_change),
    '</strong></div><div><span>Over 18 weeks</span><strong>',
    diagnostic_count(current$over_18_change), '</strong></div>',
    '<div><span>New periods less completions</span><strong>',
    diagnostic_count(balance), '</strong></div></div></section>'
  )
  list(summary = summary, providers = changes, specialties = specialty_change,
       specialty_levels = specialty_levels,
       html = html, compact_html = compact_html)
}
