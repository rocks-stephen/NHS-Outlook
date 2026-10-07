core_source_fields <- function(row) {
  list(
    source_file = basename(row$local_path),
    source_url = row$source_url,
    source_sha256 = row$sha256
  )
}

core_latest_nonmissing_character <- function(x) {
  z <- as.character(x)
  z <- z[!is.na(z) & nzchar(trimws(z))]
  if (length(z)) utils::tail(z, 1L) else NA_character_
}

core_make_national_rows <- function(metric_id, month, value, numerator, denominator,
                                    complete, source, method,
                                    activity_volume_proxy = NA_real_,
                                    activity_volume_proxy_method = NA_character_) {
  data.table::data.table(
    metric_id = metric_id,
    calendar_month = data.table::as.IDate(month),
    entity_id = "ENGLAND",
    entity_name = "England",
    numerator = as.numeric(numerator),
    denominator = as.numeric(denominator),
    activity_volume_proxy = as.numeric(activity_volume_proxy),
    activity_volume_proxy_method = as.character(activity_volume_proxy_method),
    value = as.numeric(value),
    complete_submission = as.logical(complete),
    source_method = method,
    source_file = source$source_file,
    source_url = source$source_url,
    source_sha256 = source$source_sha256
  )
}

core_make_provider_rows <- function(metric_id, month, entity_id, entity_name,
                                    value, numerator, denominator, complete,
                                    source, method,
                                    activity_volume_proxy = NA_real_,
                                    activity_volume_proxy_method = NA_character_) {
  data.table::data.table(
    metric_id = metric_id,
    calendar_month = data.table::as.IDate(month),
    entity_id = as.character(entity_id),
    entity_name = as.character(entity_name),
    numerator = as.numeric(numerator),
    denominator = as.numeric(denominator),
    activity_volume_proxy = as.numeric(activity_volume_proxy),
    activity_volume_proxy_method = as.character(activity_volume_proxy_method),
    value = as.numeric(value),
    complete_submission = as.logical(complete),
    source_method = method,
    source_file = source$source_file,
    source_url = source$source_url,
    source_sha256 = source$source_sha256
  )
}

core_first_exact_column <- function(names_value, candidates, field) {
  hit <- intersect(candidates, names_value)
  if (length(hit) != 1L) {
    stop("Expected one column for ", field, "; found ", length(hit), ".")
  }
  hit
}

core_read_rtt_national <- function(row) {
  source <- core_source_fields(row)
  sheets <- readxl::excel_sheets(row$local_path)
  sheet <- sheets[grepl("(?i)full.*time.*series", sheets, perl = TRUE)][1L]
  if (is.na(sheet)) stop("RTT national workbook lacks a Full Time Series sheet.")
  m <- read_excel_matrix(row$local_path, sheet)
  header_rows <- which(apply(m, 1L, function(cells) {
    any(tolower(trimws(cells)) == "year") &&
      any(tolower(trimws(cells)) == "month") &&
      any(grepl("(?i)incomplete RTT pathways", cells, perl = TRUE))
  }))
  if (length(header_rows) != 1L) {
    stop("Expected one RTT national header row; found ", length(header_rows), ".")
  }
  header_row <- header_rows[1L]
  group_header <- trimws(m[header_row, ])
  detail_header <- trimws(m[header_row + 1L, ])
  for (j in seq_along(group_header)) {
    if (!nzchar(group_header[j]) && j > 1L) group_header[j] <- group_header[j - 1L]
  }
  merged_names <- clean_names_transparent(paste(group_header, detail_header))
  month_col <- which(grepl("(^|_)month(_|$)", merged_names))
  pct_col <- which(grepl(
    "incomplete_rtt_pathways.*pct_within_18_weeks_with_estimates_for_missing_data$",
    merged_names
  ))
  numerator_col <- which(grepl(
    "incomplete_rtt_pathways.*no_within_18_weeks_with_estimates_for_missing_data$",
    merged_names
  ))
  # The workbook can leave an estimated cell blank when no missing-provider
  # adjustment is needed.  Retain the preferred estimated series, but fall
  # back row by row to the corresponding published unadjusted national value.
  base_pct_col <- which(grepl(
    "incomplete_rtt_pathways.*pct_within_18_weeks$", merged_names
  ))
  base_numerator_col <- which(grepl(
    "incomplete_rtt_pathways.*no_within_18_weeks$", merged_names
  ))
  if (length(month_col) != 1L || length(pct_col) != 1L ||
      length(numerator_col) != 1L || length(base_pct_col) != 1L ||
      length(base_numerator_col) != 1L) {
    stop(
      "RTT national workbook schema changed: expected one month, estimated ",
      "and unadjusted percentage and numerator column."
    )
  }
  data_rows <- seq.int(header_row + 2L, nrow(m))
  month <- parse_month_cell(m[data_rows, month_col])
  value <- numeric_cell(m[data_rows, pct_col])
  numerator <- numeric_cell(m[data_rows, numerator_col])
  base_value <- numeric_cell(m[data_rows, base_pct_col])
  base_numerator <- numeric_cell(m[data_rows, base_numerator_col])
  use_base_value <- !is.finite(value) | value <= 0 | value >= 1
  use_base_numerator <- !is.finite(numerator) | numerator <= 0
  value[use_base_value] <- base_value[use_base_value]
  numerator[use_base_numerator] <- base_numerator[use_base_numerator]
  denominator <- numerator / value
  keep <- !is.na(month) & is.finite(value) & value > 0 & value < 1 &
    is.finite(numerator) & numerator > 0
  core_make_national_rows(
    "rtt_18w", month[keep], value[keep], numerator[keep], denominator[keep],
    TRUE, source, "official_rtt_overview_estimates_with_published_row_fallback"
  )
}

core_unzip_one_csv <- function(path) {
  files <- utils::unzip(path, list = TRUE)
  csv <- files$Name[grepl("(?i)\\.csv$", files$Name, perl = TRUE)]
  if (length(csv) != 1L) {
    stop("Expected one CSV inside ", basename(path), "; found ", length(csv), ".")
  }
  directory <- tempfile("core-unzip-")
  dir.create(directory)
  utils::unzip(path, files = csv, exdir = directory)
  extracted <- file.path(directory, csv)
  attr(extracted, "temporary_directory") <- directory
  extracted
}

core_read_rtt_provider <- function(row) {
  source <- core_source_fields(row)
  csv_path <- if (tolower(tools::file_ext(row$local_path)) == "zip") {
    core_unzip_one_csv(row$local_path)
  } else {
    row$local_path
  }
  temporary_directory <- attr(csv_path, "temporary_directory")
  if (!is.null(temporary_directory)) {
    on.exit(unlink(temporary_directory, recursive = TRUE), add = TRUE)
  }
  header <- names(data.table::fread(csv_path, nrows = 0L, encoding = "UTF-8"))
  clean <- clean_names_transparent(header)
  names(clean) <- header
  required_clean <- c(
    "provider_org_code", "provider_org_name", "rtt_part_type",
    "treatment_function_code", "total_all"
  )
  if (!all(required_clean %in% clean)) {
    stop("RTT full extract schema changed; missing: ", paste(
      setdiff(required_clean, clean), collapse = ", "
    ))
  }
  week_clean <- clean[grepl(
    "^gt_[0-9]{2}_to_[0-9]{2}_weeks_sum_1$", clean
  )]
  upper_week <- as.integer(sub(
    "^gt_[0-9]{2}_to_([0-9]{2})_weeks_sum_1$", "\\1", week_clean
  ))
  week_clean <- week_clean[upper_week <= 18L]
  if (length(week_clean) != 18L) {
    stop("RTT full extract must contain 18 weekly bands ending at week 18.")
  }
  wanted_clean <- c(required_clean, week_clean)
  wanted_original <- names(clean)[match(wanted_clean, clean)]
  x <- data.table::fread(
    csv_path,
    select = wanted_original,
    na.strings = c("", "NA", "N/A", "-"),
    encoding = "UTF-8",
    showProgress = FALSE
  )
  data.table::setnames(x, clean_names_transparent(names(x)))
  x <- x[
    rtt_part_type == "Part_2" & treatment_function_code == "C_999"
  ]
  for (column in c("total_all", week_clean)) {
    data.table::set(x, j = column, value = suppressWarnings(as.numeric(x[[column]])))
  }
  x[, within_18 := rowSums(.SD, na.rm = FALSE), .SDcols = week_clean]
  x <- x[
    grepl("^[A-Z][A-Z0-9]{2}$", provider_org_code) &
      grepl("(?i)NHS.*(TRUST|FOUNDATION)", provider_org_name, perl = TRUE)
  ]
  x <- x[, .(
    entity_name = core_latest_nonmissing_character(provider_org_name),
    numerator = sum(within_18, na.rm = FALSE),
    denominator = sum(total_all, na.rm = FALSE)
  ), by = .(entity_id = provider_org_code)]
  x[, `:=`(
    value = numerator / denominator,
    complete = is.finite(numerator) & is.finite(denominator) & denominator > 0 &
      numerator >= 0 & numerator <= denominator
  )]
  core_make_provider_rows(
    "rtt_18w", row$activity_month, x$entity_id, x$entity_name,
    x$value, x$numerator, x$denominator, x$complete, source,
    "rtt_part_2_total_treatment_function_provider_extract"
  )
}

core_diagnostics_total_series <- function(path, sheet) {
  m <- read_excel_matrix(path, sheet)
  header_rows <- which(vapply(seq_len(nrow(m)), function(i) {
    cells <- m[i, ]
    parsed_months <- parse_month_cell(cells)
    any(grepl("(?i)^Diagnostic ID$", cells, perl = TRUE)) &&
      sum(!is.na(parsed_months)) >= 12L
  }, logical(1)))
  if (length(header_rows) != 1L) {
    stop("Expected one dated header row on diagnostics sheet '", sheet, "'.")
  }
  header_row <- header_rows[1L]
  header <- m[header_row, ]
  months <- parse_month_cell(header)
  date_columns <- which(!is.na(months))
  selected_months <- months[date_columns]
  if (anyDuplicated(selected_months)) {
    stop("Diagnostics sheet '", sheet, "' contains duplicate month headings.")
  }
  expected_months <- data.table::as.IDate(seq(
    as.Date(min(selected_months)), as.Date(max(selected_months)), by = "month"
  ))
  if (!identical(as.character(selected_months), as.character(expected_months))) {
    stop("Diagnostics sheet '", sheet, "' does not contain a consecutive monthly series.")
  }
  total_rows <- which(seq_len(nrow(m)) > header_row & apply(m, 1L, function(cells) {
    any(tolower(trimws(cells)) == "total")
  }))
  if (!length(total_rows)) stop("No England Total row on diagnostics sheet '", sheet, "'.")
  total_row <- total_rows[1L]
  data.table::data.table(
    calendar_month = selected_months,
    amount = numeric_cell(m[total_row, date_columns])
  )
}

core_read_diagnostics_national <- function(row) {
  source <- core_source_fields(row)
  sheets <- readxl::excel_sheets(row$local_path)
  required_sheets <- c("Total Waiting List", "6+ Week Waits")
  if (!all(required_sheets %in% sheets)) {
    stop("Diagnostics time-series workbook is missing: ", paste(
      setdiff(required_sheets, sheets), collapse = ", "
    ))
  }
  denominator <- core_diagnostics_total_series(row$local_path, "Total Waiting List")
  numerator <- core_diagnostics_total_series(row$local_path, "6+ Week Waits")
  data.table::setnames(denominator, "amount", "denominator")
  data.table::setnames(numerator, "amount", "numerator")
  x <- merge(denominator, numerator, by = "calendar_month", all = TRUE)
  x[, `:=`(
    value = numerator / denominator,
    complete = is.finite(numerator) & is.finite(denominator) & denominator > 0 &
      numerator >= 0 & numerator <= denominator
  )]
  x <- x[complete == TRUE]
  core_make_national_rows(
    "diagnostics_6w", x$calendar_month, x$value, x$numerator,
    x$denominator, x$complete, source, "official_dm01_england_time_series"
  )
}

core_read_diagnostics_provider <- function(row) {
  source <- core_source_fields(row)
  sheets <- readxl::excel_sheets(row$local_path)
  sheet <- sheets[tolower(sheets) == "provider"][1L]
  if (is.na(sheet)) stop("Diagnostics provider workbook lacks a Provider sheet.")
  m <- read_excel_matrix(row$local_path, sheet)
  header_row <- locate_header_row(
    m,
    c("(?i)^Provider Code$", "(?i)^Provider Name$",
      "(?i)^Total Waiting List$", "(?i)^Number waiting 6\\+ Weeks$"),
    basename(row$local_path)
  )
  # Use the matrix in which the header was located. Reopening with skip based
  # on header_row is unsafe because readxl trims a blank leading workbook row,
  # making that matrix-relative row number one less than the Excel row number.
  cleaned_header <- clean_names_transparent(m[header_row, ])
  required <- c(
    "provider_code", "provider_name", "total_waiting_list",
    "number_waiting_6_weeks"
  )
  column_index <- vapply(required, function(field) {
    hit <- which(cleaned_header == field)
    if (length(hit) != 1L) {
      stop("Diagnostics provider sheet expected one column for ", field, ".")
    }
    hit
  }, integer(1))
  data_rows <- seq.int(header_row + 1L, nrow(m))
  x <- data.table::data.table(
    provider_code = m[data_rows, column_index[["provider_code"]]],
    provider_name = m[data_rows, column_index[["provider_name"]]],
    total_waiting_list = m[data_rows, column_index[["total_waiting_list"]]],
    number_waiting_6_weeks = m[
      data_rows, column_index[["number_waiting_6_weeks"]]
    ]
  )
  x <- x[
    !is.na(provider_code) & nzchar(trimws(provider_code)) &
      grepl("^[A-Z][A-Z0-9]{2}$", trimws(provider_code)) &
      grepl("(?i)NHS.*(TRUST|FOUNDATION)", provider_name, perl = TRUE)
  ]
  x[, `:=`(
    numerator = numeric_cell(number_waiting_6_weeks),
    denominator = numeric_cell(total_waiting_list)
  )]
  x[, `:=`(
    value = numerator / denominator,
    complete = is.finite(numerator) & is.finite(denominator) & denominator > 0 &
      numerator >= 0 & numerator <= denominator
  )]
  core_make_provider_rows(
    "diagnostics_6w", row$activity_month, trimws(x$provider_code),
    trimws(x$provider_name), x$value, x$numerator, x$denominator,
    x$complete, source, "dm01_provider_headline_total"
  )
}

core_read_cancer_national_timeseries <- function(row) {
  source <- core_source_fields(row)
  sheets <- readxl::excel_sheets(row$local_path)
  sheet <- sheets[tolower(sheets) == "monthly performance"][1L]
  if (is.na(sheet)) {
    stop("Cancer national time-series workbook lacks a Monthly Performance sheet.")
  }
  m <- read_excel_matrix(row$local_path, sheet)
  anchor_mask <- matrix(
    grepl("(?i)two month", m, perl = TRUE) & grepl("62", m, fixed = TRUE),
    nrow = nrow(m), ncol = ncol(m)
  )
  anchors <- which(anchor_mask, arr.ind = TRUE)
  if (nrow(anchors) != 1L) {
    stop(
      "Cancer national time series expected one combined 62-day column group; found ",
      nrow(anchors), "."
    )
  }
  anchor_row <- anchors[1L, "row"]
  total_col <- anchors[1L, "col"]
  columns <- seq.int(total_col - 1L, total_col + 3L)
  if (min(columns) < 1L || max(columns) > ncol(m)) {
    stop("Cancer national 62-day column group is incomplete.")
  }
  expected_header <- c(
    "monthly", "total", "within_standard", "outside_standard", "performance_pct"
  )
  candidate_rows <- seq.int(anchor_row, min(nrow(m), anchor_row + 5L))
  header_rows <- candidate_rows[vapply(candidate_rows, function(i) {
    identical(clean_names_transparent(m[i, columns]), expected_header)
  }, logical(1))]
  if (length(header_rows) != 1L) {
    stop(
      "Cancer national time series expected one labelled combined 62-day header row; found ",
      length(header_rows), "."
    )
  }
  data_rows <- seq.int(header_rows[1L] + 1L, nrow(m))
  x <- data.table::data.table(
    calendar_month = parse_month_cell(m[data_rows, columns[1L]]),
    denominator = numeric_cell(m[data_rows, columns[2L]]),
    numerator = numeric_cell(m[data_rows, columns[3L]]),
    outside = numeric_cell(m[data_rows, columns[4L]]),
    published_value = numeric_cell(m[data_rows, columns[5L]])
  )
  x[, value := numerator / denominator]
  x[, complete :=
    !is.na(calendar_month) & is.finite(numerator) & is.finite(outside) &
      is.finite(denominator) & denominator > 0 & numerator >= 0 & outside >= 0 &
      abs(numerator + outside - denominator) < 1e-8 &
      is.finite(published_value) & abs(value - published_value) < 1e-8
  ]
  x <- x[complete == TRUE]
  if (!nrow(x)) stop("Cancer national time series contains no complete 62-day months.")
  if (anyDuplicated(x$calendar_month)) {
    stop("Cancer national time series contains duplicate 62-day months.")
  }
  expected_months <- data.table::as.IDate(seq(
    as.Date(min(x$calendar_month)), as.Date(max(x$calendar_month)), by = "month"
  ))
  if (!identical(as.character(x$calendar_month), as.character(expected_months))) {
    stop("Cancer national 62-day time series is not consecutive.")
  }
  core_make_national_rows(
    "cancer_62d", x$calendar_month, x$value, x$numerator, x$denominator,
    x$complete, source, "cwt_crs_national_combined_62d_monthly_timeseries"
  )
}

core_read_cancer_combined_month <- function(row) {
  source <- core_source_fields(row)
  x <- data.table::fread(
    row$local_path,
    na.strings = c("", "NA", "N/A", "-", "*"),
    encoding = "UTF-8",
    showProgress = FALSE
  )
  data.table::setnames(x, clean_names_transparent(names(x)))
  required <- c(
    "basis", "org_code", "org_name", "standard_or_item", "cancer_type",
    "referral_route_or_stage", "treatment_modality", "total", "within", "after"
  )
  assert_columns(x, required, "cancer combined CSV")
  normalise <- function(value) toupper(trimws(as.character(value)))
  x <- x[
    normalise(basis) == "PROVIDER" &
      normalise(standard_or_item) == "62D" &
      normalise(cancer_type) == "ALL CANCERS" &
      normalise(referral_route_or_stage) == "ALL ROUTES" &
      normalise(treatment_modality) == "ALL MODALITIES"
  ]
  x[, `:=`(
    numerator = suppressWarnings(as.numeric(within)),
    denominator = suppressWarnings(as.numeric(total)),
    after_value = suppressWarnings(as.numeric(after))
  )]
  x[, `:=`(
    value = numerator / denominator,
    complete = is.finite(numerator) & is.finite(denominator) & denominator > 0 &
      numerator >= 0 & numerator <= denominator &
      is.finite(after_value) & abs(numerator + after_value - denominator) < 1e-8
  )]
  national <- x[normalise(org_code) == "TOTAL"]
  if (nrow(national) != 1L) {
    stop("Cancer CSV expected one Provider / England / Total 62D headline row.")
  }
  providers <- x[
    normalise(org_code) != "TOTAL" &
      grepl("^[A-Z][A-Z0-9]{2}$", trimws(org_code)) &
      grepl("(?i)NHS.*(TRUST|FOUNDATION)", org_name, perl = TRUE)
  ]
  if (anyDuplicated(providers$org_code)) {
    stop("Cancer headline filter produced duplicate provider codes.")
  }
  list(
    national = core_make_national_rows(
      "cancer_62d", row$activity_month, national$value,
      national$numerator, national$denominator, national$complete,
      source, "cwt_provider_basis_62d_all_cancers_all_routes_all_modalities"
    ),
    provider = core_make_provider_rows(
      "cancer_62d", row$activity_month, trimws(providers$org_code),
      trimws(providers$org_name), providers$value, providers$numerator,
      providers$denominator, providers$complete, source,
      "cwt_provider_basis_62d_all_cancers_all_routes_all_modalities"
    )
  )
}

core_read_cancer_standard_month <- function(row) {
  source <- core_source_fields(row)
  raw <- data.table::fread(
    row$local_path,
    header = FALSE,
    fill = TRUE,
    colClasses = "character",
    encoding = "UTF-8",
    showProgress = FALSE,
    blank.lines.skip = FALSE
  )
  required_header <- c(
    "ods_code_1", "accountable_provider", "referral_route", "cancer_type",
    "total", "within_62_days", "after_62_days"
  )
  header_rows <- which(vapply(seq_len(nrow(raw)), function(i) {
    cleaned <- clean_names_transparent(as.character(unlist(raw[i], use.names = FALSE)))
    all(required_header %in% cleaned)
  }, logical(1)))
  if (!length(header_rows)) {
    stop(
      "Cancer 62-day provider CSV did not contain the labelled table header."
    )
  }
  header_row <- header_rows[1L]
  context_rows <- if (header_row > 1L) {
    seq.int(max(1L, header_row - 5L), header_row - 1L)
  } else {
    integer()
  }
  part_a_labelled <- length(context_rows) > 0L && any(grepl(
    "(?i)PART A.*ALL ROUTES",
    as.character(unlist(raw[context_rows], use.names = FALSE)),
    perl = TRUE
  ), na.rm = TRUE)
  if (!part_a_labelled) {
    stop("The first cancer 62-day table is not labelled PART A: ALL ROUTES.")
  }
  cleaned_header <- clean_names_transparent(
    as.character(unlist(raw[header_row], use.names = FALSE))
  )
  column_index <- function(field) {
    hit <- which(cleaned_header == field)
    if (length(hit) != 1L) {
      stop("Cancer 62-day provider CSV expected one column for ", field, ".")
    }
    hit
  }
  # The official file repeats the same header for route-specific Parts B-E.
  # Read only Part A so repeated headings and later subtables cannot be treated
  # as observations even if their wording changes in a future release.
  data_end <- if (length(header_rows) > 1L) {
    header_rows[2L] - 1L
  } else {
    nrow(raw)
  }
  data_rows <- seq.int(header_row + 1L, data_end)
  x <- data.table::data.table(
    org_code = raw[[column_index("ods_code_1")]][data_rows],
    org_name = raw[[column_index("accountable_provider")]][data_rows],
    referral_route = raw[[column_index("referral_route")]][data_rows],
    cancer_type = raw[[column_index("cancer_type")]][data_rows],
    total = raw[[column_index("total")]][data_rows],
    within = raw[[column_index("within_62_days")]][data_rows],
    after = raw[[column_index("after_62_days")]][data_rows]
  )
  normalise <- function(value) toupper(trimws(as.character(value)))
  x <- x[
    normalise(referral_route) == "ALL ROUTES" &
      normalise(cancer_type) == "ALL CANCERS"
  ]
  x[, `:=`(
    numerator = numeric_cell(within),
    denominator = numeric_cell(total),
    after_value = numeric_cell(after)
  )]
  x[, `:=`(
    value = numerator / denominator,
    complete = is.finite(numerator) & is.finite(denominator) & denominator > 0 &
      numerator >= 0 & numerator <= denominator & is.finite(after_value) &
      abs(numerator + after_value - denominator) < 1e-8
  )]
  national <- x[normalise(org_name) == "ALL ENGLISH PROVIDERS"]
  if (nrow(national) != 1L) {
    stop("Cancer 62-day provider CSV expected one ALL ENGLISH PROVIDERS row.")
  }
  providers <- x[
    normalise(org_name) != "ALL ENGLISH PROVIDERS" &
      grepl("^[A-Z][A-Z0-9]{2}$", trimws(org_code)) &
      grepl("(?i)NHS.*(TRUST|FOUNDATION)", org_name, perl = TRUE)
  ]
  if (anyDuplicated(providers$org_code)) {
    stop("Cancer 62-day headline filter produced duplicate provider codes.")
  }
  method <- "cwt_62d_all_cancers_all_routes_provider_standard_csv"
  list(
    national = core_make_national_rows(
      "cancer_62d", row$activity_month, national$value,
      national$numerator, national$denominator, national$complete,
      source, method
    ),
    provider = core_make_provider_rows(
      "cancer_62d", row$activity_month, trimws(providers$org_code),
      trimws(providers$org_name), providers$value, providers$numerator,
      providers$denominator, providers$complete, source, method
    )
  )
}

core_read_cancer_month <- function(row) {
  header <- names(data.table::fread(
    row$local_path, nrows = 0L, encoding = "UTF-8", showProgress = FALSE
  ))
  cleaned_header <- clean_names_transparent(header)
  combined_signature <- c("basis", "org_code", "standard_or_item")
  if (all(combined_signature %in% cleaned_header)) {
    core_read_cancer_combined_month(row)
  } else {
    core_read_cancer_standard_month(row)
  }
}

core_validate_cancer_national_overlap <- function(timeseries, monthly) {
  if (!nrow(monthly)) stop("No monthly cancer England totals were imported for validation.")
  missing_months <- setdiff(monthly$calendar_month, timeseries$calendar_month)
  if (length(missing_months)) {
    stop(
      "Cancer national time series does not cover monthly provider publications for: ",
      paste(as.character(missing_months), collapse = ", "), "."
    )
  }
  comparison <- merge(
    timeseries[, .(
      calendar_month, timeseries_value = value,
      timeseries_numerator = numerator, timeseries_denominator = denominator
    )],
    monthly[, .(
      calendar_month, monthly_value = value,
      monthly_numerator = numerator, monthly_denominator = denominator
    )],
    by = "calendar_month", all = FALSE
  )
  comparison[, `:=`(
    value_difference = timeseries_value - monthly_value,
    difference_percentage_points = 100 * (timeseries_value - monthly_value),
    numerator_difference = timeseries_numerator - monthly_numerator,
    denominator_difference = timeseries_denominator - monthly_denominator
  )]
  # The national workbook is explicitly issued "with revisions", so its
  # historical counts need not be byte-for-byte identical to an archived
  # monthly file.  A material rate difference is still a schema warning: it
  # would suggest that unlike standards or populations had been joined.
  tolerance_native <- 0.005
  comparison[, within_half_percentage_point :=
    is.finite(value_difference) & abs(value_difference) <= tolerance_native
  ]
  mismatch <- comparison[within_half_percentage_point == FALSE]
  if (nrow(mismatch)) {
    stop(
      "Cancer national time series differs from the monthly England total by ",
      "more than 0.5 percentage points for: ",
      paste(as.character(mismatch$calendar_month), collapse = ", "), "."
    )
  }
  data.table::setorder(comparison, calendar_month)
  comparison[]
}

core_read_ambulance_timeseries <- function(row) {
  source <- core_source_fields(row)
  x <- data.table::fread(
    row$local_path,
    na.strings = c("", ".", "NA", "N/A", "-"),
    encoding = "UTF-8"
  )
  data.table::setnames(x, clean_names_transparent(names(x)))
  required <- c("year", "month", "org_code", "org_name", "a31")
  assert_columns(x, required, "AmbSYS CSV")
  x[, calendar_month := data.table::as.IDate(sprintf(
    "%04d-%02d-01", as.integer(year), as.integer(month)
  ))]
  x[, response_minutes := suppressWarnings(as.numeric(a31)) / 60]
  x[, complete := is.finite(response_minutes) & response_minutes > 0]
  national <- x[toupper(trimws(org_code)) == "ENG" & complete == TRUE]
  if (anyDuplicated(national$calendar_month)) {
    stop("AmbSYS contains duplicate England months for A31.")
  }
  providers <- x[
    toupper(trimws(org_code)) != "ENG" & complete == TRUE &
      grepl("(?i)AMBULANCE SERVICE|ISLE OF WIGHT", org_name, perl = TRUE)
  ]
  list(
    national = core_make_national_rows(
      "ambulance_cat2", national$calendar_month, national$response_minutes,
      NA_real_, NA_real_, national$complete, source,
      "ambsys_a31_category_2_mean_seconds_converted_to_minutes"
    ),
    provider = core_make_provider_rows(
      "ambulance_cat2", providers$calendar_month, trimws(providers$org_code),
      trimws(providers$org_name), providers$response_minutes,
      NA_real_, NA_real_, providers$complete, source,
      "ambsys_a31_category_2_mean_seconds_converted_to_minutes"
    )
  )
}

core_read_ucr_wide_table <- function(row, sheet, value_type) {
  sheets <- readxl::excel_sheets(row$local_path)
  if (!sheet %in% sheets) stop("UCR workbook lacks required sheet '", sheet, "'.")
  m <- read_excel_matrix(row$local_path, sheet)
  table_title <- paste(m[seq_len(min(3L, nrow(m))), ], collapse = " ")
  if (value_type == "proportion" &&
      !grepl("(?i)%.*2[- ]hour.*UCR|2[- ]hour.*UCR.*%", table_title, perl = TRUE)) {
    stop("UCR ", sheet, " is not the labelled two-hour percentage table.")
  }
  if (value_type == "count" &&
      !grepl("(?i)count.*2[- ]hour.*UCR.*referral", table_title, perl = TRUE)) {
    stop("UCR ", sheet, " is not the labelled two-hour referral count table.")
  }
  header_row <- locate_header_row(
    m,
    c("(?i)^ODS Code$", "(?i)^Organisation Name$", "(?i)^Organisation Type$"),
    basename(row$local_path)
  )
  header <- clean_names_transparent(m[header_row, ])
  code_col <- locate_one_column(header, "^ods_code$", "UCR ODS code")
  name_col <- locate_one_column(
    header, "^organisation_name$", "UCR organisation name"
  )
  type_col <- locate_one_column(
    header, "^organisation_type$", "UCR organisation type"
  )
  months <- parse_month_cell(m[header_row, ])
  month_cols <- which(!is.na(months))
  if (!length(month_cols)) {
    stop("UCR ", sheet, " contains no dated month columns.")
  }
  data_rows <- seq.int(header_row + 1L, nrow(m))
  organisation <- data.table::data.table(
    code = trimws(m[data_rows, code_col]),
    name = trimws(m[data_rows, name_col]),
    type = trimws(m[data_rows, type_col])
  )
  pieces <- lapply(month_cols, function(column) {
    value <- if (value_type == "proportion") {
      core_normalise_percentage(m[data_rows, column])
    } else {
      numeric_cell(m[data_rows, column])
    }
    data.table::data.table(
      month = months[column],
      code = organisation$code,
      name = organisation$name,
      type = organisation$type,
      value = value,
      complete = if (value_type == "proportion") {
        is.finite(value) & value >= 0 & value <= 1
      } else {
        is.finite(value) & value >= 0
      }
    )
  })
  x <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  x[complete == TRUE & nzchar(name)]
}

core_read_ucr_workbook <- function(row) {
  source <- core_source_fields(row)
  sheets <- readxl::excel_sheets(row$local_path)
  rate_sheet <- sheets[grepl("(?i)^Table[ _-]*1$", sheets, perl = TRUE)][1L]
  if (is.na(rate_sheet)) {
    stop("UCR workbook lacks Table 1 containing the two-hour percentage.")
  }
  # The activity table was Table 2 through 2025/26. From 2026/27 Table 2 is
  # a standardised population rate, while Table 3b is the comparable count of
  # two-hour UCR referrals. These counts are a volume screen only: their
  # received-date cohort is not asserted to be the exact Table 1 denominator.
  activity_sheet <- if ("Table 3b" %in% sheets) {
    "Table 3b"
  } else {
    candidate <- sheets[grepl("(?i)^Table[ _-]*2$", sheets, perl = TRUE)][1L]
    if (is.na(candidate)) {
      stop("UCR workbook lacks a provider two-hour referral activity table.")
    }
    candidate
  }
  rate <- core_read_ucr_wide_table(row, rate_sheet, "proportion")
  activity <- core_read_ucr_wide_table(row, activity_sheet, "count")
  activity_method <- if (identical(activity_sheet, "Table 3b")) {
    "official_ucr_table_3b_two_hour_referrals_received_activity_proxy"
  } else {
    "official_ucr_table_2_two_hour_referrals_received_activity_proxy"
  }
  activity <- activity[, .(
    activity_volume_proxy = value,
    activity_volume_proxy_method = activity_method
  ), by = .(month, code)]
  x <- merge(rate, activity, by = c("month", "code"), all.x = TRUE)
  national <- x[
    grepl("(?i)^national$", type, perl = TRUE) |
      grepl("(?i)^national$|^england$", code, perl = TRUE) |
      grepl("(?i)^national$|^england$", name, perl = TRUE)
  ]
  if (!nrow(national)) {
    stop("UCR Table 1 contains no national percentage rows.")
  }
  national <- national[, .SD[.N], by = month]
  provider <- x[
    grepl("(?i)^provider$", type, perl = TRUE) &
      nzchar(code) & !grepl("(?i)^unknown$|^national$|^england$", code, perl = TRUE)
  ]
  if (nrow(provider)) {
    provider <- provider[, .SD[.N], by = .(month, code)]
  }
  list(
    national = core_make_national_rows(
      "ucr_2h", national$month, national$value, NA_real_, NA_real_,
      national$complete, source, "official_ucr_table_1_two_hour_percentage",
      national$activity_volume_proxy,
      national$activity_volume_proxy_method
    ),
    provider = core_make_provider_rows(
      "ucr_2h", provider$month, provider$code, provider$name, provider$value,
      NA_real_, NA_real_, provider$complete, source,
      "official_ucr_table_1_two_hour_percentage_with_activity_proxy",
      provider$activity_volume_proxy,
      provider$activity_volume_proxy_method
    )
  )
}

core_community_normalise_label <- function(x) {
  z <- enc2utf8(as.character(x))
  z <- tolower(trimws(z))
  z <- gsub("%", " pct ", z, fixed = TRUE)
  z <- gsub("[^a-z0-9]+", "_", z)
  gsub("^_+|_+$", "", z)
}

core_community_wait_band_definition <- function(label) {
  z <- core_community_normalise_label(label)[1L]
  if (is.na(z) || !nzchar(z)) {
    return(list(id = NA_character_, label = NA_character_))
  }
  definitions <- list(
    list("total", "Total waiting list", "total.*waiting.*list|waiting.*list.*total"),
    list("wait_0_1", "Waiting 0–1 weeks", "(^|_)0_1_week|0_7_day"),
    list("wait_1_2", "Waiting 1–2 weeks", "(^|_)1_2_week|8_14_day"),
    list("wait_2_4", "Waiting 2–4 weeks", "(^|_)2_4_week|15_28_day"),
    list("wait_4_12", "Waiting 4–12 weeks", "(^|_)4_12_week|29_84_day"),
    list("wait_12_18", "Waiting 12–18 weeks", "(^|_)12_18_week|85_126_day"),
    list("wait_18_52", "Waiting 18–52 weeks", "(^|_)18_52_week|127_364_day"),
    list(
      "wait_over_104", "Waiting over 104 weeks",
      "over_104_week|over_2_year|(^|_)waiting_104_weeks?$"
    ),
    list("wait_52_104", "Waiting 52–104 weeks", "(^|_)52_104_week|365_728_day"),
    list(
      "wait_over_52", "Waiting over 52 weeks",
      "over_52_week|over_365_day|(^|_)waiting_52_weeks?$"
    )
  )
  hits <- definitions[vapply(definitions, function(x) {
    grepl(x[[3L]], z, perl = TRUE)
  }, logical(1))]
  if (length(hits) != 1L) {
    return(list(id = NA_character_, label = NA_character_))
  }
  list(id = hits[[1L]][[1L]], label = hits[[1L]][[2L]])
}

core_empty_community_service_band <- function() {
  data.table::data.table(
    calendar_month = data.table::as.IDate(character()),
    geography_type = character(), geography_name = character(),
    source_geography_key = character(), service_group = character(),
    service_id = character(), service_name = character(),
    wait_band = character(), wait_band_label = character(),
    waiting_count = numeric(), cell_status = character(),
    source_sheet = character(), source_method = character(),
    source_file = character(), source_url = character(), source_sha256 = character()
  )
}

core_empty_community_service_summary <- function() {
  data.table::data.table(
    calendar_month = data.table::as.IDate(character()),
    geography_type = character(), geography_name = character(),
    source_geography_key = character(), service_group = character(),
    service_id = character(), service_name = character(),
    total_waiting_list = numeric(), over_18_weeks_count = numeric(),
    within_18_weeks_count = numeric(), within_18_weeks_proportion = numeric(),
    published_band_sum = numeric(), reconciliation_gap_count = numeric(),
    reconciliation_gap_share = numeric(), reconciliation_tolerance_count = numeric(),
    band_schema = character(), band_breakdown_complete = logical(),
    complete_submission = logical(), reconciliation_status = character(),
    identity_status = character(), source_method = character(),
    source_file = character(), source_url = character(), source_sha256 = character()
  )
}

core_community_geography_type <- function(x) {
  z <- core_community_normalise_label(x)
  data.table::fcase(
    z %in% c("region", "commissioning_region", "nhs_region"), "Region",
    z %in% c("icb", "integrated_care_board"), "ICB",
    z %in% c("organisation", "organization", "provider"), "Organisation",
    default = NA_character_
  )
}

core_community_service_sheet <- function(row, sheet) {
  source <- core_source_fields(row)
  m <- read_excel_matrix(row$local_path, sheet)
  title_cells <- as.character(m[seq_len(min(5L, nrow(m))), , drop = FALSE])
  title_candidates <- title_cells[
    grepl("(?i)waiting.*by service|waiting list.*service", title_cells, perl = TRUE)
  ]
  if (!length(title_candidates)) {
    stop("Could not identify the waiting-band title on community sheet '", sheet, "'.")
  }
  band <- core_community_wait_band_definition(title_candidates[1L])
  if (is.na(band$id)) {
    stop(
      "Could not classify the waiting band on community sheet '", sheet,
      "' from title: ", title_candidates[1L], "."
    )
  }
  service_header_pattern <- "^\\((A|CYP)\\)[[:space:]]*"
  header_rows <- which(vapply(seq_len(nrow(m)), function(i) {
    sum(grepl(service_header_pattern, trimws(m[i, ]), perl = TRUE)) >= 10L
  }, logical(1)))
  if (length(header_rows) != 1L) {
    stop(
      "Expected one service header row on community sheet '", sheet,
      "'; found ", length(header_rows), "."
    )
  }
  header_row <- header_rows[1L]
  header <- trimws(m[header_row, ])
  service_columns <- which(grepl(service_header_pattern, header, perl = TRUE))
  england_cells <- which(
    m == "England" & row(m) > header_row & col(m) < min(service_columns),
    arr.ind = TRUE
  )
  entity_name_columns <- unique(england_cells[, "col"])
  if (length(entity_name_columns) != 1L) {
    stop(
      "Expected one geography-name column on community sheet '", sheet,
      "'; found ", length(entity_name_columns), "."
    )
  }
  entity_name_column <- entity_name_columns[1L]
  marker_candidates <- seq_len(entity_name_column - 1L)
  marker_scores <- vapply(marker_candidates, function(column) {
    sum(!is.na(core_community_geography_type(trimws(m[, column]))))
  }, integer(1))
  if (!length(marker_scores) || max(marker_scores) < 1L) {
    stop("Could not identify geography levels on community sheet '", sheet, "'.")
  }
  geography_column <- marker_candidates[which.max(marker_scores)]
  data_rows <- seq.int(header_row + 1L, nrow(m))
  geography_name <- trimws(m[data_rows, entity_name_column])
  geography_marker <- trimws(m[data_rows, geography_column])
  geography_type <- character(length(data_rows))
  current_type <- NA_character_
  for (i in seq_along(data_rows)) {
    marker_type <- core_community_geography_type(geography_marker[i])
    if (!is.na(marker_type)) {
      current_type <- marker_type
    }
    geography_type[i] <- if (identical(geography_name[i], "England")) {
      "England"
    } else {
      current_type
    }
  }
  valid_row <- nzchar(geography_name) &
    geography_type %in% c("England", "Region", "ICB", "Organisation")
  data_rows <- data_rows[valid_row]
  geography_name <- geography_name[valid_row]
  geography_type <- geography_type[valid_row]
  pieces <- lapply(service_columns, function(column) {
    service_header <- header[column]
    service_group <- if (grepl("^\\(A\\)", service_header)) {
      "Adult"
    } else {
      "Children and young people"
    }
    service_name <- trimws(sub(service_header_pattern, "", service_header, perl = TRUE))
    raw_value <- trimws(m[data_rows, column])
    waiting_count <- numeric_cell(raw_value)
    cell_status <- data.table::fifelse(
      is.finite(waiting_count), "reported",
      data.table::fifelse(
        raw_value %in% c("*", "**", "***"), "suppressed",
        data.table::fifelse(!nzchar(raw_value), "not_submitted", "non_numeric")
      )
    )
    data.table::data.table(
      calendar_month = data.table::as.IDate(row$activity_month),
      geography_type = geography_type,
      geography_name = geography_name,
      source_geography_key = paste0(
        tolower(geography_type), "::", core_community_normalise_label(geography_name)
      ),
      service_group = service_group,
      service_id = paste0(
        if (service_group == "Adult") "adult__" else "cyp__",
        core_community_normalise_label(service_name)
      ),
      service_name = service_name,
      wait_band = band$id,
      wait_band_label = band$label,
      waiting_count = waiting_count,
      cell_status = cell_status,
      source_sheet = sheet,
      source_method = "community_tables_4_to_4h_service_wait_bands",
      source_file = source$source_file,
      source_url = source$source_url,
      source_sha256 = source$source_sha256
    )
  })
  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

core_community_service_summary <- function(service_band) {
  if (!nrow(service_band)) return(core_empty_community_service_summary())
  key <- c(
    "calendar_month", "geography_type", "geography_name", "source_geography_key",
    "service_group", "service_id", "service_name", "source_file", "source_url",
    "source_sha256"
  )
  if (anyDuplicated(service_band[, c(key, "wait_band"), with = FALSE])) {
    stop("Community service-band panel contains duplicate month/geography/service/band keys.")
  }
  formula <- stats::as.formula(paste(paste(key, collapse = " + "), "~ wait_band"))
  wide <- data.table::dcast(
    service_band, formula, value.var = "waiting_count", fill = NA_real_
  )
  expected_bands <- c(
    "total", "wait_0_1", "wait_1_2", "wait_2_4", "wait_4_12",
    "wait_12_18", "wait_18_52", "wait_over_52", "wait_52_104",
    "wait_over_104"
  )
  for (band in setdiff(expected_bands, names(wide))) {
    wide[, (band) := NA_real_]
  }
  wide[, band_schema := data.table::fifelse(
    is.finite(wait_over_52), "legacy_over_52",
    data.table::fifelse(
      is.finite(wait_52_104) & is.finite(wait_over_104),
      "split_52_to_104_and_over_104", "incomplete_over_52"
    )
  )]
  wide[, over_52_count___ := data.table::fifelse(
    band_schema == "legacy_over_52", wait_over_52,
    wait_52_104 + wait_over_104
  )]
  wide[, `:=`(
    total_waiting_list = total,
    over_18_weeks_count = wait_18_52 + over_52_count___
  )]
  wide[, `:=`(
    within_18_weeks_count = total_waiting_list - over_18_weeks_count,
    within_18_weeks_proportion =
      (total_waiting_list - over_18_weeks_count) / total_waiting_list,
    band_breakdown_complete = is.finite(wait_0_1) & is.finite(wait_1_2) &
      is.finite(wait_2_4) & is.finite(wait_4_12) & is.finite(wait_12_18) &
      is.finite(wait_18_52) & is.finite(over_52_count___),
    complete_submission = is.finite(total_waiting_list) & total_waiting_list > 0 &
      is.finite(over_18_weeks_count) & over_18_weeks_count >= 0 &
      over_18_weeks_count <= total_waiting_list
  )]
  wide[, published_band_sum := data.table::fifelse(
    band_breakdown_complete,
    wait_0_1 + wait_1_2 + wait_2_4 + wait_4_12 + wait_12_18 +
      wait_18_52 + over_52_count___,
    NA_real_
  )]
  wide[, `:=`(
    reconciliation_gap_count = total_waiting_list - published_band_sum,
    reconciliation_gap_share =
      (total_waiting_list - published_band_sum) / total_waiting_list,
    reconciliation_tolerance_count = pmax(5, 0.005 * total_waiting_list)
  )]
  wide[, reconciliation_status := data.table::fcase(
    !is.finite(total_waiting_list), "total_not_reported",
    !complete_submission, "over_18_bands_incomplete_or_invalid",
    !band_breakdown_complete, "full_band_breakdown_incomplete",
    abs(reconciliation_gap_count) <= reconciliation_tolerance_count,
      "within_0_5_percent_or_5_people",
    default = "material_published_band_gap"
  )]
  wide[, `:=`(
    identity_status = data.table::fifelse(
      geography_type == "Organisation", "source_name_only_unharmonised",
      "published_aggregate_name"
    ),
    source_method = "community_service_total_less_published_over_18_bands"
  )]
  columns <- names(core_empty_community_service_summary())
  wide[, ..columns]
}

core_read_community_18w_workbook <- function(row) {
  source <- core_source_fields(row)
  sheets <- readxl::excel_sheets(row$local_path)
  sheet <- sheets[grepl("(?i)^Table[ _-]*3$", sheets, perl = TRUE)][1L]
  if (is.na(sheet)) {
    stop("Community waits workbook lacks the national overview in Table 3.")
  }
  m <- read_excel_matrix(row$local_path, sheet)
  clean_row <- function(i) clean_names_transparent(m[i, ])
  header_rows <- which(vapply(seq_len(nrow(m)), function(i) {
    band_ids <- vapply(clean_row(i), function(cell) {
      core_community_wait_band_definition(cell)$id
    }, character(1))
    sum(band_ids == "total", na.rm = TRUE) == 1L &&
      sum(band_ids == "wait_18_52", na.rm = TRUE) == 1L &&
      (
        sum(band_ids == "wait_over_52", na.rm = TRUE) == 1L ||
          (
            sum(band_ids == "wait_52_104", na.rm = TRUE) == 1L &&
              sum(band_ids == "wait_over_104", na.rm = TRUE) == 1L
          )
      )
  }, logical(1)))
  if (length(header_rows) != 1L) {
    populated <- which(vapply(seq_len(nrow(m)), function(i) {
      any(nzchar(trimws(as.character(m[i, ]))))
    }, logical(1)))
    preview <- paste(vapply(utils::head(populated, 12L), function(i) {
      cells <- clean_row(i)
      cells <- cells[nzchar(cells)]
      paste0("row ", i, ": ", paste(utils::head(cells, 10L), collapse = " | "))
    }, character(1)), collapse = "; ")
    stop(
      "Expected one semantic waiting-list header row in ", basename(row$local_path),
      "; found ", length(header_rows), ". Schema preview: ", preview
    )
  }
  header_row <- header_rows[[1L]]
  header <- clean_row(header_row)
  header_band <- vapply(header, function(cell) {
    core_community_wait_band_definition(cell)$id
  }, character(1))
  one_band_column <- function(id, field) {
    hits <- which(header_band == id)
    if (length(hits) != 1L) {
      stop("Expected one ", field, " column in ", basename(row$local_path), ".")
    }
    hits[1L]
  }
  denominator_col <- one_band_column("total", "community total waiting-list")
  over_18_52_col <- one_band_column("wait_18_52", "community 18-to-52-week")
  legacy_over_52 <- which(header_band == "wait_over_52")
  split_52_104 <- which(header_band == "wait_52_104")
  split_over_104 <- which(header_band == "wait_over_104")
  england_rows <- which(
    seq_len(nrow(m)) > header_row &
      apply(m, 1L, function(cells) any(
        grepl("(?i)^England$", trimws(cells), perl = TRUE)
      ))
  )
  if (length(england_rows) != 1L) {
    stop(
      "Community national overview expected one England row; found ",
      length(england_rows), "."
    )
  }
  england_row <- england_rows[1L]
  denominator <- numeric_cell(m[england_row, denominator_col])
  over_18_52 <- numeric_cell(m[england_row, over_18_52_col])
  if (length(legacy_over_52) == 1L) {
    over_52 <- numeric_cell(m[england_row, legacy_over_52])
    source_schema <- "legacy_over_52"
  } else if (length(split_52_104) == 1L && length(split_over_104) == 1L) {
    over_52 <- numeric_cell(m[england_row, split_52_104]) +
      numeric_cell(m[england_row, split_over_104])
    source_schema <- "split_52_to_104_and_over_104"
  } else {
    stop("Community Table 3 lacks a recognised set of over-52-week bands.")
  }
  over_18 <- over_18_52 + over_52
  numerator <- denominator - over_18
  value <- numerator / denominator
  complete <- is.finite(numerator) & is.finite(denominator) &
    denominator > 0 & numerator >= 0 & numerator <= denominator
  if (!complete) {
    stop("Community Table 3 contains an invalid England 18-week calculation.")
  }
  service_sheets <- sheets[
    grepl("(?i)^Table[ _-]*4[a-h]?$", sheets, perl = TRUE)
  ]
  if (!length(service_sheets)) {
    stop("Community waits workbook contains no service-level Tables 4 to 4h.")
  }
  service_band <- data.table::rbindlist(lapply(
    service_sheets, function(service_sheet) {
      core_community_service_sheet(row, service_sheet)
    }
  ), use.names = TRUE, fill = TRUE)
  imported_bands <- unique(service_band$wait_band)
  required_bands <- c("total", "wait_18_52")
  over_52_ok <- "wait_over_52" %in% imported_bands ||
    all(c("wait_52_104", "wait_over_104") %in% imported_bands)
  if (!all(required_bands %in% imported_bands) || !over_52_ok) {
    stop(
      "Community service tables lack total, 18-to-52 and recognised over-52 bands. ",
      "Imported: ", paste(sort(imported_bands), collapse = ", "), "."
    )
  }
  service_summary <- core_community_service_summary(service_band)
  list(
    national = core_make_national_rows(
      "community_18w", row$activity_month, value, numerator, denominator,
      complete, source,
      paste0("community_table_3_total_less_published_over_18_bands_", source_schema)
    ),
    # Organisation names are retained in the service panel for mapping and
    # descriptive QA, but are not promoted to stable provider identifiers.
    provider = data.table::data.table(),
    community_service_band = service_band,
    community_service_summary = service_summary
  )
}

core_read_talking_therapies_chart <- function(row) {
  source <- core_source_fields(row)
  path <- as.character(row$local_path[1L])
  source_format <- tolower(as.character(row$source_format[1L]))
  temporary_path <- NULL
  if (!tolower(tools::file_ext(path)) %in% c("xls", "xlsx") &&
      source_format %in% c("xls", "xlsx")) {
    temporary_path <- tempfile(fileext = paste0(".", source_format))
    if (!file.copy(path, temporary_path, overwrite = TRUE)) {
      stop("Could not create a readable copy of the Talking Therapies chart file.")
    }
    on.exit(unlink(temporary_path), add = TRUE)
    path <- temporary_path
  }
  sheets <- readxl::excel_sheets(path)
  matches <- lapply(sheets, function(sheet) {
    m <- read_excel_matrix(path, sheet)
    header_rows <- which(vapply(seq_len(nrow(m)), function(i) {
      clean <- clean_names_transparent(m[i, ])
      any(clean == "month") &&
        any(grepl("6.*week|week.*6", clean, perl = TRUE) &
              !grepl("18", clean, perl = TRUE))
    }, logical(1)))
    if (length(header_rows) != 1L) return(NULL)
    list(sheet = sheet, matrix = m, header_row = header_rows[1L])
  })
  matches <- matches[!vapply(matches, is.null, logical(1))]
  if (length(matches) != 1L) {
    stop(
      "Talking Therapies chart expected one month-by-six-week table; found ",
      length(matches), "."
    )
  }
  found <- matches[[1L]]
  m <- found$matrix
  header <- clean_names_transparent(m[found$header_row, ])
  month_col <- locate_one_column(header, "^month$", "Talking Therapies month")
  value_hits <- which(
    grepl("6.*week|week.*6", header, perl = TRUE) &
      !grepl("18", header, perl = TRUE)
  )
  if (length(value_hits) != 1L) {
    stop(
      "Talking Therapies chart expected one six-week value column; found ",
      length(value_hits), "."
    )
  }
  data_rows <- seq.int(found$header_row + 1L, nrow(m))
  month <- parse_month_cell(m[data_rows, month_col])
  value <- core_normalise_percentage(m[data_rows, value_hits[1L]])
  complete <- !is.na(month) & is.finite(value) & value >= 0 & value <= 1
  if (!any(complete)) {
    raw_month <- as.character(m[data_rows, month_col])
    raw_value <- as.character(m[data_rows, value_hits[1L]])
    stop(
      "Talking Therapies chart contains no complete six-week observations. ",
      "Parsed months: ", sum(!is.na(month)), "; numeric values: ",
      sum(is.finite(value)), ". Example month cells: ",
      paste(utils::head(raw_month, 4L), collapse = " | "),
      ". Example value cells: ",
      paste(utils::head(raw_value, 4L), collapse = " | ")
    )
  }
  list(
    national = core_make_national_rows(
      "talking_therapies_6w", month[complete], value[complete],
      NA_real_, NA_real_, TRUE, source,
      "official_talking_therapies_waiting_times_chart_six_weeks"
    ),
    provider = data.table::data.table()
  )
}

core_generic_matrix_table <- function(matrix_value, label) {
  m <- as.matrix(matrix_value)
  if (!nrow(m) || !ncol(m)) stop(label, " is empty.")
  m[is.na(m)] <- ""
  scan_rows <- seq_len(min(60L, nrow(m)))
  header_score <- vapply(scan_rows, function(i) {
    header <- clean_names_transparent(m[i, ])
    sum(grepl(paste0(
      "org|organisation|provider|commissioner|icb|england|month|date|period|",
      "percentage|pct|proportion|rate|value|measure|metric|indicator|referral|",
      "response|waiting|within|total|count|numerator|denominator"
    ), header) & nzchar(header))
  }, integer(1))
  header_row <- scan_rows[which.max(header_score)]
  if (!length(header_row) || max(header_score) < 2L) {
    stop("Could not identify a labelled data header in ", label, ".")
  }
  header <- clean_names_transparent(m[header_row, ])
  keep <- nzchar(header) & !duplicated(header)
  if (!any(keep)) stop("No usable columns were found in ", label, ".")
  rows <- if (header_row < nrow(m)) seq.int(header_row + 1L, nrow(m)) else integer()
  out <- data.table::as.data.table(m[rows, keep, drop = FALSE])
  data.table::setnames(out, header[keep])
  nonempty <- Reduce(`|`, lapply(
    out, function(x) nzchar(trimws(as.character(x)))
  ))
  out <- out[nonempty]
  out[]
}

core_read_generic_tables <- function(path, source_format = NA_character_) {
  declared_format <- tolower(trimws(as.character(source_format[1L])))
  extension <- if (!is.na(declared_format) && nzchar(declared_format)) {
    declared_format
  } else {
    tolower(tools::file_ext(path))
  }
  if (extension == "csv") {
    raw <- data.table::fread(
      path, header = FALSE, fill = TRUE, colClasses = "character",
      encoding = "UTF-8", showProgress = FALSE, blank.lines.skip = FALSE
    )
    return(list(core_generic_matrix_table(raw, basename(path))))
  }
  if (!extension %in% c("xls", "xlsx")) {
    stop("Unsupported generic core source format: ", extension, ".")
  }
  sheets <- readxl::excel_sheets(path)
  tables <- lapply(sheets, function(sheet) {
    tryCatch(
      core_generic_matrix_table(
        read_excel_matrix(path, sheet), paste0(basename(path), " / ", sheet)
      ),
      error = function(e) NULL
    )
  })
  tables[!vapply(tables, is.null, logical(1))]
}

core_first_matching_column <- function(names_value, patterns, exclude = character()) {
  for (pattern in patterns) {
    hit <- names_value[grepl(pattern, names_value, perl = TRUE)]
    if (length(exclude)) {
      hit <- hit[!vapply(hit, function(value) any(grepl(
        exclude, value, perl = TRUE
      )), logical(1))]
    }
    if (length(hit)) return(hit[1L])
  }
  NA_character_
}

core_normalise_percentage <- function(x) {
  raw <- trimws(as.character(x))
  labelled_percent <- grepl("%", raw, fixed = TRUE)
  out <- numeric_cell(gsub("%", "", raw, fixed = TRUE))
  out[labelled_percent & is.finite(out)] <-
    out[labelled_percent & is.finite(out)] / 100
  unlabelled_scale <- !labelled_percent & is.finite(out) & abs(out) > 1
  out[unlabelled_scale] <- out[unlabelled_scale] / 100
  out
}

core_extract_generic_percentage <- function(row, metric_id, measure_pattern,
                                            exclude_pattern = "a^") {
  source <- core_source_fields(row)
  tables <- core_read_generic_tables(row$local_path, row$source_format)
  column_measure_pattern <- sub("^[(][?]i[)]", "", measure_pattern)
  imported <- lapply(tables, function(x) {
    names_value <- names(x)
    measure_col <- core_first_matching_column(
      names_value, c("^(measure|metric|indicator|description|standard)$",
                     "measure|metric|indicator")
    )
    if (!is.na(measure_col)) {
      keep <- grepl(measure_pattern, x[[measure_col]], perl = TRUE) &
        !grepl(exclude_pattern, x[[measure_col]], perl = TRUE)
      if (any(keep, na.rm = TRUE)) x <- x[keep]
    }
    if (!nrow(x)) return(NULL)
    value_col <- core_first_matching_column(
      names_value,
      c(
        paste0("(?i)(percentage|pct|proportion|rate).*", column_measure_pattern),
        paste0("(?i)", column_measure_pattern, ".*(percentage|pct|proportion|rate)"),
        paste0("(?i)^", column_measure_pattern, "$"),
        "^(percentage|pct|proportion|rate|value|measure_value)$"
      ),
      exclude = exclude_pattern
    )
    numerator_col <- core_first_matching_column(
      names_value,
      c(paste0("(?i)(number|count|numerator).*", column_measure_pattern),
        "^(numerator|within_standard|within_target)$"),
      exclude = exclude_pattern
    )
    denominator_col <- core_first_matching_column(
      names_value,
      c("^(denominator|total|eligible|total_referrals|total_waits)$",
        "(total|eligible).*(referral|wait|pathway|response)")
    )
    if (is.na(value_col) && (is.na(numerator_col) || is.na(denominator_col))) {
      return(NULL)
    }
    numerator <- if (is.na(numerator_col)) rep(NA_real_, nrow(x)) else {
      numeric_cell(x[[numerator_col]])
    }
    denominator <- if (is.na(denominator_col)) rep(NA_real_, nrow(x)) else {
      numeric_cell(x[[denominator_col]])
    }
    value <- if (!is.na(value_col)) {
      core_normalise_percentage(x[[value_col]])
    } else {
      numerator / denominator
    }
    date_col <- core_first_matching_column(
      names_value, c("^(calendar_month|month|date|period|reporting_period)$",
                     "month|date|period")
    )
    month <- if (is.na(date_col)) {
      rep(data.table::as.IDate(row$activity_month), nrow(x))
    } else {
      parsed <- parse_month_cell(x[[date_col]])
      parsed[is.na(parsed)] <- data.table::as.IDate(row$activity_month)
      parsed
    }
    code_col <- core_first_matching_column(
      names_value,
      c("^(organisation|organization|org|provider|commissioner|icb)_?(code|id)$",
        "(org|provider|commissioner|icb).*code")
    )
    name_col <- core_first_matching_column(
      names_value,
      c("^(organisation|organization|org|provider|commissioner|icb)_?name$",
        "(org|provider|commissioner|icb).*name", "^geography$")
    )
    code <- if (is.na(code_col)) rep("ENGLAND", nrow(x)) else trimws(x[[code_col]])
    name <- if (is.na(name_col)) rep("England", nrow(x)) else trimws(x[[name_col]])
    national <- toupper(code) %in% c("ENG", "ENGLAND", "TOTAL", "NATIONAL") |
      grepl("(?i)^England$|all England|national total", name, perl = TRUE)
    complete <- is.finite(value) & value >= 0 & value <= 1
    data.table::data.table(
      month, code, name, numerator, denominator, value, national, complete
    )[complete == TRUE]
  })
  x <- data.table::rbindlist(imported, use.names = TRUE, fill = TRUE)
  if (!nrow(x)) {
    schemas <- vapply(seq_along(tables), function(i) {
      columns <- names(tables[[i]])
      if (length(columns) > 16L) {
        columns <- c(columns[seq_len(16L)], "...")
      }
      paste0("table ", i, ": ", paste(columns, collapse = ", "))
    }, character(1))
    stop(
      "Could not locate the configured ", metric_id,
      " percentage in ", basename(row$local_path), ". Candidate schemas: ",
      paste(schemas, collapse = " | ")
    )
  }
  # A monthly chart file represents one release and may repeat earlier months;
  # a period time-series workbook is itself the source for every dated row.
  if (identical(as.character(row$selection_mode[1L]), "monthly")) {
    activity_month <- data.table::as.IDate(row$activity_month)
    if (any(x$month == activity_month, na.rm = TRUE)) {
      x <- x[month == activity_month]
    } else {
      available <- x[month <= activity_month, unique(month)]
      if (length(available)) x <- x[month == max(available)]
    }
  }
  national <- x[national == TRUE]
  if (!nrow(national) && all(x$code == "ENGLAND")) national <- x
  if (!nrow(national)) {
    stop("No England observation was found in ", basename(row$local_path), ".")
  }
  national <- national[, .SD[.N], by = month]
  provider <- x[national == FALSE & nzchar(code)]
  if (nrow(provider)) provider <- provider[, .SD[.N], by = .(month, code)]
  method <- paste0("official_generic_validated_", metric_id)
  provider_rows <- if (nrow(provider)) {
    core_make_provider_rows(
      metric_id, provider$month, provider$code, provider$name, provider$value,
      provider$numerator, provider$denominator, provider$complete, source, method
    )
  } else {
    data.table::data.table()
  }
  list(
    national = core_make_national_rows(
      metric_id, national$month, national$value, national$numerator,
      national$denominator, national$complete, source, method
    ),
    provider = provider_rows
  )
}

core_empty_optional_import_failures <- function() {
  data.table::data.table(
    metric_id = character(), dataset_role = character(),
    source_file = character(), source_url = character(), detail = character()
  )
}

core_import_optional_percentage_metric <- function(manifest, metric_id,
                                                   measure_pattern = NULL,
                                                   exclude_pattern = "a^",
                                                   reader = NULL) {
  metric_id_value <- metric_id
  rows <- manifest[metric_id == metric_id_value]
  if (!nrow(rows)) return(list(
    national = data.table::data.table(), provider = data.table::data.table(),
    failures = core_empty_optional_import_failures()
  ))
  if (is.null(reader)) {
    if (is.null(measure_pattern) || !nzchar(measure_pattern)) {
      stop("An optional metric importer requires either reader or measure_pattern.")
    }
    reader <- function(source_row) {
      core_extract_generic_percentage(
        source_row, metric_id, measure_pattern, exclude_pattern
      )
    }
  }
  failures <- list()
  pieces <- lapply(seq_len(nrow(rows)), function(i) {
    tryCatch(
      reader(rows[i]),
      error = function(e) {
        failures[[length(failures) + 1L]] <<- data.table::data.table(
          metric_id = metric_id,
          dataset_role = rows$dataset_role[i],
          source_file = basename(rows$local_path[i]),
          source_url = rows$source_url[i],
          detail = conditionMessage(e)
        )
        message(metric_id, ": skipped ", basename(rows$local_path[i]), " — ",
                conditionMessage(e))
        NULL
      }
    )
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(list(
    national = data.table::data.table(), provider = data.table::data.table(),
    failures = if (length(failures)) {
      data.table::rbindlist(failures, use.names = TRUE, fill = TRUE)
    } else {
      core_empty_optional_import_failures()
    }
  ))
  national <- data.table::rbindlist(
    lapply(pieces, `[[`, "national"), use.names = TRUE, fill = TRUE
  )
  provider <- data.table::rbindlist(
    lapply(pieces, `[[`, "provider"), use.names = TRUE, fill = TRUE
  )
  if (nrow(national)) {
    data.table::setorder(national, calendar_month, source_file)
    national <- unique(national, by = "calendar_month", fromLast = TRUE)
  }
  if (nrow(provider)) {
    data.table::setorder(provider, entity_id, calendar_month, source_file)
    provider <- unique(
      provider, by = c("entity_id", "calendar_month"), fromLast = TRUE
    )
  }
  list(
    national = national[], provider = provider[],
    failures = {
      out <- data.table::rbindlist(failures, use.names = TRUE, fill = TRUE)
      if (!ncol(out)) {
        out <- core_empty_optional_import_failures()
      }
      out[]
    }
  )
}

core_import_community_waits <- function(manifest) {
  rows <- manifest[metric_id == "community_18w"]
  if (!nrow(rows)) return(list(
    national = data.table::data.table(), provider = data.table::data.table(),
    community_service_band = core_empty_community_service_band(),
    community_service_summary = core_empty_community_service_summary(),
    failures = core_empty_optional_import_failures()
  ))
  failures <- list()
  pieces <- lapply(seq_len(nrow(rows)), function(i) {
    tryCatch(
      core_read_community_18w_workbook(rows[i]),
      error = function(e) {
        failures[[length(failures) + 1L]] <<- data.table::data.table(
          metric_id = "community_18w",
          dataset_role = rows$dataset_role[i],
          source_file = basename(rows$local_path[i]),
          source_url = rows$source_url[i],
          detail = conditionMessage(e)
        )
        message(
          "community_18w: skipped ", basename(rows$local_path[i]), " — ",
          conditionMessage(e)
        )
        NULL
      }
    )
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(list(
    national = data.table::data.table(), provider = data.table::data.table(),
    community_service_band = core_empty_community_service_band(),
    community_service_summary = core_empty_community_service_summary(),
    failures = if (length(failures)) {
      data.table::rbindlist(failures, use.names = TRUE, fill = TRUE)
    } else {
      core_empty_optional_import_failures()
    }
  ))
  national <- data.table::rbindlist(
    lapply(pieces, `[[`, "national"), use.names = TRUE, fill = TRUE
  )
  data.table::setorder(national, calendar_month, source_file)
  national <- unique(national, by = "calendar_month", fromLast = TRUE)
  service_band <- data.table::rbindlist(
    lapply(pieces, `[[`, "community_service_band"),
    use.names = TRUE, fill = TRUE
  )
  service_summary <- data.table::rbindlist(
    lapply(pieces, `[[`, "community_service_summary"),
    use.names = TRUE, fill = TRUE
  )
  service_band_key <- c(
    "calendar_month", "source_geography_key", "service_id", "wait_band"
  )
  service_summary_key <- c(
    "calendar_month", "source_geography_key", "service_id"
  )
  data.table::setorderv(service_band, c(service_band_key, "source_file"))
  service_band <- unique(service_band, by = service_band_key, fromLast = TRUE)
  data.table::setorderv(service_summary, c(service_summary_key, "source_file"))
  service_summary <- unique(
    service_summary, by = service_summary_key, fromLast = TRUE
  )
  list(
    national = national[], provider = data.table::data.table(),
    community_service_band = service_band[],
    community_service_summary = service_summary[],
    failures = {
      out <- data.table::rbindlist(failures, use.names = TRUE, fill = TRUE)
      if (!ncol(out)) out <- core_empty_optional_import_failures()
      out[]
    }
  )
}

core_complete_provider_panel <- function(x) {
  if (!nrow(x)) return(x)
  parts <- lapply(unique(x$metric_id), function(metric) {
    z <- data.table::copy(x[metric_id == metric])
    months <- data.table::as.IDate(seq(
      as.Date(min(z$calendar_month)), as.Date(max(z$calendar_month)), by = "month"
    ))
    entities <- unique(z$entity_id)
    grid <- data.table::CJ(entity_id = entities, calendar_month = months, unique = TRUE)
    grid[, metric_id := metric]
    out <- merge(
      grid, z, by = c("metric_id", "entity_id", "calendar_month"), all.x = TRUE
    )
    names_by_entity <- z[!is.na(entity_name), .(
      entity_name_fill = core_latest_nonmissing_character(entity_name)
    ), by = entity_id]
    out <- merge(out, names_by_entity, by = "entity_id", all.x = TRUE)
    out[is.na(entity_name), entity_name := entity_name_fill]
    out[, entity_name_fill := NULL]
    out[is.na(complete_submission), complete_submission := FALSE]
    out[]
  })
  out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  data.table::setorder(out, metric_id, entity_id, calendar_month)
  out[]
}

validate_core_import_panels <- function(national, provider) {
  required <- c(
    "metric_id", "calendar_month", "entity_id", "entity_name", "numerator",
    "denominator", "activity_volume_proxy", "activity_volume_proxy_method",
    "value", "complete_submission", "source_method",
    "source_file", "source_url", "source_sha256"
  )
  assert_columns(national, required, "core national panel")
  assert_columns(provider, required, "core provider panel")
  if (!nrow(national)) stop("Core import produced no national series.")
  if (anyDuplicated(national[, .(metric_id, calendar_month)])) {
    stop("Core national panel contains duplicate metric-month keys.")
  }
  if (anyDuplicated(provider[, .(metric_id, entity_id, calendar_month)])) {
    stop("Core provider panel contains duplicate metric-provider-month keys.")
  }
  national_complete <- national[complete_submission == TRUE]
  provider_complete <- provider[complete_submission == TRUE]
  if (any(!is.finite(national_complete$value)) ||
      any(!is.finite(provider_complete$value))) {
    stop("Complete core rows contain non-finite values.")
  }
  complete <- data.table::rbindlist(
    list(national_complete, provider_complete), use.names = TRUE, fill = TRUE
  )
  if (any(format(as.Date(complete$calendar_month), "%d") != "01")) {
    stop("Complete core rows contain dates that are not first-of-month periods.")
  }
  provenance_ok <-
    !is.na(complete$source_file) & nzchar(trimws(complete$source_file)) &
    !is.na(complete$source_url) & nzchar(trimws(complete$source_url)) &
    grepl("^[0-9a-fA-F]{64}$", complete$source_sha256)
  if (any(!provenance_ok)) {
    stop("Complete core rows must retain source file, URL and SHA-256 provenance.")
  }
  reconcilable <- complete[
    is.finite(numerator) & is.finite(denominator) & denominator > 0
  ]
  if (nrow(reconcilable) && any(
    abs(reconcilable$value - reconcilable$numerator / reconcilable$denominator) > 1e-8
  )) {
    stop("A complete core rate does not reconcile to its numerator and denominator.")
  }
  probability_metrics <- c(
    "rtt_18w", "diagnostics_6w", "cancer_62d", "ucr_2h",
    "community_18w", "talking_therapies_6w"
  )
  if (any(national_complete[metric_id %in% probability_metrics, value < 0 | value > 1]) ||
      any(provider_complete[metric_id %in% probability_metrics, value < 0 | value > 1])) {
    stop("A probability core metric falls outside [0, 1].")
  }
  if (any(national_complete[metric_id == "ambulance_cat2", value <= 0]) ||
      any(provider_complete[metric_id == "ambulance_cat2", value <= 0])) {
    stop("Category 2 response times must be positive.")
  }
  ucr_proxy <- provider_complete[
    metric_id == "ucr_2h" & is.finite(activity_volume_proxy)
  ]
  if (nrow(ucr_proxy) && any(
    ucr_proxy$activity_volume_proxy < 0 |
      is.na(ucr_proxy$activity_volume_proxy_method) |
      !nzchar(ucr_proxy$activity_volume_proxy_method)
  )) {
    stop("UCR provider activity proxies must be non-negative and explicitly labelled.")
  }
  invisible(TRUE)
}

import_core_metrics <- function(downloaded_manifest) {
  downloaded_manifest[, activity_month := data.table::as.IDate(activity_month)]
  national_parts <- list()
  provider_parts <- list()
  ni <- 0L
  pi <- 0L

  rtt_national <- downloaded_manifest[
    metric_id == "rtt_18w" & dataset_role == "national_time_series"
  ]
  if (nrow(rtt_national) != 1L) stop("Expected one downloaded RTT national time series.")
  ni <- ni + 1L
  national_parts[[ni]] <- core_read_rtt_national(rtt_national[1L])
  for (i in seq_len(nrow(downloaded_manifest[
    metric_id == "rtt_18w" & dataset_role == "provider_monthly"
  ]))) {
    row <- downloaded_manifest[
      metric_id == "rtt_18w" & dataset_role == "provider_monthly"
    ][i]
    pi <- pi + 1L
    provider_parts[[pi]] <- core_read_rtt_provider(row)
  }

  diagnostics_national <- downloaded_manifest[
    metric_id == "diagnostics_6w" & dataset_role == "national_time_series"
  ]
  if (nrow(diagnostics_national) != 1L) {
    stop("Expected one downloaded diagnostics national time series.")
  }
  ni <- ni + 1L
  national_parts[[ni]] <- core_read_diagnostics_national(diagnostics_national[1L])
  diagnostics_provider <- downloaded_manifest[
    metric_id == "diagnostics_6w" & dataset_role == "provider_monthly"
  ]
  for (i in seq_len(nrow(diagnostics_provider))) {
    pi <- pi + 1L
    provider_parts[[pi]] <- core_read_diagnostics_provider(diagnostics_provider[i])
  }

  cancer_national <- downloaded_manifest[
    metric_id == "cancer_62d" & dataset_role == "national_time_series"
  ]
  if (nrow(cancer_national) != 1L) {
    stop("Expected one downloaded cancer national time series.")
  }
  cancer_national_rows <- core_read_cancer_national_timeseries(cancer_national[1L])
  ni <- ni + 1L
  national_parts[[ni]] <- cancer_national_rows

  cancer_rows <- downloaded_manifest[
    metric_id == "cancer_62d" & dataset_role == "combined_monthly"
  ]
  cancer_monthly_national <- vector("list", nrow(cancer_rows))
  for (i in seq_len(nrow(cancer_rows))) {
    imported <- core_read_cancer_month(cancer_rows[i])
    cancer_monthly_national[[i]] <- imported$national
    pi <- pi + 1L
    provider_parts[[pi]] <- imported$provider
  }
  cancer_monthly_national <- data.table::rbindlist(
    cancer_monthly_national, use.names = TRUE, fill = TRUE
  )
  cancer_national_reconciliation <- core_validate_cancer_national_overlap(
    cancer_national_rows, cancer_monthly_national
  )

  ambulance <- downloaded_manifest[
    metric_id == "ambulance_cat2" & dataset_role == "combined_time_series"
  ]
  if (nrow(ambulance) != 1L) stop("Expected one downloaded AmbSYS time series.")
  imported_ambulance <- core_read_ambulance_timeseries(ambulance[1L])
  ni <- ni + 1L
  national_parts[[ni]] <- imported_ambulance$national
  pi <- pi + 1L
  provider_parts[[pi]] <- imported_ambulance$provider

  optional_import_failures <- list()
  community_import <- core_import_community_waits(downloaded_manifest)
  if (nrow(community_import$national)) {
    ni <- ni + 1L
    national_parts[[ni]] <- community_import$national
  }
  if (nrow(community_import$failures)) {
    optional_import_failures[[length(optional_import_failures) + 1L]] <-
      community_import$failures
  }

  optional_specs <- list(
    list(metric_id = "ucr_2h", reader = core_read_ucr_workbook),
    list(
      metric_id = "talking_therapies_6w",
      reader = core_read_talking_therapies_chart
    )
  )
  for (spec in optional_specs) {
    imported <- core_import_optional_percentage_metric(
      downloaded_manifest, spec$metric_id, reader = spec$reader
    )
    if (nrow(imported$national)) {
      ni <- ni + 1L
      national_parts[[ni]] <- imported$national
    }
    if (nrow(imported$provider)) {
      pi <- pi + 1L
      provider_parts[[pi]] <- imported$provider
    }
    if (nrow(imported$failures)) {
      optional_import_failures[[length(optional_import_failures) + 1L]] <-
        imported$failures
    }
  }

  national <- data.table::rbindlist(national_parts, use.names = TRUE, fill = TRUE)
  provider <- data.table::rbindlist(provider_parts, use.names = TRUE, fill = TRUE)
  national <- unique(national, by = c("metric_id", "calendar_month"))
  provider <- core_complete_provider_panel(provider)
  data.table::setorder(national, metric_id, calendar_month)
  validate_core_import_panels(national, provider)
  list(
    national = national[],
    provider = provider[],
    community_service_band = community_import$community_service_band[],
    community_service_summary = community_import$community_service_summary[],
    cancer_national_reconciliation = cancer_national_reconciliation[],
    optional_import_failures = if (length(optional_import_failures)) {
      data.table::rbindlist(
        optional_import_failures, use.names = TRUE, fill = TRUE
      )
    } else {
      core_empty_optional_import_failures()
    }
  )
}
