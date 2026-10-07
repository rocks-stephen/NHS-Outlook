provider_column_candidates <- list(
  source_org_code = c("org_code", "organisation_code", "provider_code", "code"),
  source_org_name = c("org_name", "organisation_name", "provider_name", "name"),
  type1_regular_attendances_n = c(
    "a_e_attendances_type_1", "number_of_a_e_attendances_type_1",
    "type_1_attendances", "attendances_type_1"
  ),
  type1_regular_over_4h_n = c(
    "attendances_over_4hrs_type_1", "number_of_attendances_over_4hrs_type_1",
    "type_1_attendances_over_4_hours", "type_1_over_4_hours"
  ),
  type2_regular_attendances_n = c(
    "a_e_attendances_type_2", "number_of_a_e_attendances_type_2",
    "type_2_attendances", "attendances_type_2"
  ),
  type3_regular_attendances_n = c(
    "a_e_attendances_other_a_e_department", "a_e_attendances_type_3",
    "type_3_attendances", "attendances_type_3"
  ),
  type2_regular_over_4h_n = c(
    "attendances_over_4hrs_type_2", "number_of_attendances_over_4hrs_type_2",
    "type_2_attendances_over_4_hours", "type_2_over_4_hours"
  ),
  type3_regular_over_4h_n = c(
    "attendances_over_4hrs_other_department", "attendances_over_4hrs_type_3",
    "type_3_attendances_over_4_hours", "type_3_over_4_hours"
  )
)

resolve_one_column <- function(nms, candidates, field) {
  hit <- intersect(candidates, nms)
  if (length(hit) != 1L) {
    stop("Expected exactly one column for ", field, "; candidates found: ",
         paste(hit, collapse = ", "), ". Record a schema mapping rather than guessing.")
  }
  hit
}

resolve_optional_column <- function(nms, candidates, field) {
  hit <- intersect(candidates, nms)
  if (length(hit) > 1L) stop("Ambiguous optional column for ", field, ": ", paste(hit, collapse = ", "))
  if (!length(hit)) NA_character_ else hit
}

read_provider_csv <- function(path) {
  x <- data.table::fread(path, encoding = "UTF-8", na.strings = c("", "NA", "N/A", "-"))
  data.table::setnames(x, clean_names_transparent(names(x)))
  cols <- vapply(names(provider_column_candidates), function(field) {
    resolve_one_column(names(x), provider_column_candidates[[field]], field)
  }, character(1))
  optional_numeric <- function(candidates, field) {
    column <- resolve_optional_column(names(x), candidates, field)
    if (is.na(column)) rep(0, nrow(x)) else numeric_cell(x[[column]])
  }
  booked_type1_attendance_n <- optional_numeric(
    "a_e_attendances_booked_appointments_type_1", "booked Type 1 attendances"
  )
  booked_type2_attendance_n <- optional_numeric(
    "a_e_attendances_booked_appointments_type_2", "booked Type 2 attendances"
  )
  booked_type3_attendance_n <- optional_numeric(
    "a_e_attendances_booked_appointments_other_department", "booked Type 3 attendances"
  )
  booked_type1_over_n <- optional_numeric(
    "attendances_over_4hrs_booked_appointments_type_1",
    "booked Type 1 attendances over four hours"
  )
  booked_type2_over_n <- optional_numeric(
    "attendances_over_4hrs_booked_appointments_type_2",
    "booked Type 2 attendances over four hours"
  )
  booked_type3_over_n <- optional_numeric(
    "attendances_over_4hrs_booked_appointments_other_department",
    "booked Type 3 attendances over four hours"
  )
  type1_attendances_n <-
    numeric_cell(x[[cols[["type1_regular_attendances_n"]]]]) + booked_type1_attendance_n
  type2_attendances_n <-
    numeric_cell(x[[cols[["type2_regular_attendances_n"]]]]) + booked_type2_attendance_n
  type3_attendances_n <-
    numeric_cell(x[[cols[["type3_regular_attendances_n"]]]]) + booked_type3_attendance_n
  type1_over_4h_n <-
    numeric_cell(x[[cols[["type1_regular_over_4h_n"]]]]) + booked_type1_over_n
  type2_over_4h_n <-
    numeric_cell(x[[cols[["type2_regular_over_4h_n"]]]]) + booked_type2_over_n
  type3_over_4h_n <-
    numeric_cell(x[[cols[["type3_regular_over_4h_n"]]]]) + booked_type3_over_n
  data.table::data.table(
    source_org_code = as.character(x[[cols[["source_org_code"]]]]),
    source_org_name = as.character(x[[cols[["source_org_name"]]]]),
    source_parent_name = if ("parent_org" %in% names(x)) as.character(x$parent_org) else NA_character_,
    type1_attendances_n = type1_attendances_n,
    type1_within_4h_direct_n = NA_real_,
    type1_over_4h_n = type1_over_4h_n,
    ae4h_attendances_n = type1_attendances_n + type2_attendances_n + type3_attendances_n,
    ae4h_within_4h_direct_n = NA_real_,
    ae4h_over_4h_n = type1_over_4h_n + type2_over_4h_n + type3_over_4h_n,
    source_representation = "csv_fallback"
  )
}

read_provider_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  sheet <- if ("Provider Level Data" %in% sheets) {
    "Provider Level Data"
  } else if ("A&E Data" %in% sheets) {
    "A&E Data"
  } else {
    stop("No provider-level sheet found in ", path, ". Available: ", paste(sheets, collapse = ", "))
  }
  cells <- read_excel_matrix(path, sheet)
  header_row <- locate_header_row(
    cells,
    c("^Code$", "^Name$", "Type 1 Departments - Major A&E"),
    paste0(basename(path), " / ", sheet)
  )
  header <- cells[header_row, ]
  code_col <- locate_one_column(header, "^Code$", "provider code")
  name_col <- locate_one_column(header, "^Name$", "provider name")
  parent_hits <- which(grepl("^(Region|Parent Org)$", header, ignore.case = TRUE, perl = TRUE))
  parent_col <- if (length(parent_hits) == 1L) parent_hits else NA_integer_
  attendance_col <- locate_group_start(cells, header_row, "^A&E attendances$", "Type 1 attendances")
  over_col <- locate_group_start(
    cells, header_row, "greater than 4 hours|>\\s*4 hours", "Type 1 over-four-hour attendances"
  )
  within_col <- locate_group_start(
    cells, header_row, "less than 4 hours|<\\s*4 hours", "direct Type 1 within-four-hour attendances",
    required = FALSE
  )
  required_type1 <- function(column, field) {
    if (!grepl(
      "^Type 1 Departments - Major A&E$", header[column],
      ignore.case = TRUE, perl = TRUE
    )) {
      stop("Schema group for ", field, " does not start with the Type 1 column in ", basename(path), ".")
    }
  }
  required_type1(attendance_col, "attendances")
  required_type1(over_col, "over-four-hour attendances")
  if (!is.na(within_col)) required_type1(within_col, "within-four-hour attendances")
  all_attendance_col <- locate_one_column(
    header, "^Total attendances$", "all-types attendances"
  )
  all_within_hits <- which(grepl(
    "^Total Attendances < 4 hours$", header, ignore.case = TRUE, perl = TRUE
  ))
  if (length(all_within_hits) > 1L) {
    stop("Ambiguous all-types within-four-hour columns in ", basename(path), ".")
  }
  all_within_col <- if (length(all_within_hits)) all_within_hits else NA_integer_
  all_over_col <- locate_one_column(
    header, "^Total Attendances > 4 hours$", "all-types over-four-hour attendances"
  )

  rows <- seq.int(header_row + 1L, nrow(cells))
  data.table::data.table(
    source_org_code = trimws(cells[rows, code_col]),
    source_org_name = trimws(cells[rows, name_col]),
    source_parent_name = if (is.na(parent_col)) NA_character_ else trimws(cells[rows, parent_col]),
    type1_attendances_n = numeric_cell(cells[rows, attendance_col]),
    type1_within_4h_direct_n = if (is.na(within_col)) NA_real_ else numeric_cell(cells[rows, within_col]),
    type1_over_4h_n = numeric_cell(cells[rows, over_col]),
    ae4h_attendances_n = numeric_cell(cells[rows, all_attendance_col]),
    ae4h_within_4h_direct_n = if (is.na(all_within_col)) {
      NA_real_
    } else {
      numeric_cell(cells[rows, all_within_col])
    },
    ae4h_over_4h_n = numeric_cell(cells[rows, all_over_col]),
    source_representation = paste0("workbook:", sheet)
  )[nzchar(source_org_code) | nzchar(source_org_name)]
}

read_provider_source <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") return(read_provider_csv(path))
  if (extension %in% c("xls", "xlsx")) return(read_provider_workbook(path))
  stop("Unsupported provider source extension: ", extension)
}

classify_source_row <- function(code, name) {
  code_upper <- toupper(trimws(code))
  name_upper <- toupper(trimws(name))
  data.table::fcase(
    name_upper == "ENGLAND" | code_upper == "-", "national_total",
    code_upper == "TOTAL" | name_upper == "TOTAL", "file_total",
    default = "provider"
  )
}

standardize_provider_month <- function(raw, manifest_row) {
  out <- data.table::copy(raw)
  out[, row_scope := classify_source_row(source_org_code, source_org_name)]
  out[, type1_within_4h_derived_n := type1_attendances_n - type1_over_4h_n]
  out[, type1_within_4h_n := data.table::fifelse(
    !is.na(type1_within_4h_direct_n), type1_within_4h_direct_n, type1_within_4h_derived_n
  )]
  out[, type1_reported_performance_denominator_n := type1_within_4h_n + type1_over_4h_n]
  out[, type1_performance := data.table::fifelse(
    type1_reported_performance_denominator_n > 0,
    type1_within_4h_n / type1_reported_performance_denominator_n,
    NA_real_
  )]
  out[, ae4h_within_4h_derived_n := ae4h_attendances_n - ae4h_over_4h_n]
  out[, ae4h_within_4h_n := data.table::fifelse(
    !is.na(ae4h_within_4h_direct_n),
    ae4h_within_4h_direct_n,
    ae4h_within_4h_derived_n
  )]
  out[, ae4h_reported_performance_denominator_n :=
    ae4h_within_4h_n + ae4h_over_4h_n]
  out[, ae4h_performance := data.table::fifelse(
    ae4h_reported_performance_denominator_n > 0,
    ae4h_within_4h_n / ae4h_reported_performance_denominator_n,
    NA_real_
  )]
  out[, submission_status := data.table::fcase(
    row_scope != "provider", "aggregate_row",
    type1_attendances_n == 0, "no_type1_activity",
    !is.na(type1_attendances_n) & is.na(type1_over_4h_n), "performance_not_submitted",
    is.na(type1_attendances_n), "missing",
    default = "submitted"
  )]
  out[, ae4h_submission_status := data.table::fcase(
    row_scope != "provider", "aggregate_row",
    type1_attendances_n == 0, "no_type1_activity",
    !is.na(ae4h_attendances_n) & is.na(ae4h_over_4h_n), "performance_not_submitted",
    is.na(ae4h_attendances_n), "missing",
    default = "submitted"
  )]
  out[, `:=`(
    calendar_month = data.table::as.IDate(manifest_row$activity_month),
    financial_year = financial_year_from_month(manifest_row$activity_month),
    publication_reporting_era = ifelse(
      data.table::as.IDate(manifest_row$activity_month) < data.table::as.IDate("2026-04-01"),
      "pre_acute_provider_table_change", "acute_provider_table_2026_04_onward"
    ),
    source_file = basename(manifest_row$local_path),
    source_url = manifest_row$source_url,
    source_publication_page = manifest_row$publication_page,
    source_revision = manifest_row$revision_label,
    source_sha256 = manifest_row$sha256,
    is_current_revision = parse_logical_strict(manifest_row$is_current),
    analysis_trust_id = source_org_code,
    analysis_trust_name = source_org_name,
    identity_status = "source_code_unharmonised",
    mapping_rule_id = NA_character_,
    qa_flags = ""
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags,
    row_scope == "provider" & type1_attendances_n > 0 & is.na(type1_over_4h_n),
    "missing_four_hour_submission"
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags,
    row_scope == "provider" & type1_attendances_n > 0 & is.na(ae4h_over_4h_n),
    "missing_all_types_four_hour_submission"
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags,
    !is.na(ae4h_within_4h_direct_n) & !is.na(ae4h_within_4h_derived_n) &
      abs(ae4h_within_4h_direct_n - ae4h_within_4h_derived_n) > 1e-8,
    "all_types_direct_derived_numerator_mismatch"
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags,
    !is.na(type1_within_4h_direct_n) & !is.na(type1_within_4h_derived_n) &
      abs(type1_within_4h_direct_n - type1_within_4h_derived_n) > 1e-8,
    "direct_derived_numerator_mismatch"
  )]
  out[]
}
