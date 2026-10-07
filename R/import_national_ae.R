read_national_performance <- function(path, manifest_row) {
  sheets <- readxl::excel_sheets(path)
  if (!"Performance" %in% sheets) stop("National workbook has no 'Performance' sheet: ", path)
  cells <- read_excel_matrix(path, "Performance")
  header_row <- locate_header_row(
    cells,
    c("^Period$", "Percentage in 4 hours or less.*type 1"),
    paste0(basename(path), " / Performance")
  )
  header <- cells[header_row, ]
  period_col <- locate_one_column(header, "^Period$", "national period")
  within_col <- locate_group_start(
    cells, header_row, "less than 4 hours|<\\s*4 hours", "national Type 1 within-four-hour count"
  )
  over_col <- locate_group_start(
    cells, header_row, "greater than 4 hours|>\\s*4 hours", "national Type 1 over-four-hour count"
  )
  percentage_col <- locate_one_column(
    header, "Percentage in 4 hours or less.*type 1", "published national Type 1 performance"
  )
  all_within_col <- locate_one_column(
    header, "^Total Attendances < 4 hours$", "national all-types within-four-hour count"
  )
  all_over_col <- locate_one_column(
    header, "^Total Attendances > 4 hours$", "national all-types over-four-hour count"
  )
  all_percentage_col <- locate_one_column(
    header, "Percentage in 4 hours or less.*all", "published national all-types performance"
  )
  rows <- seq.int(header_row + 1L, nrow(cells))
  out <- data.table::data.table(
    calendar_month = parse_month_cell(cells[rows, period_col]),
    type1_within_4h_n = numeric_cell(cells[rows, within_col]),
    type1_over_4h_n = numeric_cell(cells[rows, over_col]),
    published_type1_performance = numeric_cell(cells[rows, percentage_col]),
    ae4h_within_4h_n = numeric_cell(cells[rows, all_within_col]),
    ae4h_over_4h_n = numeric_cell(cells[rows, all_over_col]),
    published_ae4h_performance = numeric_cell(cells[rows, all_percentage_col])
  )[!is.na(calendar_month)]
  out[, type1_attendances_n := type1_within_4h_n + type1_over_4h_n]
  out[, type1_performance := type1_within_4h_n / type1_attendances_n]
  out[, performance_identity_difference := type1_performance - published_type1_performance]
  out[, ae4h_attendances_n := ae4h_within_4h_n + ae4h_over_4h_n]
  out[, ae4h_performance := ae4h_within_4h_n / ae4h_attendances_n]
  out[, ae4h_performance_identity_difference :=
    ae4h_performance - published_ae4h_performance]
  out[, source_method := ifelse(
    calendar_month < data.table::as.IDate("2015-06-01"),
    "weekly_apportioned_monthly_estimate", "monthly_collection"
  )]
  out[, national_comparability_era := data.table::fcase(
    calendar_month < data.table::as.IDate("2015-06-01"), "estimated_pre_monthly_collection",
    calendar_month < data.table::as.IDate("2019-05-01"), "monthly_full_pre_crs",
    calendar_month <= data.table::as.IDate("2023-05-01"), "monthly_crs_excluding_14_trusts",
    default = "monthly_full_post_crs"
  )]
  out[, `:=`(
    crs_14_trusts_excluded = calendar_month >= data.table::as.IDate("2019-05-01") &
      calendar_month <= data.table::as.IDate("2023-05-01"),
    booked_appointments_definition_era = ifelse(
      calendar_month >= data.table::as.IDate("2020-08-01"),
      "booked_appointments_included", "pre_booked_appointments_field"
    ),
    april_2026_provider_reporting_change = calendar_month >= data.table::as.IDate("2026-04-01"),
    source_file = basename(manifest_row$local_path),
    source_url = manifest_row$source_url,
    source_publication_page = manifest_row$publication_page,
    source_revision = manifest_row$revision_label,
    source_sha256 = manifest_row$sha256,
    qa_flags = ""
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags, source_method == "weekly_apportioned_monthly_estimate", "weekly_apportioned_estimate"
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags, crs_14_trusts_excluded, "crs_excludes_14_field_test_trusts"
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags, booked_appointments_definition_era == "booked_appointments_included",
    "booked_appointments_definition_era"
  )]
  out[, qa_flags := append_qa_flag(
    qa_flags, april_2026_provider_reporting_change, "provider_reporting_change_not_national_break"
  )]
  data.table::setorder(out, calendar_month)
  out[]
}
