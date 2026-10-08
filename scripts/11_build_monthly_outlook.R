source("R/utils.R")
source("R/national_forecast.R")
source("R/monthly_outlook.R")

national_config <- read_key_value_config("config/national_model.csv")
provider_config <- read_key_value_config("config/provider_model.csv")
target_performance <- parse_numeric_setting(
  national_config, "target_performance", 0, 1
)
prior_target_month <- parse_date_setting(national_config, "prior_target_month")
prior_target_performance <- parse_numeric_setting(
  national_config, "prior_target_performance", 0, 1
)
intermediate_target_month <- parse_date_setting(
  national_config, "intermediate_target_month"
)
intermediate_target_performance <- parse_numeric_setting(
  national_config, "intermediate_target_performance", 0, 1
)
final_target_start_month <- parse_date_setting(
  national_config, "final_target_start_month"
)
provider_window_months <- parse_integer_setting(
  provider_config, "persistent_window_months", 2L
)
provider_materiality_pp <- parse_numeric_setting(
  provider_config, "persistent_materiality_pp", 0
)
provider_direction_share <- parse_numeric_setting(
  provider_config, "persistent_direction_share", 0.5, 1
)

required_files <- c(
  "data-interim/national_month_stage1.csv",
  "output/national/next_release_forecast.csv",
  "output/national/reference_forecast.csv",
  "output/national/release_forecast_archive.csv",
  "output/national/release_forecast_scorecard.csv",
  "output/national/next_release_model_comparison.csv",
  "output/national/forecast_method.csv",
  "output/provider/latest_watchlist.csv",
  "output/provider/provider_signal_archive.csv",
  "reports/ae_four_hour_outlook_template.html"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Cannot build the monthly outlook; missing: ",
    paste(missing_files, collapse = ", "),
    ". Run scripts/06_model_national.R and scripts/08_model_provider.R first."
  )
}

read_outlook_csv <- function(path, date_columns = character()) {
  out <- data.table::fread(path, encoding = "UTF-8")
  for (column in intersect(date_columns, names(out))) {
    out[, (column) := data.table::as.IDate(get(column))]
  }
  out[]
}

empty_provider_signals <- function() {
  data.table::data.table(
    signal_version = character(),
    data_through_month = data.table::as.IDate(character()),
    analysis_trust_id = character(),
    analysis_trust_name = character(),
    latest_actual_performance = numeric(),
    latest_ae4h_attendances_n = numeric(),
    six_month_actual_average = numeric(),
    six_month_expected_average = numeric(),
    six_month_gap_to_trajectory_pp = numeric(),
    six_month_direction_share = numeric(),
    current_direction_streak_months = integer(),
    statistically_unusual_6m = logical(),
    practically_material_6m = logical(),
    directionally_persistent_6m = logical(),
    latest_error_same_direction = logical(),
    genuine_release_vintages_6m = integer(),
    simulated_vintages_6m = integer(),
    signal_evidence = character(),
    signal = character(),
    signal_status = character(),
    review_priority = character(),
    identity_status = character(),
    source_org_codes = character()
  )
}

national <- read_outlook_csv(
  "data-interim/national_month_stage1.csv", "calendar_month"
)
next_release <- read_outlook_csv(
  "output/national/next_release_forecast.csv",
  c("data_through_month", "forecast_month")
)
medium_forecast <- read_outlook_csv(
  "output/national/reference_forecast.csv", "forecast_month"
)
national_archive <- read_outlook_csv(
  "output/national/release_forecast_archive.csv",
  c("data_through_month", "forecast_month")
)
scorecard <- read_outlook_csv(
  "output/national/release_forecast_scorecard.csv",
  c("data_through_month", "forecast_month")
)
forecast_components <- read_outlook_csv(
  "output/national/next_release_model_comparison.csv", "forecast_month"
)
watchlist <- read_outlook_csv(
  "output/provider/latest_watchlist.csv",
  c("data_through_month", "forecast_month")
)
signal_archive <- read_outlook_csv(
  "output/provider/provider_signal_archive.csv", "data_through_month"
)
forecast_method <- read_outlook_csv(
  "output/national/forecast_method.csv",
  c(
    "data_first_month", "data_through_month", "forecast_month",
    "backtest_first_target_month", "backtest_last_target_month"
  )
)

template_path <- "reports/ae_four_hour_outlook_template.html"
output_dir <- "output/releases"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

archive_match <- national_archive[
  forecast_version == next_release$forecast_version[1L] &
    data_through_month == next_release$data_through_month[1L] &
    forecast_month == next_release$forecast_month[1L] &
    model == next_release$model[1L]
]
current <- data.table::copy(next_release)
current[, `:=`(
  actual_performance = NA_real_,
  forecast_status = "awaiting_release",
  forecast_created_at_utc = if (nrow(archive_match)) {
    archive_match$forecast_created_at_utc[1L]
  } else {
    NA_character_
  }
)]
current_signals <- watchlist[
  data_through_month == current$data_through_month[1L]
]
current_filename <- paste0(
  "ae-four-hour-outlook-", format(current$forecast_month[1L], "%Y-%m"), ".html"
)
current_path <- file.path(output_dir, current_filename)
build_ae_outlook_page(
  national = national,
  medium_forecast = medium_forecast,
  forecast_row = current,
  provider_signals = current_signals,
  template_path = template_path,
  output_path = current_path,
  target_performance = target_performance,
  provider_window_months = provider_window_months,
  provider_materiality_pp = provider_materiality_pp,
  provider_direction_share = provider_direction_share,
  prior_target_month = prior_target_month,
  prior_target_performance = prior_target_performance,
  intermediate_target_month = intermediate_target_month,
  intermediate_target_performance = intermediate_target_performance,
  final_target_start_month = final_target_start_month,
  forecast_method = forecast_method,
  forecast_components = forecast_components
)
file.copy(
  current_path,
  file.path(output_dir, "ae-four-hour-outlook-latest.html"),
  overwrite = TRUE
)

manifest <- data.table::data.table(
  target_month = current$forecast_month[1L],
  release_state = "forecast_issued",
  output_file = current_filename
)

scored <- scorecard[
  forecast_version == national_config$release_forecast_version &
    forecast_status == "scored" & !is.na(actual_performance)
]
if (nrow(scored)) {
  data.table::setorder(scored, forecast_month, forecast_created_at_utc)
  result <- scored[.N]
  result_signals <- signal_archive[
    signal_version == provider_config$release_forecast_version &
      data_through_month == result$forecast_month[1L]
  ]
  if (!nrow(result_signals)) result_signals <- empty_provider_signals()
  result_filename <- paste0(
    "ae-four-hour-outlook-", format(result$forecast_month[1L], "%Y-%m"), ".html"
  )
  build_ae_outlook_page(
    national = national,
    medium_forecast = medium_forecast,
    forecast_row = result,
    provider_signals = result_signals,
    template_path = template_path,
    output_path = file.path(output_dir, result_filename),
    target_performance = target_performance,
    provider_window_months = provider_window_months,
    provider_materiality_pp = provider_materiality_pp,
    provider_direction_share = provider_direction_share,
    prior_target_month = prior_target_month,
    prior_target_performance = prior_target_performance,
    intermediate_target_month = intermediate_target_month,
    intermediate_target_performance = intermediate_target_performance,
    final_target_start_month = final_target_start_month,
    forecast_method = forecast_method,
    forecast_components = forecast_components
  )
  if (identical(getOption("nhs.outlook.publication_mode"), "outturn")) {
    file.copy(
      file.path(output_dir, result_filename),
      file.path(output_dir, "ae-four-hour-outturn-latest.html"),
      overwrite = TRUE
    )
  }
  manifest <- data.table::rbindlist(list(
    manifest,
    data.table::data.table(
      target_month = result$forecast_month[1L],
      release_state = "actual_released",
      output_file = result_filename
    )
  ))
}

data.table::setorder(manifest, target_month, release_state)
data.table::fwrite(manifest, file.path(output_dir, "release_manifest.csv"))
message(
  "Monthly outlook built: ", current_path,
  ". Open output/releases/ae-four-hour-outlook-latest.html in a browser."
)
