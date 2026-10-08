source("R/utils.R")
source("R/national_forecast.R")
source("R/performance_outlook.R")
source("R/core_outlook.R")

publication_mode <- nhs_outlook_publication_mode()
publication_issue_date <- nhs_outlook_issue_date()
publication_status <- nhs_outlook_publication_status()
issue_day <- format(as.Date(publication_issue_date), "%Y-%m-%d")

required_files <- c(
  "config/performance_metrics.csv",
  "config/national_model.csv",
  "config/provider_model.csv",
  "config/core_targets.csv",
  "output/national/next_release_forecast.csv",
  "output/national/reference_forecast.csv",
  "output/national/release_forecast_archive.csv",
  "output/national/release_forecast_scorecard.csv",
  "output/national/forecast_method.csv",
  "output/provider/latest_watchlist.csv",
  "output/core/overview_metric_rows.csv",
  "output/core/model_status.csv",
  "output/core/forecast_method_register.csv",
  "data-interim/national_month_stage1.csv",
  "data-interim/core/national_panel.csv",
  "data-interim/core/community_waits_service_summary.csv",
  "reports/nhs_performance_outlook_template.html",
  "reports/core_metric_outlook_template.html",
  "reports/provider_watch_outturn_template.html"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Cannot build the performance outlook; missing: ",
    paste(missing_files, collapse = ", "),
    ". Run scripts/06, 08 and 15 first."
  )
}

read_performance_csv <- function(path, date_columns = character()) {
  out <- data.table::fread(path, encoding = "UTF-8")
  for (column in intersect(date_columns, names(out))) {
    out[, (column) := data.table::as.IDate(get(column))]
  }
  out[]
}

metric_config <- data.table::fread(
  "config/performance_metrics.csv", encoding = "UTF-8"
)
required_config <- c(
  "metric_id", "display_order", "overview_group", "display_name", "short_name", "active",
  "adapter", "unit", "digits", "higher_is_better", "flat_threshold_native",
  "provider_signal_enabled", "deep_dive_file", "benchmark_label",
  "data_source_label", "data_source_url"
)
assert_columns(metric_config, required_config, "performance metric config")
if (anyDuplicated(metric_config$metric_id)) {
  stop("Performance metric config contains duplicate metric IDs.")
}
metric_config[, `:=`(
  active = parse_logical_strict(active),
  higher_is_better = parse_logical_strict(higher_is_better),
  provider_signal_enabled = parse_logical_strict(provider_signal_enabled)
)]
if (anyNA(metric_config$active) || anyNA(metric_config$higher_is_better)) {
  stop("Performance metric config contains an invalid logical setting.")
}
configured_active <- metric_config[active == TRUE]
if (!nrow(configured_active)) stop("No performance metrics are active.")

national_config <- read_key_value_config("config/national_model.csv")
provider_config <- read_key_value_config("config/provider_model.csv")
next_release <- read_performance_csv(
  "output/national/next_release_forecast.csv",
  c("data_through_month", "forecast_month")
)
reference_forecast <- read_performance_csv(
  "output/national/reference_forecast.csv", "forecast_month"
)
release_archive <- read_performance_csv(
  "output/national/release_forecast_archive.csv",
  c("data_through_month", "forecast_month")
)
scorecard <- read_performance_csv(
  "output/national/release_forecast_scorecard.csv",
  c("data_through_month", "forecast_month")
)
watchlist <- read_performance_csv(
  "output/provider/latest_watchlist.csv",
  c("data_through_month", "forecast_month")
)
core_rows <- read_performance_csv(
  "output/core/overview_metric_rows.csv",
  c("latest_month", "forecast_month", "target_month")
)
core_model_status <- read_performance_csv("output/core/model_status.csv")
assert_columns(core_model_status, c(
  "metric_id", "display_name", "model_status", "status_reason"
), "core model status")
included_core <- core_model_status[
  model_status %in% c("included", "included_national_only"), metric_id
]
excluded_core <- core_model_status[
  model_status %in% c("excluded_no_data", "excluded_insufficient_history")
]
active_config <- configured_active[
  adapter != "core_metric" | metric_id %in% included_core
]

rows <- lapply(seq_len(nrow(active_config)), function(i) {
  config_row <- active_config[i]
  if (config_row$adapter == "ae4h") {
    return(performance_ae4h_row(
      config_row, national_config, next_release, reference_forecast,
      release_archive, scorecard, watchlist
    ))
  }
  if (config_row$adapter == "core_metric") {
    row <- core_rows[metric_id == config_row$metric_id[1L]]
    if (nrow(row) != 1L) {
      stop("Expected one modelled overview row for ", config_row$metric_id[1L], ".")
    }
    return(row)
  }
  stop("Unknown performance-outlook adapter: ", config_row$adapter[1L])
})
rows <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
metadata_columns <- c(
  "metric_id", "overview_group", "provider_signal_enabled",
  "benchmark_label", "data_source_label", "data_source_url"
)
rows <- merge(
  rows,
  metric_config[, ..metadata_columns],
  by = "metric_id", all.x = TRUE, sort = FALSE
)
data.table::setorder(rows, display_order)
validate_performance_outlook_rows(rows)

ae_history <- read_performance_csv(
  "data-interim/national_month_stage1.csv", "calendar_month"
)[, .(
  metric_id = "ae4h_all", calendar_month,
  value = ae4h_performance
)]
core_national_panel <- read_performance_csv(
  "data-interim/core/national_panel.csv", "calendar_month"
)
community_service_summary <- read_performance_csv(
  "data-interim/core/community_waits_service_summary.csv", "calendar_month"
)
history <- data.table::rbindlist(list(
  ae_history,
  core_national_panel[, .(metric_id, calendar_month, value)]
), use.names = TRUE, fill = TRUE)
forecast_rows <- performance_prepare_edition(rows, history, "forecast")

output_data_dir <- "output/performance"
output_release_dir <- "output/releases"
dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_release_dir, recursive = TRUE, showWarnings = FALSE)
published_snapshot_path <- file.path(
  output_data_dir, "latest_published_forecast_rows.csv"
)
if (publication_mode == "forecast") {
  # Do not let an accidental forecast-mode run replace a snapshot whose
  # observations have arrived but have not yet passed through an outturn build.
  if (file.exists(published_snapshot_path)) {
    previous_snapshot <- read_performance_csv(
      published_snapshot_path,
      c("forecast_month", "publication_issue_date")
    )
    observed_keys <- unique(history[is.finite(value), .(
      metric_id,
      forecast_month = data.table::as.IDate(calendar_month),
      observation_available = TRUE
    )])
    previous_state <- merge(
      previous_snapshot[, .(metric_id, forecast_month)],
      observed_keys,
      by = c("metric_id", "forecast_month"), all.x = TRUE, sort = FALSE
    )
    previous_has_new_observation <- nrow(previous_state) > 0L &&
      any(
        !is.na(previous_state$observation_available) &
          previous_state$observation_available == TRUE
      )
    if (previous_has_new_observation) {
      previous_manifest_path <- file.path(
        output_data_dir, "overview_manifest.csv"
      )
      previous_outturn_complete <- FALSE
      if (file.exists(previous_manifest_path)) {
        previous_manifest <- read_performance_csv(previous_manifest_path)
        previous_issue <- unique(as.character(
          previous_snapshot$publication_issue_date
        ))
        previous_outturn_complete <- nrow(previous_manifest) == 1L &&
          identical(previous_manifest$publication_mode[1L], "outturn") &&
          length(previous_issue) == 1L &&
          identical(
            as.character(previous_manifest$source_forecast_issue_date[1L]),
            previous_issue
          ) &&
          file.exists(file.path(
            "output/publication",
            as.character(previous_manifest$issue_date[1L]),
            "outturn", "publication_manifest.csv"
          ))
      }
      if (!previous_outturn_complete) {
        stop(
          "The existing published forecast snapshot now has actual observations, ",
          "but no completed outturn bundle records that comparison. Run the full ",
          "pipeline in outturn mode before issuing another forecast."
        )
      }
    }
  }
  published_snapshot <- data.table::copy(forecast_rows)
  published_snapshot[, publication_issue_date := publication_issue_date]
  data.table::fwrite(published_snapshot, published_snapshot_path)
  data.table::fwrite(
    published_snapshot,
    file.path(
      output_data_dir,
      paste0("published_forecast_rows_", issue_day, ".csv")
    )
  )
  outturn_rows <- forecast_rows[0L]
} else {
  if (!file.exists(published_snapshot_path)) {
    stop(
      "Outturn mode requires the Monday forecast snapshot at ",
      published_snapshot_path,
      ". Run one forecast-mode refresh before the release."
    )
  }
  published_snapshot <- read_performance_csv(
    published_snapshot_path,
    c(
      "latest_month", "forecast_month", "target_month", "actual_month",
      "previous_month", "publication_issue_date"
    )
  )
  outturn_rows <- performance_prepare_published_outturn(
    published_snapshot, history, require_all = TRUE
  )
  provider_count_columns <- c(
    "providers_assessed", "providers_favourable", "providers_adverse",
    "providers_no_signal"
  )
  current_provider_row <- match(outturn_rows$metric_id, forecast_rows$metric_id)
  if (anyNA(current_provider_row)) {
    stop(
      "Current provider results are missing for one or more published outturn metrics."
    )
  }
  for (column in provider_count_columns) {
    outturn_rows[, (column) := forecast_rows[[column]][current_provider_row]]
  }
  outturn_rows[, deep_dive_file := data.table::fifelse(
    metric_id == "ae4h_all",
    "ae-four-hour-outturn-latest.html",
    sub("-outlook-latest\\.html$", "-outturn-latest.html", deep_dive_file)
  )]
}

commentary_config <- if (file.exists("config/editorial_commentary.csv")) {
  data.table::fread(
    "config/editorial_commentary.csv", encoding = "UTF-8",
    colClasses = "character"
  )
} else {
  data.table::data.table()
}
editorial_commentary <- function(edition_value, month_value) {
  if (!nrow(commentary_config)) return("")
  assert_columns(
    commentary_config, c("issue_month", "edition", "commentary"),
    "editorial commentary config"
  )
  month_text <- format(as.Date(month_value), "%Y-%m")
  exact <- commentary_config[
    edition == edition_value & issue_month == month_text &
      !is.na(commentary) & nzchar(trimws(commentary))
  ]
  if (nrow(exact)) return(exact$commentary[.N])
  fallback <- commentary_config[
    edition == edition_value & issue_month == "DEFAULT" &
      !is.na(commentary) & nzchar(trimws(commentary))
  ]
  if (nrow(fallback)) fallback$commentary[.N] else ""
}

data.table::fwrite(forecast_rows, file.path(output_data_dir, "overview_metric_rows.csv"))
data.table::fwrite(outturn_rows, file.path(output_data_dir, "outturn_metric_rows.csv"))
ae_method <- read_performance_csv(
  "output/national/forecast_method.csv",
  c(
    "data_first_month", "data_through_month", "forecast_month",
    "backtest_first_target_month", "backtest_last_target_month"
  )
)
core_method_register <- read_performance_csv(
  "output/core/forecast_method_register.csv",
  c(
    "data_first_month", "data_through_month", "forecast_month",
    "backtest_first_target_month", "backtest_last_target_month"
  )
)
forecast_method_register <- data.table::rbindlist(
  list(ae_method, core_method_register), use.names = TRUE, fill = TRUE
)
forecast_method_register[, display_order___ := match(metric_id, rows$metric_id)]
data.table::setorder(forecast_method_register, display_order___)
forecast_method_register[, display_order___ := NULL]
data.table::fwrite(
  forecast_method_register,
  file.path(output_data_dir, "forecast_method_register.csv")
)

dated_filename <- NA_character_
outturn_filename <- NA_character_
if (publication_mode == "forecast") {
  dated_filename <- paste0(
    "nhs-performance-outlook-forecast-", issue_day, ".html"
  )
  dated_path <- file.path(output_release_dir, dated_filename)
  build_performance_outlook_page(
    forecast_rows, "reports/nhs_performance_outlook_template.html", dated_path,
    excluded_metrics = excluded_core$display_name,
    edition = "forecast",
    editorial_commentary = editorial_commentary(
      "forecast", max(forecast_rows$forecast_month)
    ),
    publication_issue_date = publication_issue_date,
    publication_status = publication_status
  )
  file.copy(
    dated_path,
    file.path(output_release_dir, "nhs-performance-outlook-latest.html"),
    overwrite = TRUE
  )
  file.copy(
    dated_path,
    file.path(output_release_dir, "nhs-performance-outlook-forecast-latest.html"),
    overwrite = TRUE
  )
} else {
  outturn_filename <- paste0("nhs-performance-outturn-", issue_day, ".html")
  outturn_path <- file.path(output_release_dir, outturn_filename)
  build_performance_outlook_page(
    outturn_rows, "reports/nhs_performance_outlook_template.html", outturn_path,
    excluded_metrics = excluded_core$display_name,
    edition = "outturn",
    editorial_commentary = editorial_commentary(
      "outturn", max(outturn_rows$actual_month)
    ),
    publication_issue_date = publication_issue_date,
    publication_status = publication_status
  )
  file.copy(
    outturn_path,
    file.path(output_release_dir, "nhs-performance-outturn-latest.html"),
    overwrite = TRUE
  )
}

core_targets <- read_performance_csv("config/core_targets.csv", "target_month")
if (publication_mode %in% c("forecast", "outturn")) {
  for (i in seq_len(nrow(active_config[adapter == "core_metric"]))) {
    metric_row <- active_config[adapter == "core_metric"][i]
    metric <- metric_row$metric_id[1L]
    metric_dir <- file.path("output/core", metric)
    national_next_core <- read_performance_csv(
      file.path(metric_dir, "national_next_release_forecast.csv"),
      c("data_through_month", "forecast_month")
    )
    detail_row <- if (publication_mode == "forecast") {
      forecast_rows[metric_id == metric]
    } else {
      outturn_rows[metric_id == metric]
    }
    if (publication_mode == "outturn") {
      national_next_core[, `:=`(
        data_through_month = detail_row$latest_month[1L],
        forecast_month = detail_row$forecast_month[1L],
        predicted_value = detail_row$forecast_value[1L],
        lower_80 = detail_row$lower_80[1L],
        upper_80 = detail_row$upper_80[1L],
        latest_actual_value = detail_row$latest_value[1L]
      )]
    }
    national_projection_core <- read_performance_csv(
      file.path(metric_dir, "national_reference_projection.csv"),
      c("data_through_month", "forecast_month")
    )
    forecast_components_core <- read_performance_csv(
      file.path(metric_dir, "national_all_model_forecasts.csv"),
      "forecast_month"
    )
    forecast_method_path <- file.path(metric_dir, "forecast_method.csv")
    if (!file.exists(forecast_method_path)) {
      stop("Missing forecast-method record for ", metric, ". Rerun script 15.")
    }
    forecast_method_core <- read_performance_csv(
      forecast_method_path,
      c(
        "data_first_month", "data_through_month", "forecast_month",
        "backtest_first_target_month", "backtest_last_target_month"
      )
    )
    watch_path <- file.path(metric_dir, "provider_latest_watchlist.csv")
    provider_watch_core <- if (
      file.exists(watch_path) && file.info(watch_path)$size > 0
    ) {
      read_performance_csv(
        watch_path, c("data_through_month", "forecast_month")
      )
    } else {
      core_empty_provider_watchlist()
    }
    output_file <- if (publication_mode == "forecast") {
      metric_row$deep_dive_file[1L]
    } else {
      sub(
        "-outlook-latest\\.html$", "-outturn-latest.html",
        metric_row$deep_dive_file[1L]
      )
    }
    output_path <- file.path(output_release_dir, output_file)
    build_core_metric_outlook(
      metric_row,
      detail_row,
      core_national_panel[metric_id == metric],
      national_next_core,
      national_projection_core,
      provider_watch_core,
      core_targets[metric_id == metric],
      "reports/core_metric_outlook_template.html",
      output_path,
      provider_status_note = core_model_status[
        metric_id == metric, status_reason
      ][1L],
      include_provider_page = isTRUE(metric_row$provider_signal_enabled[1L]) &&
        core_model_status[metric_id == metric, model_status][1L] == "included",
      forecast_method = forecast_method_core,
      forecast_components = forecast_components_core,
      community_service_summary = if (metric == "community_18w") {
        community_service_summary
      } else {
        data.table::data.table()
      },
      edition = publication_mode,
      publication_status = publication_status
    )
    file.copy(
      output_path,
      file.path(
        output_release_dir,
        sub(
          "-latest\\.html$", paste0("-", issue_day, ".html"),
          basename(output_path)
        )
      ),
      overwrite = TRUE
    )
  }
}

outturn_provider_watch_manifest <- data.table::data.table(
  metric_id = character(),
  display_name = character(),
  data_through_month = data.table::as.IDate(character()),
  actual_month = data.table::as.IDate(character()),
  output_file = character()
)
if (publication_mode == "outturn") {
  provider_watch_parts <- list()
  provider_watch_index <- 0L
  provider_metrics <- active_config[
    provider_signal_enabled == TRUE & metric_id %in% outturn_rows$metric_id
  ]
  ae_window_months <- parse_integer_setting(
    provider_config, "persistent_window_months", 2L
  )
  for (i in seq_len(nrow(provider_metrics))) {
    metric_row <- provider_metrics[i]
    metric <- metric_row$metric_id[1L]
    current_overview <- forecast_rows[metric_id == metric]
    actual_month <- outturn_rows[metric_id == metric, actual_month][1L]
    if (metric_row$adapter[1L] == "ae4h") {
      provider_watch <- core_normalise_ae_provider_watchlist(
        watchlist,
        higher_is_better = metric_row$higher_is_better[1L],
        window_months = ae_window_months
      )
      # Provider distribution uses the constitutional standard, not the
      # lower medium-term recovery milestone used in the national projection.
      benchmark_value <- 0.95
      benchmark_note <- metric_row$benchmark_label[1L]
      provider_status_note <- ""
    } else {
      model_status <- core_model_status[metric_id == metric, model_status][1L]
      if (is.na(model_status) || model_status != "included") next
      provider_watch_path <- file.path(
        "output/core", metric, "provider_latest_watchlist.csv"
      )
      if (!file.exists(provider_watch_path) ||
          file.info(provider_watch_path)$size <= 0) {
        stop("Missing current provider watchlist for outturn metric ", metric, ".")
      }
      provider_watch <- read_performance_csv(
        provider_watch_path, c("data_through_month", "forecast_month")
      )
      benchmark_value <- current_overview$target_value[1L]
      benchmark_note <- current_overview$target_label[1L]
      provider_status_note <- core_model_status[
        metric_id == metric, status_reason
      ][1L]
    }
    if (!nrow(provider_watch)) {
      stop("No current provider rows were available for outturn metric ", metric, ".")
    }
    watch_month <- max(
      data.table::as.IDate(provider_watch$data_through_month), na.rm = TRUE
    )
    if (!identical(
      as.character(watch_month), as.character(data.table::as.IDate(actual_month))
    )) {
      stop(
        "Provider watch for ", metric, " is stale: data through ", watch_month,
        ", but the outturn is for ", actual_month, ". Rerun from stage 1 after ",
        "the provider file is available."
      )
    }
    output_file <- sub(
      "-outlook-latest\\.html$", "-provider-watch-outturn-latest.html",
      metric_row$deep_dive_file[1L]
    )
    output_path <- file.path(output_release_dir, output_file)
    build_provider_watch_outturn(
      metric_row = metric_row,
      overview_row = current_overview,
      watchlist = provider_watch,
      benchmark_value = benchmark_value,
      benchmark_note = benchmark_note,
      template_path = "reports/provider_watch_outturn_template.html",
      output_path = output_path,
      provider_status_note = provider_status_note
    )
    file.copy(
      output_path,
      file.path(
        output_release_dir,
        sub("-latest\\.html$", paste0("-", issue_day, ".html"), output_file)
      ),
      overwrite = TRUE
    )
    provider_watch_index <- provider_watch_index + 1L
    provider_watch_parts[[provider_watch_index]] <- data.table::data.table(
      metric_id = metric,
      display_name = metric_row$display_name[1L],
      data_through_month = data.table::as.IDate(watch_month),
      actual_month = data.table::as.IDate(actual_month),
      output_file = output_file
    )
  }
  if (length(provider_watch_parts)) {
    outturn_provider_watch_manifest <- data.table::rbindlist(
      provider_watch_parts, use.names = TRUE, fill = TRUE
    )
  }
}
data.table::fwrite(
  outturn_provider_watch_manifest,
  file.path(output_data_dir, "outturn_provider_watch_manifest.csv")
)

data.table::fwrite(data.table::data.table(
  issue_date = issue_day,
  publication_mode = publication_mode,
  publication_status = publication_status,
  source_forecast_issue_date = if (publication_mode == "outturn") {
    format(as.Date(unique(outturn_rows$publication_issue_date)[1L]), "%Y-%m-%d")
  } else {
    issue_day
  },
  active_metrics = if (publication_mode == "forecast") {
    nrow(forecast_rows)
  } else {
    nrow(outturn_rows)
  },
  registered_metrics = nrow(metric_config),
  excluded_metrics = nrow(excluded_core),
  excluded_no_data = sum(excluded_core$model_status == "excluded_no_data"),
  excluded_insufficient_history = sum(
    excluded_core$model_status == "excluded_insufficient_history"
  ),
  forecast_output_file = dated_filename,
  outturn_output_file = outturn_filename,
  outturn_provider_watch_files = nrow(outturn_provider_watch_manifest)
), file.path(output_data_dir, "overview_manifest.csv"))
message(
  "NHS Performance Outlook ", publication_mode, " edition built with ",
  if (publication_mode == "forecast") nrow(forecast_rows) else nrow(outturn_rows),
  " published metrics and ",
  nrow(excluded_core), " excluded (",
  sum(excluded_core$model_status == "excluded_no_data"), " with no imported data; ",
  sum(excluded_core$model_status == "excluded_insufficient_history"),
  " with insufficient history); ",
  if (publication_mode == "outturn") {
    paste(nrow(outturn_provider_watch_manifest), "provider-watch detail file(s). Open ")
  } else {
    "Open "
  },
  if (publication_mode == "forecast") {
    "output/releases/nhs-performance-outlook-forecast-latest.html."
  } else {
    "output/releases/nhs-performance-outturn-latest.html."
  }
)
