source("R/utils.R")
source("R/national_forecast.R")
source("R/national_plots.R")

config <- read_key_value_config("config/national_model.csv")
national <- data.table::fread("data-interim/national_month_stage1.csv", encoding = "UTF-8")
national[, calendar_month := data.table::as.IDate(calendar_month)]
forecast <- data.table::fread("output/national/reference_forecast.csv", encoding = "UTF-8")
forecast[, forecast_month := data.table::as.IDate(forecast_month)]
all_forecasts <- data.table::fread(
  "output/national/final_forecasts_all_models.csv", encoding = "UTF-8"
)
all_forecasts[, forecast_month := data.table::as.IDate(forecast_month)]
comparison <- data.table::fread(
  "output/national/model_comparison_pooled.csv", encoding = "UTF-8"
)
rolling <- data.table::fread(
  "output/national/rolling_predictions.csv", encoding = "UTF-8"
)
rolling[, `:=`(
  origin_month = data.table::as.IDate(origin_month),
  target_month = data.table::as.IDate(target_month)
)]
deviations <- data.table::fread("output/national/deviation_monitor.csv", encoding = "UTF-8")
deviations[, target_month := data.table::as.IDate(target_month)]
next_release <- data.table::fread(
  "output/national/next_release_forecast.csv", encoding = "UTF-8"
)
next_release[, `:=`(
  data_through_month = data.table::as.IDate(data_through_month),
  forecast_month = data.table::as.IDate(forecast_month)
)]

dir.create("output/national/charts", recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  "output/national/charts/national_history_forecast.png",
  plot_national_history_forecast(national, forecast, config),
  width = 10, height = 6, dpi = 300
)
ggplot2::ggsave(
  "output/national/charts/national_model_accuracy.png",
  plot_national_model_accuracy(comparison),
  width = 8, height = 5.5, dpi = 300
)
ggplot2::ggsave(
  "output/national/charts/national_deviations.png",
  plot_national_deviations(deviations, config),
  width = 10, height = 5.5, dpi = 300
)
ggplot2::ggsave(
  "output/national/charts/national_next_release_forecast.png",
  plot_national_next_release(national, next_release, config),
  width = 9, height = 5.5, dpi = 300
)
ggplot2::ggsave(
  "output/national/charts/national_one_step_track_record.png",
  plot_national_one_step_track_record(rolling, config),
  width = 9, height = 5.5, dpi = 300
)
ggplot2::ggsave(
  "output/national/charts/national_reference_components.png",
  plot_national_reference_components(national, all_forecasts, config),
  width = 10, height = 6, dpi = 300
)
message("National charts written to output/national/charts/.")
