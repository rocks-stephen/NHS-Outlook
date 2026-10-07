source("R/utils.R")
source("R/national_forecast.R")
source("R/provider_plots.R")

config <- read_key_value_config("config/provider_model.csv")
next_release <- data.table::fread(
  "output/provider/next_release_forecast.csv", encoding = "UTF-8"
)
next_release[, forecast_month := data.table::as.IDate(forecast_month)]
watchlist <- data.table::fread(
  "output/provider/latest_watchlist.csv", encoding = "UTF-8"
)
watchlist[, `:=`(
  data_through_month = data.table::as.IDate(data_through_month),
  forecast_month = data.table::as.IDate(forecast_month)
)]
deviations <- data.table::fread(
  "output/provider/deviation_history.csv", encoding = "UTF-8"
)
deviations[, `:=`(
  origin_month = data.table::as.IDate(origin_month),
  target_month = data.table::as.IDate(target_month)
)]
comparison <- data.table::fread(
  "output/provider/model_comparison.csv", encoding = "UTF-8"
)

dir.create("output/provider/charts", recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  "output/provider/charts/provider_next_release_extremes.png",
  plot_provider_next_release_extremes(next_release, config),
  width = 10, height = 8, dpi = 300
)
ggplot2::ggsave(
  "output/provider/charts/provider_next_release_distribution.png",
  plot_provider_forecast_distribution(next_release, config),
  width = 8, height = 5.5, dpi = 300
)
ggplot2::ggsave(
  "output/provider/charts/provider_sustained_deviations.png",
  plot_provider_sustained_deviations(watchlist, config),
  width = 10, height = 8, dpi = 300
)
ggplot2::ggsave(
  "output/provider/charts/provider_flagged_trajectories.png",
  plot_provider_flagged_trajectories(deviations, watchlist, config),
  width = 12, height = 9, dpi = 300
)
ggplot2::ggsave(
  "output/provider/charts/provider_model_accuracy.png",
  plot_provider_model_accuracy(comparison, config),
  width = 8, height = 5.5, dpi = 300
)
message("Provider charts written to output/provider/charts/.")
