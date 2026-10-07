source("R/utils.R")
source("R/download_sources.R")

config <- read_key_value_config("config/acquisition.csv")
manifest <- data.table::fread("data-interim/source_manifest.csv", encoding = "UTF-8")
manifest[, activity_month := data.table::as.IDate(activity_month)]

include <- rep(FALSE, nrow(manifest))
if (parse_logical_strict(config$download_national)) {
  include <- include | manifest$dataset_level == "national_time_series"
}
if (parse_logical_strict(config$download_provider)) {
  include <- include | manifest$dataset_level == "provider_monthly"
}
todo <- manifest[include]
if (!nrow(todo)) stop("Acquisition configuration selected no sources for download.")

download_manifest_path <- "data-interim/downloaded_source_manifest.csv"
downloaded <- reuse_completed_download_manifest(download_manifest_path, todo)
if (is.null(downloaded)) {
  downloaded <- download_selected_sources(todo, config$user_agent)
} else {
  message(
    "Reusing ", nrow(downloaded),
    " files completed for this exact source-discovery run."
  )
}
data.table::fwrite(downloaded, download_manifest_path)

log_path <- "data-raw/nhse/download_log.csv"
old_log <- if (file.exists(log_path)) {
  data.table::fread(log_path, encoding = "UTF-8")
} else {
  data.table::data.table()
}
old_log <- normalise_download_log_types(old_log)
downloaded_log <- normalise_download_log_types(downloaded)
download_log <- unique(
  data.table::rbindlist(
    list(old_log, downloaded_log), use.names = TRUE, fill = TRUE
  ),
  by = c("source_url", "sha256")
)
download_log[, is_current := FALSE]
download_log[
  downloaded_log[, .(source_url, sha256)],
  on = .(source_url, sha256),
  is_current := TRUE
]
data.table::fwrite(download_log, log_path)
message("Downloaded or reused ", nrow(downloaded), " immutable source files.")
