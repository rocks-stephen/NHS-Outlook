source("R/utils.R")
source("R/download_sources.R")
source("R/core_sources.R")

manifest_path <- "data-interim/core/source_manifest.csv"
if (!file.exists(manifest_path)) {
  stop("Run scripts/12_discover_core_sources.R first.")
}
manifest <- data.table::fread(manifest_path, encoding = "UTF-8")
manifest[, activity_month := data.table::as.IDate(activity_month)]
acquisition_config <- read_key_value_config("config/acquisition.csv")
download_manifest_path <- "data-interim/core/downloaded_source_manifest.csv"
allow_partial_reuse <- isTRUE(getOption(
  "ae.core.reuse_unchanged_downloads", FALSE
))
options(ae.core.reuse_unchanged_downloads = NULL)
downloaded <- reuse_completed_download_manifest(download_manifest_path, manifest)
if (is.null(downloaded)) {
  reusable_manifest <- if (allow_partial_reuse && file.exists(download_manifest_path)) {
    data.table::fread(
      download_manifest_path, encoding = "UTF-8", colClasses = "character"
    )
  } else {
    data.table::data.table()
  }
  downloaded <- download_core_sources(
    manifest,
    acquisition_config$user_agent,
    reusable_manifest = reusable_manifest
  )
} else {
  message(
    "Reusing ", nrow(downloaded),
    " files completed for this exact core source-discovery run."
  )
}

dir.create("data-interim/core", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(
  downloaded, download_manifest_path
)
log_path <- "data-raw/nhse-core/download_log.csv"
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
  by = c("metric_id", "dataset_role", "source_url", "sha256")
)
download_log[, is_current := FALSE]
download_log[
  downloaded_log[, .(metric_id, dataset_role, source_url, sha256)],
  on = .(metric_id, dataset_role, source_url, sha256),
  is_current := TRUE
]
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(download_log, log_path)
message("Downloaded or reused ", nrow(downloaded), " immutable core source files.")
