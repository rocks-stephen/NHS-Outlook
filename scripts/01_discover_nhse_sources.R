source("R/utils.R")
source("R/scrape_nhse.R")

config <- read_key_value_config("config/acquisition.csv")
format_order <- trimws(strsplit(config$provider_format_order, ",", fixed = TRUE)[[1L]])
discovery <- discover_nhse_sources(
  main_index_url = config$main_index_url,
  user_agent = config$user_agent,
  provider_format_order = format_order
)
assert_consecutive_provider_coverage(discovery$selected_manifest, config$provider_start_month)

national_end <- discovery$selected_manifest[
  dataset_level == "national_time_series", activity_month
]
provider_end <- max(discovery$selected_manifest[
  dataset_level == "provider_monthly", activity_month
])
if (provider_end != national_end) {
  stop("Latest selected provider month (", provider_end,
       ") does not match national workbook coverage end (", national_end, ").")
}

dir.create("data-interim", recursive = TRUE, showWarnings = FALSE)
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(discovery$publication_pages, "data-interim/publication_pages.csv")
data.table::fwrite(discovery$link_inventory, "data-interim/source_link_inventory.csv")
data.table::fwrite(discovery$selected_manifest, "data-interim/source_manifest.csv")

coverage <- discovery$selected_manifest[, .(
  selected_files = .N,
  first_represented_month = min(activity_month),
  last_represented_month = max(activity_month)
), by = dataset_level]
data.table::fwrite(coverage, "output/qa/source_coverage.csv")
message(
  "Discovered ", nrow(discovery$link_inventory), " relevant file links; selected ",
  discovery$selected_manifest[dataset_level == "provider_monthly", .N],
  " provider workbooks plus one national time-series workbook."
)
