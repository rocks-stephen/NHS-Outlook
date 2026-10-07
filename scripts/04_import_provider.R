source("R/utils.R")
source("R/trust_mapping.R")
source("R/import_monthly_ae.R")

manifest <- data.table::fread("data-interim/downloaded_source_manifest.csv", encoding = "UTF-8")
manifest[, activity_month := data.table::as.IDate(activity_month)]
provider_sources <- manifest[
  dataset_level == "provider_monthly" & parse_logical_strict(is_current) == TRUE
]
if (!nrow(provider_sources)) stop("No current provider workbooks in the downloaded manifest.")
if (anyDuplicated(provider_sources$activity_month)) stop("Multiple current provider sources for one month.")

parts <- lapply(seq_len(nrow(provider_sources)), function(i) {
  source_row <- provider_sources[i]
  if (!file.exists(source_row$local_path)) stop("Missing provider source: ", source_row$local_path)
  raw <- read_provider_source(source_row$local_path)
  standardize_provider_month(raw, source_row)
})
provider_source <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
provider_ae4h <- provider_source[row_scope == "provider" & type1_attendances_n > 0]

dir.create("data-interim", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(provider_source, "data-interim/provider_month_source_rows.csv")
data.table::fwrite(provider_ae4h, "data-interim/provider_month_ae4h_unharmonised.csv")

mapping <- data.table::fread("config/trust_mapping.csv", encoding = "UTF-8")
approved_mapping <- mapping[review_status == "approved"]
if (nrow(approved_mapping)) {
  mapped <- apply_trust_mapping(provider_ae4h, mapping)
  data.table::fwrite(mapped, "data-interim/provider_month_ae4h_mapped.csv")
} else {
  message("No approved trust mappings: wrote the source-code panel only; do not span code changes or mergers.")
}
message(
  "Imported ", data.table::uniqueN(provider_ae4h$calendar_month),
  " provider months for the all-types metric and retained explicit missing submissions."
)
