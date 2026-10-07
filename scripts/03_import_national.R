source("R/utils.R")
source("R/import_national_ae.R")

manifest <- data.table::fread("data-interim/downloaded_source_manifest.csv", encoding = "UTF-8")
manifest[, activity_month := data.table::as.IDate(activity_month)]
national_source <- manifest[dataset_level == "national_time_series" &
                              parse_logical_strict(is_current) == TRUE]
if (nrow(national_source) != 1L) stop("Expected exactly one current national time-series workbook.")
if (!file.exists(national_source$local_path)) stop("Missing national source: ", national_source$local_path)

national_month <- read_national_performance(national_source$local_path, national_source)
dir.create("data-interim", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(national_month, "data-interim/national_month_stage1.csv")
message(
  "Imported ", nrow(national_month), " national months from ",
  min(national_month$calendar_month), " to ", max(national_month$calendar_month), "."
)
