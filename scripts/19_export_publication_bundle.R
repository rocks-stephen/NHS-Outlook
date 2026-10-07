source("R/utils.R")
source("R/performance_outlook.R")
source("R/publication_export.R")

required <- "output/performance/overview_manifest.csv"
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(
    "Cannot export the publication bundle; missing: ",
    paste(missing, collapse = ", "), ". Run script 17 first."
  )
}

overview_manifest <- data.table::fread(
  "output/performance/overview_manifest.csv", encoding = "UTF-8"
)
assert_columns(
  overview_manifest, c("issue_date", "publication_mode", "publication_status"),
  "performance overview manifest"
)
issue_day <- overview_manifest$issue_date[1L]
publication_mode <- overview_manifest$publication_mode[1L]
publication_status <- overview_manifest$publication_status[1L]
if (!publication_mode %in% c("forecast", "outturn")) {
  stop("Overview manifest has an invalid publication mode: ", publication_mode, ".")
}
mode_required <- if (publication_mode == "forecast") {
  c(
    "output/performance/overview_metric_rows.csv",
    "output/releases/nhs-performance-outlook-forecast-latest.html"
  )
} else {
  c(
    "output/performance/outturn_metric_rows.csv",
    "output/performance/outturn_provider_watch_manifest.csv",
    "output/releases/nhs-performance-outturn-latest.html"
  )
}
missing <- mode_required[!file.exists(mode_required)]
if (length(missing)) {
  stop(
    "Cannot export the ", publication_mode, " publication bundle; missing: ",
    paste(missing, collapse = ", "), ". Run script 17 first."
  )
}
bundle_dir <- file.path("output/publication", issue_day, publication_mode)
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

if (publication_mode == "forecast") {
  # Export only deep dives referenced by this forecast edition. Scanning every
  # *-outlook-latest.html file could republish a stale excluded indicator.
  current_metric_rows <- data.table::fread(
    "output/performance/overview_metric_rows.csv", encoding = "UTF-8"
  )
  assert_columns(
    current_metric_rows, c("metric_id", "deep_dive_file"),
    "current performance outlook rows"
  )
  deep_dive_html <- file.path(
    "output/releases", unique(current_metric_rows$deep_dive_file)
  )
  missing_deep_dives <- deep_dive_html[!file.exists(deep_dive_html)]
  if (length(missing_deep_dives)) {
    stop(
      "Current publication rows reference missing deep dives: ",
      paste(missing_deep_dives, collapse = ", "),
      ". Rerun script 17 before exporting."
    )
  }
  html_files <- unique(c(
    "output/releases/nhs-performance-outlook-forecast-latest.html",
    deep_dive_html
  ))
} else {
  provider_watch_manifest <- data.table::fread(
    "output/performance/outturn_provider_watch_manifest.csv", encoding = "UTF-8"
  )
  assert_columns(
    provider_watch_manifest,
    c("metric_id", "data_through_month", "actual_month", "output_file"),
    "outturn provider-watch manifest"
  )
  if (!nrow(provider_watch_manifest)) {
    stop("The outturn bundle contains no refreshed provider-watch detail files.")
  }
  provider_watch_html <- file.path(
    "output/releases", unique(provider_watch_manifest$output_file)
  )
  missing_provider_watch <- provider_watch_html[!file.exists(provider_watch_html)]
  if (length(missing_provider_watch)) {
    stop(
      "Outturn provider-watch manifest references missing files: ",
      paste(missing_provider_watch, collapse = ", "), "."
    )
  }
  html_files <- unique(c(
    "output/releases/nhs-performance-outturn-latest.html",
    provider_watch_html
  ))
}

manifest_parts <- lapply(html_files, function(html_path) {
  base <- sub("[.]html$", "", basename(html_path))
  base <- sub("-latest$", "", base)
  if (publication_mode == "forecast" && !grepl("forecast", base, fixed = TRUE)) {
    base <- paste0(base, "-forecast")
  }
  pdf_name <- paste0(base, "-", issue_day, ".pdf")
  pdf_path <- file.path(bundle_dir, pdf_name)
  publication_render_pdf(html_path, pdf_path)
  title <- gsub("-", " ", base)
  title <- paste0(toupper(substr(title, 1L, 1L)), substr(title, 2L, nchar(title)))
  data.table::data.table(
    issue_date = issue_day,
    publication_mode = publication_mode,
    publication_status = publication_status,
    artifact_role = if (grepl("nhs-performance-", basename(html_path))) {
      "overview"
    } else if (grepl("provider-watch", basename(html_path), fixed = TRUE)) {
      "provider_watch"
    } else {
      "indicator_deep_dive"
    },
    title = title,
    edition = publication_mode,
    file_type = "pdf",
    source_html = html_path,
    output_file = pdf_path,
    bytes = file.info(pdf_path)$size
  )
})
publication_manifest <- data.table::rbindlist(
  manifest_parts, use.names = TRUE, fill = TRUE
)
data.table::fwrite(
  publication_manifest, file.path(bundle_dir, "publication_manifest.csv")
)

read_rows <- function(path) {
  x <- data.table::fread(path, encoding = "UTF-8")
  for (column in intersect(
    c("latest_month", "forecast_month", "actual_month"), names(x)
  )) x[, (column) := data.table::as.IDate(as.character(get(column)))]
  x[]
}
if (publication_mode == "forecast") {
  forecast_rows <- read_rows("output/performance/overview_metric_rows.csv")
  build_substack_draft(
    forecast_rows,
    publication_manifest,
    "forecast",
    file.path(bundle_dir, paste0("substack-forecast-", issue_day, ".md"))
  )
} else {
  outturn_rows <- read_rows("output/performance/outturn_metric_rows.csv")
  build_substack_draft(
    outturn_rows,
    publication_manifest,
    "outturn",
    file.path(bundle_dir, paste0("substack-outturn-", issue_day, ".md"))
  )
}

message(
  tools::toTitleCase(publication_mode), " publication bundle exported to ",
  bundle_dir, ": ",
  nrow(publication_manifest), " portrait PDF(s) plus Substack draft(s)."
)
