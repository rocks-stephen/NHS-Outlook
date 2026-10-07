# Build the static website from the outputs produced by the publication pipeline.
# The homepage remains the NHS Outlook itself; web-only controls are added here.

source("R/utils.R")

project_root <- getwd()
html_source_dir <- file.path(project_root, "output", "releases")
publication_root <- file.path(project_root, "output", "publication")
site_dir <- file.path(project_root, "publication_site")
overview_manifest_path <- file.path(
  project_root, "output", "performance", "overview_manifest.csv"
)

required_directories <- c(html_source_dir, publication_root)
missing_directories <- required_directories[!dir.exists(required_directories)]
if (length(missing_directories)) {
  stop("Website inputs are missing: ", paste(missing_directories, collapse = ", "), ".")
}
if (!file.exists(overview_manifest_path)) {
  stop("Website build requires ", overview_manifest_path, ". Run script 17 first.")
}

overview_manifest <- data.table::fread(overview_manifest_path, encoding = "UTF-8")
assert_columns(
  overview_manifest,
  c("issue_date", "publication_mode", "publication_status"),
  "performance overview manifest"
)
if (nrow(overview_manifest) != 1L ||
    overview_manifest$publication_mode[1L] != "forecast") {
  stop("The public website is built from one forecast-edition overview manifest.")
}
release_date <- as.Date(overview_manifest$issue_date[1L])
if (is.na(release_date)) stop("Overview manifest contains an invalid issue date.")
if (overview_manifest$publication_status[1L] != "pilot") {
  stop("This publication is configured as a pilot but the manifest is not marked pilot.")
}
issue_day <- format(release_date, "%Y-%m-%d")
forecast_dir <- file.path(publication_root, issue_day, "forecast")
if (!dir.exists(forecast_dir)) {
  stop("Forecast publication folder not found: ", forecast_dir, ".")
}

metric_config <- data.table::fread(
  file.path(project_root, "config", "performance_metrics.csv"),
  encoding = "UTF-8"
)
metric_config[, active := parse_logical_strict(active)]
detail_tbl <- metric_config[
  active == TRUE,
  .(metric_id, display_order, label = display_name, file = deep_dive_file)
][order(display_order)]
expected_metric_ids <- c(
  "ae4h_all", "ambulance_cat2", "rtt_18w", "diagnostics_6w",
  "cancer_62d", "ucr_2h", "community_18w", "talking_therapies_6w"
)
if (nrow(detail_tbl) != 8L ||
    !identical(detail_tbl$metric_id, expected_metric_ids)) {
  stop(
    "Website publication requires the eight configured headline indicators in ",
    "their expected order; found: ", paste(detail_tbl$metric_id, collapse = ", "), "."
  )
}
if (detail_tbl[metric_id == "community_18w", file] !=
      "community-18-week-outlook-latest.html" ||
    detail_tbl[metric_id == "rtt_18w", file] !=
      "rtt-18-week-outlook-latest.html") {
  stop("Community 18-week and RTT detailed outputs are not distinctly mapped.")
}
detail_tbl[, source_file := file.path(html_source_dir, file)]
missing_details <- detail_tbl[!file.exists(source_file), file]
if (length(missing_details)) {
  stop(
    "All eight current detailed HTML forecasts are required; missing: ",
    paste(missing_details, collapse = ", "), "."
  )
}

main_html <- file.path(
  html_source_dir, "nhs-performance-outlook-forecast-latest.html"
)
if (!file.exists(main_html)) {
  stop("Current main NHS Outlook HTML was not found: ", main_html, ".")
}

dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
forecast_asset_dir <- file.path(site_dir, "forecasts")
if (dir.exists(forecast_asset_dir)) {
  unlink(forecast_asset_dir, recursive = TRUE, force = TRUE)
}
pdf_site_dir <- file.path(forecast_asset_dir, issue_day)
dir.create(pdf_site_dir, recursive = TRUE, showWarnings = FALSE)

html_to_copy <- unique(c(main_html, detail_tbl$source_file))
copied_html <- file.copy(
  html_to_copy,
  file.path(site_dir, basename(html_to_copy)),
  overwrite = TRUE
)
if (!all(copied_html)) {
  stop("One or more current publication HTML files could not be copied.")
}

pdf_sources <- list.files(
  forecast_dir, pattern = "[.]pdf$", full.names = TRUE, ignore.case = TRUE
)
if (!length(pdf_sources)) stop("The forecast publication bundle contains no PDFs.")
copied_pdf <- file.copy(
  pdf_sources,
  file.path(pdf_site_dir, basename(pdf_sources)),
  overwrite = TRUE
)
if (!all(copied_pdf)) stop("One or more forecast PDFs could not be copied.")

main_pdf <- pdf_sources[grepl(
  "^nhs-performance-outlook-forecast-.*[.]pdf$",
  basename(pdf_sources), ignore.case = TRUE
)]
if (!length(main_pdf)) {
  stop("The publication bundle has no main NHS Performance Outlook PDF.")
}
dated_main_pdf <- main_pdf[grepl(issue_day, basename(main_pdf), fixed = TRUE)]
main_pdf <- if (length(dated_main_pdf)) dated_main_pdf[1L] else main_pdf[1L]
main_pdf_path <- gsub(
  "\\\\", "/",
  file.path("forecasts", issue_day, basename(main_pdf))
)

html <- paste(
  readLines(main_html, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
if (!grepl("Pilot", html, fixed = TRUE)) {
  stop("Main NHS Outlook HTML is not marked Pilot; rebuild script 17.")
}
if (!grepl('<th class="detail-head">Detail</th>', html, fixed = TRUE)) {
  stop("Main NHS Outlook HTML does not contain the web-only Detail column.")
}
for (detail_file in detail_tbl$file) {
  link <- paste0('href="', detail_file, '"')
  if (!grepl(link, html, fixed = TRUE)) {
    stop("Main NHS Outlook is missing its Detail link to ", detail_file, ".")
  }
}

extra_css <- paste0(
  "\n    .outlook-actions{display:flex;gap:10px;align-items:center;",
  "margin:13px 0 2px;flex-wrap:wrap}",
  ".button-link{display:inline-block;padding:8px 12px;border:1px solid ",
  "var(--brand);border-radius:3px;color:var(--brand);text-decoration:none;",
  "font-size:9px;font-weight:800;letter-spacing:.04em}",
  ".button-link.primary{background:var(--brand);color:#fff}",
  "@media print{.outlook-actions{display:none!important}}\n"
)
html <- sub("</style>", paste0(extra_css, "</style>"), html, fixed = TRUE)
actions <- paste0(
  '<div class="outlook-actions">',
  '<a class="button-link primary" href="', main_pdf_path,
  '">Download this edition as PDF</a></div>'
)
html <- sub("</table>", paste0("</table>\n", actions), html, fixed = TRUE)

if (grepl('id="detailed-forecasts"', html, fixed = TRUE)) {
  stop("The homepage must not contain a second Detailed forecasts section.")
}
index_file <- file.path(site_dir, "index.html")
writeLines(html, index_file, useBytes = TRUE)
file.create(file.path(site_dir, ".nojekyll"), showWarnings = FALSE)

website_manifest <- data.table::rbindlist(list(
  data.table::data.table(
    artifact_role = "homepage", metric_id = NA_character_,
    relative_path = "index.html"
  ),
  detail_tbl[, .(
    artifact_role = "indicator_detail", metric_id,
    relative_path = file
  )],
  data.table::data.table(
    artifact_role = "pdf", metric_id = NA_character_,
    relative_path = gsub(
      "\\\\", "/",
      file.path("forecasts", issue_day, basename(pdf_sources))
    )
  )
), use.names = TRUE, fill = TRUE)
website_manifest[, `:=`(
  issue_date = issue_day,
  exists = file.exists(file.path(site_dir, relative_path))
)]
data.table::fwrite(
  website_manifest, file.path(site_dir, "website_manifest.csv")
)
if (any(!website_manifest$exists)) {
  stop("Website manifest contains one or more missing files.")
}

message(
  "NHS Outlook website built at ", index_file, " with eight detailed HTML ",
  "links and ", length(pdf_sources), " PDF(s) under forecasts/", issue_day, "/."
)
if (isTRUE(getOption("nhs.outlook.open_website", interactive()))) {
  utils::browseURL(normalizePath(index_file, winslash = "/"))
}
