collapse_space <- function(x) gsub("[[:space:]]+", " ", trimws(x))

fetch_html_links <- function(page_url, user_agent) {
  handle <- curl::new_handle(useragent = user_agent, followlocation = TRUE)
  response <- curl::curl_fetch_memory(page_url, handle = handle)
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop("HTTP ", response$status_code, " while reading ", page_url)
  }
  document <- xml2::read_html(rawToChar(response$content))
  anchors <- xml2::xml_find_all(document, ".//a[@href]")
  out <- data.table::data.table(
    link_text = collapse_space(xml2::xml_text(anchors)),
    source_url = xml2::url_absolute(xml2::xml_attr(anchors, "href"), page_url),
    publication_page = page_url
  )
  unique(out[nzchar(source_url)])
}

file_extension_from_url <- function(x) {
  path <- sub("[?#].*$", "", x)
  tolower(tools::file_ext(path))
}

parse_named_month <- function(x) {
  pattern <- paste0("(?i)\\b(", paste(month.name, collapse = "|"), ")\\s+(20[0-9]{2})\\b")
  matches <- regexec(pattern, x, perl = TRUE)
  pieces <- regmatches(x, matches)
  out <- rep(as.Date(NA), length(x))
  for (i in seq_along(pieces)) {
    if (length(pieces[[i]]) == 3L) {
      month_number <- base::match(tolower(pieces[[i]][2L]), tolower(month.name))
      out[i] <- as.Date(sprintf("%s-%02d-01", pieces[[i]][3L], month_number))
    }
  }
  data.table::as.IDate(out)
}

extract_revision_label <- function(x) {
  pattern <- "(?i)revis(?:ed|ion)[^)]*"
  hit <- regexpr(pattern, x, perl = TRUE)
  out <- rep("as_published_on_page", length(x))
  has_hit <- hit > 0L
  out[has_hit] <- regmatches(x, hit)[has_hit]
  out
}

discover_nhse_sources <- function(main_index_url, user_agent,
                                  provider_format_order = c("xls", "xlsx")) {
  discovered_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  main_links <- fetch_html_links(main_index_url, user_agent)

  page_pattern <- "(?i)^Monthly A&E Attendances and Emergency Admissions 20[0-9]{2}-[0-9]{2}$"
  pages <- unique(main_links[grepl(page_pattern, link_text, perl = TRUE), .(
    financial_year = sub(".*(20[0-9]{2}-[0-9]{2}).*", "\\1", link_text),
    publication_page_title = link_text,
    publication_page = source_url
  )])
  if (!nrow(pages)) stop("No financial-year publication pages were found on the NHS England index.")

  national <- main_links[
    grepl("(?i)Monthly\\s+A&E\\s+Time\\s+Series", link_text, perl = TRUE)
  ]
  national[, source_format := file_extension_from_url(source_url)]
  national <- national[source_format %in% c("xls", "xlsx")]
  if (!nrow(national)) stop("No monthly England A&E time-series workbook was found.")
  national[, `:=`(
    dataset_level = "national_time_series",
    activity_month = parse_named_month(link_text),
    financial_year = NA_character_,
    revision_label = extract_revision_label(link_text),
    discovered_at_utc = discovered_at
  )]

  provider_parts <- lapply(seq_len(nrow(pages)), function(i) {
    links <- fetch_html_links(pages$publication_page[i], user_agent)
    links[, `:=`(
      publication_page_title = pages$publication_page_title[i],
      financial_year = pages$financial_year[i]
    )]
    links
  })
  provider <- data.table::rbindlist(provider_parts, use.names = TRUE, fill = TRUE)
  provider[, source_format := file_extension_from_url(source_url)]
  provider <- provider[
    grepl("(?i)^Monthly\\s+A(?:&|\\s+and\\s+)E\\s+", link_text, perl = TRUE) &
      source_format %in% c("csv", "xls", "xlsx")
  ]
  provider[, activity_month := parse_named_month(link_text)]
  provider <- provider[!is.na(activity_month)]
  provider[, `:=`(
    dataset_level = "provider_monthly",
    revision_label = extract_revision_label(link_text),
    discovered_at_utc = discovered_at
  )]

  inventory <- data.table::rbindlist(list(
    national[, .(dataset_level, activity_month, financial_year, source_format,
                 link_text, revision_label, publication_page, source_url, discovered_at_utc)],
    provider[, .(dataset_level, activity_month, financial_year, source_format,
                 link_text, revision_label, publication_page, source_url, discovered_at_utc)]
  ), use.names = TRUE, fill = TRUE)
  inventory <- unique(inventory, by = c("dataset_level", "activity_month", "source_url"))
  inventory[, `:=`(selected = FALSE, selection_reason = NA_character_)]

  national_rows <- inventory[dataset_level == "national_time_series"]
  data.table::setorder(national_rows, -activity_month)
  national_choice <- national_rows[1L, source_url]
  inventory[source_url == national_choice & dataset_level == "national_time_series", `:=`(
    selected = TRUE,
    selection_reason = "Latest England monthly performance workbook linked from the index"
  )]

  provider_rows <- inventory[dataset_level == "provider_monthly"]
  provider_rows[, format_priority := match(source_format, provider_format_order)]
  provider_rows <- provider_rows[!is.na(format_priority)]
  data.table::setorder(provider_rows, activity_month, format_priority, source_url)
  provider_rows[, chosen := seq_len(.N) == 1L, by = activity_month]
  provider_choices <- provider_rows[chosen == TRUE, source_url]
  inventory[source_url %in% provider_choices & dataset_level == "provider_monthly", `:=`(
    selected = TRUE,
    selection_reason = paste0(
      "Workbook selected (", paste(provider_format_order, collapse = " > "),
      ") because it preserves explicit missing/suppressed performance cells"
    )
  )]

  inventory[, source_id := sprintf(
    "%s_%s_%s",
    dataset_level,
    ifelse(is.na(activity_month), "unknown", format(activity_month, "%Y_%m")),
    seq_len(.N)
  )]
  data.table::setcolorder(inventory, c(
    "source_id", "dataset_level", "activity_month", "financial_year", "source_format",
    "link_text", "revision_label", "publication_page", "source_url", "discovered_at_utc",
    "selected", "selection_reason"
  ))
  data.table::setorder(inventory, dataset_level, activity_month, source_format, source_url)

  selected <- inventory[selected == TRUE]
  if (selected[dataset_level == "national_time_series", .N] != 1L) {
    stop("Source selection did not produce exactly one national workbook.")
  }
  if (anyDuplicated(selected[dataset_level == "provider_monthly", activity_month])) {
    stop("Source selection produced duplicate provider months.")
  }
  list(publication_pages = pages, link_inventory = inventory, selected_manifest = selected)
}

assert_consecutive_provider_coverage <- function(selected_manifest, expected_start) {
  months <- sort(unique(selected_manifest[dataset_level == "provider_monthly", activity_month]))
  if (!length(months)) stop("No selected provider months.")
  expected <- data.table::as.IDate(seq(as.Date(expected_start), as.Date(max(months)), by = "month"))
  missing <- setdiff(expected, months)
  if (length(missing)) stop("Missing selected provider months: ", paste(missing, collapse = ", "))
  if (min(months) != data.table::as.IDate(expected_start)) {
    stop("Provider coverage starts at ", min(months), ", expected ", expected_start, ".")
  }
  invisible(TRUE)
}
