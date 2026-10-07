core_month_lookup <- c(
  jan = 1L, january = 1L, feb = 2L, february = 2L,
  mar = 3L, march = 3L, apr = 4L, april = 4L,
  may = 5L, jun = 6L, june = 6L, jul = 7L, july = 7L,
  aug = 8L, august = 8L, sep = 9L, sept = 9L, september = 9L,
  oct = 10L, october = 10L, nov = 11L, november = 11L,
  dec = 12L, december = 12L
)

core_parse_activity_month <- function(x) {
  z <- collapse_space(as.character(x))
  pattern <- paste0(
    "(?i)\\b(", paste(names(core_month_lookup), collapse = "|"),
    ")[[:space:]_/-]*('?)([0-9]{2}|20[0-9]{2})\\b"
  )
  matches <- regexec(pattern, z, perl = TRUE)
  pieces <- regmatches(z, matches)
  out <- rep(as.Date(NA), length(z))
  for (i in seq_along(pieces)) {
    if (length(pieces[[i]]) < 4L) next
    month_value <- unname(core_month_lookup[[tolower(pieces[[i]][2L])]])
    year_text <- pieces[[i]][4L]
    year_value <- as.integer(year_text)
    if (nchar(year_text) == 2L) year_value <- 2000L + year_value
    # Some cancer pages label January-March by financial year, for example
    # "January 2024-25" even though the activity month is January 2025.
    # Prefer the second FY year for those three months only.
    fy_pattern <- paste0(
      "(?i)\\b(", paste(names(core_month_lookup), collapse = "|"),
      ")[[:space:]_/-]*(20[0-9]{2})[[:space:]]*[-–—][[:space:]]*([0-9]{2}|20[0-9]{2})\\b"
    )
    fy_match <- regexec(fy_pattern, z[i], perl = TRUE)
    fy_piece <- regmatches(z[i], fy_match)[[1L]]
    if (month_value <= 3L && length(fy_piece) >= 4L) {
      fy_start <- as.integer(fy_piece[3L])
      fy_end_text <- fy_piece[4L]
      fy_end <- as.integer(fy_end_text)
      if (nchar(fy_end_text) == 2L) {
        fy_end <- (fy_start %/% 100L) * 100L + fy_end
        if (fy_end < fy_start) fy_end <- fy_end + 100L
      }
      year_value <- fy_end
    }
    out[i] <- as.Date(sprintf("%04d-%02d-01", year_value, month_value))
  }
  # Some NHS England filenames put the financial year before the month, for
  # example "2025-26_January.xlsx".  The reporting month is in the second FY
  # year for January-March and the first FY year for April-December.
  reverse_pattern <- paste0(
    "(?i)\\b(20[0-9]{2})[-–—]",
    "([0-9]{2}|20[0-9]{2})[[:space:]_/-]+(",
    paste(names(core_month_lookup), collapse = "|"), ")\\b"
  )
  for (i in which(is.na(out))) {
    reverse_match <- regexec(reverse_pattern, z[i], perl = TRUE)
    reverse_piece <- regmatches(z[i], reverse_match)[[1L]]
    if (length(reverse_piece) < 4L) next
    fy_start <- as.integer(reverse_piece[2L])
    fy_end_text <- reverse_piece[3L]
    fy_end <- as.integer(fy_end_text)
    if (nchar(fy_end_text) == 2L) {
      fy_end <- (fy_start %/% 100L) * 100L + fy_end
      if (fy_end < fy_start) fy_end <- fy_end + 100L
    }
    month_value <- unname(core_month_lookup[[tolower(reverse_piece[4L])]])
    year_value <- if (month_value <= 3L) fy_end else fy_start
    out[i] <- as.Date(sprintf("%04d-%02d-01", year_value, month_value))
  }
  data.table::as.IDate(out)
}

core_parse_latest_activity_month <- function(x) {
  z <- collapse_space(as.character(x))
  pattern <- paste0(
    "(?i)\\b(", paste(names(core_month_lookup), collapse = "|"),
    ")[[:space:]_/-]*('?)([0-9]{2}|20[0-9]{2})\\b"
  )
  matches <- regmatches(z, gregexpr(pattern, z, perl = TRUE))
  out <- rep(as.Date(NA), length(z))
  for (i in seq_along(matches)) {
    if (!length(matches[[i]]) || identical(matches[[i]], "")) next
    parsed <- as.Date(core_parse_activity_month(matches[[i]]))
    parsed <- parsed[!is.na(parsed)]
    if (length(parsed)) out[i] <- max(parsed)
  }
  data.table::as.IDate(out)
}

core_page_overlaps_start <- function(link_text, source_url, start_month) {
  z <- paste(link_text, source_url)
  match <- regexec("(20[0-9]{2})[-/](?:[0-9]{2}|20[0-9]{2})", z, perl = TRUE)
  pieces <- regmatches(z, match)[[1L]]
  if (length(pieces) < 2L) return(TRUE)
  fy_start <- as.integer(pieces[2L])
  page_last <- as.Date(sprintf("%04d-03-01", fy_start + 1L))
  page_last >= as.Date(start_month)
}

core_revision_priority <- function(x) {
  data.table::fcase(
    grepl("(?i)\\bfinal\\b", x, perl = TRUE), 1L,
    grepl("(?i)corrected|revised|new ICB", x, perl = TRUE), 2L,
    grepl("(?i)provisional", x, perl = TRUE), 3L,
    default = 4L
  )
}

discover_one_core_source <- function(spec, user_agent) {
  start_month <- data.table::as.IDate(spec$start_month)
  index_links <- fetch_html_links(spec$index_url, user_agent)
  page_pattern <- trimws(spec$page_link_pattern)
  if (nzchar(page_pattern)) {
    page_candidates <- index_links[
      grepl(page_pattern, link_text, perl = TRUE) |
        grepl(page_pattern, source_url, perl = TRUE)
    ]
    page_candidates[, keep_page___ := vapply(
      seq_len(.N),
      function(i) core_page_overlaps_start(
        link_text[i], source_url[i], start_month
      ),
      logical(1)
    )]
    publication_pages <- unique(page_candidates[
      keep_page___ == TRUE,
      .(publication_page = source_url, publication_page_title = link_text)
    ])
    if (!nrow(publication_pages)) {
      stop("No publication pages matched for ", spec$metric_id, " / ", spec$dataset_role, ".")
    }
  } else {
    publication_pages <- data.table::data.table(
      publication_page = spec$index_url,
      publication_page_title = spec$metric_id
    )
  }

  fetch_publication_links <- function(pages) {
    data.table::rbindlist(lapply(seq_len(nrow(pages)), function(i) {
      out <- fetch_html_links(pages$publication_page[i], user_agent)
      out[, publication_page_title := pages$publication_page_title[i]]
      out
    }), use.names = TRUE, fill = TRUE)
  }

  # Most collections link files directly from each financial-year page. Cancer
  # releases before September 2025 add one more layer: the annual page links to
  # a month-specific Final page, which then links to the provider CSV. Keep the
  # extra crawl config-driven so an unexpected page hierarchy cannot silently
  # broaden discovery for every metric.
  links <- fetch_publication_links(publication_pages)
  child_page_pattern <- if ("child_page_link_pattern" %in% names(spec)) {
    as.character(spec$child_page_link_pattern[1L])
  } else {
    ""
  }
  if (is.na(child_page_pattern)) child_page_pattern <- ""
  child_page_pattern <- trimws(child_page_pattern)
  if (nzchar(child_page_pattern)) {
    child_candidates <- links[
      grepl(child_page_pattern, link_text, perl = TRUE) |
        grepl(child_page_pattern, source_url, perl = TRUE)
    ]
    child_candidates[, child_activity_month___ := core_parse_activity_month(
      paste(link_text, source_url)
    )]
    child_pages <- unique(child_candidates[
      is.na(child_activity_month___) | child_activity_month___ >= start_month,
      .(
        publication_page = source_url,
        # Retain the parent release title so a generic child label such as
        # "Waiting Times" still carries its reporting month into file selection.
        publication_page_title = paste(publication_page_title, link_text)
      )
    ])
    if (!nrow(child_pages)) {
      stop(
        "No child publication pages matched for ", spec$metric_id,
        " / ", spec$dataset_role, "."
      )
    }
    links <- data.table::rbindlist(
      list(links, fetch_publication_links(child_pages)),
      use.names = TRUE, fill = TRUE
    )
  }
  formats <- trimws(strsplit(spec$format_order, ",", fixed = TRUE)[[1L]])
  links[, source_format := file_extension_from_url(source_url)]
  # NHS Digital chart downloads use a content-addressed dataFile route without
  # a filename extension.  These links currently serve XLSX workbooks; retain
  # that explicit host/path rule rather than discarding a valid chart source.
  links[
    !nzchar(source_format) &
      grepl("(?i)digital[.]nhs[.]uk/.+dataFile", source_url, perl = TRUE),
    source_format := "xlsx"
  ]
  links <- unique(links[
    (
      grepl(spec$file_link_pattern, link_text, perl = TRUE) |
        grepl(spec$file_link_pattern, source_url, perl = TRUE)
    ) &
      source_format %in% formats,
    .(link_text, source_url, source_format, publication_page,
      publication_page_title)
  ], by = "source_url")
  if (!nrow(links)) {
    stop("No source files matched for ", spec$metric_id, " / ", spec$dataset_role, ".")
  }
  activity_text <- paste(
    links$link_text, links$publication_page_title, links$source_url
  )
  parsed_activity_month <- if (spec$selection_mode == "latest_time_series") {
    core_parse_latest_activity_month(activity_text)
  } else {
    core_parse_activity_month(activity_text)
  }
  links[, `:=`(
    metric_id = spec$metric_id,
    dataset_role = spec$dataset_role,
    selection_mode = spec$selection_mode,
    # Some NHS England links have generic labels such as "Full CSV data file"
    # or "Monthly Combined CSV".  The activity month is then carried by the
    # publication-page title or file URL rather than the link label itself.
    activity_month = parsed_activity_month,
    revision_label = extract_revision_label(paste(link_text, publication_page_title)),
    revision_priority = core_revision_priority(paste(link_text, publication_page_title)),
    format_priority = match(source_format, formats),
    discovered_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    selected = FALSE,
    selection_reason = NA_character_
  )]

  if (spec$selection_mode == "monthly") {
    links <- links[!is.na(activity_month) & activity_month >= start_month]
    if (!nrow(links)) {
      stop("No monthly files at or after ", start_month, " for ", spec$metric_id, ".")
    }
    data.table::setorder(
      links, activity_month, revision_priority, format_priority, source_url
    )
    links[, selected := seq_len(.N) == 1L, by = activity_month]
    links[selected == TRUE, selection_reason := paste0(
      "Preferred publication and format for ", format(activity_month, "%B %Y")
    )]
  } else if (spec$selection_mode == "period_time_series") {
    # UCR and community waiting-time pages publish workbooks containing several
    # internally dated months, commonly one workbook per financial year.  Keep
    # every matching official period file; the importer validates and combines
    # the monthly rows inside them.
    data.table::setorder(
      links, activity_month, revision_priority, format_priority, source_url
    )
    links[, `:=`(
      selected = TRUE,
      selection_reason = "Official period time-series file"
    )]
  } else if (spec$selection_mode == "latest_time_series") {
    dated <- links[!is.na(activity_month)]
    if (nrow(dated)) {
      data.table::setorder(
        dated, -activity_month, revision_priority, format_priority, source_url
      )
      chosen <- dated$source_url[1L]
    } else {
      data.table::setorder(links, revision_priority, format_priority, source_url)
      chosen <- links$source_url[1L]
    }
    links[source_url == chosen, `:=`(
      selected = TRUE,
      selection_reason = "Latest available official time-series file"
    )]
  } else {
    stop("Unknown core source selection mode: ", spec$selection_mode)
  }
  links[]
}

discover_core_sources <- function(source_config, user_agent) {
  required <- c(
    "metric_id", "dataset_role", "index_url", "page_link_pattern",
    "child_page_link_pattern",
    "file_link_pattern", "selection_mode", "format_order", "start_month",
    "required", "strict_coverage"
  )
  assert_columns(source_config, required, "core source config")
  source_config[, `:=`(
    required = parse_logical_strict(required),
    strict_coverage = parse_logical_strict(strict_coverage)
  )]
  if (anyNA(source_config$required) || anyNA(source_config$strict_coverage)) {
    stop("Core source config contains an invalid logical setting.")
  }
  failures <- list()
  parts <- lapply(seq_len(nrow(source_config)), function(i) {
    tryCatch(
      discover_one_core_source(source_config[i], user_agent),
      error = function(e) {
        if (isTRUE(source_config$required[i])) stop(e)
        failures[[length(failures) + 1L]] <<- data.table::data.table(
          metric_id = source_config$metric_id[i],
          dataset_role = source_config$dataset_role[i],
          discovery_status = "not_available",
          detail = conditionMessage(e)
        )
        message(
          source_config$metric_id[i], " / ", source_config$dataset_role[i],
          ": optional source not selected — ", conditionMessage(e)
        )
        NULL
      }
    )
  })
  inventory <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  if (!nrow(inventory)) stop("Core source discovery selected no files.")
  inventory[, source_id := sprintf(
    "%s_%s_%s_%03d",
    metric_id,
    dataset_role,
    data.table::fifelse(
      is.na(activity_month), "undated", format(activity_month, "%Y_%m")
    ),
    seq_len(.N)
  )]
  data.table::setcolorder(inventory, c(
    "source_id", "metric_id", "dataset_role", "selection_mode",
    "activity_month", "source_format", "link_text", "revision_label",
    "publication_page", "publication_page_title", "source_url",
    "discovered_at_utc", "selected", "selection_reason",
    "revision_priority", "format_priority"
  ))
  selected <- inventory[selected == TRUE]
  for (i in seq_len(nrow(source_config))) {
    spec <- source_config[i]
    z <- selected[
      metric_id == spec$metric_id & dataset_role == spec$dataset_role
    ]
    if (spec$selection_mode == "latest_time_series" && nrow(z) != 1L) {
      stop("Expected one selected time-series file for ", spec$metric_id, ".")
    }
    if (spec$selection_mode == "monthly" && anyDuplicated(z$activity_month)) {
      stop("Duplicate selected months for ", spec$metric_id, ".")
    }
  }
  data.table::setorder(inventory, metric_id, dataset_role, activity_month, source_url)
  data.table::setorder(selected, metric_id, dataset_role, activity_month)
  discovery_failures <- data.table::rbindlist(
    failures, use.names = TRUE, fill = TRUE
  )
  if (!ncol(discovery_failures)) {
    discovery_failures <- data.table::data.table(
      metric_id = character(),
      dataset_role = character(),
      discovery_status = character(),
      detail = character()
    )
  }
  list(
    link_inventory = inventory[],
    selected_manifest = selected[],
    discovery_failures = discovery_failures[]
  )
}

download_core_source_row <- function(row, user_agent, raw_root = "data-raw/nhse-core") {
  temporary <- tempfile(fileext = paste0(".", row$source_format))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  handle <- curl::new_handle(useragent = user_agent, followlocation = TRUE)
  curl::curl_download(row$source_url, temporary, quiet = TRUE, handle = handle)
  if (!file.exists(temporary) || file.info(temporary)$size <= 0) {
    stop("Downloaded source is empty: ", row$source_url)
  }
  hash <- sha256_file(temporary)
  destination_directory <- file.path(raw_root, row$metric_id, row$dataset_role)
  dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
  month_prefix <- if (is.na(row$activity_month)) {
    "undated"
  } else {
    format(row$activity_month, "%Y-%m")
  }
  source_name <- safe_url_basename(
    row$source_url, row$source_id, row$source_format
  )
  destination <- file.path(
    destination_directory,
    paste0(month_prefix, "__", substr(hash, 1L, 12L), "__", source_name)
  )
  if (!file.exists(destination) && !file.copy(temporary, destination)) {
    stop("Could not preserve downloaded source at ", destination)
  }
  row[, `:=`(
    local_path = destination,
    sha256 = hash,
    file_size_bytes = file.info(destination)$size,
    downloaded_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    is_current = TRUE
  )]
  row[]
}

normalise_core_download_result_types <- function(x) {
  out <- data.table::copy(x)
  date_columns <- intersect("activity_month", names(out))
  logical_columns <- intersect(c("selected", "is_current"), names(out))
  integer_columns <- intersect(
    c("revision_priority", "format_priority"), names(out)
  )
  numeric_columns <- intersect("file_size_bytes", names(out))
  character_columns <- setdiff(
    names(out),
    c(date_columns, logical_columns, integer_columns, numeric_columns)
  )
  for (column in date_columns) {
    data.table::set(
      out, j = column, value = data.table::as.IDate(out[[column]])
    )
  }
  for (column in logical_columns) {
    data.table::set(
      out, j = column, value = parse_logical_strict(out[[column]])
    )
  }
  for (column in integer_columns) {
    data.table::set(
      out, j = column,
      value = suppressWarnings(as.integer(out[[column]]))
    )
  }
  for (column in numeric_columns) {
    data.table::set(
      out, j = column,
      value = suppressWarnings(as.numeric(out[[column]]))
    )
  }
  for (column in character_columns) {
    value <- as.character(out[[column]])
    # Drop attributes such as openssl's hash class while preserving real NAs.
    attributes(value) <- NULL
    data.table::set(out, j = column, value = value)
  }
  out[]
}

download_core_sources <- function(selected_manifest, user_agent,
                                  raw_root = "data-raw/nhse-core",
                                  reusable_manifest = NULL) {
  if (!nrow(selected_manifest)) stop("Core source manifest is empty.")
  reusable <- if (is.null(reusable_manifest)) {
    data.table::data.table()
  } else {
    data.table::as.data.table(reusable_manifest)
  }
  reuse_required <- c(
    "metric_id", "dataset_role", "source_url", "local_path", "sha256",
    "file_size_bytes", "downloaded_at_utc"
  )
  if (!all(reuse_required %in% names(reusable))) {
    reusable <- data.table::data.table()
  }
  reused_n <- 0L
  parts <- lapply(seq_len(nrow(selected_manifest)), function(i) {
    row <- data.table::copy(selected_manifest[i])
    if (nrow(reusable)) {
      cached <- reusable[
        metric_id == row$metric_id & dataset_role == row$dataset_role &
          source_url == row$source_url & !is.na(local_path) & nzchar(local_path)
      ]
      cached <- cached[
        file.exists(local_path) & file.info(local_path)$size > 0 &
          !is.na(sha256) & nzchar(sha256)
      ]
      if (nrow(cached) > 1L) {
        stop("Multiple reusable core downloads found for ", row$source_url, ".")
      }
      if (nrow(cached) == 1L) {
        row[, `:=`(
          local_path = as.character(cached$local_path[1L]),
          sha256 = as.character(cached$sha256[1L]),
          file_size_bytes = suppressWarnings(as.numeric(cached$file_size_bytes[1L])),
          downloaded_at_utc = as.character(cached$downloaded_at_utc[1L]),
          is_current = TRUE
        )]
        reused_n <<- reused_n + 1L
        return(row[])
      }
    }
    download_core_source_row(row, user_agent, raw_root)
  })
  parts <- lapply(parts, normalise_core_download_result_types)
  out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  if (reused_n > 0L) {
    message(
      "Reused ", reused_n, " unchanged core source files; downloaded ",
      nrow(out) - reused_n, " new or corrected files."
    )
  }
  out[]
}
