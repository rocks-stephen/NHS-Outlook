safe_url_basename <- function(url, fallback, extension) {
  path <- URLdecode(sub("[?#].*$", "", url))
  value <- basename(path)
  value <- gsub("[^A-Za-z0-9._-]+", "_", value)
  if (!nzchar(value) || value %in% c(".", "..")) value <- paste0(fallback, ".", extension)
  # NHS Digital dataFile links are often opaque identifiers with no suffix.
  # Preserve a usable local filename by adding the format established during
  # discovery; otherwise downstream readers cannot distinguish xlsx from csv.
  if (!nzchar(tools::file_ext(value)) && nzchar(extension)) {
    value <- paste0(value, ".", tolower(extension))
  }
  value
}

normalise_download_log_types <- function(x) {
  if (is.null(x)) return(data.table::data.table())
  out <- data.table::copy(x)
  logical_columns <- intersect(c("selected", "is_current"), names(out))
  numeric_columns <- intersect("file_size_bytes", names(out))

  # fread may restore ISO dates/timestamps or long hashes with different class
  # attributes from those present on freshly discovered rows. Download logs are
  # audit metadata, so store all non-logical/non-size fields as plain character
  # before appending. This keeps the manifest's typed activity_month unchanged.
  for (column in names(out)) {
    if (column %in% logical_columns) {
      data.table::set(out, j = column, value = parse_logical_strict(out[[column]]))
    } else if (column %in% numeric_columns) {
      data.table::set(
        out, j = column,
        value = suppressWarnings(as.numeric(out[[column]]))
      )
    } else {
      data.table::set(out, j = column, value = as.character(out[[column]]))
    }
  }
  out[]
}

reuse_completed_download_manifest <- function(path, selected_manifest) {
  if (!file.exists(path)) return(NULL)
  cached <- data.table::fread(
    path, encoding = "UTF-8", colClasses = "character"
  )
  identity_columns <- c(
    "source_id", "source_url", "discovered_at_utc"
  )
  required_cached <- c(identity_columns, "local_path", "sha256")
  if (!all(required_cached %in% names(cached)) ||
      !all(identity_columns %in% names(selected_manifest))) {
    return(NULL)
  }
  make_key <- function(x) do.call(
    paste, c(lapply(identity_columns, function(column) {
      as.character(x[[column]])
    }), sep = "\r")
  )
  same_discovery <- nrow(cached) == nrow(selected_manifest) && identical(
    sort(make_key(cached)), sort(make_key(selected_manifest))
  )
  local_files_ok <- nrow(cached) > 0L && all(
    !is.na(cached$local_path) & nzchar(cached$local_path) &
      file.exists(cached$local_path) & file.info(cached$local_path)$size > 0
  )
  if (!same_discovery || !local_files_ok) return(NULL)
  cached[]
}

download_source_row <- function(row, user_agent, raw_root = "data-raw/nhse") {
  temporary <- tempfile(fileext = paste0(".", row$source_format))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  handle <- curl::new_handle(useragent = user_agent, followlocation = TRUE)
  curl::curl_download(row$source_url, temporary, quiet = TRUE, handle = handle)
  if (!file.exists(temporary) || file.info(temporary)$size <= 0) {
    stop("Downloaded source is empty: ", row$source_url)
  }
  hash <- sha256_file(temporary)
  if (row$dataset_level == "national_time_series") {
    destination_directory <- file.path(raw_root, "national")
    month_prefix <- format(row$activity_month, "%Y-%m")
  } else {
    destination_directory <- file.path(raw_root, "provider_monthly", row$financial_year)
    month_prefix <- format(row$activity_month, "%Y-%m")
  }
  dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
  source_name <- safe_url_basename(row$source_url, row$source_id, row$source_format)
  destination <- file.path(
    destination_directory,
    paste0(month_prefix, "__", substr(hash, 1L, 12L), "__", source_name)
  )
  if (!file.exists(destination)) {
    copied <- file.copy(temporary, destination, overwrite = FALSE)
    if (!copied) stop("Could not preserve downloaded source at ", destination)
  }
  row[, `:=`(
    local_path = destination,
    sha256 = hash,
    file_size_bytes = file.info(destination)$size,
    downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    is_current = TRUE
  )]
  row[]
}

download_selected_sources <- function(selected_manifest, user_agent,
                                      raw_root = "data-raw/nhse") {
  parts <- lapply(seq_len(nrow(selected_manifest)), function(i) {
    download_source_row(data.table::copy(selected_manifest[i]), user_agent, raw_root)
  })
  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}
