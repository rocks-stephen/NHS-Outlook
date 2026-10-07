assert_columns <- function(x, required, object_name = deparse(substitute(x))) {
  missing <- setdiff(required, names(x))
  if (length(missing)) stop(object_name, " is missing: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

nhs_outlook_publication_mode <- function(default = NULL) {
  value <- getOption("nhs.outlook.publication_mode", default)
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    stop(
      "Set option 'nhs.outlook.publication_mode' explicitly to ",
      "'forecast' or 'outturn' before running the publication pipeline."
    )
  }
  value <- tolower(trimws(as.character(value)))
  if (!value %in% c("forecast", "outturn")) {
    stop("Option 'nhs.outlook.publication_mode' must be 'forecast' or 'outturn'.")
  }
  value
}

nhs_outlook_issue_date <- function(default = Sys.Date()) {
  value <- getOption("nhs.outlook.issue_date", default)
  if (length(value) != 1L || is.na(value)) {
    stop("Option 'nhs.outlook.issue_date' must be one date in YYYY-MM-DD form.")
  }
  parsed <- suppressWarnings(as.Date(as.character(value)))
  if (is.na(parsed)) {
    stop("Option 'nhs.outlook.issue_date' must be one date in YYYY-MM-DD form.")
  }
  data.table::as.IDate(parsed)
}

nhs_outlook_publication_status <- function(default = "pilot") {
  value <- getOption("nhs.outlook.publication_status", default)
  if (length(value) != 1L || is.na(value)) {
    stop("Option 'nhs.outlook.publication_status' must be 'pilot' or 'standard'.")
  }
  value <- tolower(trimws(as.character(value)))
  if (!value %in% c("pilot", "standard")) {
    stop("Option 'nhs.outlook.publication_status' must be 'pilot' or 'standard'.")
  }
  value
}

nhs_outlook_edition_label <- function(edition, status = "pilot") {
  edition <- match.arg(edition, c("forecast", "outturn"))
  status <- match.arg(status, c("pilot", "standard"))
  edition_text <- if (edition == "forecast") "Forecast edition" else "Outturn edition"
  if (status == "pilot") paste("Pilot", edition_text, sep = " · ") else edition_text
}

read_key_value_config <- function(path) {
  x <- data.table::fread(path, encoding = "UTF-8")
  assert_columns(x, c("key", "value"), basename(path))
  if (anyDuplicated(x$key)) stop("Duplicate configuration keys in ", path)
  stats::setNames(as.list(x$value), x$key)
}

clean_names_transparent <- function(x) {
  x <- enc2utf8(x)
  x <- sub("^\\ufeff", "", x)
  x <- tolower(trimws(x))
  x <- gsub("%", " pct ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "__dup")
}

parse_logical_strict <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(z))
  out[z %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[z %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

sha256_file <- function(path) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("Package 'openssl' is required for cross-platform SHA-256 provenance.")
  }
  size <- file.info(path)$size
  bytes <- readBin(path, what = "raw", n = size)
  as.character(openssl::sha256(bytes))
}

read_excel_matrix <- function(path, sheet) {
  x <- suppressWarnings(readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ))
  m <- as.matrix(x)
  m[is.na(m)] <- ""
  m
}

numeric_cell <- function(x) {
  z <- trimws(as.character(x))
  z[z %in% c("", "-", "NA", "N/A", "n/a", "..", "suppressed")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", z, fixed = TRUE)))
}

parse_month_cell <- function(x) {
  z <- trimws(as.character(x))
  # Official workbooks sometimes prefix a valid month with a footnote marker,
  # for example RTT labels February 2024 as "* Feb-24".  Strip only a short
  # leading marker; do not remove annotations elsewhere in the cell.
  z <- sub("^[*†‡#]+[[:space:]]*", "", z, perl = TRUE)
  out <- rep(as.Date(NA), length(z))
  # Retain a valid month token even when the cell also carries a revision or
  # footnote suffix, for example "Feb-24 (R)".  Parsing the whole annotated
  # cell would otherwise create a single artificial gap in a long series.
  token_pattern <- paste0(
    "(?i)\\b(", paste(c(month.abb, month.name), collapse = "|"),
    ")[ -]([0-9]{2}|20[0-9]{2})\\b"
  )
  token_matches <- regexec(token_pattern, z, perl = TRUE)
  token_parts <- regmatches(z, token_matches)
  for (i in seq_along(token_parts)) {
    if (length(token_parts[[i]]) != 3L) next
    month_value <- match(
      tolower(substr(token_parts[[i]][2L], 1L, 3L)),
      tolower(month.abb)
    )
    year_text <- token_parts[[i]][3L]
    year_value <- as.integer(year_text)
    if (nchar(year_text) == 2L) year_value <- 2000L + year_value
    out[i] <- as.Date(sprintf("%04d-%02d-01", year_value, month_value))
  }
  iso_prefix <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", z)
  use_iso <- is.na(out) & iso_prefix
  out[use_iso] <- as.Date(substr(z[use_iso], 1L, 10L), format = "%Y-%m-%d")
  # Historic monthly files can use an unambiguous bare YYYY-MM period label.
  # Represent it as the first day of that month, as elsewhere in the project.
  year_month <- is.na(out) & grepl("^[0-9]{4}-[0-9]{2}$", z)
  out[year_month] <- as.Date(paste0(z[year_month], "-01"), format = "%Y-%m-%d")
  formats <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%b-%y", "%B %Y")
  for (fmt in formats) {
    miss <- is.na(out) & nzchar(z)
    out[miss] <- as.Date(z[miss], format = fmt)
  }
  numeric_z <- suppressWarnings(as.numeric(z))
  use_serial <- is.na(out) & !is.na(numeric_z)
  out[use_serial] <- as.Date(numeric_z[use_serial], origin = "1899-12-30")
  data.table::as.IDate(format(out, "%Y-%m-01"))
}

locate_header_row <- function(m, required_patterns, object_name) {
  hits <- which(apply(m, 1L, function(row) {
    all(vapply(required_patterns, function(pattern) {
      any(grepl(pattern, row, ignore.case = TRUE, perl = TRUE))
    }, logical(1)))
  }))
  if (length(hits) != 1L) {
    stop("Expected one header row in ", object_name, "; found ", length(hits),
         ". Record a schema rule rather than guessing.")
  }
  hits
}

locate_one_column <- function(header, pattern, field) {
  hits <- which(grepl(pattern, header, ignore.case = TRUE, perl = TRUE))
  if (length(hits) != 1L) {
    stop("Expected one column for ", field, "; found ", length(hits), ".")
  }
  hits
}

locate_group_start <- function(m, header_row, pattern, field, required = TRUE) {
  rows <- seq.int(max(1L, header_row - 4L), header_row - 1L)
  cells <- which(apply(m[rows, , drop = FALSE], c(1L, 2L), function(z) {
    grepl(pattern, z, ignore.case = TRUE, perl = TRUE)
  }), arr.ind = TRUE)
  cols <- unique(cells[, "col"])
  if (!length(cols) && !required) return(NA_integer_)
  if (length(cols) != 1L) {
    stop("Expected one group start for ", field, "; found ", length(cols), ".")
  }
  cols
}

append_qa_flag <- function(current, condition, flag) {
  out <- current
  use <- !is.na(condition) & condition
  out[use] <- ifelse(nzchar(out[use]), paste(out[use], flag, sep = ";"), flag)
  out
}

financial_year_from_month <- function(x, separator = "-") {
  d <- as.Date(x)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  start <- ifelse(m >= 4L, y, y - 1L)
  sprintf("%04d%s%02d", start, separator, (start + 1L) %% 100L)
}
