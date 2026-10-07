publication_safe_slug <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z <- gsub("[^a-z0-9]+", "-", z)
  gsub("(^-|-$)", "", z)
}

publication_render_pdf <- function(html_path, pdf_path) {
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop(
      "PDF export requires the pagedown package. Run ",
      "source('scripts/00_bootstrap_renv.R') and renv::snapshot()."
    )
  }
  if (!file.exists(html_path)) stop("Cannot render missing HTML: ", html_path)
  dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)
  # Chrome's overwrite behaviour differs by platform.  Remove only the exact
  # generated destination so a rerun cannot leave an earlier PDF in place.
  if (file.exists(pdf_path) && unlink(pdf_path) != 0L) {
    stop("Could not replace existing PDF: ", pdf_path, ". Close it and rerun.")
  }
  pagedown::chrome_print(
    input = normalizePath(html_path, winslash = "/", mustWork = TRUE),
    output = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
    wait = 2
  )
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0) {
    stop("PDF renderer did not create ", pdf_path, ".")
  }
  invisible(pdf_path)
}

publication_markdown_table <- function(rows) {
  rows <- data.table::copy(rows)
  data.table::setorder(rows, display_order)
  lines <- c(
    "| Indicator | Latest | Outlook | Actual |",
    "|---|---:|---:|---:|"
  )
  body <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i]
    actual <- if (
      "actual_value" %in% names(row) && !is.na(row$actual_value)
    ) performance_format_value(row$actual_value, row$unit, row$digits) else "—"
    paste0(
      "| ", row$display_name, " | ",
      performance_format_value(row$latest_value, row$unit, row$digits), " | ",
      "**", performance_format_value(row$forecast_value, row$unit, row$digits),
      "** | ", actual, " |"
    )
  }, character(1))
  paste(c(lines, body), collapse = "\n")
}

publication_download_list <- function(manifest) {
  z <- manifest[file_type == "pdf"]
  if (!nrow(z)) return("")
  paste(vapply(seq_len(nrow(z)), function(i) paste0(
    "- [", z$title[i], "](", basename(z$output_file[i]), ")"
  ), character(1)), collapse = "\n")
}

build_substack_draft <- function(rows, manifest, edition, output_path) {
  if (!nrow(rows)) return(invisible(NULL))
  commentary <- performance_outlook_commentary(rows, edition)
  target_months <- unique(format(as.Date(rows$forecast_month), "%B %Y"))
  period <- if (length(target_months) == 1L) target_months else {
    format(Sys.Date(), "%B %Y")
  }
  title <- if (edition == "forecast") {
    paste0("NHS Outlook: what to expect in the next release — ", period)
  } else {
    paste0("NHS Outlook: what the latest release showed — ", period)
  }
  subtitle <- if (edition == "forecast") {
    paste0(
      "A release-ahead view of ", nrow(rows),
      " prominent NHS performance indicators."
    )
  } else {
    "The published results compared with forecasts made before release."
  }
  edition_note <- if (edition == "forecast") c(
    "## What to watch",
    "",
    "The detailed PDFs separate providers’ latest performance levels from sustained movement against their own archived trajectories. A trajectory signal requires six consecutive release surprises, at least five in the same direction, a material average gap and the latest month confirming the direction.",
    ""
  ) else c(
    "## Reading the update",
    "",
    "Each result is compared with the exact forecast published before release. A result can be better or worse than the point forecast while still lying inside the 80% expected range. The provider-watch PDFs use the newly released observations to update the provider distribution and six-release trajectory screen.",
    ""
  )
  text <- c(
    paste0("# ", title),
    "",
    paste0("*", subtitle, "*"),
    "",
    commentary,
    "",
    publication_markdown_table(rows),
    "",
    edition_note,
    "## Downloads",
    "",
    publication_download_list(manifest),
    "",
    "---",
    "",
    "*ROCKS / HEALTH is independent analysis by Stephen Rocks and is not an NHS publication. Forecasts are descriptive, uncertain and should be read alongside the source and methodology notes in each PDF.*"
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, output_path, useBytes = TRUE)
  invisible(output_path)
}
