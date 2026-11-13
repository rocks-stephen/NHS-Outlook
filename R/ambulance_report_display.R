# Replace the published *observed* Cat2 headline in the individual HTML report.
# Forecasts, ranges and charts remain in decimal minutes (their model units).
# The mean on the chart is relabelled without changing the plotted coordinates.
nhs_ambsys_format_detail_outturn <- function(html_path, official_minutes) {
  if (!file.exists(html_path)) stop("Missing outturn report: ", html_path)
  html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  clock <- nhs_cat2_clock(official_minutes)
  actual_card <- '(<div class="label">Actual</div><div class="value">)[0-9.]+ min(</div>)'
  if (!grepl(actual_card, html, perl = TRUE) &&
      !grepl(paste0('<div class="value">', clock, '</div>'), html, fixed = TRUE)) {
    stop("Could not find the ambulance Actual card to align with AmbSYS in ", html_path)
  }
  html <- sub(actual_card, paste0('\\1', clock, '\\2'), html, perl = TRUE)
  html <- sub('(The published result was )[0-9.]+ min',
              paste0('\\1', clock), html, perl = TRUE)
  html <- sub('(>Actual )[0-9.]+ min(</text>)',
              paste0('\\1', clock, '\\2'), html, perl = TRUE)
  writeLines(html, html_path, useBytes = TRUE)
  invisible(html_path)
}
