# NHS Outlook: four-data-column outturn grid.
# Preserves the original pre-release forecast and compares it with the actual,
# while showing the CURRENTLY refreshed target outlook and probability.
# Called only after build_performance_outlook_page() has rendered the outturn HTML.

nhs_outturn_four_columns <- function(path, outturn_rows, issue_date) {
  if (!file.exists(path)) stop("Outturn HTML does not exist: ", path)
  needed <- c("metric_id", "display_name", "overview_group", "latest_value",
              "forecast_value", "projected_target_value", "target_probability",
              "target_month", "target_value", "lower_80", "upper_80",
              "at_forecast_target_probability")
  missing <- setdiff(needed, names(outturn_rows))
  if (length(missing)) stop("Cannot build outturn grid; missing: ",
                            paste(missing, collapse = ", "))
  actual_field <- intersect(c("actual_value", "outturn_value", "observed_value"),
                            names(outturn_rows))
  if (!length(actual_field)) stop("Outturn rows do not contain an actual result.")
  actual_field <- actual_field[1L]
  if (anyDuplicated(outturn_rows$metric_id)) stop("Duplicate metric IDs in outturn.")
  if (anyNA(outturn_rows$target_probability)) {
    stop("Missing refreshed target probability for one or more metrics.")
  }

  escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    gsub('"', "&quot;", x, fixed = TRUE)
  }
  num <- function(x) {
    if (!length(x)) return(NA_real_)
    suppressWarnings(as.numeric(x[1L]))
  }
  short_month <- function(x) {
    if (!length(x) || is.na(x[1L])) return("Date unavailable")
    d <- suppressWarnings(as.Date(x[1L]))
    if (is.na(d)) return("Date unavailable")
    format(d, "%b %Y")
  }
  short_date <- function(x) {
    d <- suppressWarnings(as.Date(x))
    if (length(d) != 1L || is.na(d)) return("latest release")
    format(d, "%d %b %Y")
  }
  is_minutes <- function(metric) identical(metric, "ambulance_cat2")
  as_display <- function(x, metric) {
    z <- num(x)
    if (!is.finite(z)) return("—")
    if (is_minutes(metric)) return(sprintf("%.1f min", z))
    if (abs(z) <= 1) z <- z * 100
    sprintf("%.1f%%", z)
  }
  # Convert to percentage points for rate metrics; minutes for ambulance.
  unit_delta <- function(x, y, metric) {
    a <- num(x); b <- num(y)
    if (!is.finite(a) || !is.finite(b)) return(NA_real_)
    d <- a - b
    if (!is_minutes(metric) && max(abs(a), abs(b)) <= 1) d <- 100 * d
    d
  }
  delta_text <- function(d, metric) {
    if (!is.finite(d)) return("—")
    if (is_minutes(metric)) return(sprintf("%.1f min", abs(d)))
    sprintf("%.1f pp", abs(d))
  }
  chance_html <- function(x) {
    p <- num(x)
    if (!is.finite(p)) return("—")
    if (p < .01) return("&lt;1%")
    if (p > .99) return("&gt;99%")
    sprintf("%.0f%%", 100 * p)
  }
  chance_class <- function(x) {
    p <- num(x)
    if (!is.finite(p)) return("chance-neutral")
    if (p < .2) return("chance-low")
    if (p > .8) return("chance-high")
    "chance-neutral"
  }
  # Compare the freshly calculated target probability with the original
  # PRE-RELEASE probability retained in at_forecast_target_probability.
  # Threshold: material movement >=2 percentage points; compare full precision.
  probability_change_html <- function(current, at_forecast) {
    new <- num(current)
    old <- num(at_forecast)
    if (!is.finite(new) || !is.finite(old) ||
        new < 0 || new > 1 || old < 0 || old > 1) {
      stop("Invalid fresh or at-forecast target probability in the outturn.")
    }
    delta_pp <- 100 * (new - old)
    if (abs(delta_pp) < 2) {
      return('<span class="probability-change probability-stable">Little change since forecast</span>')
    }
    label <- if (delta_pp > 0) "Up" else "Down"
    status <- if (delta_pp > 0) "probability-up" else "probability-down"
    # Whole percentage points deliberately match the rounded probability.
    paste0('<span class="probability-change ', status, '">',
           label, ' ', sprintf('%.0f', abs(delta_pp)),
           'pp since forecast</span>')
  }
  higher_is_better <- function(r, metric) {
    if ("higher_is_better" %in% names(r) && !is.na(r$higher_is_better[1L])) {
      val <- suppressWarnings(as.logical(as.character(r$higher_is_better[1L])))
      if (!is.na(val)) return(val)
    }
    !(metric %in% c("ambulance_cat2", "diagnostics_6w"))
  }

  one_row <- function(r) {
    metric <- as.character(r$metric_id[1L])
    better <- higher_is_better(r, metric)
    direction <- if (better) 1 else -1
    previous <- num(r$latest_value)
    forecast <- num(r$forecast_value)
    actual <- num(r[[actual_field]])
    lower <- num(r$lower_80); upper <- num(r$upper_80)
    movement <- unit_delta(actual, previous, metric)
    error <- unit_delta(actual, forecast, metric)

    # 0.5pp / 0.5min: readable movement signal, not a significance test.
    # Never use this threshold to determine whether a result is outside its 80% range.
    change <- if (!is.finite(movement) || abs(movement) < .5) {
      '<span class="signal-muted">Broadly unchanged</span>'
    } else if (direction * movement > 0) {
      paste0('<span class="signal-good">Improved ',
             escape(delta_text(movement, metric)), '</span>')
    } else {
      paste0('<span class="signal-bad">Worsened ',
             escape(delta_text(movement, metric)), '</span>')
    }
    comparison <- if (!is.finite(error)) {
      '<span class="signal-muted">Forecast comparison unavailable</span>'
    } else if (abs(error) < .05) {
      '<span class="signal-muted">In line with forecast</span>'
    } else if (direction * error > 0) {
      paste0('<span class="signal-good">', escape(delta_text(error, metric)),
             ' better than forecast</span>')
    } else {
      paste0('<span class="signal-bad">', escape(delta_text(error, metric)),
             ' worse than forecast</span>')
    }
    # Show which side of the expected range, not a generic "outside range".
    range_signal <- if (!is.finite(actual) || !is.finite(lower) ||
                        !is.finite(upper)) {
      '<span class="range-badge range-neutral">Range unavailable</span>'
    } else if (actual > upper) {
      paste0('<span class="range-badge ', if (better) "range-good" else "range-bad",
             '">Above expected range</span>')
    } else if (actual < lower) {
      paste0('<span class="range-badge ', if (!better) "range-good" else "range-bad",
             '">Below expected range</span>')
    } else {
      '<span class="range-badge range-neutral">Within expected range</span>'
    }
    actual_class <- if (is.finite(error) && abs(error) >= .05) {
      if (direction * error > 0) "actual-good" else "actual-bad"
    } else "actual-neutral"

    dest <- if ("deep_dive_file" %in% names(r)) as.character(r$deep_dive_file[1L]) else ""
    title <- escape(r$display_name[1L])
    link <- if (!is.na(dest) && nzchar(dest)) {
      paste0('<a class="metric-name" href="', escape(basename(dest)), '">',
             title, '</a>')
    } else title
    benchmark <- if ("benchmark_label" %in% names(r) &&
                     !is.na(r$benchmark_label[1L])) {
      paste0('<small class="benchmark">', escape(r$benchmark_label[1L]), '</small>')
    } else ""
    previous_month <- if ("latest_month" %in% names(r)) short_month(r$latest_month) else "—"
    forecast_month <- if ("forecast_month" %in% names(r)) short_month(r$forecast_month) else "—"
    actual_month <- if ("actual_month" %in% names(r)) short_month(r$actual_month) else forecast_month
    condition <- if (better) "&ge;" else "&le;"

    paste0(
      '<tr class="metric-row">',
      '<th scope="row" class="indicator-cell">', link, benchmark, '</th>',
      '<td class="previous-cell"><strong>', escape(as_display(previous, metric)),
      '</strong><small>', escape(previous_month), '</small></td>',
      '<td class="forecast-cell"><strong>', escape(as_display(forecast, metric)),
      '</strong><small>', escape(forecast_month), '</small>',
      '<span class="forecast-range">80% range ',
      escape(as_display(lower, metric)), '–',
      escape(as_display(upper, metric)), '</span></td>',
      '<td class="actual-cell ', actual_class, '"><strong>',
      escape(as_display(actual, metric)), '</strong><small>',
      escape(actual_month), '</small><span class="actual-signal">',
      change, '</span><span class="actual-signal">', comparison,
      '</span>', range_signal, '</td>',
      '<td class="target-cell"><span class="target-date">',
      escape(short_month(r$target_month)), '</span>',
      '<span class="target-numbers"><strong>',
      escape(as_display(r$projected_target_value, metric)),
      '</strong><span>Goal ', condition, escape(as_display(r$target_value, metric)),
      '</span></span><span class="target-chance ',
      chance_class(r$target_probability), '"><b>',
      chance_html(r$target_probability), '</b> chance of meeting goal</span>',
      probability_change_html(r$target_probability,
                              r$at_forecast_target_probability),
      '</td></tr>'
    )
  }

  rows <- data.table::copy(data.table::as.data.table(outturn_rows))
  if ("display_order" %in% names(rows)) data.table::setorderv(rows, "display_order")
  groups <- unique(as.character(rows$overview_group))
  head <- paste0(
    '<table class="outlook-four-column-table" aria-label="NHS outturn, pre-release forecast and refreshed target outlook">',
    '<thead><tr><th scope="col">Indicator</th>',
    '<th scope="col">Previous</th>',
    '<th scope="col" class="forecast-cell">Forecast</th>',
    '<th scope="col">Actual</th>',
    '<th scope="col">Target outlook<small>updated ', escape(short_date(issue_date)),
    '</small></th></tr></thead><tbody>'
  )
  html_rows <- c(head)
  for (grp in groups) {
    label <- if (is.na(grp) || !nzchar(grp)) "Other" else grp
    html_rows <- c(html_rows, paste0('<tr class="group-row"><th colspan="5">',
                                     escape(label), '</th></tr>'))
    data <- if (is.na(grp)) rows[is.na(overview_group)] else
      rows[!is.na(overview_group) & overview_group == grp]
    for (i in seq_len(nrow(data))) html_rows <- c(html_rows, one_row(data[i]))
  }
  new_table <- paste(c(html_rows, '</tbody></table>'), collapse = "\n")

  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  pattern <- "(?s)<table\\b[^>]*>.*?</table>"
  match <- gregexpr(pattern, html, perl = TRUE)
  tables <- regmatches(html, match)[[1L]]
  select <- which(grepl('class="metric-row"', tables, fixed = TRUE))
  if (length(select) != 1L) {
    stop("Expected exactly one main metrics table; found ", length(select),
         ". Leaving original HTML untouched.")
  }
  tables[select] <- new_table
  regmatches(html, match) <- list(tables)

  style <- paste0(
    '<style id="outturn-four-columns-css">\n',
    '.outlook-four-column-table{width:100%;border-collapse:collapse;table-layout:fixed;margin-top:6px;color:#192831}\n',
    '.outlook-four-column-table th,.outlook-four-column-table td{box-sizing:border-box;padding:9px 9px;vertical-align:top;text-align:left;border-bottom:1px solid #d9e0e3}\n',
    '.outlook-four-column-table thead th{border-top:2px solid #17252f;font-size:10px;line-height:1.25;letter-spacing:.04em;text-transform:uppercase;background:#fff;color:#43515c}\n',
    '.outlook-four-column-table thead th:nth-child(1){width:24%}\n',
    '.outlook-four-column-table thead th:nth-child(2){width:13%}\n',
    '.outlook-four-column-table thead th:nth-child(3){width:18%}\n',
    '.outlook-four-column-table thead th:nth-child(4){width:23%}\n',
    '.outlook-four-column-table thead th:nth-child(5){width:22%}\n',
    '.outlook-four-column-table tbody .indicator-cell{font-size:11px;line-height:1.26;font-weight:750}\n',
    '.outlook-four-column-table .indicator-cell .benchmark{display:block;font-size:8px;color:#71808a;font-weight:400;margin-top:5px}\n',
    '.outlook-four-column-table td strong{display:block;font-size:17px;line-height:1.15;color:#1e2e38}\n',
    '.outlook-four-column-table td small{display:block;font-size:8.5px;margin-top:3px;color:#697781;line-height:1.25}\n',
    '.outlook-four-column-table .forecast-cell{background:#f0f2f4!important;color:#63717b!important}\n',
    '.outlook-four-column-table .forecast-cell strong{color:#5a6874!important;font-weight:700}\n',
    '.outlook-four-column-table .forecast-range{display:block;margin-top:6px;font-size:8px;color:#71808a}\n',
    '.outlook-four-column-table .actual-cell{background:#fff!important}\n',
    '.outlook-four-column-table .actual-cell.actual-good{box-shadow:inset 3px 0 0 #32785c}\n',
    '.outlook-four-column-table .actual-cell.actual-bad{box-shadow:inset 3px 0 0 #bd5660}\n',
    '.outlook-four-column-table .actual-signal{display:block;font-size:8.5px;line-height:1.25;margin-top:4px}\n',
    '.outlook-four-column-table .signal-good{color:#247452!important;font-weight:650}\n',
    '.outlook-four-column-table .signal-bad{color:#ac3f4b!important;font-weight:650}\n',
    '.outlook-four-column-table .signal-muted{color:#64727b!important}\n',
    '.outlook-four-column-table .range-badge{display:inline-block;margin-top:6px;font-size:7px;font-weight:700;line-height:1.3;padding:2px 5px;border-radius:3px;letter-spacing:.015em}\n',
    '.outlook-four-column-table .range-neutral{color:#556773!important;border:1px solid #bfcbd1;background:#f6f8f9}\n',
    '.outlook-four-column-table .range-good{color:#216a49!important;border:1px solid #9ac9ae;background:#edf6ef}\n',
    '.outlook-four-column-table .range-bad{color:#a43c47!important;border:1px solid #e2a9ae;background:#fbf0f0}\n',
    '.outlook-four-column-table .target-cell{background:#fafbfb!important}\n',
    '.outlook-four-column-table .target-date{display:block;font-size:9px;font-weight:750;color:#384e58}\n',
    '.outlook-four-column-table .target-numbers{display:flex;justify-content:space-between;align-items:baseline;gap:4px;margin-top:5px}\n',
    '.outlook-four-column-table .target-numbers strong{font-size:15px}\n',
    '.outlook-four-column-table .target-numbers>span{font-size:10px;font-weight:650;white-space:nowrap}\n',
    '.outlook-four-column-table .target-chance{display:block;font-size:8.4px;line-height:1.3;margin-top:7px;color:#536570}\n',
    '.outlook-four-column-table .target-chance b{font-size:12px;color:#2d4b40}\n',
    '.outlook-four-column-table .chance-low b{color:#ad4751!important}\n',
    '.outlook-four-column-table .chance-high b{color:#287450!important}\n',
    '.outlook-four-column-table .probability-change{display:block;margin-top:3px;font-size:8px;line-height:1.25;font-weight:650}\n',
    '.outlook-four-column-table .probability-up{color:#247452!important}\n',
    '.outlook-four-column-table .probability-down{color:#ac3f4b!important}\n',
    '.outlook-four-column-table .probability-stable{color:#64727b!important;font-weight:500}\n',
    '.outlook-four-column-table .group-row th{font-size:9px;font-weight:700;letter-spacing:.07em;text-transform:uppercase;color:#326354;background:#eef4f0;padding:7px 9px;border-bottom:1px solid #bccbc5}\n',
    '.outlook-four-column-table thead th small{display:block;font-size:8px;letter-spacing:0;text-transform:none;font-weight:400;color:#74828b;margin-top:3px}\n',
    '.outlook-grid-note{font-size:10px;color:#62717b;margin:5px 0 3px}\n',
    '@media print{\n',
    '.outlook-four-column-table{margin-top:1mm}\n',
    '.outlook-four-column-table th,.outlook-four-column-table td{padding:1.6mm 1.5mm}\n',
    '.outlook-four-column-table thead th{font-size:5.8pt}\n',
    '.outlook-four-column-table tbody .indicator-cell{font-size:6.1pt}\n',
    '.outlook-four-column-table .indicator-cell .benchmark{font-size:4.8pt;margin-top:.7mm}\n',
    '.outlook-four-column-table td strong{font-size:9pt}\n',
    '.outlook-four-column-table td small{font-size:4.8pt;margin-top:.4mm}\n',
    '.outlook-four-column-table .forecast-range{font-size:4.7pt;margin-top:.8mm}\n',
    '.outlook-four-column-table .actual-signal{font-size:4.8pt;margin-top:.65mm}\n',
    '.outlook-four-column-table .range-badge{font-size:4.5pt;margin-top:.8mm;padding:.25mm .7mm}\n',
    '.outlook-four-column-table .target-date{font-size:5pt}\n',
    '.outlook-four-column-table .target-numbers{margin-top:.65mm}\n',
    '.outlook-four-column-table .target-numbers strong{font-size:8.4pt}\n',
    '.outlook-four-column-table .target-numbers>span{font-size:5.3pt}\n',
    '.outlook-four-column-table .target-chance{font-size:4.8pt;margin-top:1mm}\n',
    '.outlook-four-column-table .target-chance b{font-size:7.5pt}\n',
    '.outlook-four-column-table .probability-change{font-size:4.6pt;margin-top:.55mm;line-height:1.17}\n',
    '.outlook-four-column-table .group-row th{font-size:5.1pt;padding:1mm 1.5mm}\n',
    '.outlook-four-column-table thead th small{font-size:4.5pt}\n',
    '.outlook-grid-note{font-size:5.5pt;margin:.5mm 0}\n',
    '}\n</style>'
  )
  if (!grepl('</head>', html, fixed = TRUE)) stop("Missing </head> in generated HTML")
  html <- sub('</head>', paste0(style, '\n</head>'), html, fixed = TRUE)
  # Explain the two distinct forecast vintages without repeating verbose text in every cell.
  note <- paste0(
    '<p class="outlook-grid-note"><b>Forecast</b> is the estimate published before the release. ',
    '<b>Target outlook</b> is updated with the new results; its chance change ',
    'is against the pre-release forecast. ',
    'Green/red indicates whether actual performance was better/worse than forecast.</p>'
  )
  html <- sub('<table class="outlook-four-column-table"',
              paste0(note, '\n<table class="outlook-four-column-table"'),
              html, fixed = TRUE)
  writeLines(html, path, useBytes = TRUE)
  message("Refreshed four-column outturn grid with forecast signals: ", path,
          " (", nrow(rows), " indicators)")
  invisible(path)
}
