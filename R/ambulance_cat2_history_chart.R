# NHS Outlook | England Category 2 historical totals + dual-view national chart
# No change to individual ambulance-provider outputs or forecasts.
# Official AmbSYS A10 = Category 2 incidents; A30 = total response seconds.

nhs_cat2_month_index <- function(x) {
  x <- as.Date(x)
  as.integer(format(x, "%Y")) * 12L + as.integer(format(x, "%m"))
}

nhs_cat2_scan_official_history <- function(csv_path, months) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
  if (!exists("nhs_cat2_parse_month", mode = "function") ||
      !exists("nhs_cat2_as_number", mode = "function")) {
    source("R/ambulance_counts_backfill.R", local = FALSE)
  }
  raw <- data.table::fread(csv_path, encoding = "UTF-8",
                            na.strings = c("", "NA", "."))
  normalized <- gsub("[^a-z0-9]", "", tolower(names(raw)))
  get_col <- function(key, required = TRUE) {
    match_index <- which(normalized == key)
    if (length(match_index) != 1L) {
      if (!required) return(NULL)
      stop("The AmbSYS CSV must contain one '", key,
           "' column. Found: ", paste(names(raw), collapse = ", "))
    }
    names(raw)[match_index]
  }
  year_col <- get_col("year")
  month_col <- get_col("month")
  code_col <- get_col("orgcode")
  name_col <- get_col("orgname", required = FALSE)
  region_col <- get_col("region", required = FALSE)
  a10_col <- get_col("a10")
  a30_col <- get_col("a30")
  a31_col <- get_col("a31", required = FALSE)
  n <- nrow(raw)
  source_rows <- data.table::data.table(
    year = suppressWarnings(as.integer(as.character(raw[[year_col]]))),
    month = nhs_cat2_parse_month(raw[[month_col]]),
    org_code = toupper(trimws(as.character(raw[[code_col]]))),
    org_name = if (is.null(name_col)) rep("", n) else trimws(as.character(raw[[name_col]])),
    region = if (is.null(region_col)) rep("", n) else trimws(as.character(raw[[region_col]])),
    a10 = nhs_cat2_as_number(raw[[a10_col]]),
    a30 = nhs_cat2_as_number(raw[[a30_col]]),
    a31 = if (is.null(a31_col)) rep(NA_real_, n) else nhs_cat2_as_number(raw[[a31_col]])
  )
  source_rows[, calendar_month := as.Date(NA_character_)]
  valid_date <- !is.na(source_rows$year) & !is.na(source_rows$month) &
    source_rows$year >= 2017 & source_rows$year <= 2035 &
    source_rows$month %in% 1:12
  source_rows[valid_date, calendar_month :=
                as.Date(sprintf("%04d-%02d-01", year, month))]
  source_rows <- source_rows[calendar_month %in% as.Date(months)]
  # Prefer a genuine official England row. Region == England on a trust row
  # does NOT by itself identify an England total.
  source_rows[, is_england_total :=
                grepl("^(england|all england|england total|national total|total england)$",
                      trimws(tolower(org_name))) |
                (org_code %in% c("ENG", "ENGLAND", "ENGTOTAL", "ENGLANDTOTAL", "NATIONAL", "TOTAL") &
                 grepl("england|national|total", paste(org_name, region),
                       ignore.case = TRUE))]
  # Provider fallback is permitted only if ALL eleven official records validate.
  source_rows[, is_ambulance_trust := grepl("^R[A-Z0-9]{2,}$", org_code)]
  output <- lapply(as.Date(months), function(d) {
    z <- source_rows[calendar_month == d]
    england <- z[is_england_total == TRUE]
    trusts <- z[is_ambulance_trust == TRUE]
    valid <- function(y) is.finite(y$a10) & y$a10 > 0 &
      abs(y$a10 - round(y$a10)) < 1e-7 &
      is.finite(y$a30) & y$a30 > 0
    method <- NA_character_
    issue <- ""
    count <- NA_real_
    seconds <- NA_real_
    published_a31 <- NA_real_
    # Unambiguous national aggregate has precedence; never combine it with
    # trust rows or double-count it.
    if (nrow(england) == 1L && all(valid(england))) {
      method <- "official_england_row"
      count <- england$a10[1L]
      seconds <- england$a30[1L]
      published_a31 <- england$a31[1L]
    } else if (nrow(england) > 1L) {
      issue <- "duplicate England total"
    } else if (nrow(england) == 1L) {
      issue <- "invalid England total"
    } else if (nrow(trusts) == 11L &&
               data.table::uniqueN(trusts$org_code) == 11L &&
               all(valid(trusts))) {
      method <- "sum_11_complete_official_trusts"
      count <- sum(trusts$a10)
      seconds <- sum(trusts$a30)
    } else {
      issue <- paste0("no valid England row; trust rows=", nrow(trusts),
                      ", unique codes=", data.table::uniqueN(trusts$org_code),
                      ", invalid rows=", sum(!valid(trusts)))
    }
    precise <- if (is.finite(count) && count > 0) seconds / count / 60 else NA_real_
    a31_disagreement <- if (is.finite(published_a31) && is.finite(precise)) {
      # CSV A31 may be seconds, or Excel-style time fractions. Confirm unit
      # by magnitude: only compare in seconds when A31 is > 100 seconds.
      if (published_a31 > 100) abs(published_a31 - precise * 60) else NA_real_
    } else NA_real_
    if (is.finite(a31_disagreement) && a31_disagreement > 1.25) {
      issue <- "England A31 does not reconcile to A10/A30"
    }
    data.table::data.table(calendar_month = as.Date(d),
      incident_count = count, response_seconds = seconds,
      precise_minutes = precise, published_rounded_seconds =
        if (is.finite(precise)) floor(precise * 60 + .5) else NA_real_,
      source_method = method, source_issue = issue,
      england_rows = nrow(england), trust_rows = nrow(trusts),
      invalid_trust_rows = sum(!valid(trusts)),
      a31_disagreement_seconds = a31_disagreement,
      source_complete = is.finite(precise) && !nzchar(issue))
  })
  list(status = data.table::rbindlist(output),
       source_rows = source_rows)
}

nhs_cat2_recover_long_history <- function(
    first_month = as.Date("2022-10-01"),
    last_csv_month = as.Date("2026-09-01"),
    panel_path = "data-interim/core/national_panel.csv",
    history_path = "output/qa/ambulance_cat2_official_history.csv",
    audit_path = "output/qa/ambulance_cat2_long_history_audit.csv") {
  if (!exists("nhs_cat2_official_csv_source", mode = "function"))
    source("R/ambulance_counts_backfill.R", local = FALSE)
  months <- seq.Date(as.Date(first_month), as.Date(last_csv_month), by = "month")
  explicit <- as.character(getOption("nhs.cat2.official_ambsys_csv", ""))[1L]
  cache <- if (dir.exists("data-external/official"))
    list.files("data-external/official", pattern = "AmbSYS.*[.]csv$",
               full.names = TRUE, ignore.case = TRUE) else character()
  if (length(cache))
    cache <- cache[order(file.info(cache)$mtime, decreasing = TRUE)]
  candidates <- if (!is.na(explicit) && nzchar(explicit)) explicit else cache
  if (!length(candidates)) candidates <- nhs_cat2_official_csv_source()$path
  best <- NULL
  selected <- NULL
  errors <- character()
  # Prefer the newest official source containing the September 2026 vintage;
  # never cherry-pick older files simply because they have fewer missing rows.
  for (candidate in unique(candidates)) {
    if (!file.exists(candidate)) next
    result <- tryCatch(nhs_cat2_scan_official_history(candidate, months),
      error = function(e) {
        errors <<- c(errors, paste(basename(candidate), conditionMessage(e)))
        NULL
      })
    if (is.null(result)) next
    has_latest_month <- any(result$source_rows$calendar_month ==
                              as.Date(last_csv_month), na.rm = TRUE)
    if (has_latest_month) {
      best <- result
      selected <- candidate
      break
    }
    # Keep the first (newest) old source only as a potential fallback.
    if (is.null(best)) {
      best <- result
      selected <- candidate
    }
  }
  if (is.null(best)) {
    if (is.na(explicit) || !nzchar(explicit)) {
      downloaded <- nhs_cat2_official_csv_source()$path
      best <- nhs_cat2_scan_official_history(downloaded, months)
      selected <- downloaded
    } else {
      stop("Cannot parse official AmbSYS CSV: ", paste(errors, collapse = " | "))
    }
  }
  if (sum(best$status$source_complete) == 0L)
    stop("No valid England totals or complete eleven-trust months in source: ", selected)
  panel <- data.table::fread(panel_path, encoding = "UTF-8")
  panel <- panel[metric_id == "ambulance_cat2"]
  panel[, calendar_month := as.Date(calendar_month)]
  if (anyDuplicated(panel$calendar_month)) stop("Duplicate national Category 2 months")
  reference <- panel[, .(calendar_month, panel_minutes = as.numeric(value))]
  joined <- merge(best$status, reference, by = "calendar_month", all.x = TRUE)
  joined[, disagreement_seconds := abs(precise_minutes - panel_minutes) * 60]
  joined[, accepted := source_complete &
            (is.na(panel_minutes) |
             (is.finite(disagreement_seconds) & disagreement_seconds <= 0.65))]
  joined[, audit_status := data.table::fifelse(
    !source_complete, "excluded_no_verified_national_total",
    data.table::fifelse(!accepted, "excluded_national_mean_disagreement", "accepted"))]
  joined[, source_csv := normalizePath(selected, winslash = "/", mustWork = TRUE)]
  dir.create(dirname(audit_path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(joined, audit_path)
  # Backfilled April-October counts in the NATIONAL panel are authoritative for
  # this already-calculated FY outlook; do not overwrite them or the model.
  historical <- joined[accepted == TRUE, .(calendar_month, incident_count,
    response_seconds, precise_minutes, published_rounded_seconds, source_method)]
  data.table::setorder(historical, calendar_month)
  data.table::fwrite(historical, history_path)
  n_england <- historical[source_method == "official_england_row", .N]
  n_trust_sum <- historical[source_method == "sum_11_complete_official_trusts", .N]
  message("Category 2 national history: ", nrow(historical), "/", length(months),
          " reconciled months (", n_england, " direct England totals; ",
          n_trust_sum, " complete-trust totals). ",
          "Audit: ", audit_path)
  excluded <- joined[accepted != TRUE]
  if (nrow(excluded)) {
    message("Missing or discrepant historical months: ",
            paste(format(excluded$calendar_month), collapse = ", "),
            ". No partial totals were used.")
  }
  invisible(historical)
}

nhs_cat2_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

nhs_cat2_chart_svg <- function(tab, column, view = c("annual", "monthly"),
                               forecast_end = as.Date("2029-03-01")) {
  view <- match.arg(view)
  d <- data.table::copy(tab)
  d[, calendar_month := as.Date(calendar_month)]
  d <- d[is.finite(get(column)) & calendar_month <= as.Date(forecast_end)]
  data.table::setorder(d, calendar_month)
  if (!nrow(d) || anyDuplicated(d$calendar_month)) stop("Invalid chart series: ", view)
  if (!any(d$kind == "actual") || !any(d$kind == "forecast"))
    stop("Chart must have actuals and future projections: ", view)
  latest <- max(d[kind == "actual", calendar_month])
  start <- if (view == "annual") as.Date("2024-01-01") else as.Date("2024-10-01")
  d <- d[calendar_month >= start]
  if (view == "annual" && nrow(d[kind == "actual"]) < 12L)
    stop("Need >=12 consecutive-window rolling averages for annual chart; got ",
         nrow(d[kind == "actual"]), ". Inspect historical national audit.")
  left <- 58
  right <- 728
  top <- 29
  bottom <- 182
  start_index <- min(nhs_cat2_month_index(d$calendar_month))
  end_index <- max(nhs_cat2_month_index(d$calendar_month))
  xi <- function(month) left + (nhs_cat2_month_index(month) - start_index) *
       (right - left) / (end_index - start_index)
  values <- d[[column]]
  # Include BOTH published planning milestones in the scale. Neither marker
  # should be clipped even where the projected mean is above both targets.
  axis_min <- min(c(18, 25, values))
  axis_max <- max(c(18, 25, values))
  pad <- max(1.5, (axis_max - axis_min) * .16)
  y_min <- max(0, floor((axis_min - pad) / 5) * 5)
  y_max <- ceiling((axis_max + pad) / 5) * 5
  if (y_max - y_min < 10) y_max <- y_min + 10
  yi <- function(v) bottom - (as.numeric(v) - y_min) *
         (bottom - top) / (y_max - y_min)
  path <- function(part) paste(sprintf("%.1f,%.1f", xi(part$calendar_month),
                                            yi(part[[column]])), collapse = " ")
  line <- function(x1, y1, x2, y2, style) sprintf(
    '<line class="%s" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
    style, x1, y1, x2, y2)
  label <- function(x, y, s, style, anchor = "start") sprintf(
    '<text class="%s" x="%.1f" y="%.1f" text-anchor="%s">%s</text>',
    style, x, y, anchor, nhs_cat2_html_escape(s))
  # Respect gaps: a rolling window is valid only when all twelve original
  # calendar months are present and weighted. Do not connect across gaps.
  runs <- function(part, style) {
    if (!nrow(part)) return("")
    indices <- nhs_cat2_month_index(part$calendar_month)
    groups <- split(seq_len(nrow(part)), cumsum(c(TRUE, diff(indices) != 1L)))
    paste(vapply(groups, function(i) {
      z <- part[i]
      if (nrow(z) >= 2L) sprintf('<polyline class="%s" points="%s"/>',
                                 style, path(z))
      else sprintf('<circle class="cat2-point" cx="%.1f" cy="%.1f" r="2"/>',
                   xi(z$calendar_month), yi(z[[column]]))
    }, character(1)), collapse = "")
  }
  actual <- d[kind == "actual"]
  forecast <- d[kind == "forecast"]
  forecast_path <- data.table::rbindlist(list(utils::tail(actual, 1L), forecast),
                                        use.names = TRUE, fill = TRUE)
  ticks <- pretty(c(y_min, y_max), n = 4)
  ticks <- ticks[ticks >= y_min & ticks <= y_max]
  guides <- paste(vapply(ticks, function(t) paste0(
    line(left, yi(t), right, yi(t), "cat2-grid"),
    label(left - 7, yi(t) + 3, paste0(format(t, trim = TRUE), " min"),
          "cat2-tick", "end")), character(1)), collapse = "")
  tick_dates <- as.Date(c("2025-01-01", "2026-01-01", "2026-10-01",
                          "2027-03-01", "2028-03-01", "2029-03-01"))
  tick_dates <- tick_dates[tick_dates >= min(d$calendar_month) &
                             tick_dates <= max(d$calendar_month)]
  dates <- paste(vapply(tick_dates, function(t) label(xi(t), 211,
                                  format(t, "%b %Y"), "cat2-tick", "middle"),
                        character(1)), collapse = "")
  forecast_wash <- sprintf(
    '<rect class="cat2-future" x="%.1f" y="%.1f" width="%.1f" height="%.1f"/>',
    xi(latest), top, right - xi(latest), bottom - top)
  target_date <- as.Date("2027-03-01")
  later_target_date <- as.Date("2029-03-01")
  if (view == "annual") {
    # The 2026/27 25-minute milestone is the INCIDENT-WEIGHTED annual mean.
    # Both milestones specify an average Category 2 response time.
    # The 18-minute milestone is specified for achievement by end-2028/29.
    target <- paste0(
      '<circle class="cat2-target-point" cx="', sprintf("%.1f", xi(target_date)),
      '" cy="', sprintf("%.1f", yi(25)), '" r="5"/>',
      label(xi(target_date) + 7, yi(25) - 8,
            "2026/27: 25 min average", "cat2-target-label"),
      '<circle class="cat2-target-late-point" cx="',
      sprintf("%.1f", xi(later_target_date)),
      '" cy="', sprintf("%.1f", yi(18)), '" r="5"/>',
      label(xi(later_target_date) - 8, yi(18) - 9,
            "2028/29: 18 min average", "cat2-target-late-label", "end"))
  } else {
    # Annual policy REFERENCES, not a claim that each monthly mean must
    # meet the threshold. Only show the line over its applicable FY.
    target <- paste0(
      line(xi(as.Date("2026-04-01")), yi(25), xi(target_date), yi(25),
           "cat2-target-dotted"),
      # Place the label in the clear top margin, not over the monthly lines.
      label(xi(as.Date("2026-04-01")) + 3, top - 11,
            "25 min annual average", "cat2-target-label"),
      line(xi(as.Date("2028-04-01")), yi(18),
           xi(later_target_date), yi(18), "cat2-target-late-dotted"),
      label(xi(later_target_date) - 6, top - 11,
            "18 min average", "cat2-target-late-label", "end"))
  }
  # Projected annual value at FY end is the comparable policy-relevant number.
  value_fy <- d[calendar_month == target_date, get(column)]
  if (view == "annual" && length(value_fy) == 1L && is.finite(value_fy)) {
    target <- paste0(target, sprintf(
      '<circle class="cat2-forecast-point" cx="%.1f" cy="%.1f" r="3.6"/>',
      xi(target_date), yi(value_fy)),
      label(xi(target_date) + 8, yi(value_fy) - 9,
            sprintf("Forecast %.1f min", value_fy), "cat2-projection-label"))
  }
  paste0('<svg class="outlook-chart ',
         if (view == "annual") 'cat2-annual-chart' else 'cat2-monthly-chart',
         '" viewBox="0 0 770 225" role="img" aria-label="',
         if (view == "annual")
           'National Category 2 incident-weighted 12-month rolling mean and forecast'
         else 'National Category 2 monthly mean response time and forecast',
         '">', forecast_wash, guides, target,
         runs(actual, "cat2-history"),
         runs(forecast_path, "cat2-projected"),
         sprintf('<circle class="cat2-last-actual" cx="%.1f" cy="%.1f" r="3.5"/>',
                 xi(latest), yi(utils::tail(actual[[column]], 1L))),
         dates, '</svg>')
}

nhs_cat2_draw_annual_chart <- function(html_path, annual) {
  if (!file.exists(html_path)) stop("Cannot find Category 2 report: ", html_path)
  # The annual series includes 2022-onwards validated official counts plus
  # future incident-weighted projections. Monthly view also needs earlier
  # headline actuals, which do not need incident weights for display.
  annual_svg <- nhs_cat2_chart_svg(annual$series,
                                    "rolling_12m_minutes", "annual")
  national <- data.table::fread("data-interim/core/national_panel.csv",
                                 encoding = "UTF-8")
  national <- national[metric_id == "ambulance_cat2"]
  national[, calendar_month := as.Date(calendar_month)]
  national <- national[is.finite(value), .(calendar_month,
                          monthly_mean_minutes = as.numeric(value), kind = "actual")]
  forecast <- annual$series[kind == "forecast",
                             .(calendar_month, monthly_mean_minutes, kind)]
  monthly <- data.table::rbindlist(list(national, forecast), use.names = TRUE)
  data.table::setorder(monthly, calendar_month)
  monthly_svg <- nhs_cat2_chart_svg(monthly, "monthly_mean_minutes", "monthly")
  original <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n")
  title_at <- regexpr('Longer-term outlook and milestones', original, fixed = TRUE)[1L]
  if (title_at < 1L) stop("Expected longer-term section not found")
  before_title <- substr(original, 1L, title_at)
  starts <- gregexpr('<section class="chart-block', before_title, fixed = TRUE)[[1L]]
  starts <- starts[starts > 0L]
  if (!length(starts)) stop("Cannot locate the longer-term chart section")
  start <- utils::tail(starts, 1L)
  remaining <- substr(original, title_at, nchar(original))
  close_rel <- regexpr('</section>', remaining, fixed = TRUE)[1L]
  if (close_rel < 1L) stop("Cannot find chart section end")
  stop_at <- title_at + close_rel + nchar('</section>') - 2L
  replacement <- paste0(
    '<section class="chart-block cat2-chart-toggle" data-cat2-chart-toggle>',
    '<div class="chart-head"><p class="chart-title">Longer-term outlook and milestones',
    '<span class="cat2-print-view-label"> · Monthly response times</span></p>',
    '<div class="cat2-view-buttons" role="group" aria-label="Category 2 chart view">',
    '<button type="button" aria-pressed="false" data-cat2-choose="annual">12-month average</button>',
    '<button type="button" class="selected" aria-pressed="true" data-cat2-choose="monthly">Monthly</button>',
    '</div></div>',
    '<div class="chart-frame cat2-panel-annual" data-cat2-panel="annual" hidden>', annual_svg, '</div>',
    '<div class="chart-frame cat2-panel-monthly" data-cat2-panel="monthly">', monthly_svg, '</div>',
    '<p class="cat2-chart-caption" data-cat2-caption>',
    'Monthly response times. Targets: 25 min annual average in 2026/27; ',
    '18 min average by March 2029.',
    '</p></section>')
  html <- paste0(substr(original, 1L, start - 1L), replacement,
                 substr(original, stop_at + 1L, nchar(original)))
  # Replace prior ad hoc style if present, to make regeneration idempotent.
  html <- gsub('<style id="cat2-annual-target-css">.*?</style>', '',
               html, perl = TRUE)
  css <- paste0(
    '<style id="cat2-annual-target-css">',
    '.cat2-chart-toggle .chart-head{align-items:center}',
    '.cat2-view-buttons{display:flex;gap:2px;padding:2px;border:1px solid #c9d9d5;border-radius:5px;background:#f4f7f6}',
    '.cat2-view-buttons button{border:0;padding:5px 9px;border-radius:3px;color:#43585b;background:transparent;font:700 10px Arial;cursor:pointer}',
    '.cat2-view-buttons button.selected{background:#1b665d;color:white}',
    '.cat2-chart-toggle [hidden]{display:none!important}',
    '.cat2-print-view-label{display:none}',
    '.cat2-chart-caption{margin:4px 0 0;color:#586b70;font-size:9px;line-height:1.3}',
    '.cat2-grid{stroke:#e4ebea;stroke-width:1}',
    '.cat2-tick{fill:#637579;font-size:9.5px}',
    '.cat2-future{fill:#edf5f2}',
    '.cat2-history{fill:none;stroke:#1d3d47;stroke-width:3.3;stroke-linejoin:round;stroke-linecap:round}',
    '.cat2-projected{fill:none;stroke:#178176;stroke-width:3;stroke-dasharray:7 4;stroke-linejoin:round}',
    '.cat2-target-point{fill:white;stroke:#a06939;stroke-width:2.7}',
    '.cat2-target-dotted{stroke:#a06939;stroke-width:2;stroke-dasharray:4 5}',
    '.cat2-target-label{fill:#895932;font-size:10px;font-weight:750;paint-order:stroke;stroke:#fff;stroke-width:2px;stroke-linejoin:round}',
    '.cat2-target-late-point{fill:white;stroke:#963c49;stroke-width:2.7}',
    '.cat2-target-late-dotted{stroke:#963c49;stroke-width:2;stroke-dasharray:4 5}',
    '.cat2-target-late-label{fill:#963c49;font-size:10px;font-weight:750;paint-order:stroke;stroke:#fff;stroke-width:2px;stroke-linejoin:round}',
    '.cat2-last-actual{fill:#1d3d47;stroke:white;stroke-width:1.5}',
    '.cat2-forecast-point{fill:#178176;stroke:white;stroke-width:1.2}',
    '.cat2-projection-label{fill:#12766c;font-size:10px;font-weight:800}',
    '.cat2-point{fill:#1d3d47}',
    '@media print{.cat2-chart-toggle .cat2-view-buttons{display:none!important}',
    '.cat2-print-view-label{display:inline!important}',
    '.cat2-chart-toggle .chart-title{font-size:8pt}',
    '.cat2-chart-toggle .cat2-panel-annual{display:none!important}',
    '.cat2-chart-toggle .cat2-panel-monthly{display:block!important}',
    '.cat2-chart-toggle .cat2-chart-caption{font-size:4.5pt}}',
    '</style>')
  html <- sub('</head>', paste0(css, '</head>'), html, fixed = TRUE)
  old_script <- '<script id="cat2-toggle-js">.*?</script>'
  html <- gsub(old_script, '', html, perl = TRUE)
  js <- paste0(
    '<script id="cat2-toggle-js">',
    '(function(){document.querySelectorAll("[data-cat2-chart-toggle]").forEach(function(root){',
    'var buttons=root.querySelectorAll("[data-cat2-choose]");',
    'var annual=root.querySelector("[data-cat2-panel=annual]");',
    'var monthly=root.querySelector("[data-cat2-panel=monthly]");',
    'var caption=root.querySelector("[data-cat2-caption]");',
    'function show(mode){var isAnnual=mode==="annual";',
    'annual.hidden=!isAnnual;monthly.hidden=isAnnual;',
    'buttons.forEach(function(b){var active=b.getAttribute("data-cat2-choose")===mode;',
    'b.classList.toggle("selected",active);b.setAttribute("aria-pressed",active?"true":"false");});',
    'caption.textContent=isAnnual?',
    '"12-month average. Targets: 25 min annual average in 2026/27; 18 min average by March 2029.":',
    '"Monthly response times. Targets: 25 min annual average in 2026/27; 18 min average by March 2029.";',
    '}',
    'buttons.forEach(function(b){b.addEventListener("click",function(){show(b.getAttribute("data-cat2-choose"));});});',
    'show("monthly");});})();',
    '</script>')
  html <- sub('</body>', paste0(js, '</body>'), html, fixed = TRUE)
  html <- sub('<strong>Benchmark:</strong>[^<]*',
              '<strong>Targets:</strong> 25-minute annual average in 2026/27; 18-minute average response time by March 2029; 90% within 40 minutes by March 2029.',
              html, perl = TRUE)
  # Target in the provider distribution was incorrectly displayed as a
  # monthly provider benchmark. Remove it, but otherwise preserve provider view.
  html <- sub('<line class="distribution-target"[^>]*></line><text class="distribution-label"[^>]*>benchmark 25[.]0 min</text>',
              '', html, perl = TRUE)
  html <- sub('Mar 2027 outlook at issue',
              '2026/27 annual-average outlook', html, fixed = TRUE)
  html <- sub('Mar 2027 target-date outlook at issue',
              '2026/27 annual-average target outlook', html, fixed = TRUE)
  html <- gsub('<p class="cat2-annual-method"[^>]*>.*?</p>', '', html, perl = TRUE)
  html <- gsub('<p style="font-size:9px;margin-top:5px;color:#425a62">.*?</p>',
               '', html, perl = TRUE)
  # Integrate the historical/annual methodology into the existing small-print
  # Method paragraph.  Do not add a larger or visually separate note under it.
  method_addendum <- paste0(
    " Rolling averages use national AmbSYS A10/A30 totals (or eleven complete ",
    "trust totals where needed) and require 12 complete months. Future incident ",
    "counts are projected; annual target probabilities use correlated forecast errors."
  )
  method_pattern <- '(<p><strong>Method:</strong>[^<]*)(</p>)'
  if (!grepl(method_pattern, html, perl = TRUE)) {
    stop("Cannot find the original Category 2 Method paragraph.")
  }
  if (!grepl("Rolling averages use national AmbSYS A10/A30 totals", html,
             fixed = TRUE)) {
    html <- sub(method_pattern, paste0("\\1", method_addendum, "\\2"),
                html, perl = TRUE)
  }
  writeLines(html, html_path, useBytes = TRUE)
  invisible(html_path)
}
