# NHS Outlook — presentation-only Category 2 chart default and source note.
# The live HTML and PDF use monthly response times by default. The HTML
# retains its 12-month toggle; PDF renders the monthly panel. No model change.
html_path <- "output/releases/ambulance-category-2-outturn-latest.html"
if (!file.exists(html_path)) stop("Category 2 outturn HTML not found: ", html_path)
html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
if (!grepl('data-cat2-chart-toggle', html, fixed = TRUE)) {
  stop("The Category 2 toggle is missing. Build script 35 first.")
}

# Make both the initially visible panel and the interactive state monthly.
annual_on <- paste0('<button type="button" class="selected" aria-pressed="true" ',
                    'data-cat2-choose="annual">')
annual_off <- paste0('<button type="button" aria-pressed="false" ',
                     'data-cat2-choose="annual">')
monthly_off <- paste0('<button type="button" aria-pressed="false" ',
                      'data-cat2-choose="monthly">')
monthly_on <- paste0('<button type="button" class="selected" aria-pressed="true" ',
                     'data-cat2-choose="monthly">')
html <- sub(annual_on, annual_off, html, fixed = TRUE)
html <- sub(monthly_off, monthly_on, html, fixed = TRUE)
html <- sub('<div class="chart-frame cat2-panel-annual" data-cat2-panel="annual">',
            '<div class="chart-frame cat2-panel-annual" data-cat2-panel="annual" hidden>',
            html, fixed = TRUE)
html <- sub('<div class="chart-frame cat2-panel-monthly" data-cat2-panel="monthly" hidden>',
            '<div class="chart-frame cat2-panel-monthly" data-cat2-panel="monthly">',
            html, fixed = TRUE)
html <- sub('show("annual");});})();', 'show("monthly");});})();',
            html, fixed = TRUE)
if (!grepl('data-cat2-choose="monthly"', html, fixed = TRUE) ||
    !grepl('show("monthly");});})();', html, fixed = TRUE)) {
  stop("Monthly toggle setup could not be verified; HTML left unchanged.")
}

# PDF title identifies the view because its interactive buttons are hidden.
html <- sub(' · Rolling 12-month average</span>',
            ' · Monthly response times</span>', html, fixed = TRUE)
if (!grepl('cat2-print-view-label', html, fixed = TRUE)) {
  html <- sub('<p class="chart-title">Longer-term outlook and milestones</p>',
              paste0('<p class="chart-title">Longer-term outlook and milestones',
                     '<span class="cat2-print-view-label"> · Monthly response times</span></p>'),
              html, fixed = TRUE)
}
if (!grepl(' · Monthly response times</span>', html, fixed = TRUE)) {
  stop("PDF heading update could not be verified; HTML left unchanged.")
}
# The displayed note below either panel is small and consistent.
html <- sub('(<p class="cat2-chart-caption" data-cat2-caption>).*?(</p>)',
            paste0('\\1',
                   'Monthly response times. Targets: 25 min annual average in 2026/27; ',
                   '18 min average by March 2029.', '\\2'),
            html, perl = TRUE)

# Remove the conspicuous separate historical methods note, then append its
# substantive content to the pre-existing Method paragraph at the same size.
html <- gsub('<p class="cat2-annual-method"[^>]*>.*?</p>', '', html, perl = TRUE)
html <- gsub('<p style="font-size:9px;margin-top:5px;color:#425a62">.*?</p>',
             '', html, perl = TRUE)
method_pattern <- '(<p><strong>Method:</strong>[^<]*)(</p>)'
if (!grepl(method_pattern, html, perl = TRUE)) {
  stop("Original Category 2 Method paragraph not found; HTML left unchanged.")
}
method_addendum <- paste0(
  " Rolling averages use national AmbSYS A10/A30 totals (or eleven complete ",
  "trust totals where needed) and require 12 complete months. Future incident ",
  "counts are projected; annual target probabilities use correlated forecast errors."
)
if (!grepl("Rolling averages use national AmbSYS A10/A30 totals", html,
           fixed = TRUE)) {
  html <- sub(method_pattern, paste0("\\1", method_addendum, "\\2"),
              html, perl = TRUE)
}

# Late CSS overrides older generator/print-label styles without changing any
# model output. Idempotent on repeated execution.
html <- gsub('<style id="cat2-pdf-view-label-css">.*?</style>', '', html,
             perl = TRUE)
html <- gsub('<style id="cat2-monthly-default-print-css">.*?</style>', '',
             html, perl = TRUE)
css <- paste0(
  '<style id="cat2-monthly-default-print-css">',
  '.cat2-chart-toggle .cat2-print-view-label{display:none!important}',
  '@media print{',
  '.cat2-chart-toggle .cat2-print-view-label{display:inline!important}',
  '.cat2-chart-toggle .chart-title{font-size:8pt}',
  '.cat2-chart-toggle .cat2-panel-annual{display:none!important}',
  '.cat2-chart-toggle .cat2-panel-monthly{display:block!important}',
  '.cat2-chart-toggle .cat2-chart-caption{font-size:4.5pt!important}',
  '}',
  '</style>'
)
html <- sub('</head>', paste0(css, '</head>'), html, fixed = TRUE)

# All checks precede the write; a failure does not leave a partial output.
stopifnot(grepl(' · Monthly response times</span>', html, fixed = TRUE))
stopifnot(grepl('cat2-panel-annual" data-cat2-panel="annual" hidden',
               html, fixed = TRUE))
stopifnot(grepl('data-cat2-panel="monthly">', html, fixed = TRUE))
stopifnot(grepl('Rolling averages use national AmbSYS A10/A30 totals',
               html, fixed = TRUE))
stopifnot(!grepl('<p class="cat2-annual-method"', html, fixed = TRUE))
writeLines(html, html_path, useBytes = TRUE)
message("Category 2 HTML and print default: Monthly response times.")
message("Historical source note integrated into the original Method paragraph.")
message('Now run source("scripts/19_export_publication_bundle.R") and release QA.')
