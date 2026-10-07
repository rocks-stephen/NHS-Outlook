# ============================================================
# NHS OUTLOOK — BUILD PUBLICATION WEBSITE
# ============================================================
#
# HTML source:
#   output/releases/
#
# PDF source:
#   output/publication/YYYY-MM-DD/forecast/
#
# Output:
#   publication_site/
#       index.html
#       [indicator HTML pages]
#       forecasts/YYYY-MM-DD/*.pdf
#
# Run:
#   source("R/publish_outlook_website.R")
#
# ============================================================

project_root <- getwd()

html_source_dir <- file.path(project_root, "output", "releases")
publication_root <- file.path(project_root, "output", "publication")
site_dir <- file.path(project_root, "publication_site")

if (!dir.exists(html_source_dir)) {
  stop("HTML source folder not found:\n", html_source_dir)
}

if (!dir.exists(publication_root)) {
  stop("Publication folder not found:\n", publication_root)
}

release_dirs <- list.dirs(publication_root, recursive = FALSE, full.names = TRUE)
release_names <- basename(release_dirs)
valid_release <- grepl("^\\d{4}-\\d{2}-\\d{2}$", release_names)

release_dirs <- release_dirs[valid_release]
release_names <- release_names[valid_release]

if (length(release_dirs) == 0) {
  stop("No YYYY-MM-DD publication folders found.")
}

release_dates <- as.Date(release_names)
latest_i <- which.max(release_dates)

release_date <- release_dates[latest_i]
release_dir <- release_dirs[latest_i]
forecast_dir <- file.path(release_dir, "forecast")

if (!dir.exists(forecast_dir)) {
  stop("Forecast folder not found:\n", forecast_dir)
}

message("Publication date: ", release_date)
message("HTML source:      ", html_source_dir)
message("PDF source:       ", forecast_dir)

html_files <- list.files(
  html_source_dir,
  pattern = "\\.html$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(html_files) == 0) {
  stop("No HTML files found in:\n", html_source_dir)
}

preferred <- file.path(
  html_source_dir,
  "nhs-performance-outlook-forecast-latest.html"
)

if (file.exists(preferred)) {

  main_html <- preferred

} else {

  candidates <- html_files[
    grepl(
      "nhs[-_ ]?performance[-_ ]?outlook.*forecast",
      basename(html_files),
      ignore.case = TRUE
    )
  ]

  if (length(candidates) == 0) {
    candidates <- html_files[
      grepl(
        "nhs[-_ ]?outlook",
        basename(html_files),
        ignore.case = TRUE
      )
    ]
  }

  if (length(candidates) == 0) {
    stop(
      "Could not identify the main NHS Outlook HTML in:\n",
      html_source_dir,
      "\n\nHTML files found:\n",
      paste(basename(html_files), collapse = "\n")
    )
  }

  date_string <- as.character(release_date)

  dated_match <- candidates[
    grepl(date_string, basename(candidates), fixed = TRUE)
  ]

  if (length(dated_match) > 0) {
    main_html <- dated_match[1]
  } else {
    info <- file.info(candidates)
    main_html <- candidates[which.max(info$mtime)]
  }
}

message("Main HTML:        ", basename(main_html))

pdf_files <- list.files(
  forecast_dir,
  pattern = "\\.pdf$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(pdf_files) == 0) {
  stop("No PDFs found in:\n", forecast_dir)
}

main_pdf_candidates <- pdf_files[
  grepl(
    "nhs[-_ ]?performance[-_ ]?outlook.*forecast",
    basename(pdf_files),
    ignore.case = TRUE
  )
]

if (length(main_pdf_candidates) == 0) {
  main_pdf_candidates <- pdf_files[
    grepl(
      "nhs[-_ ]?outlook",
      basename(pdf_files),
      ignore.case = TRUE
    )
  ]
}

main_pdf <- NA_character_

if (length(main_pdf_candidates) > 0) {

  dated_match <- main_pdf_candidates[
    grepl(
      as.character(release_date),
      basename(main_pdf_candidates),
      fixed = TRUE
    )
  ]

  if (length(dated_match) > 0) {
    main_pdf <- dated_match[1]
  } else {
    info <- file.info(main_pdf_candidates)
    main_pdf <- main_pdf_candidates[which.max(info$mtime)]
  }

  message("Main PDF:         ", basename(main_pdf))

} else {
  message("Main PDF:         not found")
}

forecast_type <- function(filename) {

  x <- tolower(basename(filename))

  if (
    grepl("nhs[-_ ]?performance[-_ ]?outlook", x) ||
    grepl("^nhs[-_ ]?outlook", x)
  ) {
    return(NA_character_)
  }

  # Community 18-week BEFORE generic RTT
  if (
    grepl("community", x) &&
    grepl("18[-_ ]?week|18week", x)
  ) {
    return("community_18")
  }

  if (
    grepl("community|ucr", x) &&
    grepl("2[-_ ]?hour|two[-_ ]?hour|ucr", x)
  ) {
    return("ucr")
  }

  if (
    grepl("a.?e", x) ||
    grepl("4[-_ ]?hour|four[-_ ]?hour", x)
  ) {
    return("ae")
  }

  if (
    grepl("ambulance", x) ||
    grepl("cat[-_ ]?2|category[-_ ]?2", x)
  ) {
    return("ambulance")
  }

  if (
    grepl("rtt", x) ||
    (
      grepl("18[-_ ]?week|18week", x) &&
      !grepl("community", x)
    )
  ) {
    return("rtt")
  }

  if (grepl("diagnostic", x)) {
    return("diagnostics")
  }

  if (
    grepl("cancer", x) ||
    grepl("62[-_ ]?day", x)
  ) {
    return("cancer")
  }

  if (
    grepl("talking", x) ||
    grepl("iapt", x)
  ) {
    return("talking")
  }

  NA_character_
}

types <- vapply(pdf_files, forecast_type, character(1))

detail_tbl <- data.frame(
  source_file = pdf_files,
  type = types,
  stringsAsFactors = FALSE
)

detail_tbl <- detail_tbl[
  !is.na(detail_tbl$type),
  ,
  drop = FALSE
]

meta <- data.frame(
  type = c(
    "ae",
    "ambulance",
    "rtt",
    "diagnostics",
    "cancer",
    "ucr",
    "community_18",
    "talking"
  ),
  label = c(
    "A&E four-hour performance",
    "Category 2 ambulance response",
    "RTT within 18 weeks",
    "Diagnostics waiting over six weeks",
    "Cancer treatment within 62 days",
    "Two-hour urgent community response",
    "Community waiting list within 18 weeks",
    "Talking Therapies within six weeks"
  ),
  group = c(
    "Urgent care",
    "Urgent care",
    "Planned care",
    "Planned care",
    "Planned care",
    "Community",
    "Community",
    "Mental health"
  ),
  order = 1:8,
  stringsAsFactors = FALSE
)

detail_tbl <- merge(
  detail_tbl,
  meta,
  by = "type",
  all.x = TRUE,
  sort = FALSE
)

if (nrow(detail_tbl) > 0) {

  detail_tbl$mtime <- file.info(detail_tbl$source_file)$mtime

  detail_tbl <- detail_tbl[
    order(detail_tbl$type, detail_tbl$mtime, decreasing = TRUE),
  ]

  detail_tbl <- detail_tbl[
    !duplicated(detail_tbl$type),
    ,
    drop = FALSE
  ]

  detail_tbl <- detail_tbl[
    order(detail_tbl$order),
    ,
    drop = FALSE
  ]
}

dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

forecast_site_dir <- file.path(site_dir, "forecasts")

if (dir.exists(forecast_site_dir)) {
  unlink(forecast_site_dir, recursive = TRUE, force = TRUE)
}

pdf_site_dir <- file.path(
  forecast_site_dir,
  as.character(release_date)
)

dir.create(
  pdf_site_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Copy all generated HTML pages so links on the main Outlook still work
for (f in html_files) {
  file.copy(
    from = f,
    to = file.path(site_dir, basename(f)),
    overwrite = TRUE
  )
}

main_pdf_web_path <- NA_character_

if (!is.na(main_pdf)) {

  main_pdf_name <- basename(main_pdf)

  file.copy(
    from = main_pdf,
    to = file.path(pdf_site_dir, main_pdf_name),
    overwrite = TRUE
  )

  main_pdf_web_path <- file.path(
    "forecasts",
    as.character(release_date),
    main_pdf_name
  )

  main_pdf_web_path <- gsub("\\\\", "/", main_pdf_web_path)
}

if (nrow(detail_tbl) > 0) {

  detail_tbl$web_name <- basename(detail_tbl$source_file)

  detail_tbl$web_path <- file.path(
    "forecasts",
    as.character(release_date),
    detail_tbl$web_name
  )

  detail_tbl$web_path <- gsub("\\\\", "/", detail_tbl$web_path)

  for (i in seq_len(nrow(detail_tbl))) {
    file.copy(
      from = detail_tbl$source_file[i],
      to = file.path(pdf_site_dir, detail_tbl$web_name[i]),
      overwrite = TRUE
    )
  }
}

html <- paste(
  readLines(
    main_html,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

html <- sub(
  '<span class="edition">Forecast edition</span>',
  '<span class="edition">Pilot · Forecast edition</span>',
  html,
  fixed = TRUE
)

extra_css <- '
    .outlook-actions{
      display:flex;
      gap:10px;
      align-items:center;
      margin:13px 0 2px;
      flex-wrap:wrap
    }

    .button-link{
      display:inline-block;
      padding:8px 12px;
      border:1px solid var(--brand);
      border-radius:3px;
      color:var(--brand);
      text-decoration:none;
      font-size:9px;
      font-weight:800;
      letter-spacing:.04em
    }

    .button-link.primary{
      background:var(--brand);
      color:#fff
    }

    .details-page{
      width:min(794px,calc(100% - 28px));
      margin:24px auto 48px;
      background:var(--paper);
      box-shadow:0 14px 38px rgba(20,44,38,.12);
      padding:30px 32px 34px
    }

    .details-kicker{
      color:var(--brand-2);
      font-size:9px;
      font-weight:800;
      letter-spacing:.14em;
      text-transform:uppercase
    }

    .details-page h2{
      margin:8px 0 5px;
      font:700 30px/1.05 Georgia,"Times New Roman",serif;
      letter-spacing:-.025em
    }

    .details-intro{
      max-width:620px;
      margin:0 0 22px;
      color:var(--muted);
      font-size:11px
    }

    .detail-group{
      margin-top:22px
    }

    .detail-group h3{
      margin:0 0 7px;
      padding-bottom:6px;
      border-bottom:1px solid var(--ink);
      color:var(--brand-2);
      font-size:8px;
      font-weight:800;
      letter-spacing:.12em;
      text-transform:uppercase
    }

    .detail-list{
      display:grid;
      grid-template-columns:1fr 1fr;
      gap:0 20px
    }

    .detail-item{
      display:flex;
      justify-content:space-between;
      gap:18px;
      align-items:center;
      padding:10px 0;
      border-bottom:1px solid var(--line)
    }

    .detail-name{
      color:var(--ink);
      font-size:11px;
      font-weight:800
    }

    .detail-link{
      white-space:nowrap;
      color:var(--brand-2);
      font-size:9px;
      font-weight:800;
      text-decoration:none
    }

    .detail-link:hover{
      text-decoration:underline
    }

    .details-note{
      margin-top:22px;
      color:var(--muted);
      font-size:8px;
      line-height:1.45
    }

    @media(max-width:680px){
      .details-page{
        width:100%;
        margin:18px 0 0;
        box-shadow:none;
        padding:24px 18px 30px
      }

      .detail-list{
        grid-template-columns:1fr
      }
    }

    @media print{
      .outlook-actions,
      .details-page{
        display:none !important
      }
    }
'

html <- sub(
  "</style>",
  paste0(extra_css, "\n  </style>"),
  html,
  fixed = TRUE
)

if (!is.na(main_pdf_web_path)) {

  actions_html <- paste0(
    '\n<div class="outlook-actions">',
    '<a class="button-link primary" href="',
    main_pdf_web_path,
    '">Download this edition as PDF</a>',
    '<a class="button-link" href="#detailed-forecasts">',
    'Detailed forecasts</a>',
    '</div>\n'
  )

} else {

  actions_html <- paste0(
    '\n<div class="outlook-actions">',
    '<a class="button-link" href="#detailed-forecasts">',
    'Detailed forecasts</a>',
    '</div>\n'
  )
}

html <- sub(
  "</table>",
  paste0("</table>", actions_html),
  html,
  fixed = TRUE
)

detail_section <- c(
  '<section class="details-page" id="detailed-forecasts">',
  '  <div class="details-kicker">Supporting analysis</div>',
  '  <h2>Detailed forecasts</h2>',
  '  <p class="details-intro">',
  '    Indicator-level forecasts provide the recent trend, modelled outlook,',
  '    uncertainty and supporting analysis behind the headline NHS Outlook.',
  '  </p>'
)

if (nrow(detail_tbl) == 0) {

  detail_section <- c(
    detail_section,
    '  <p>No detailed forecast PDFs were found for this release.</p>'
  )

} else {

  groups <- unique(detail_tbl$group)

  for (g in groups) {

    rows <- detail_tbl[
      detail_tbl$group == g,
      ,
      drop = FALSE
    ]

    detail_section <- c(
      detail_section,
      '  <div class="detail-group">',
      paste0("    <h3>", g, "</h3>"),
      '    <div class="detail-list">'
    )

    for (i in seq_len(nrow(rows))) {

      detail_section <- c(
        detail_section,
        paste0(
          '      <div class="detail-item">',
          '<span class="detail-name">',
          rows$label[i],
          '</span>',
          '<a class="detail-link" href="',
          rows$web_path[i],
          '">View PDF →</a>',
          '</div>'
        )
      )
    }

    detail_section <- c(
      detail_section,
      '    </div>',
      '  </div>'
    )
  }
}

detail_section <- c(
  detail_section,
  paste0(
    '  <p class="details-note">',
    'Release: ',
    format(release_date, "%d %B %Y"),
    '. The NHS Outlook above is the headline publication; ',
    'these PDFs provide supporting indicator-level detail.',
    '</p>'
  ),
  '</section>'
)

detail_section <- paste(detail_section, collapse = "\n")

html <- sub(
  "</article>",
  paste0("</article>\n", detail_section),
  html,
  fixed = TRUE
)

site_index <- file.path(site_dir, "index.html")

writeLines(
  html,
  site_index,
  useBytes = TRUE
)

message("")
message("============================================")
message("NHS Outlook website built successfully")
message("============================================")
message("")
message("Release: ", release_date)
message("Homepage source: ", basename(main_html))
message("Homepage: ", site_index)
message("Detailed forecasts: ", nrow(detail_tbl))

if (nrow(detail_tbl) > 0) {
  for (i in seq_len(nrow(detail_tbl))) {
    message(
      "  - ",
      detail_tbl$label[i],
      " -> ",
      basename(detail_tbl$source_file[i])
    )
  }
}

message("")

browseURL(
  normalizePath(
    site_index,
    winslash = "/"
  )
)
