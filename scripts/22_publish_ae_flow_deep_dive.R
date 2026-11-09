# Optional outturn companion. Always write a candid status page; run source
# extraction separately (scripts/21_build_ae_flow_deep_dive.R).
if (!dir.exists("output/releases")) dir.create("output/releases", recursive = TRUE)
case_file <- "ae-george-eliot-flow-deep-dive-latest.html"
case_path <- file.path("output/releases", case_file)
panel_path <- "output/deep-dives/george-eliot/george_eliot_panel.csv"
watch_path <- "output/performance/outturn_provider_watch_manifest.csv"
escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
status <- "Awaiting verified supplementary patient-flow series"
notes <- "Source panels for delayed discharge and seven-day discharge have not yet been validated for publication."
rows_html <- ""
if (file.exists(panel_path)) {
  p <- tryCatch(read.csv(panel_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(p) && "month" %in% names(p)) {
    metrics <- c(ae_all_attendances="All A&E attendances", ae_type1_attendances="Type 1 attendances",
      ae_within4_pct="A&E four-hour performance (%)", ga_occupancy_pct="G&A occupancy (%)",
      los21_occupied_share_pct="21+ day patients as share of occupied G&A beds (%)",
      drd_delayed_days_share_pct="Delayed discharge bed days (% of available G&A bed days)",
      weekend_share_pct="Share of weekly discharges occurring at weekends (%)")
    out <- lapply(names(metrics), function(m) {
      if (!(m %in% names(p))) return(NULL)
      v <- suppressWarnings(as.numeric(p[[m]]))
      ix <- which(is.finite(v) & !is.na(p$month))
      if (!length(ix)) return(NULL)
      k <- tail(ix, 1)
      sprintf("<tr><td>%s</td><td>%s</td><td>%.1f</td><td>%s months</td></tr>",
              escape(metrics[[m]]), escape(p$month[k]), v[k], length(ix))
    })
    rows_html <- paste(unlist(out), collapse="\n")
    if (nzchar(rows_html)) {
      status <- "Exploratory provider flow data (not causal evidence)"
      notes <- "Different metrics can end in different months. Review coverage, source definitions and data quality before using this page for external interpretation."
    }
  }
}
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>George Eliot | A&E flow deep dive | NHS Outlook</title>',
'<style>body{font:17px/1.6 system-ui,-apple-system,sans-serif;max-width:920px;margin:2.5rem auto;padding:0 1.2rem;color:#17334b}a{color:#176eb3}h1{line-height:1.2}table{border-collapse:collapse;width:100%;margin:1.5rem 0}th,td{border-bottom:1px solid #ddd;padding:.7rem;text-align:left}.meta{color:#526779}.notice{border-left:4px solid #376ba0;padding:.65rem 1rem;background:#f3f7fa}</style></head><body>',
'<p><a href="ae-four-hour-outturn-latest.html">← A&E monthly outturn</a></p>',
'<h1>George Eliot: is improvement in A&E linked to hospital flow?</h1>',
'<p class="meta">NHS Outlook · exploratory positive-deviance investigation</p>',
'<p class="notice"><b>',escape(status),'</b><br>',escape(notes),'</p>',
'<h2>Why George Eliot?</h2><p>A sustained favourable departure from its historical A&E trajectory makes George Eliot a candidate for investigation. The monitoring signal identifies a question, not its cause.</p>',
'<h2>Patient-flow indicators</h2>',if (nzchar(rows_html)) paste0('<table><thead><tr><th>Indicator</th><th>Latest period</th><th>Value</th><th>Observed months</th></tr></thead><tbody>', rows_html,'</tbody></table>') else '<p>No validated provider flow panel has been loaded.</p>',
'<h2>What could explain the change?</h2><p>Attendance levels and mix; inpatient discharge delays; weekend discharge patterns; bed availability and occupancy; long-stay patients; and changes in clinical decision-making, staffing and operational coordination. These remain hypotheses until tested against consistent time series and operational evidence.</p>',
'<h2>Method and limitations</h2><p>Use NHS England monthly A&E, monthly Beds SitRep, discharge-ready-date and acute discharge collections. Do not combine incompatible definitions or infer causality from coincident changes. See <code>docs/GEORGE_ELIOT_FLOW_DEEP_DIVE.md</code> in the project for methods and QA requirements.</p>',
'</body></html>')
writeLines(html, case_path, useBytes=TRUE)
issue_day <- if (exists("issue_day", inherits=TRUE)) get("issue_day",inherits=TRUE) else format(Sys.Date(),"%Y-%m-%d")
file.copy(case_path, file.path("output/releases", sub("-latest\\.html$",paste0("-",issue_day,".html"),case_file)),overwrite=TRUE)
# A small link on the A&E outturn; only change its body once per release.
for (f in c("output/releases/ae-four-hour-outturn-latest.html",
            "output/releases/ae-four-hour-provider-watch-outturn-latest.html")) {
  if (!file.exists(f)) next
  txt <- paste(readLines(f, warn=FALSE, encoding="UTF-8"), collapse="\n")
  link_id <- 'id="george-eliot-flow-deep-dive"'
  if (grepl(link_id, txt, fixed=TRUE)) next
  insert <- paste0('<aside ',link_id,' style="max-width:960px;margin:2rem auto;padding:1rem;border-left:3px solid #3973a6;background:#f4f8fc">',
     '<strong>Provider improvement investigation</strong><p>George Eliot: investigating A&E improvement alongside attendances, discharge and bed pressure. ',
     '<a href="',case_file,'">Read the exploratory deep dive</a>.</p></aside>')
  txt <- sub("</body>",paste0(insert,"</body>"),txt,fixed=TRUE)
  writeLines(txt,f,useBytes=TRUE)
}
message("Deep dive companion: ",case_path)
