NHS Outlook | Category 2 monthly default + methods integration | 13 November 2026

CHANGES
- HTML defaults to Monthly; the 12-month-average toggle remains available.
- PDF/print always displays Monthly (toggle hidden) and identifies the view
  in the chart heading as "Monthly response times".
- Both 25-minute 2026/27 and 18-minute 2028/29 average targets remain.
- Removes the separate, oversized historical-methods paragraph. Integrates
  its content into the existing small-print Method paragraph.
- No changes to source data, national or provider forecasts, target
  probabilities, archived analytical revisions or other indicators.

FILES
R/ambulance_cat2_history_chart.R         Full generator for future builds
scripts/36_clarify_cat2_pdf_chart.R      Presentation-only existing HTML fix

INSTALL
Extract the ZIP into the root of the ae-trust-forecast project, preserving
R/ and scripts/. Replace both existing files.

RUN IN RSTUDIO (does not create a new analytical revision):
  parse(file="R/ambulance_cat2_history_chart.R")
  parse(file="scripts/36_clarify_cat2_pdf_chart.R")
  source("scripts/36_clarify_cat2_pdf_chart.R")
  browseURL("output/releases/ambulance-category-2-outturn-latest.html")
  source("scripts/19_export_publication_bundle.R")
  source("scripts/20_release_qa.R")

REVIEW
- HTML initially shows Monthly; switching to 12-month changes the chart.
- The PDF shows Monthly with dotted target lines for the respective years.
- The methods paragraph under the chart contains the history and probability
  source note in the same typographic style as the other method text.
- Review the exported Category 2 PDF at actual A4 size before publishing.

Note: The script only updates the "-latest" HTML and standard publication bundle;
if your site has a separate immutable dated/revision HTML copy, rebuild/copy
that publication artifact as part of your normal approved release workflow.
