# NHS Outlook design and outturn update

This update changes the shared publication generators rather than individual
generated HTML files.

## What changed

- The web layout is wider and more legible while the print layout remains A4.
- The outturn overview now distinguishes three separate questions: whether the
  indicator improved, whether it beat the forecast, and whether it was inside
  the expected range.
- The baseline column is labelled **At forecast** and indicator names link to
  their detail pages, removing the separate Detail column.
- Provider counts have been removed from the national overview.
- The first page of each indicator contains a compact **What changed?** panel.
- Outturn charts retain the published forecast, its 80% range and the actual.
- Provider sparklines compare actual results with one six-month path fixed
  before the window and start at the same observed value.
- Technical provider labels have been replaced with reader-facing language.
- The website publisher now supports both forecast and outturn editions.
- Release QA has been updated for the revised headings and link structure.

## Run the refreshed outturn

From the project root in R:

```r
options(
  nhs.outlook.publication_mode = "outturn",
  nhs.outlook.publication_status = "pilot",
  nhs.outlook.issue_date = as.Date("2026-10-13")
)
source("scripts/18_run_monthly_refresh.R")
source("R/run_publication.R")
```

The second command assembles `publication_site/` and mirrors the current public
files into the repository root for GitHub Pages.

Before committing, open:

- `output/releases/nhs-performance-outturn-latest.html`
- at least one national indicator page;
- at least one provider-watch page; and
- `publication_site/index.html`.

Then publish with:

```bash
git add -A
git commit -m "Improve NHS Outlook design and outturn interpretation"
git push origin main
```

If Git reports an existing `.git/index.lock`, first close any Git process or
editor using the repository. Only if none is running, remove that single lock
file and retry the commit.
