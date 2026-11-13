NHS OUTLOOK — RETAIN PROBABILITY IN OUTTURN EDITIONS

Apply these files over the previous NHS_Outlook_target_probability_all_reports_2026-11-10.zip.
This patch retains the 9 November forecasts and does not alter models, data, or archives.

Forecast edition:
  Indicator / Latest / Next-release Forecast / March 2027 outlook
Outturn edition:
  Indicator / Issued Forecast / Published Actual / March 2027 outlook at issue

Deep dive forecast: 3 cards — latest, forecast, target-date outlook.
Deep dive outturn: 3 cards — baseline, forecast, actual; compact target-date
outlook strip directly below them, including the original model-implied chance.

TEST (after applying patch):
  source("tests/test_target_probability_cards.R")
  source("tests/test_outturn_target_retention.R")

Existing locked 9 November forecasts can be rerendered using:
  options(nhs.outlook.render_issue_date=as.Date("2026-11-09"),
          nhs.outlook.render_mode="forecast",
          nhs.outlook.render_pdf=FALSE)
  source("scripts/24_render_frozen_edition.R")

The outturn is NOT rerendered from forecast mode. When official results arrive,
run the normal outturn refresh against the originally issued forecast snapshot.
The probability shown in the outturn is the chance calculated at issue,
not a revised probability conditioned on the newly published actual. A&E
uses the issued overview snapshot; if it is unavailable or mismatched,
the value is shown as Not estimated rather than silently recalculated.

NOTE: R is unavailable in this environment; tests not executed here.
