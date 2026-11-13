NHS OUTLOOK — EXPLICIT TARGET-DATE DISPLAY AND SAFE RE-RENDER
10 November 2026

Builds on the previously provided TARGET PROBABILITY PILOT.
It does NOT replace its files R/target_probability.R, scripts/17_build_performance_outlook.R,
scripts/20_release_qa.R or tests/test_target_probabilities.R.

FILES IN THIS ZIP:
 R/performance_outlook.R                  (from probability pilot, with new display)
 R/edition_archive.R                      (archive core_model.csv in future editions)
 reports/nhs_performance_outlook_template.html
 scripts/24_render_frozen_edition.R       (adds optional revision support and legacy config fallback)
 tests/test_target_display.R              (new)

WHAT CHANGES:
 * In forecast editions, the column previously occupied by the empty 'Actual'
   placeholder becomes 'Target-date outlook'.
 * Each metric explicitly shows the target month, model projection in that month,
   numerical target threshold (>= if higher is better, <= if lower is better),
   and the model-implied probability of meeting it.
 * The NEXT-RELEASE forecast remains in the adjacent column; the two different
   forecast horizons are not conflated.
 * Outturn editions retain the actual-vs-issued-forecast comparison as before.
 * The frozen-edition renderer now restores the working directory and options
   and cleans up temporary files after a render or an error.
 * An optional nhs.outlook.render_revision = 'r1' selects a previously locked
   analytical revision when re-rendering. Original edition is the default.
 * Future editions also preserve config/core_model.csv, a missing
   presentation dependency in older archives. For older editions, the frozen
   renderer temporarily copies the current config/core_model.csv, warns about
   the limitation, and never modifies locked archives.
 * No model, underlying forecast or archived forecast is changed.

BEFORE STARTING:
 Ensure you have already applied NHS_Outlook_target_probability_pilot.zip.
 Also retain your chart-axis patch; this ZIP does not overwrite chart code.
 If you have made separate local changes to R/performance_outlook.R or the
 overview template, compare and merge rather than blindly overwrite.

RUN TESTS (from NHS-Outlook project root, with prior pilot installed):
 source('tests/test_target_probabilities.R')
 source('tests/test_target_display.R')

PRESENTATION-ONLY RENDER OF A LOCKED EDITION (recommended):
 options(
   nhs.outlook.render_issue_date = as.Date('2026-11-09'),
   nhs.outlook.render_mode = 'forecast',
   nhs.outlook.render_revision = '',  # select 'r1' if you want the locked r1 edition
   nhs.outlook.render_pdf = FALSE  # HTML first; set TRUE for PDFs after review
 )
 source('scripts/24_render_frozen_edition.R')

 This generates under output/rerenders/2026-11-09/forecast/ (or
 revisions/r1), without updating the website or locked analytical archives.
 Review the HTML first. Then set render_pdf=TRUE and re-run for PDFs.
 This is presentation-only: it does not run stage 20 release QA.

Check locked edition(s) first:
 file.exists('output/editions/2026-11-09/forecast/edition_manifest.csv')
 file.exists('output/editions/2026-11-09/forecast/revisions/r1/edition_manifest.csv')
 Choose render_revision='' for original, or 'r1' for the separately locked r1.

IF YOU NEED A NEW ANALYTICAL FORECAST, do not re-render a frozen edition.
 Use your usual R/run_publication.R workflow with a genuinely new issue date,
 refresh data, and complete stage 20 QA. This patch does not create a new
 forecast or publication.

IF AN OUTTURN ALREADY USES DATA RELEASED AFTER THAT FORECAST:
 Do NOT rebuild a new analytic forecast with hindsight; use the locked
 edition to re-render, preserving the forecast issue date.

IMPORTANT: Review and approve before copying re-rendered files to your
 site. Do not run R/run_publication.R to make a cosmetic correction to a
 locked analytical edition, and do not delete output/editions.

R was not available in the patch preparation environment.
