# Monthly publication workflow

The pipeline supports two short publication moments without creating a separate manual
analysis process.

## Monday: forecast edition

1. Run a full forecast refresh. The mode is deliberately explicit so a Thursday run
   cannot silently replace the forecast that is waiting to be scored:

   ```r
   options(
     nhs.outlook.publication_mode = "forecast",
     ae.refresh.start_stage = 1
   )
   source("scripts/18_run_monthly_refresh.R")
   ```

   This freezes the exact published rows in
   `output/performance/latest_published_forecast_rows.csv` and also writes a dated
   copy. The publication bundle is under
   `output/publication/YYYY-MM-DD/forecast/`.
2. Confirm that stage 20 reports no fatal failure. Review
   `output/qa/release_signoff.csv`, `output/qa/release_qa_warnings.csv`,
   `output/qa/core_import_profile.csv` and `output/core/model_status.csv`.
3. Complete the pending manual rows in the release sign-off. If needed, add one factual
   paragraph to `config/editorial_commentary.csv` using the forecast month (`YYYY-MM`)
   and edition `forecast`, then set
   `options(nhs.outlook.publication_mode = "forecast")` and rerun stages 17, 19 and 20.
4. Publish `nhs-performance-outlook-forecast-*.pdf`, with the metric PDFs as optional
   deep-dive downloads. Paste the generated `substack-forecast-*.md` into the post.

The Actual column is intentionally blank. Do not fill it with an estimate or revised
model run.

## Thursday: outturn edition

1. Run a new full discovery after the official release files are available:

   ```r
   options(
     nhs.outlook.publication_mode = "outturn",
     ae.refresh.start_stage = 1
   )
   source("scripts/18_run_monthly_refresh.R")
   ```

   Starting at stage 1 matters: it rediscovers publication pages before downloading
   and importing the new files. Do not resume at stage 14 on release day. There is no
   need to delete downloads, logs or forecast archives.
2. The pipeline matches each new result to the frozen Monday snapshot and creates
   `nhs-performance-outturn-latest.html`, the outturn PDF and Markdown draft under
   `output/publication/YYYY-MM-DD/outturn/`. It also creates a one-page outturn
   provider watch for every published metric with defensible provider evidence. These
   pages use the newly released observations to update the provider distribution,
   sustained-signal counts and largest favourable/adverse trajectory moves. Newly
   recalculated next-release national forecasts are not put into the outturn bundle.
3. If one or more new observations have not yet appeared upstream, the outturn run
   stops and names the missing indicators. Wait briefly and rerun the same stage-1
   command; it will reuse unchanged files and fetch new or corrected ones. In practice,
   allow a little time after the nominal 09:30 release rather than assuming every file
   is visible at exactly 09:30.
4. Check the genuine-release interval coverage rows and confirm that the archived
   forecast values have not changed. Add a short factual outturn paragraph in
   `config/editorial_commentary.csv` only when
   the generated summary needs context. Use edition `outturn`.
5. Publish the outturn overview as an update to the Monday post. Keep the Monday PDF
   available so the sequence is auditable.

For a backdated or reproducible build, also set
`nhs.outlook.issue_date = "YYYY-MM-DD"`. The runner clears the mode and date after a
successful refresh, so set the publication mode on every publication run.

## Pre-publication checklist

- Confirm that all fatal checks pass in `output/qa/release_signoff.csv`; warnings require
  judgement but do not automatically suppress a genuine performance movement.
- Review rejected optional sources, missing months, source-method transitions and the
  largest monthly changes in `output/qa/core_import_anomalies.csv`.
- For community waits, review `community_waits_service_import_coverage.csv`,
  `community_waits_service_reconciliation.csv` and the exact England total
  reconciliation before publishing the service page. The release gate also checks
  the hand-verified January 2023 musculoskeletal service observation in
  `config/community_service_qa_reference_values.csv` when that month is imported.
- Read every omission or provider-withheld reason in `output/core/model_status.csv`.
- Check that Latest, Outlook and Actual months are correctly dated.
- Inspect the method register, model-comparison warnings, forecasts, 80% ranges and
  target labels for implausible values.
- Confirm that provider spotlights separate latest performance level from movement
  against trajectory. On Thursday, confirm each provider-watch page is dated through
  the newly released month rather than the preceding forecast month.
- The automated gate checks PDF existence, A4 portrait geometry and page count. Still
  inspect the overview, one national page and one provider page at 100% print scale for
  clipping and legibility.
- Check data and target links, commentary wording and the independent-analysis footer.
- Preserve `output/national/*archive*`, `output/provider/*archive*` and
  `output/core/*/*archive*` when moving the project between computers.

If PDF export cannot find Chrome or Edge, the HTML products are still complete. Open
each `*-latest.html` file in a browser and print with A4 portrait, 100% scale, background
graphics enabled, and browser headers and footers disabled.
