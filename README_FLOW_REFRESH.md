# NHS Outlook: provider flow source refresh

Add `scripts/20_download_flow_sources.R` and `scripts/23_refresh_ae_flow_deep_dive.R` to the project. Keep the existing scripts 21 and 22.

Run from the NHS Outlook project root:

```r
source('scripts/23_refresh_ae_flow_deep_dive.R')
```

This discovers and downloads **monthly provider CSVs** from the NHS England beds, Acute Discharge SitRep, and Discharge Ready Date publication pages. Existing files are cached under `data/flow/downloaded/{beds,acute,drd}/`, with an inventory and a full month-by-month coverage report. Data published after the latest available release are reported as missing, not filled in. To refresh revised source files, use `options(nhs.outlook.flow.force_refresh=TRUE)` before running.

The existing `21_build_ae_flow_deep_dive.R` should then inspect the cached provider files. **IMPORTANT:** its discharge mapping is not yet fully validated; finding files does not guarantee discharge fields will populate. Use the output coverage QA and candidate metrics to verify definition and parser coverage. All types of delayed discharge are NOT interchangeable. The old automatic download option inside script 21 remains turned off by the wrapper to avoid duplicate downloads.

Set `options(nhs.outlook.flow.start_month='2024-04-01')` and `options(nhs.outlook.flow.end_month='2026-09-01')` if you want a fixed retrospective window. Monthly A&E source data continue to come from NHS Outlook's existing import pipeline.

Requires `rvest`, `xml2`, `dplyr`, `stringr`, `readr`, and `tibble` through the project's existing `renv` setup. Commit the two scripts and updated `renv.lock`, but not the downloaded raw data.
