# National and provider month specification

## Shared core-metric panel

RTT, diagnostics, cancer, ambulance, UCR, community waits and Talking Therapies data
are standardized to one national panel, with provider rows retained only where their
identifiers and definitions support longitudinal analysis. Each row contains
`metric_id`, `calendar_month`, `entity_id`,
`entity_name`, `numerator`, `denominator`, `value`, `complete_submission`,
`source_method`, `source_file`, `source_url` and `source_sha256`. Proportion measures
retain numerator and denominator; Category 2 mean response time has no additive
numerator/denominator and is stored in minutes.

| Metric | Official row used | Direction |
|---|---|---|
| RTT within 18 weeks | England overview including missing-trust estimates; provider full extract `Part_2`, treatment function `C_999`, weeks 0–18 / `Total All` | Higher is better |
| Diagnostics over six weeks | DM01 total waiting list and `Number waiting 6+ Weeks` | Lower is better |
| Cancer within 62 days | Revised national combined-standard time series; final standard-specific provider CSV (`ALL CANCERS`, `ALL ROUTES`) before the combined-file era; provider basis, `62D`, `ALL CANCERS`, `ALL ROUTES`, `ALL MODALITIES` thereafter | Higher is better |
| Category 2 response | AmbSYS `A31` mean seconds divided by 60 | Lower is better |
| Two-hour UCR | `Table 1` national percentage by internal workbook month | Higher is better |
| Community waiting list within 18 weeks | `Table 3` England total less all published over-18-week bands, divided by total waiting list | Higher is better |
| Talking Therapies within six weeks | Official Waiting Times chart value by month, normalized from percentage points | Higher is better |

Provider rows are restricted to NHS trust/ambulance-service organisations and preserve
explicit missing months. RTT, diagnostics and cancer also apply minimum current-volume
rules before a sustained signal can be issued. The source contracts are intentionally
strict: missing or renamed required fields stop import for review.

UCR workbooks contain stable provider codes, but their performance table does not
contain the matching completed-referral denominator. Community Tables 4–4h contain
England, region, ICB and organisation-name rows across service columns, but no stable
provider codes. Those rows are retained in separate service-band and derived-summary
panels for descriptive analysis and later mapping; they are not promoted into the
provider forecasting panel. The Talking Therapies chart is national. These three
indicators therefore publish national outlooks without provider trajectory pages rather
than manufacturing identifiers or volume controls.

## Community service waiting-list panel

`data-interim/core/community_waits_service_bands.csv` reshapes Tables 4–4h to one
row per month, geography, service and wait band. It distinguishes reported values,
suppression, non-submission and unexpected non-numeric cells. Newer workbooks split
waits over 52 weeks into 52–104 and over 104 weeks; older workbooks publish one over-52
band. Both schemas are normalized without splicing or guessing.

`data-interim/core/community_waits_service_summary.csv` derives:

`within_18_weeks = total_waiting_list - all_published_over_18_week_bands`

and divides that residual by the total waiting list. It separately sums every published
wait band and records the difference from the total. NHS England notes that weekly-band
sums may not match the total because some providers were still collecting waits by
weeks. The difference is therefore a visible review field, not silently forced to zero.
Blank service cells remain missing, never zero. Organisation identities are labelled
`source_name_only_unharmonised` until a reviewed code mapping is available.

The cancer archive has separate national and provider inputs. England performance is
taken from the official provider-based national time-series workbook "with revisions".
Its comparable combined 62-day block is consecutive from April 2022; the older dates
in the workbook title relate to other standards and are not spliced into this measure.
This supplies 52 months through July 2026, enough for the configured 24-month training
window and 24 one-step backtests. From October 2023 through August 2025, the selected
final monthly publication page supplies file 7, the all-cancer 62-day provider table.
From September 2025 onward, discovery can select the direct monthly combined CSV.
The importer reconciles overlapping England rates to within 0.5 percentage points,
while allowing the revised national workbook's historical counts to differ from an
archived monthly vintage.

`output/qa/core_national_history.csv` records the first, last and latest consecutive
months for every headline series against its configured training-plus-backtest
requirement. Import stops if any of the four added core measures cannot support the
full rolling backtest. This is a data-contract failure, not a reason to silently
shorten the five-indicator bulletin. Provider panels deliberately start in recent,
stable organisational eras and are used for six-month signals rather than to extend
the England-level forecasting history.

## 1. National-month table

One row per England calendar month. The principal outcome is:

`ae4h_performance = ae4h_within_4h_n / ae4h_attendances_n`

where the performance denominator is preserved as:

`ae4h_attendances_n = ae4h_within_4h_n + ae4h_over_4h_n`

The official national `Performance` sheet supplies the within- and over-four-hour
counts directly. The importer recalculates the percentage and compares it with the
published all-types percentage. The corresponding Type 1 fields are retained as
diagnostics but are not the headline modelling outcome.

| Field | Type | Rule |
|---|---|---|
| `calendar_month` | Date | First day of month |
| `ae4h_within_4h_n` | numeric | Published all-types numerator |
| `ae4h_over_4h_n` | numeric | Published all-types breach count |
| `ae4h_attendances_n` | numeric | All-types performance denominator |
| `ae4h_performance` | numeric | Recalculated all-types proportion |
| `published_ae4h_performance` | numeric | Direct workbook value retained for QA |
| `type1_within_4h_n` | numeric | Published national numerator; may be fractional in apportioned pre-June-2015 months |
| `type1_over_4h_n` | numeric | Published national breach count |
| `type1_attendances_n` | numeric | Sum of within and over counts for the population contributing performance |
| `type1_performance` | numeric | Recalculated proportion |
| `published_type1_performance` | numeric | Direct workbook value retained for QA |
| `source_method` | character | `weekly_apportioned_monthly_estimate` or `monthly_collection` |
| `national_comparability_era` | character | Estimated, pre-CRS full, CRS-excluding-14, or post-CRS full |
| `crs_14_trusts_excluded` | logical | True May 2019–May 2023 |
| `booked_appointments_definition_era` | character | Definition flag from August 2020 |
| `april_2026_provider_reporting_change` | logical | Context flag; not asserted to be a national break |
| source fields | character | URL, page, filename, revision label and SHA-256 |
| `qa_flags` | character | Semicolon-delimited audit flags |

National coverage begins in November 2010 because that is the earliest Type 1
performance month in the current official workbook. The Activity sheet starts in
August 2010, but activity alone cannot supply the principal four-hour outcome.

## 2. Provider-month source table

One row per source organisation × calendar month, plus separately labelled aggregate
rows retained from the publication. No trust identity is overwritten.

| Field | Type | Rule |
|---|---|---|
| `calendar_month` | Date | First day of calendar month |
| `financial_year` | character | NHS financial year, e.g. `2025-26` |
| `source_org_code` | character | Code exactly as published |
| `source_org_name` | character | Name exactly as published |
| `source_parent_name` | character | Published region/parent label, if present |
| `row_scope` | character | `provider`, `national_total`, or `file_total` |
| `ae4h_attendances_n` | numeric | Published total attendances across department types |
| `ae4h_within_4h_n` | numeric | Direct total numerator where available, otherwise total minus breaches |
| `ae4h_over_4h_n` | numeric | Published total breach count; missing remains `NA` |
| `ae4h_performance` | numeric | All-types within-four-hours divided by its reported denominator |
| `ae4h_submission_status` | character | Submission state for the all-types outcome |
| `type1_attendances_n` | numeric | Published Type 1 count, including booked appointments where the workbook definition does |
| `type1_within_4h_direct_n` | numeric | Direct workbook numerator when available |
| `type1_over_4h_n` | numeric | Published breach count; missing remains `NA` |
| `type1_within_4h_derived_n` | numeric | Attendance minus breach count when both exist |
| `type1_within_4h_n` | numeric | Direct numerator preferred; otherwise derived |
| `type1_reported_performance_denominator_n` | numeric | Within plus over; can differ from full attendance only for aggregate rows with excluded submissions |
| `type1_performance` | numeric | Within divided by reported performance denominator |
| `submission_status` | character | `submitted`, `performance_not_submitted`, `no_type1_activity`, `missing`, or `aggregate_row` |
| `publication_reporting_era` | character | Pre-April-2026 or Acute Provider Table change era |
| `source_representation` | character | Workbook and sheet used; CSV fallback is not selected by default |
| source fields | character | URL, page, filename, revision label and SHA-256 |
| `analysis_trust_id` | character | Initially source code only |
| `identity_status` | character | `source_code_unharmonised` until mapping approval |
| `mapping_rule_id` | character | Approved effective-dated mapping identifier, otherwise missing |
| `qa_flags` | character | Machine audit flags |

The principal provider denominator is the all-types attendance count for a trust that submitted
four-hour data. During CRS field testing, affected provider numerator/breach cells are
missing and performance is `NA`; their zero-looking CSV values are not used.

## 3. Identity, aggregation and April 2026

- Preserve the source organisation series unchanged.
- Keep effective dates, predecessor/successor codes, rule type, rationale, evidence
  URL and reviewer in `config/trust_mapping.csv`.
- Do not back-cast a successor automatically. Aggregate predecessors only where their
  combined Type 1 footprint is demonstrably equivalent to the successor.
- Until mappings are approved, forecasts and persistence assessments must remain
  within uninterrupted source-code series.
- The source-provider A&E panel and an acute-trust-footprint panel are different
  estimands. The April 2026 Acute Provider Table change is tagged, but no footprint
  bridge is created in stage one.

## 4. Missingness, revisions and exclusions

- Missing submissions are `NA`, never zero. Published dashes remain missing.
- Every downloaded revision is content-addressed with SHA-256. A changed file creates
  a new raw file; it does not overwrite an earlier download.
- National and aggregate rows are retained for reconciliation and excluded from the
  provider modelling panel.
- Zero Type 1 activity produces no provider performance value and is not interpreted
  as missing performance.
- Provider counts must be whole, non-negative and internally consistent. Fractional
  counts are allowed only in the official pre-June-2015 national apportioned estimates.
- COVID-era months are not automatically excluded. Their treatment belongs to the
  modelling stage and must be tested out of sample.

## 5. Modelling constraints

- National first, followed by source-provider and then approved mapped-trust analyses.
- Rolling-origin out-of-sample evaluation only; no random train/test split.
- Benchmarks at minimum: last observation, seasonal naive (12 months), and a simple
  seasonal trend model.
- Candidate models retain numerator and denominator information and assess interval
  calibration, not just point error.
- Seasonal terms and longer-run trend are permitted only with explicit treatment of
  the estimated-monthly, CRS and booked-appointment regimes.
- Statistical unusualness and practical materiality are separate labels.
- Three- and six-month persistence summaries may not span an unbridged identity,
  submission or reporting-footprint break.
- Residual performance is not automatically management quality, clinical quality or
  productivity.
