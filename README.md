# NHS monthly performance outlook

This reproducible R project acquires, prepares and models eight prominent monthly NHS
performance measures: all-types A&E four-hour performance, Category 2 ambulance response,
two-hour urgent community response, RTT within 18 weeks, diagnostics waiting over six
weeks, cancer treatment within 62 days, community waits within 18 weeks and NHS Talking
Therapies access within six weeks.
It produces national release-ahead forecasts, planning trajectories and provider
early-warning views in a printable monthly bulletin.
Type 1 fields are retained as source diagnostics and to define a consistent acute
provider cohort, but they are not the headline model outcome.

The project contains source discovery, immutable downloads, schema-aware import,
provenance, structural QA, rolling national and provider model comparisons, empirical
next-release forecast intervals, sustained provider-deviation monitoring and
publication-ready charts.

## Coverage

- **National:** starts in November 2010. November 2010–May 2015 are NHS England's
  estimates made by apportioning weekly totals into calendar months.
- **Provider:** genuine monthly provider workbooks start in June 2015.
- The supplied stage-two validation run contained 190 national months through
  August 2026 and 135 provider months through August 2026.

The discovery script derives these counts at run time and fails if provider months
are missing. Nothing relies on a manually maintained list of monthly file URLs.

## Why national and provider series are separate

The national outcome comes from the `Performance` sheet of the official England
time-series workbook. It retains the published all-types within-four-hours and
over-four-hours counts, then checks the published all-types percentage. Type 1
counts are retained alongside them for audit.

Provider data come from each monthly workbook's `Provider Level Data` or older
`A&E Data` sheet. Workbooks are preferred to CSV because, during the May 2019–May
2023 Clinical Review of Standards (CRS) field test, workbook dashes preserve the
fact that 14 trusts did not submit four-hour performance; some CSV representations
turn those cells into zero. Treating those zeros as 100% performance would be a
serious error.

## Important comparability flags

- Pre-June 2015 national months are apportioned estimates, not actual monthly returns.
- May 2019–May 2023 national performance excludes 14 CRS field-test trusts. Provider
  performance for those trusts remains missing, never zero or imputed.
- Booked-appointment fields were added from August 2020. The workbook all-types total
  is used because it consistently combines department types and appointment routes.
- From April 2026, the Acute Provider Table moved to an acute-trust-footprint basis.
  The raw A&E provider panel remains source-provider based and is tagged with the
  reporting era; an acute-footprint series must not be silently spliced into it.
- Trust source codes are initially unharmonised. No persistence result may cross a
  merger or code change until an effective-dated mapping is approved.

These issues do not make seasonal or trend modelling impossible, but they rule out
an unqualified single smooth trend over the whole period. The national modelling
stage therefore compares explicit persistence, recent-trend and segmented choices
using rolling out-of-sample performance.

## Project layout

- `config/acquisition.csv`: root publication page and acquisition choices
- `config/national_model.csv`: explicit national modelling and review choices
- `config/core_sources.csv`: official discovery rules for the seven non-A&E indicators
- `config/core_model.csv`: common forecast and sustained-signal settings by measure
- `config/core_targets.csv`: constitutional/operational standards and planning milestones
- `config/trust_mapping.csv`: effective-dated, manually approved identity rules
- `R/`: discovery, download, import, mapping, QA, forecasting and plotting functions
- `scripts/`: ordered acquisition/import pipeline
- `docs/`: source inventory, specifications, decision log and validation notes
- `data-raw/`: immutable content-addressed downloads, created at run time
- `data-interim/`: discovered manifests and standardized source panels
- `output/qa/`: machine-readable QA results

Raw and generated data are excluded from this bundle and are recreated from NHS
England. Every downloaded file receives a SHA-256 hash and a content-addressed name;
a changed source is preserved as a new file rather than overwriting the old one.

## R environment and rerun

Use R 4.4 or later. The required packages are `data.table`, `ggplot2`, `readxl`,
`curl`, `xml2`, `openssl`, `pagedown` and `renv`. Chrome, Chromium, Edge or another
browser supported by `pagedown` is needed for automatic PDF export. The scraping packages are included because
they provide robust HTML link resolution, HTTP handling and cryptographic provenance.

```r
source("scripts/00_bootstrap_renv.R")
renv::snapshot()

source("scripts/01_discover_nhse_sources.R")
source("scripts/02_download_nhse_sources.R")
source("scripts/03_import_national.R")
source("scripts/04_import_provider.R")
source("scripts/05_qa_stage1.R")
source("scripts/06_model_national.R")
source("scripts/07_plot_national.R")
source("scripts/08_model_provider.R")
source("scripts/09_plot_provider.R")
source("scripts/10_validate_stage3_outputs.R")
source("scripts/11_build_monthly_outlook.R")
source("scripts/12_discover_core_sources.R")
source("scripts/13_download_core_sources.R")
source("scripts/14_import_core_metrics.R")
source("scripts/15_model_core_metrics.R")
source("scripts/16_validate_core_metrics.R")
options(nhs.outlook.publication_mode = "forecast")
source("scripts/17_build_performance_outlook.R")
source("scripts/19_export_publication_bundle.R")
source("scripts/20_release_qa.R")
```

After the environment has been bootstrapped, use an explicit publication mode. For the
Monday pre-release forecast:

```r
options(nhs.outlook.publication_mode = "forecast")
source("scripts/18_run_monthly_refresh.R")
```

For the Thursday result, start again at discovery so newly published files are found:

```r
options(
  nhs.outlook.publication_mode = "outturn",
  ae.refresh.start_stage = 1
)
source("scripts/18_run_monthly_refresh.R")
```

The runner stops at the first failed discovery, schema or QA check. It does not remove
the forecast archives: preserving those files is what turns each next-release forecast
into a genuine pre-release score once the actual is published.

If a stage fails after creating its intermediate files, fix the cause and resume from
that numbered stage without repeating earlier work. For example:

```r
options(ae.refresh.start_stage = 2)
source("scripts/18_run_monthly_refresh.R")
```

The option is consumed and cleared immediately, so the following monthly run starts
normally at stage 1. Download stages reuse files only for the exact same discovery run;
a new discovery timestamp triggers a fresh upstream check for revisions.

For a reviewed same-session correction that changes only source selection, the core
download stage also has an explicit one-use recovery option. It reuses matching local
immutable files and downloads only newly selected URLs:

```r
options(
  ae.refresh.start_stage = 12,
  ae.core.reuse_unchanged_downloads = TRUE
)
source("scripts/18_run_monthly_refresh.R")
```

Do not use that recovery option for a normal monthly refresh, because the normal run
redownloads selected URLs to detect upstream file replacements.

`01_discover_nhse_sources.R` starts from the official index, discovers each current
financial-year page, inventories all relevant workbook/CSV links, selects one
workbook per provider month, and selects the latest national time-series workbook.
It deliberately stops on missing months or duplicate selections.

`04_import_provider.R` writes two audit products: all source rows (including published
England totals) and an all-types provider panel restricted to providers with Type 1
activity. Until mappings are approved, the latter
is labelled `source_code_unharmonised`; it is usable within uninterrupted source-code
series but not across organisational changes.

`06_model_national.R` runs expanding rolling-origin predictions, compares simple and
more structured models, creates the all-model forecast paths, builds empirical
uncertainty intervals and assesses one-, three- and six-month deviations. The default
reference forecast is an explicit equal-weight ensemble; every component remains in
`output/national/final_forecasts_all_models.csv`.

`07_plot_national.R` creates the history/forecast, rolling-accuracy and deviation
charts under `output/national/charts/`, including a dedicated next-release forecast
and rolling one-step track record.

`08_model_provider.R` creates next-release forecasts, exploratory paths to March 2029,
rolling accuracy results and a six-release sustained-deviation watchlist. Each monthly
error is attached to the forecast archived before that release; historical rolling
predictions temporarily bootstrap the signal until six genuine vintages accumulate.
The script also writes a six-month-old fixed-outlook comparison as supporting context.
It uses approved
effective-dated trust mappings where they exist and otherwise limits interpretation to
uninterrupted source-code series. `09_plot_provider.R` creates the provider forecast,
model-accuracy and watchlist charts under `output/provider/charts/`.

`10_validate_stage3_outputs.R` checks forecast-month alignment, interval ordering,
bounded projections, archive uniqueness and whether every flagged provider satisfies
the configured six-month rule. It writes `output/qa/stage3_output_checks.csv` and
stops on a failed check.

`11_build_monthly_outlook.R` creates a self-contained monthly release page under
`output/releases/`. The latest page shows the forecast for the next data release,
the latest value known when the forecast was made, the published actual once available,
the medium-term forecast against the 78%, 82% and 85% planning milestones,
the overall number of providers meaningfully above and below their own trajectory,
and up to three of the largest qualifying signals in each direction. It also writes
short, data-derived commentary on recent momentum, target distance, calendar-month
seasonality and the balance of provider signals. The deep dive uses a two-page A4
portrait layout: national outlook first, provider distribution and trajectory watch
second. When an actual
becomes available, the dated page for that target month is rebuilt with the original
forecast, the published result and forecast error retained together.

Scripts 12–16 discover, download, import, model and validate the seven non-A&E
indicators. The established RTT, diagnostics, cancer and Category 2 importers use explicit official-file
contracts: RTT incomplete pathways (`Part_2`), the DM01 provider headline total,
the revised CWT national combined `62D` time series plus its all-cancer/all-route
provider table (including both the older final standard-specific CSV and newer
combined-CSV layouts), and AmbSYS `A31` converted from seconds to minutes. Schema
changes stop the run rather than being guessed. `output/qa/core_national_history.csv`
must show enough consecutive England months for every configured training window and
full rolling backtest. UCR, community waits and Talking Therapies use explicit,
schema-checked official-source contracts: UCR `Table 1`, the community `Table 3`
headline bands plus service-by-band Tables 4–4h, and the Talking Therapies Waiting
Times chart. The community importer writes a long service-band panel and a derived
service summary across England, region, ICB and organisation-name rows. It reconciles
service totals to the headline total and reports published band-sum gaps without
treating blanks as zero. Organisation names remain explicitly unharmonised and are not
used as provider-model identifiers. If an upstream page or layout cannot be
validated, the indicator is labelled and omitted for that edition while the established
series continue.
Forecast ranges use empirical one-step errors when the configured calibration count
is available. A labelled Student-t predictive fallback remains a contingency for
8--11 usable errors, rather than a substitute for the required national history. An
indicator with inadequate provider evidence can still appear nationally, but its provider
watch is withheld and labelled in `output/core/model_status.csv`. A national series with
insufficient history is omitted from that edition and named on the bulletin.
Every included indicator also writes `output/core/<metric_id>/forecast_method.csv`.
That one-row audit record states the exact component definitions, transformation,
training length, rolling-test dates and count, accuracy, interval method and calibration
sample used on the run. `output/core/forecast_method_register.csv` combines those rows.
See `docs/forecast_method_explainer.md` for publication wording and the technical method.

`17_build_performance_outlook.R` creates a compact, consensus-economics-style A4 portrait
bulletin at `output/releases/nhs-performance-outlook-latest.html`. Its headline columns
are Latest, Outlook and Actual. The forecast edition reserves the Actual column; once a
release arrives, an outturn edition compares it with the genuine forecast archived before
publication. The bulletin is branded `ROCKS / HEALTH`; each indicator links to a portrait
deep dive with the national outlook on page 1. Where defensible provider history exists,
page 2 separately shows the latest provider distribution and trajectory watch. A
national-only measure does not receive a decorative empty provider page. The community
waiting-list deep dive instead uses page 2 for the England service breakdown, ranked by
the published number waiting over 18 weeks.

`19_export_publication_bundle.R` prints the overview and available deep dives to portrait
PDF using `pagedown`, writes a machine-readable manifest and creates concise forecast and
outturn Markdown drafts ready to paste into Substack. Files are placed under
`output/publication/YYYY-MM-DD/forecast/` or
`output/publication/YYYY-MM-DD/outturn/`. Edit `config/editorial_commentary.csv` when a short
human-written lead is preferable to the factual automatic commentary; an exact `YYYY-MM`
row takes precedence over `DEFAULT`.

The forecast bundle contains the national indicator deep dives. The outturn bundle
contains the actual-versus-forecast overview plus a refreshed one-page provider watch
for each eligible provider metric. Those pages incorporate the new release into the
provider distribution and six-release trajectory screen without presenting the
post-release next-month forecast as part of the outturn.

`20_release_qa.R` is the final automated release gate. It consolidates the earlier
structural checks, verifies source provenance and numerator/denominator arithmetic,
profiles gaps and unusual latest changes, compares selected imported values with
hand-checked official workbook observations, reviews forecast accuracy against the
seasonal-naive benchmark, confirms that scorecards retain the forecasts archived before
release, validates the community service panel and its exact England-total
reconciliation, and checks the exported PDFs against their HTML page counts and A4 portrait
geometry. Fatal failures stop the run. Review warnings and the pending human checks in
`output/qa/release_signoff.csv` before publication. Reference observations are kept in
`config/core_qa_reference_values.csv` and
`config/community_service_qa_reference_values.csv` so additions and revisions are explicit.

The national and provider model scripts retain a pre-release forecast archive. Do not
delete these archive CSVs during refreshes: after a later release, they become the
genuine forecast scorecards. See `docs/using_forecast_outputs.md` for the operating
workflow and `docs/provider_modelling_specification.md` for the provider method.
The Monday/Thursday operating checklist is in
`docs/monthly_publication_workflow.md`.

The national modelling choices and limitations are documented in
`docs/national_modelling_specification.md`. In particular, the 1 percentage-point
materiality threshold remains a reviewable setting, and rolling evaluation uses the
latest revised series rather than reconstructed historical data vintages.

R is not installed in the build environment used to assemble this bundle. Because the
headline outcome is all-types A&E, seven additional indicators are configured and the
community service panel is new, run scripts 03 through 20 again in the user's R 4.4
project environment before using any prior generated outputs.

## Interpretation boundary

Future residuals will mean performance is higher or lower than the model expected
given its information set. They are not, by themselves, estimates of management
quality, clinical quality or productivity. Case mix, access, coding, pathways,
capacity, neighbouring services and measurement changes may remain unobserved.
