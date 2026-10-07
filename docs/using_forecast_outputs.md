# Using the release forecast and provider watchlist

The project now produces two distinct analytical products:

1. a forecast made before the next monthly data release; and
2. a provider watchlist for sustained departures from each provider's own expected
   trajectory.

They answer different questions. The release forecast asks what the next published
number is likely to be. The watchlist asks whether several successive results have
been better or worse than could reasonably have been expected from that provider's
own history.

## Monthly release

There are two publication layers:

- `source("scripts/17_build_performance_outlook.R")` builds the compact cross-metric
  forecast bulletin; and
- `source("scripts/11_build_monthly_outlook.R")` builds the detailed A&E page.

The forecast bulletin is written to
`output/releases/nhs-performance-outlook-forecast-latest.html`, with the shorter
`nhs-performance-outlook-latest.html` alias. Its table reports Latest, Outlook and
Actual; Actual is deliberately blank until publication. In outturn mode,
`nhs-performance-outturn-latest.html` is built from the exact row set frozen in the
forecast edition. The outturn bundle also includes a refreshed one-page provider watch
for each eligible metric, using the new actual to update the distribution and sustained
trajectory signals. The overview is one
A4 portrait page. Each available indicator has a portrait national deep dive on page 1.
Where defensible provider history exists, page 2 separately shows provider distribution
and trajectory watch. National-only measures omit page 2 and state why.

The bulletin uses “favourable” and “adverse” provider deviations rather than simply
“above” and “below”. This matters when the framework is extended to measures such as
ambulance response time, where a lower value is better.

## Core-indicator workflow

Run scripts 12–17 after the A&E pipeline. Outputs for each measure are held under
`output/core/<metric_id>/`. The headline files are:

- `national_next_release_forecast.csv` and `national_reference_projection.csv`;
- `national_model_comparison.csv` and the national release archive/scorecard;
- `provider_next_release_forecast.csv` and `provider_latest_watchlist.csv`;
- `provider_release_surprise_history.csv` and the provider archive/scorecard;
- `overview_row.csv`, the validated contract consumed by the cross-metric page; and
- `forecast_method.csv`, the exact settings, history, tests, accuracy and interval
  calibration used for that indicator on the current run.

The common reference forecast averages seasonal persistence, damped year-on-year
drift and a damped recent seasonal trend when at least two components are available.
Proportions are modelled on a logit scale and response times on a log scale. One-step
errors produce the release range. Where the configured empirical calibration count is
available, the range uses empirical error quantiles. With 8--11 errors, a labelled
small-sample Student-t predictive range is available as a contingency. Every headline
England series must first satisfy the training-plus-full-backtest contract in
`output/qa/core_national_history.csv`. A short or unavailable national series is
labelled and omitted for that edition. Inadequate provider evidence withholds only the
provider page evidence; the validated national outlook can still appear. The
reason and calibration counts are written to `output/core/model_status.csv`, and the
one-page bulletin names any omitted indicator. Longer projection ranges scale the
one-step range by the square root of horizon and should be treated as planning context.
The combined `output/core/forecast_method_register.csv` is the quickest way to compare
the exact method inputs across indicators. Publication-ready and technical explanations
are in `docs/forecast_method_explainer.md`.

The community deep dive adds a separate service page when Tables 4–4h were imported.
It ranks England service categories by published waits over 18 weeks and reports how
many services have complete over-18 bands. It does not substitute organisation names
for provider codes. The underlying files are
`data-interim/core/community_waits_service_bands.csv` and
`data-interim/core/community_waits_service_summary.csv`; reconciliation details are
under `output/qa/community_waits_*`.

For every provider and release month, the sustained screen uses the forecast made
before that result. A six-month window therefore contains six fixed historical
expectations even though the next forecast is updated after every release. At least
five errors must point in the same direction, the mean must exceed the metric-specific
materiality threshold and the latest error must confirm the direction. Current review
thresholds are 2 percentage points for RTT and diagnostics, 3 percentage points for
cancer and 3 minutes for Category 2 response. UCR, community waits and Talking
Therapies are currently national-only. The UCR percentage table does not publish its
matching provider denominator, the community workbook lacks stable provider codes and
the Talking Therapies chart is an England series.

## A&E metric deep dive

After the national and provider models have run, use
`source("scripts/11_build_monthly_outlook.R")`. It writes:

- `output/releases/ae-four-hour-outlook-latest.html`: the current release-ahead page;
- `output/releases/ae-four-hour-outlook-YYYY-MM.html`: the durable page for each
  forecast target month; and
- `output/releases/release_manifest.csv`: the target month and release state of the
  pages built on that run.

The page first shows the latest published result known at forecast time, the national
point forecast, 80% expected range and then the actual when it is published. A second
chart places the medium-term outlook against the 78% March 2026, 82% March 2027 and
85% 2028/29 planning milestones. When the actual is published, the dated target-month
page retains the forecast alongside the actual and forecast error.

The national commentary is generated from the data on each run. It describes the
latest three-month movement, next-month forecast, historical median change for the
forecast calendar month and the projected gap to the March 2027 milestone. It does
not hard-code a claim that performance is improving or stalling. The provider
commentary reports which direction has more sustained signals and how many assessed
providers have no active signal.

`source("scripts/19_export_publication_bundle.R")` automatically renders the overview
and deep dives to portrait PDFs and creates forecast/outturn Markdown drafts for
Substack under `output/publication/YYYY-MM-DD/forecast/` or
`output/publication/YYYY-MM-DD/outturn/`.

The two bundles intentionally contain different detail. Monday's forecast bundle has
the national indicator outlooks and their provider pages. Thursday's outturn bundle has
the actual-versus-forecast overview plus provider-watch-only pages updated through the
new release. It does not relabel a newly calculated next-month forecast as part of the
outturn.

The provider section reports the number assessed, meaningfully above trajectory,
meaningfully below trajectory and within the sustained threshold. It then spotlights
up to the three largest qualifying six-month gaps in each direction. It does not pad
the list: if only one provider qualifies, only one is shown. These are investigation
prompts rather than an absolute provider ranking.

## National release-ahead forecast

Run `scripts/06_model_national.R` after importing the latest published month. The
main file is:

- `output/national/next_release_forecast.csv`

It contains the point forecast, empirical 80% and 95% predictive ranges, expected
month-on-month and year-on-year changes, disagreement across candidate models and
recent one-step backtest accuracy.

Each run also adds the forecast to
`output/national/release_forecast_archive.csv`. A row with the same data month,
target month, model and version is retained only once. When the target month's
actual value becomes available, the next run writes its error to
`output/national/release_forecast_scorecard.csv`.

This archive is the genuine public track record. The rolling backtest remains useful,
but it uses the latest revised history and is therefore described as pseudo-real-time.
Do not edit or delete the archive when refreshing the analysis.

The most useful national charts are:

- `national_next_release_forecast.png`: recent actuals and the next observation with
  empirical ranges;
- `national_one_step_track_record.png`: rolling one-month predictions against actuals;
- `national_reference_components.png`: the three component paths and their ensemble;
- `national_history_forecast.png`: the full history and forecast to March 2029.

## Provider release-ahead forecast

Run `scripts/08_model_provider.R` after stage-one QA passes. The provider model uses
an equal average of the available seasonal-naive, damped year-on-year drift and recent
count-weighted seasonal-trend predictions. At least two components must be available.
This common model avoids selecting a different winner from a short and noisy history
for every provider.

The principal files are:

- `output/provider/next_release_forecast.csv`: forecast, empirical interval and
  expected month-on-month and year-on-year change for every eligible current provider;
- `output/provider/reference_projection.csv`: monthly provider point projections to
  March 2029;
- `output/provider/model_comparison.csv`: rolling model accuracy across providers;
- `output/provider/reference_accuracy_by_provider.csv`: provider-specific historical
  error for judging reliability;
- `output/provider/release_forecast_archive.csv` and
  `release_forecast_scorecard.csv`: the genuine pre-release forecast record.
- `output/provider/release_surprise_history.csv`: one forecast error per provider and
  target month, preferring the genuine archived forecast over the backtest equivalent;
- `output/provider/latest_six_month_fixed_outlook.csv`: supporting comparison with a
  single path fixed six months earlier;
- `output/provider/provider_signal_archive.csv`: the provider signal snapshot for
  each monthly release, used to reconstruct the dated one-page product.

Only the one-month-ahead forecast receives an empirical interval. Longer provider
paths are explicitly labelled exploratory point projections because one-step errors
do not validate uncertainty 12 to 31 months ahead.

## Provider sustained-deviation watchlist

The headline provider watch fixes a six-month forecast path at the beginning of
the latest six-month window. Each subsequent actual is compared with its matching
month on that unchanged path. The rolling one-month forecasts remain available as
a QA diagnostic but do not drive the headline signal. The path gap is:

`actual performance - expectation from the path fixed six months earlier`

A provider is flagged only when all of the following apply over six consecutive
calendar months:

- the absolute mean residual is at least 2 percentage points;
- at least five of the six residuals have the same sign; and
- the latest residual still points in that direction; and
- all six monthly predictions and actuals are available.

Positive values mean performance has been persistently above its own expected
trajectory. Negative values mean it has been persistently below. A separate empirical
unusualness flag compares the six-month mean with that provider's earlier non-overlapping
calibration history where at least 12 prior windows are available. The practical and
statistical tests remain separate in the output.

`signal_status` distinguishes a new signal, a continuing signal and one that is
easing. `signal_evidence` distinguishes six genuine release vintages, a mixed window
and a historical simulation. This lets the monthly product update continuously without
resetting the provider's baseline every time new data arrive.

Use these files and charts:

- `output/provider/latest_watchlist.csv`: current ranked review list;
- `output/provider/provider_review_list.csv`: only providers currently meeting the
  sustained above/below rule;
- `output/provider/deviation_history.csv`: every historical forecast error and
  rolling signal;
- `provider_sustained_deviations.png`: current above/below trajectory ranking;
- `provider_flagged_trajectories.png`: actual and expected paths for the largest
  current signals;
- `provider_next_release_extremes.png`: highest and lowest next-release forecasts.

The 2 percentage-point threshold and five-of-six direction rule are initial review
choices, not results selected from the data. Change them in
`config/provider_model.csv` and compare the watchlist at, for example, 1, 2 and 3
percentage points before fixing a publication rule.

## Interpretation

The watchlist is a screening tool. A flag does not establish management quality,
productivity or a causal effect. Review service changes, coding, case mix, demand,
capacity, neighbouring pathways and data quality before drawing a conclusion.

Until effective-dated trust mappings are approved, the provider analysis uses an
uninterrupted source organisation code as its identity. Do not interpret a trajectory
across a merger or code change. The output retains `identity_status` and source codes
so this limitation is visible.
## One-command monthly refresh

Once `scripts/00_bootstrap_renv.R` has prepared the R environment, run this for the
pre-release forecast:

```r
source("R/run_publication.R")
```

After the release, use outturn mode and restart discovery:

```r
options(
  nhs.outlook.publication_mode = "outturn",
  ae.refresh.start_stage = 1
)
source("R/run_publication.R")
```

The publication runner updates the eight configured headline measures, stops on failed source/schema/QA
checks, retains the forecast archives, and rebuilds the overview plus each eligible
metric's one- or two-page deep dive. Optional measures with insufficient validated history are
reported and omitted rather than allowed to weaken the publication. The main file
to open or print is `output/releases/nhs-performance-outlook-latest.html`. The final
automated gate writes `output/qa/release_signoff.csv`; publication should wait until its
fatal checks pass and its manual review rows have been completed. Forecast mode is the
default and publication options are cleared after a successful run. Forecast mode
freezes the published row set;
outturn mode refuses to build until every corresponding new actual has been imported.
On release day, do not resume after discovery: a full stage-1 run is what makes the code
see new publication pages and files.

To resume an ordinary failed run at a numbered stage without rerunning earlier stages, set the one-use
option before sourcing the runner. For example, after a stage 02 failure:

```r
options(ae.refresh.start_stage = 2)
source("R/run_publication.R")
```
