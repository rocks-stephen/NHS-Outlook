# Explaining the monthly forecast

## Short public explanation

The outlook is a one-month-ahead forecast based only on data available before the
release. For each core indicator it combines three deliberately simple views of the
series: the usual month-to-month seasonal change applied to the latest result,
year-on-year drift, and the recent trend after allowing for calendar-month patterns.
The components receive weights based on their errors in rolling historical forecasts,
with the weights pulled halfway back towards equal weights for stability. Annual drift
and the recent trend are damped so recent movements are not projected indefinitely.
The expected range comes from errors made in rolling historical one-month-ahead tests.

## One-sentence version

The core forecast is a backtest-weighted ensemble of current-level seasonality,
damped annual drift and a recent seasonal trend, tested using rolling
one-month-ahead forecasts.

## What the model calculates

For the core indicators, the reference forecast contains three components:

1. **Current-level seasonality.** Start from the latest observation and add the median
   historical change normally associated with the move into the forecast month. The
   twelve monthly changes are centred so this component adds seasonality but not a
   second long-run trend.
2. **Damped annual drift.** Calculate recent year-on-year changes and add their average
   to the same-month value. Each additional forecast year receives progressively less
   drift.
3. **Damped recent seasonal trend.** Fit a linear trend and calendar-month effects to a
   recent window. Damp the trend as the forecast horizon increases.

The published point forecast is a weighted mean of the available components. Inverse
RMSE weights are calculated from rolling one-month-ahead forecasts and then shrunk 50%
towards equal weights. At least two components must be available. The corresponding
same-month-last-year forecast is retained as a benchmark, but is no longer an ensemble
component because it can snap back to an obsolete level after a sustained change.

Proportions are modelled on a logit scale and converted back to the 0–100% range.
Response times and counts are modelled on a log scale and converted back to a
non-negative level.

The A&E model remains separate: it retains same-month persistence and damped
annual drift. Its third component is a quasi-binomial count model that estimates
shared calendar-month effects
from the pre-CRS and current eras while allowing those eras to have separate trends.
All three A&E components must be available. This distinction is recorded explicitly in
`output/national/forecast_method.csv`.

## How the model is tested

The rolling test recreates the release-ahead task. For each historical target month,
the model is fitted only to observations available through the previous month. Its
one-month forecast is then compared with the published result. The output reports the
number and dates of these tests, mean absolute error, root mean squared error and bias.

The 80% and 95% ranges use the distribution of the reference model's one-step errors.
When the configured empirical sample is available, the code uses empirical error
quantiles. With only 8–11 usable errors it uses a clearly labelled Student-t predictive
fallback. Longer-horizon planning ranges scale one-step errors by the square root of
the forecast horizon; they should be treated as broad planning context.

## Configured history by indicator

| Indicator | Minimum training | Rolling tests | Recent-trend window | Annual-drift comparisons |
|---|---:|---:|---:|---:|
| A&E four-hour performance | 72 months | Expanding origins; primary one-step tests from June 2025 | Shared pre-CRS/current-era seasonal count model | 12 |
| RTT within 18 weeks | 24 months | 36 months | 36 months | 12 |
| Diagnostics over six weeks | 24 | 36 | 36 | 12 |
| Cancer within 62 days | 24 | 24 | 24 | 12 |
| Category 2 ambulance response | 36 | 36 | 36 | 12 |
| Two-hour urgent community response | 18 | 18 | 24 | 6 |
| Community waiting list within 18 weeks | 18 | 12 | 24 | 6 |
| Talking Therapies within six weeks | 36 | 36 | 36 | 12 |

The final fit can use more history than the minimum. The exact number used in each run
is written to the method record and printed in the indicator deep dive.

## How to audit a run

For each included metric, inspect:

- `output/performance/forecast_method_register.csv` for the combined A&E and
  core-indicator method register used by the publication;
- `output/national/forecast_method.csv` for the exact A&E method and current run;
- `output/core/<metric_id>/forecast_method.csv` for the exact settings, dates,
  component availability, training length, test count, accuracy and interval method;
- `output/core/<metric_id>/national_ensemble_weights.csv` for the current component
  weights and the component RMSEs used to form them;
- `output/core/<metric_id>/national_forecast_reversal_diagnostic.csv` for the check
  against material reversals of a sustained recent movement;
- `output/core/<metric_id>/national_model_comparison.csv` for component and ensemble
  error statistics;
- `output/core/<metric_id>/national_rolling_predictions.csv` for every historical
  forecast and error;
- `output/core/<metric_id>/national_all_model_forecasts.csv` for each component path;
- `output/core/forecast_method_register.csv` for one row per included core indicator; and
- `output/core/<metric_id>/national_release_forecast_archive.csv` for the immutable
  forecasts genuinely issued before release.

The genuine release archive is the most important public track record. Rolling tests
use the latest revised historical series and are therefore pseudo-real-time rather than
reconstructed historical vintages.

## Interpretation boundary

The method extrapolates recurring seasonality and recent movement. It does not model
policy announcements, weather, industrial action, epidemics or operational changes
unless their effects are already visible in the data. The outlook is descriptive, not
a target-conditioned projection or a causal estimate.
