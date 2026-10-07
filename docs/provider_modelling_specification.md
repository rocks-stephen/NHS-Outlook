# Provider forecast and sustained-deviation specification

## Estimand

For each current acute provider, estimate the next published monthly proportion of
all-types attendances completed within four hours. Produce an exploratory monthly
point path to March 2029 and monitor whether six successive releases have been
consistently above or below the forecasts made before those releases.

The provider is the approved effective-dated analysis identity where mappings exist.
Otherwise it is the uninterrupted source organisation code. Rows with a missing
four-hour submission are retained as missing and never converted to 100% performance.

## Candidate models

| Model | Definition | Role |
| --- | --- | --- |
| Seasonal naive | Same provider and calendar month one year earlier. | Transparent persistence benchmark. |
| Seasonal mean | Mean of up to three prior observations for the same calendar month. | Stable seasonal benchmark. |
| Damped seasonal drift | Latest same-month performance plus the recent mean year-on-year logit change; later annual changes are damped by 0.8. | Recent improvement or deterioration path. |
| Recent count trend | Quasi-binomial logit model of within and over-four-hour counts with a trend and month effects over the latest 36 observations. | Count-weighted local trend. |
| Provider ensemble | Equal mean of available seasonal-naive, damped-drift and recent count-trend forecasts; at least two are required. | Reference forecast. |

All models use only submitted observations available by the forecast origin. A rolling
prediction is generated only when the provider submitted in the immediately preceding
calendar month and has at least 24 submitted training months.

## Validation

Every eligible historical provider-month receives one-step-ahead predictions. Accuracy
is reported as MAE, RMSE and bias in percentage points for all available targets and for
the current era from June 2023. The pooled table weights every provider-month equally
and also reports attendance-weighted MAE. Provider-specific reference-model accuracy is
retained separately.

The current implementation deliberately uses one common reference model across
providers. Selecting a separate model for each provider from a small history would add
model-selection noise and make comparisons harder to interpret.

## One-step predictive intervals

The next-release provider forecast uses empirical reference-model residuals. A provider's
own current-era residuals are used where at least 18 exist. Otherwise the code uses the
pooled current-era provider residual distribution, falling back to all available pooled
residuals only if fewer than 100 current-era residuals exist.

The interval captures observed model error, not merely sampling error in large attendance
counts. It is not applied to longer provider projections.

## Six-release forecast-surprise rule

Let the monthly surprise be actual minus the one-month-ahead reference prediction that
existed before the release. A current signal requires six consecutive surprises, an
absolute mean of at least 2 percentage points, at least five surprises with the same
sign, and a latest surprise that still points in that direction. If the six-month rule
still qualifies but the latest surprise reverses, the provider is labelled `easing`
rather than shown as an active signal. Active signals are also labelled `new` or
`continuing`.

Genuine archived forecast vintages replace pseudo-real-time rolling predictions one
month at a time. `signal_evidence` states whether a window uses six genuine vintages,
a mixture, or only a historical simulation. This avoids implying that revised-history
backtests were forecasts genuinely published at the time.

The output separately tests whether the latest six-month mean lies outside the empirical
2.5th to 97.5th percentile range of earlier six-month means for the same provider. Earlier
windows overlapping the current six-month period are excluded. At least 12 prior windows
are required. A high-review signal meets both the practical persistence rule and the
statistical unusualness rule; a review signal meets the practical rule alone.

## Boundaries

- The rolling test uses the latest revised files rather than exact historical data vintages.
- Genuine forecasts are therefore archived separately when issued before a release.
- The first actual recorded after a release is retained in the scorecard; later source
  revisions do not rewrite the original forecast surprise.
- A separate six-month-old fixed-origin path is retained as supporting context. It is
  not the headline signal because it would otherwise be refreshed only twice a year
  or repeatedly reset if re-estimated monthly.
- A provider flag is descriptive, not causal and not automatically a productivity measure.
- Unapproved merger or code-change bridges are not imposed.
- Long-range provider point paths are exploratory; only the next-release forecasts have
  empirically calibrated ranges.
