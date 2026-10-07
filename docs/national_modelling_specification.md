# National all-types four-hour performance modelling specification

## Purpose

The national stage predicts the monthly proportion of all A&E attendances admitted,
transferred or discharged within four hours. It uses the retained within-four-hour
and over-four-hour counts and never substitutes a rounded published percentage.

The forecast is descriptive. It is not a causal estimate of policy, management or
productivity. Future access, case mix, coding, pathways, capacity and reporting may
differ from the historical series.

## Data used

The validated run supplied for stage two contains 190 consecutive months from
November 2010 to August 2026.

- November 2010 to May 2015: estimated calendar months apportioned from weekly data.
- June 2015 to April 2019: actual monthly collection with the full pre-CRS footprint.
- May 2019 to May 2023: national performance excludes 14 CRS field-test trusts and
  overlaps the pandemic shock.
- June 2023 onward: current full-footprint national era.

Every month remains in the source table and rolling benchmark evaluation. The
shared-season current-trend model deliberately estimates seasonality from June 2015
to April 2019 plus June 2023 onward, and estimates the current trend only from June
2023 onward. It therefore does not ask the CRS/pandemic period to determine the
current trend.

## Forecast candidates

| Model | Purpose | Main limitation |
| --- | --- | --- |
| `seasonal_naive` | Repeats the latest observation for the same calendar month. | Assumes no underlying improvement or deterioration. |
| `seasonal_mean_3y` | Uses the latest three observations for each calendar month. | Responds slowly when the level changes. |
| `seasonal_drift_damped` | Adds recent average year-on-year logit change to the seasonal-naive path, reducing each additional year's drift to 80% of the previous year. | Long-horizon results depend on the recent drift period and damping choice. |
| `count_glm_trend_36` | Count-weighted seasonal logit trend over the latest 36 months. | Three years is short for separating seasonality and trend. |
| `count_glm_trend_60` | Count-weighted seasonal logit trend over the latest 60 months. | The window can retain obsolete pandemic-era trends. |
| `count_glm_trend_all` | Diagnostic count-weighted seasonal logit trend using every month available at each origin. | A single trend across collection, CRS and pandemic breaks is deliberately demanding and is not assumed valid. |
| `dynamic_ar1_trend_84` | Seasonal dynamic regression with an AR(1) error on logit performance. | Counts do not directly weight the ARIMA fit. |
| `shared_season_current_trend` | Count-weighted model with shared seasonality and separate pre-CRS and current-era trends. | Current-trend evidence is still limited to the post-May-2023 period. |
| `reference_ensemble` | Equal average of seasonal naive, damped drift and shared-season current trend. | Equal weights are a transparent robustness choice, not statistically optimal weights. |

The reference ensemble is used because the components encode materially different
views of persistence and trend. Component forecasts remain in the output and their
spread should be reviewed. The configuration can change the reference model without
changing the code.

## Rolling evaluation

- Expanding rolling origins start after 72 months, giving six annual cycles.
- Every origin predicts up to 31 months, matching the August 2026 to March 2029
  final horizon.
- Accuracy is reported using mean absolute error, root mean squared error and bias,
  all in percentage points.
- The primary recent comparison uses 1, 3, 6 and 12-month forecasts with target
  months from June 2025. This ensures the shared-season model had at least twelve
  current-era observations at the matching 12-month origins.
- Full-history and all-current-era results are also retained. A model is not judged
  from the primary window alone.

The exercise is pseudo-real-time with the final revised historical series. It does
not reconstruct the exact data vintage that was available at each historical
forecast origin. This distinction must be stated if results are published.

## Uncertainty intervals

The reference forecast uses empirical errors from rolling current-era predictions.
Residuals are grouped into 1–3, 4–6, 7–12, 13–24 and 25-plus-month horizon bands.
If a band contains fewer than 20 residuals, the code uses a documented wider pool.
The 80% and 95% intervals therefore reflect observed forecast error rather than only
binomial sampling error or coefficient uncertainty.

These are empirical predictive ranges, not guarantees. The 25–31-month range has
less direct validation than shorter horizons.

## Unusual and persistent deviations

The deviation monitor uses one-month-ahead reference-ensemble residuals.

- Statistical unusualness compares the current residual with the empirical 2.5th
  and 97.5th percentiles of up to 24 prior residuals. At least 12 prior values are
  required, reflecting the short history of the reference ensemble.
- Practical materiality defaults to an absolute 1 percentage-point difference.
- Three- and six-month material persistence requires the absolute mean residual to
  reach the threshold and at least two-thirds of months to have the same sign.
- Statistical and practical flags are separate fields. Neither should be interpreted
  automatically as management quality or productivity.

The 1 percentage-point threshold and two-thirds direction rule remain reviewable
configuration choices.

## Required review before publication

1. Approve or change the 1 percentage-point materiality threshold.
2. Review the reference ensemble against each component forecast.
3. Check the 78% (March 2026), 82% (March 2027) and 85% (2028/29) planning milestones.
4. Confirm that the June 2023 current-era start remains appropriate for the question.
5. State that rolling tests use the final revised series rather than historical vintages.
6. Avoid causal or managerial interpretations of forecast residuals.
