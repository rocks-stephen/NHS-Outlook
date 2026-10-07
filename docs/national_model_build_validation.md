# National model build validation (superseded Type 1 run)

This file records the earlier Type 1 build only. It is retained as an audit note and
must not be used as a validation anchor for the current all-types model. Regenerate
the outputs with scripts 03–11 and record the all-types results before publication.

## Input inspection

The supplied stage-one CSV was inspected independently of the R implementation.

- 190 rows: November 2010 to August 2026.
- No missing months, duplicate months or missing outcome counts.
- The within-four-hour and over-four-hour counts sum to the denominator throughout.
- Latest observed Type 1 four-hour performance: 61.10% in August 2026.

## Independent numerical cross-check

An independent Python implementation was used to cross-check the main deterministic
forecast formulas. Using the supplied August 2026 vintage, indicative mean performance
in financial year 2028/29 was:

| Projection | FY 2028/29 mean |
| --- | ---: |
| Seasonal naive | 60.98% |
| Damped recent seasonal drift | 63.85% |
| Shared-season current trend | 64.12% |
| Equal-weight reference ensemble | 62.98% |

These figures are validation anchors, not frozen published results. The R pipeline
must recreate outputs from the current downloaded data, and refreshed NHS England
revisions or later months will legitimately change them.

Recent rolling tests found that seasonal naive and recent-drift approaches materially
outperformed long-window linear trends. Long-window trends were pulled down by the
pandemic/CRS period. This result motivated retaining simple benchmarks and using a
multi-model reference rather than selecting a single extrapolated long trend.

## R runtime result

The user completed the structural QA and national model run in R using the August 2026
data vintage. Stage-one QA passed. The R outputs gave:

- latest performance: 61.10%;
- reference ensemble FY 2028/29 mean: 62.98%;
- reference ensemble March 2029: 65.24%;
- March 2029 gap to the 85% comparison line: -19.76 percentage points; and
- March 2029 empirical 95% range: 58.79% to 68.40%.

In the primary rolling comparison, damped seasonal drift ranked first with RMSE 1.82
percentage points and the reference ensemble ranked second with RMSE 1.95 percentage
points. The ensemble bias was -0.03 percentage points. These results match the
independent numerical anchors closely and support retaining the ensemble as a robust
reference while reporting damped drift as the best-performing sensitivity model.

## Next-release validation anchor

An independent calculation using data through August 2026 gives the following forecast
for the September 2026 release:

| Component | Forecast |
| --- | ---: |
| Seasonal persistence | 61.08% |
| Damped seasonal drift | 62.41% |
| Shared-season current trend | 63.31% |
| Reference ensemble | 62.27% |

The independently calculated empirical range is 59.72% to 64.86% at 80% and 59.25%
to 65.72% at 95%, using 78 current-era rolling residuals at one- to three-month
horizons. Current-era one-step reference errors have MAE 1.36 percentage points, RMSE
1.74 percentage points and bias +0.08 percentage points. These are validation anchors
for the revised `next_release_forecast.csv`, not fixed values after later releases.

R is not installed in the environment used to assemble the revised bundle. The newly
added release-ahead outputs, extended chart scripts and provider stage must therefore
be run and reviewed in the user's R 4.4 project environment.
