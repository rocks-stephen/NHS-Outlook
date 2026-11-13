NHS Outlook: bounded chart axes (10 November 2026)
==================================================

This package is an INCREMENTAL change for the current NHS-Outlook project.
It does not include or supersede the target-probability pilot, the RTT page
feature flag, previous template revisions, or the national/provider forecasts.
Apply the target-probability package first if you have not already done so.

Copy into the project root, preserving folders:
  R/utils.R
  R/core_outlook.R
  R/monthly_outlook.R
  R/national_plots.R
  R/provider_plots.R
  scripts/21_build_ae_flow_deep_dive.R  (optional George Eliot charts)
  scripts/26_analyse_george_eliot_change.R  (optional George Eliot charts)
  tests/test_chart_axis_bounds.R

IMPORTANT VERSION NOTE:
* R/core_outlook.R in this package is based on the more recent community/RTT
  specialty update, not the older 30 October repository snapshot. It retains
  the community adult/CYP labels and optional RTT page functions.
* R/monthly_outlook.R is based on the repository ZIP provided in this chat.
* R/utils.R, R/national_plots.R and R/provider_plots.R likewise start from the
  uploaded repository. If you have separately modified any of these files since
  uploading, merge rather than blindly replace.

What changes:
* The provider trajectory explorer (A&E and other indicators) clamps its
  percentage plot axes to 0-100%, and all nonnegative metrics to zero minimum.
* Provider sparklines respect these same physical bounds.
* Historical R-generated A&E forecast and provider plots use bounded percent
  axes, retaining dynamic zoom where possible. Percent-point deviations and
  error charts are NOT clamped to 0-100%.
* The main A&E and other eight indicator SVG charts already respected 0-100%; 
  their behavior is left unchanged.
* Optional George Eliot charts for bounded percentage shares are also fixed.
  G&A occupancy is deliberately not capped: published values can sometimes
  exceed nominal bed capacity, so 100% is not an absolute data bound there.

Review and smoke test in RStudio:
  source("tests/test_chart_axis_bounds.R")
  source("tests/test_target_probabilities.R")   # if prior pilot installed

No complete R run or report export was possible in the editing environment.
Use your normal release workflow for an unused analytical edition; do not
overwrite a locked forecast edition. A presentation-only re-render of a locked
edition can use scripts/24_render_frozen_edition.R, but does not automatically
republish the website. Complete the usual visual QA before approving output.
