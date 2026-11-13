NHS Outlook target probability pilot (10 November 2026)
=========================================================

Files to merge into your project root:
  R/target_probability.R                     NEW
  R/performance_outlook.R                    REPLACE (based on ZIP uploaded 9 Nov)
  scripts/17_build_performance_outlook.R     REPLACE (retains RTT specialty arg from prior patch)
  scripts/20_release_qa.R                 REPLACE (based on last RTT QA fix)
  scripts/25_preview_target_probabilities.R NEW
  reports/nhs_performance_outlook_template.html REPLACE
  tests/test_target_probabilities.R          NEW

These changes add a PROVISIONAL model-implied chance of meeting the nearest
future milestone (currently March 2027) to each forecast row in the overall
NHS Outlook HTML. This does not change forecasts, intervals, deep dives or
provider signals. Changes to earlier forecast snapshots are prohibited by
the existing archive locks. The current interval calibration, especially for
longer horizons, has NOT been shown to produce calibrated target probabilities.

1. Back up your current repository or use a fresh Git branch before merging.
2. Run source("tests/test_target_probabilities.R").
3. BEFORE releasing, run source("scripts/25_preview_target_probabilities.R").
   Review output/preview/target_probabilities.csv and reject implausible values.
4. The new, incomplete RTT specialty page is intentionally OFF by default;
   its fatal validation applies only when explicitly re-enabled. This is an
   acknowledged deferral of the OPTIONAL specialty page, NOT of RTT national
   forecasts or core RTT QA. To revisit it later in November set:
     options(nhs.outlook.enable_rtt_specialty_page = TRUE)
   and pass the specialty QA before publishing that page.
5. Only generate a new approved edition once all active-feature QA passes.
   Do NOT bypass release QA or overwrite locked editions.
5. The newly issued overview snapshot will retain target probabilities, so
   the subsequent outturn can refer back to the probability originally shown.

Interpretation: a Normal approximation with sigma = the WIDER of the lower
and upper half-widths of the projected central 80% interval / qnorm(0.9).
This does NOT account for regime changes, model uncertainty, joint monthly
probabilities or whether target-improvement rates are historically achievable.
Treat it as an exploratory model-implied statistic. Do not refer to <1% as
"impossible" or >99% as "certain". The target comparison is a single month's
level, not a probability of meeting a whole-year average or remaining above
standard thereafter.

If your local scripts changed since the ZIP, merge differences rather than
blindly overwriting. No R runtime was available to execute the full pipeline
in the editing environment; only static checks were done.
