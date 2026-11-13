NHS OUTLOOK — PREPUBLICATION NATIONAL HEADLINE FIX (12 NOV 2026)
==============================================================

Purpose
-------
Fix Category 2 ambulance headline so published observations read "32 min 45 sec",
as in the official England AmbSYS workbook, and add an explicit eight-metric
national-headline reconciliation file. Model values stay in precise decimal
minutes; the public displayed observation rounds to the nearest whole second.

IMPORTANT: The supplied scripts have been reviewed statically, but have NOT
been run in your Windows RStudio project. Do not publish until tests pass.

Installation
------------
From the ZIP, copy into the existing ae-trust-forecast project, preserving
folder structure:

  REPLACE R/outturn_four_columns.R
  REPLACE scripts/17_build_performance_outlook.R
  KEEP scripts/17b_verify_four_columns.R up to date (same as existing)
  ADD R/ambulance_official.R
  ADD R/ambulance_report_display.R
  ADD scripts/14a_align_ambulance_official.R
  ADD scripts/29_audit_official_headlines.R
  ADD scripts/30_rebuild_audited_outturn.R
  ADD config/ambsys_official_sources.csv
  ADD config/official_headline_reference_2026-11-12.csv
  ADD data-external/official/AmbSYS-2026-October-8237G.xlsx

Note: The included workbook is the official AmbSYS file you uploaded.

Run in RStudio, at project root
-------------------------------
First inspect whether output/editions/2026-11-12/outturn/revisions/r4 already
exists. If it does, choose the next UNUSED revision in script 30.

  source("scripts/30_rebuild_audited_outturn.R")

This reruns ONLY:
  14a (reconcile national C2 using English national AmbSYS G35/E35),
  15-16 (core forecast and validation),
  17 (outturn rendering, now displaying official C2 clock),
  17b (four-column and frozen forecast checks),
  29 (independent headline source checks),
  19-20 (PDF export and release QA, only after headline audit passes).

The models are rerun for the updated national data, so refreshed target
probabilities follow from the revised modelling input. No need to rerun source
research/download or stages 1-14. Keep Monday forecast archive unchanged.

WHAT IS VERIFIED / STILL OPEN
-----------------------------
Verified externally at headline presentation precision:
  A&E Oct 2026 74.0%
  Cat2 Oct 2026 32 min 45 sec (AmbSYS Response Times!H35)
  RTT Sep 2026 65.8%
  Cancer Sep 2026 70.7%

PENDING independent original-source national figure check:
  Diagnostics Sep 2026 22.6%
  UCR Sep 2026 85.4%
  Community 18w Sep 2026 76.1%
  Talking Therapies Sep 2026 84.2%

The reference CSV includes those four values as CANDIDATES from the existing
Outlook, NOT independently verified facts. The audit intentionally FAILS
closed for these four. Verify the respective official NHSE NATIONAL/England
published cell/figure (including correct denominator), then update
verification_status from needs_independent_check to verified in the CSV.
Only do so if the official published number actually matches. Do not simply
turn off this control to proceed to publication.

If you only want to preview the formatted HTML while remaining unverified:
  options(nhs.outlook.require_all_official_signoff = FALSE)
  source("scripts/29_audit_official_headlines.R")
But this is NOT a clearance to publish.

Details and caveats
-------------------
- Source workbook: Response Times!E35 is 418,942 Cat2 incidents; G35 is
  228,632.135 response-time hours; H35 is 00:32:45 (Excel date fraction).
- Source totals yield 32.744218 decimal minutes; rounded to nearest second
  this is 32 min 45 sec. Do NOT round the decimal-minute mean to 1 dp for
  an observed headline, because 32.8 min does not literally match AmbSYS.
- The Stage 14a patch updates the national panel's VALUE and, when available,
  numerator (total minutes) and denominator (incidents), so the QA identity
  remains correct. This is a SOURCE adjustment, not a cosmetic overwrite.
- March 2027 25-minute Cat2 threshold is NOT equivalent to the policy
  objective of 25 minutes averaged across FY 2026/27. The current
  probability measures a March monthly outcome only, now labelled that way.
- The different data months across metrics are intentional.
- Talking Therapies six-week access is measured among referrals completing
  a course of treatment, not all referrals seen during the month.
- Community UCR latest month is provisional and can be revised at the next
  refresh. RTT national estimates for non-reporting organisations require
  care if using provider aggregates as a source cross-check.

For subsequent releases, add the approved new AmbSYS monthly workbook to
config/ambsys_official_sources.csv and run 14a immediately after the normal
Stage 14 importer. To make this automatic permanently, insert this line at
THE END of scripts/14_import_core_metrics.R (after national_panel.csv written):

  source("scripts/14a_align_ambulance_official.R")

We did not replace the full core_import.R because it was not supplied.
Treat this targeted Stage 14a source-reconciliation step as the immediate fix.

Release outputs
---------------
output/qa/ambulance_official_source_reconciliation.csv
output/qa/national_headline_source_audit.csv
output/releases/nhs-performance-outturn-latest.html
output/releases/ambulance-category-2-outturn-latest.html

Only after all eight records say VERIFIED_MATCH and release QA passes:
  source("R/publish_outlook_website.R")
  # inspect website files, stage, commit and push via Git Bash
