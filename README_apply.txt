NHS OUTLOOK — RTT SPECIALTY DATA VALIDITY PATCH
9 November 2026

BACKGROUND
The additional RTT specialty page failed release QA because zero specialties
passed the 'valid current wait bands' check. This patch addresses ONE plausible
cause: zero-pathway source rows with blank weekly bands were being treated as
missing, which can invalidate an entire specialty at aggregation time.
It does not impute any missing bands for positive-count rows.

CONTENTS
- R/diagnostics.R : properly recognises blank weekly bands where total_all=0;
  fails clearly if the source no longer has exactly 18 weekly bands; records
  unresolved positive-total rows and pathway counts for each specialty.
- scripts/20_release_qa.R : adds meaningful missing-data context to the existing
  fatal RTT specialty check. It DOES NOT weaken the check or invent values.
- tests/test_rtt_specialty_missingness.R : synthetic tests covering true zeros,
  valid positive rows, incomplete positive rows and contradictory rows.

INSTALL
Unzip over the NHS-Outlook project root, preserving folders. These files are
based on the PREVIOUS specialty update, so apply AFTER that update. They do not
replace the newer national HTML templates, which can remain unchanged.

TEST
  source("tests/test_rtt_specialty_missingness.R")

CHECK THE REAL IMPORT WITHOUT ISSUING A NEW FORECAST
From the project root, in R:
  source("R/utils.R")
  source("R/core_import.R")
  source("R/diagnostics.R")
  manifest <- data.table::fread("data-interim/core/downloaded_source_manifest.csv")
  d <- build_rtt_diagnostics(manifest)
  x <- d$specialty_levels
  cat("Specialties:", nrow(x), " valid:", sum(x$complete_submission), "\n")
  cat("Incomplete positive-total source rows:",
      sum(x$unresolved_positive_rows), "\n")
  print(x[, .(specialty_name, total_waiting_list,
              within_18_weeks_count, unresolved_positive_rows,
              unresolved_pathways, complete_submission)][1:min(.N,10L)])

If VALID > 0: the new RTT specialty view has usable source data. First test it
by rerendering a previously frozen edition (scripts/24_render_frozen_edition.R)
with the current templates; do not rerun a locked analytical edition just for
this presentation fix. QA can also be rerun separately after confirming the
rtt_specialty_levels.csv output was updated.

If VALID remains 0: STOP; the root problem is genuinely missing/invalid positive-
count weekly source data (or another source reconciliation issue). Do NOT bypass
QA, replace NA with zero, or publish claimed specialty waiting-time counts.
Inspect output/diagnostics/rtt_specialty_levels.csv and the latest two raw RTT
CSV extracts before choosing an alternative official source or revised rule.

This is a cautious data-quality repair, not a guarantee that every specialty
has fully reported weekly bands. Existing forecasters and provider-watch rules
are unchanged. No R execution was possible in the preparation environment.
