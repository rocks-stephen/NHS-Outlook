NHS Outlook: probability display across all eight indicator reports
10 November 2026

Contains 10 permanent source files (including a guarded publishing helper), one test and these instructions. The forecast probabilities
are NOT recalculated with new models. The third card in PRE-RELEASE indicator
reports becomes a compact March 2027 comparison (projection, target and chance).
Post-release reports retain the actual-result card.

The NHS Outlook overview keeps the existing projection/target/chance layout,
with only the probability number in muted red (<20%), dark neutral (20%-<80%),
or muted green (>=80%). These are visual cues, not evidence categories.

IMPORTANT: The frozen renderer now runs script 11 in its isolated scratch
folder, before stage 17, to render the A&E deep dive too. It does not write to
locked analytical archives or update the live site.

1. Back up R/, scripts/, reports/ if your local files differ from prior patches.
2. Extract ZIP into the current active project root, preserving directories.
3. Run in RStudio:
   source("tests/test_target_probabilities.R")
   source("tests/test_target_display.R")
   source("tests/test_target_probability_cards.R")
4. Select original or r1 frozen 2026-11-09 forecast edition:
   options(nhs.outlook.render_issue_date = as.Date("2026-11-09"),
           nhs.outlook.render_mode = "forecast",
           nhs.outlook.render_revision = "",
           nhs.outlook.render_pdf = FALSE)
   source("scripts/24_render_frozen_edition.R")
   If publishing a locked revision, set render_revision = "r1" before sourcing.
5. Inspect output/rerenders/2026-11-09/forecast/ for overview and all eight
   indicator HTMLs. For r1 the output is in forecast/revisions/r1/.
6. Once reviewed, set nhs.outlook.render_pdf=TRUE and source frozen renderer again.
   This is presentation-only: it does not run release QA or promote to live site.
7. Verify all eight indicator links, exact target thresholds and the A4 PDFs.
8. ONLY AFTER manual approval of the rendered pages, use:
   options(nhs.outlook.approve_rerender_publishing = TRUE)
   source("scripts/27_publish_approved_rerender.R")
   options(nhs.outlook.approve_rerender_publishing = NULL)
   This checks all nine target-card/overview artifacts and copies the approved
   HTML/PDF into release and website locations; no archive writes or git push.
9. Inspect publication_site/index.html and git status/diff before committing.
   For GitHub Pages root deploy, stage updated root HTML, publication_site,
   and forecasts/ then commit and git push origin main. Do not force push.
   The existing site publisher removes and rebuilds publication_site/forecasts,
   so the helper preserves historical dated PDF folders.

Note: Older editions may warn that config/core_model.csv was not archived.
Compare provider signal labels before publication; forecasts are unchanged.

R is not available in the build environment, so R execution remains to be
confirmed locally. This package was checked statically for template and file
references but NOT end-to-end executed against the frozen data.
