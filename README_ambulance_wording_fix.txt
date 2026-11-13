NHS OUTLOOK — AMBULANCE WORDING FIX (9 November 2026)

This small update removes the automatic explanatory paragraph in the
Category 2 ambulance outlook and retains only two factual values:
monthly and annual change, in minutes. It also removes the unused
"Volume breakdown: Not available" box.

HOW TO APPLY
1. Keep a backup of the current NHS-Outlook project or create a Git branch.
2. Unzip this file into the ROOT of your NHS-Outlook folder,
   preserving the paths, and allow these 3 files to replace the existing files.
3. Review 'git diff' to confirm only these 3 files changed.
4. Commit and push to update the current GitHub Pages site.

The R/diagnostics.R change is permanent for future builds.
The other two files update only the mutable latest forecast page,
including the GitHub Pages copy, without rerunning the R pipeline.
Historical dated editions are intentionally unchanged.

No model, forecast, provider-watch or community/RTT specialty code is altered.

Note: full R pipeline execution was not available in the review environment.
We verified the exact HTML replacement and parsed the updated pages.
