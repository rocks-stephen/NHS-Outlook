# NHS Outlook: project dependency setup

This repository already uses `renv` and has a `renv.lock`. Do not run `renv::init()` again.

1. Copy `scripts/00_setup_dependencies.R` into the existing project's `scripts` directory.
2. Open the NHS Outlook `.Rproj` file or set the working directory to its root.
3. Run `source("scripts/00_setup_dependencies.R")` **once**, after introducing the George Eliot addendum or when dependencies change.
4. Confirm the process finishes, then run `source("scripts/21_build_ae_flow_deep_dive.R")`.
5. Commit **both** `scripts/00_setup_dependencies.R` and the updated `renv.lock` to Git. Do not commit `renv/library/`.

For a fresh checkout, install `renv` (if absent), then run `renv::restore(prompt = FALSE)` from the project root. Normal monthly outturn runs should NOT install or update packages.

The setup script includes the deep-dive packages explicitly, scans R scripts for common library-loading calls, installs missing packages into the renv project environment, and snapshots the installed project library. Review the lockfile diff; dynamic package references may need manual addition to `deep_dive_packages`.

**Note:** This script was not executed against your Windows R installation. Check the printed package list and `renv::status()` after running it.
