# Run from the repository root: source("tests/test_rtt_specialty_missingness.R")
# No external downloads or archived forecast files are required.
stopifnot(requireNamespace("data.table", quietly = TRUE))
source("R/diagnostics.R")

x <- data.table::data.table(
  total_all = c(0, 50, 50, 0, 15),
  gt_00_to_01_weeks_sum_1 = c(NA_real_, 20, NA_real_, 2, 5),
  gt_01_to_02_weeks_sum_1 = c(NA_real_, 10, 10, NA_real_, 2)
)
result <- rtt_diagnostic_within_18_rows(
  x, c("gt_00_to_01_weeks_sum_1", "gt_01_to_02_weeks_sum_1")
)
# Missing bands with zero total have a known zero contribution.
stopifnot(identical(result[[1L]], 0), identical(result[[2L]], 30))
# Positive total + missing band must remain unknown, not be silently zero-filled.
stopifnot(is.na(result[[3L]]))
# A zero-total row with contradictory positive observed values must remain invalid.
stopifnot(is.na(result[[4L]]))
stopifnot(identical(result[[5L]], 7))
cat("RTT specialty missingness tests passed.\n")
