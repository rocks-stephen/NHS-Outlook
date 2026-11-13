# Tests for display axes only; never alters archived forecasts or source data.
source("R/utils.R")
source("R/core_outlook.R")
source("R/monthly_outlook.R")

almost_equal <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y)))

stopifnot(
  almost_equal(nhs_proportion_axis_limits(c(0.96, 1.06)), c(0.96, 1)),
  almost_equal(nhs_proportion_axis_limits(c(-0.06, 0.04)), c(0, 0.04)),
  almost_equal(nhs_proportion_axis_limits(c(0.7, 0.8)), c(0.7, 0.8)),
  almost_equal(nhs_proportion_axis_limits(c(1, 1)), c(0.99, 1)),
  almost_equal(nhs_proportion_axis_limits(c(0, 0)), c(0, 0.01)),
  almost_equal(nhs_percentage_axis_limits(c(97, 105)), c(97, 100)),
  almost_equal(nhs_percentage_axis_limits(c(-5, 3)), c(0, 3))
)
for (test_limits in list(c(-0.05, 0.05), c(0.97, 1.04), c(0.6, 0.8))) {
  ticks <- nhs_proportion_axis_breaks(test_limits)
  stopifnot(length(ticks) > 0L, all(ticks >= 0 & ticks <= 1))
}
stopifnot(
  max(core_outlook_y_range(c(0.99, 1), "proportion")) <= 1,
  min(core_outlook_y_range(c(0, 0.01), "proportion")) >= 0,
  min(core_outlook_y_range(c(3, 4), "minutes")) >= 0,
  max(core_outlook_y_range(c(3, 4), "minutes")) > 1
)
# Both interactive explorers set their bounds based on the selected metric.
for (filename in c("R/core_outlook.R", "R/monthly_outlook.R")) {
  code <- paste(readLines(filename, warn = FALSE), collapse = "\n")
  stopifnot(grepl("lo=Math.max(0,lo);hi=Math.min(1,hi)", code, fixed = TRUE))
}

if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("scales", quietly = TRUE)) {
  plot <- ggplot2::ggplot(data.frame(month = 1:3, rate = c(.985, .995, 1)),
                          ggplot2::aes(x = month, y = rate)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(
      limits = nhs_proportion_axis_limits,
      breaks = nhs_proportion_axis_breaks,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = 0)
    )
  panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
  stopifnot(all(panel$y.range >= 0 & panel$y.range <= 1))
  y_breaks <- panel$y$breaks
  y_breaks <- y_breaks[is.finite(y_breaks)]
  stopifnot(length(y_breaks) >= 1L, all(y_breaks >= 0 & y_breaks <= 1))
}
message("Chart-axis bounds tests passed.")
