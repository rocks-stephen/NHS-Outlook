# Run from the NHS Outlook R project root before the annual outturn build.
library(data.table)
p <- "data-interim/core/national_panel.csv"
stopifnot(file.exists(p))
x <- fread(p)[metric_id == "ambulance_cat2"]
x[, calendar_month := as.Date(calendar_month)]
setorder(x, calendar_month)
fy <- x[calendar_month >= as.Date("2026-04-01") &
        calendar_month <= as.Date("2026-10-01"),
        .(calendar_month, value, numerator, denominator)]
print(fy)
missing <- setdiff(as.character(seq.Date(as.Date("2026-04-01"),
  as.Date("2026-10-01"), by="month")),
  as.character(fy[is.finite(denominator) & denominator > 0]$calendar_month))
if (length(missing)) {
  stop("Cannot publish annual Category 2 target: official incident counts missing for ",
       paste(missing, collapse=", "),
       ". Import those months' AmbSYS national Category 2 counts before rerunning.")
}
cat("All seven observed FY months have valid incident weights.\n")
cat("Count history available:",
    sum(is.finite(x$denominator) & x$denominator > 0), "month(s).\n")
