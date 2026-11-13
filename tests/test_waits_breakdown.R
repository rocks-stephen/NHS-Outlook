# Run from the repository root: source("tests/test_waits_breakdown.R")
# Small rendering tests; no NHS download or forecast run required.
source("R/performance_outlook.R")
source("R/core_outlook.R")

stopifnot(requireNamespace("data.table", quietly = TRUE))
month <- data.table::as.IDate("2026-08-01")

community <- data.table::data.table(
  calendar_month = c(month, month),
  geography_type = c("England", "England"),
  service_id = c("adult__physiotherapy", "cyp__physiotherapy"),
  service_name = c("Physiotherapy", "Physiotherapy"),
  service_group = c("Adult", "Children and young people"),
  total_waiting_list = c(1000, 200),
  over_18_weeks_count = c(400, 80),
  within_18_weeks_proportion = c(0.6, 0.6),
  complete_submission = c(TRUE, TRUE)
)
community_page <- core_community_service_page(community, month)
stopifnot(
  isTRUE(community_page$available),
  community_page$services_total == 2L,
  grepl("Physiotherapy (Adult)", community_page$rows_html, fixed = TRUE),
  grepl("Physiotherapy (Children and young people)",
        community_page$rows_html, fixed = TRUE),
  grepl("400", community_page$rows_html, fixed = TRUE)
)

rtt <- data.table::data.table(
  calendar_month = rep(month, 3L),
  specialty_code = c("100", "110", "120"),
  specialty_name = c("General surgery", "Trauma & orthopaedics", "Cardiology"),
  total_waiting_list = c(1000, 1200, 800),
  over_18_weeks_count = c(500, 400, NA_real_),
  within_18_weeks_proportion = c(0.5, 2/3, NA_real_),
  complete_submission = c(TRUE, TRUE, FALSE)
)
rtt_page <- core_rtt_specialty_page(rtt, month)
stopifnot(
  isTRUE(rtt_page$available),
  rtt_page$services_total == 3L,
  rtt_page$services_complete == 2L,
  grepl("General surgery", rtt_page$rows_html, fixed = TRUE),
  grepl("Trauma &amp; orthopaedics", rtt_page$rows_html, fixed = TRUE),
  !grepl("Cardiology", rtt_page$rows_html, fixed = TRUE)
)

# A month with no matching data must not cause a missing-column error or show a page.
stopifnot(
  !isTRUE(core_community_service_page(community, data.table::as.IDate("2026-09-01"))$available),
  !isTRUE(core_rtt_specialty_page(rtt, data.table::as.IDate("2026-09-01"))$available),
  !isTRUE(core_rtt_specialty_page(data.table::data.table(), month)$available)
)
cat("Community and RTT specialty-page rendering tests passed.\n")
