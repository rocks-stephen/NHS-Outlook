source("R/utils.R")
source("R/qa.R")

national <- data.table::fread("data-interim/national_month_stage1.csv")
national[, calendar_month := data.table::as.IDate(calendar_month)]
provider <- data.table::fread("data-interim/provider_month_source_rows.csv")
provider[, calendar_month := data.table::as.IDate(calendar_month)]

national_qa <- qa_national_month(national)
provider_qa <- qa_provider_month(provider)
reconciliation <- provider_month_reconciliation(provider)

dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(national_qa$issues, "output/qa/national_row_issues.csv")
data.table::fwrite(national_qa$duplicates, "output/qa/national_duplicate_months.csv")
data.table::fwrite(national_qa$gaps, "output/qa/national_missing_months.csv")
data.table::fwrite(provider_qa$issues, "output/qa/provider_row_issues.csv")
data.table::fwrite(provider_qa$duplicates, "output/qa/provider_duplicate_keys.csv")
data.table::fwrite(
  provider_qa$source_anomalies,
  "output/qa/provider_nonfatal_source_anomalies.csv"
)
data.table::fwrite(reconciliation, "output/qa/provider_submitted_reconciliation.csv")

fatal_n <- nrow(national_qa$issues) + nrow(national_qa$duplicates) + nrow(national_qa$gaps) +
  nrow(provider_qa$issues) + nrow(provider_qa$duplicates)
if (fatal_n) stop("Stage-one QA failed with ", fatal_n, " fatal issue rows; inspect output/qa.")
message("Stage-one structural QA passed. Missing four-hour submissions remain recorded, not imputed.")
