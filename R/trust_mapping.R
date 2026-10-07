apply_trust_mapping <- function(dt, mapping) {
  assert_columns(dt, c("source_org_code", "calendar_month", "analysis_trust_id", "analysis_trust_name"))
  assert_columns(mapping, c(
    "mapping_rule_id", "source_org_code", "effective_from", "effective_to",
    "analysis_trust_id", "analysis_trust_name", "review_status"
  ))
  m <- data.table::copy(mapping[review_status == "approved"])
  if (!nrow(m)) stop("apply_trust_mapping() requires at least one approved rule.")
  m[, `:=`(
    effective_from = data.table::as.IDate(effective_from),
    effective_to = data.table::as.IDate(effective_to)
  )]
  if (any(is.na(m$effective_from))) stop("Approved mapping rules require effective_from.")
  m[is.na(effective_to), effective_to := data.table::as.IDate("9999-12-01")]

  x <- data.table::copy(dt)
  x[, row_id___ := .I]
  hits <- m[x, on = .(
    source_org_code,
    effective_from <= calendar_month,
    effective_to >= calendar_month
  ), .(
    row_id___ = i.row_id___,
    mapped_rule_id = x.mapping_rule_id,
    mapped_analysis_trust_id = x.analysis_trust_id,
    mapped_analysis_trust_name = x.analysis_trust_name
  ), allow.cartesian = TRUE]
  duplicates <- hits[!is.na(mapped_rule_id), .N, by = row_id___][N > 1L]
  if (nrow(duplicates)) {
    stop("Overlapping approved mapping rules for ", nrow(duplicates), " source rows.")
  }
  x[hits, on = .(row_id___), `:=`(
    mapping_rule_id = i.mapped_rule_id,
    analysis_trust_id = data.table::fifelse(
      !is.na(i.mapped_analysis_trust_id), i.mapped_analysis_trust_id, analysis_trust_id
    ),
    analysis_trust_name = data.table::fifelse(
      !is.na(i.mapped_analysis_trust_name), i.mapped_analysis_trust_name, analysis_trust_name
    )
  )]
  x[, `:=`(
    mapping_missing = is.na(mapping_rule_id),
    identity_status = data.table::fifelse(
      is.na(mapping_rule_id), "source_code_unharmonised", "approved_effective_dated_mapping"
    ),
    row_id___ = NULL
  )]
  x[]
}
