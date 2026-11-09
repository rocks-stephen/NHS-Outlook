# NHS Outlook: parse downloaded NHS England flow data, provider RLT.
# Run AFTER scripts/21_build_ae_flow_deep_dive.R.
# Only exact, unambiguous metric definitions are imported; other candidates are logged.
stopifnot(file.exists('output/deep-dives/george-eliot/george_eliot_panel.csv'))
needed <- c('dplyr','tidyr','readr','stringr','lubridate','purrr','tibble')
missing <- setdiff(needed, rownames(installed.packages()))
if(length(missing)) stop('Missing project dependencies: ',paste(missing, collapse=', '),'. Run project dependency setup.')

library(dplyr); library(tidyr); library(readr); library(stringr); library(lubridate); library(purrr); library(tibble)
root <- 'data/flow/downloaded'; out <- 'output/deep-dives/george-eliot'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
provider <- 'RLT'
files_for <- function(subdir) {
  p <- file.path(root,subdir)
  if(!dir.exists(p)) return(character())
  list.files(p,pattern='\\.csv$',full.names=TRUE,recursive=TRUE,ignore.case=TRUE)
}
# NHS source CSVs can mix UTF-8 and Windows-1252, especially in free-text
# discharge measure labels. Normalise strings before trimws()/regex matching.
read_source <- function(f) {
  load_csv <- function(encoding) suppressMessages(readr::read_csv(
    f, show_col_types = FALSE, guess_max = 3000,
    name_repair = 'unique', locale = readr::locale(encoding = encoding)
  ))
  x <- load_csv('UTF-8')
  invalid <- any(vapply(x, function(z) {
    is.character(z) && any(!validUTF8(z[!is.na(z)]))
  }, logical(1)))
  if (invalid) {
    message('Non-UTF-8 text in ', basename(f), '; reading as Windows-1252')
    x <- load_csv('Windows-1252')
  }
  # Defensive fallback: ensure no invalid strings survive in either case.
  x <- dplyr::mutate(x, dplyr::across(
    where(is.character),
    ~ iconv(.x, from = 'UTF-8', to = 'UTF-8', sub = 'byte')
  ))
  names(x) <- names(x) |> tolower() |>
    stringr::str_replace_all('[^a-z0-9]+', '_') |>
    stringr::str_replace_all('^_|_$', '')
  x
}

month_file <- function(f) {
  s <- stringr::str_match(basename(f),'(20[0-9]{2})([01][0-9])')
  if(is.na(s[1,1])) return(as.Date(NA))
  as.Date(sprintf('%s-%s-01',s[1,2],s[1,3]))
}
to_num <- function(x) suppressWarnings(as.numeric(gsub(',','',as.character(x),fixed=TRUE)))
unique_metric <- function(x, metric, value, month, source) {
  x <- x %>% filter(!is.na(.data[[month]]),!is.na(.data[[value]]))
  if(nrow(x)==0) return(NULL)
  dup <- x %>% count(.data[[month]],.data[[metric]]) %>% filter(n>1)
  if(nrow(dup)) {
    warning(source, ': duplicated metric/month rows; no automatic import. See QA output.')
    return(NULL)
  }
  x
}

# 1. G&A beds (already evidenced: long-format Metric, Type, Value)
bed_metric <- c(
 'G&A beds available'='beds_available',
 'G&A beds occupied'='beds_occupied',
 'G&A core beds available'='core_beds_available',
 'G&A escalation beds available'='escalation_beds_available',
 'Number of G&A beds occupied by patients with a length of stay of - 7 or more days'='los7_beds',
 'Number of G&A beds occupied by patients with a length of stay of - 14 or more days'='los14_beds',
 'Number of G&A beds occupied by patients with a length of stay of - 21 or more days'='los21_beds'
)
read_bed <- function(f){
 x<-read_source(f)
 req<-c('org_code','level','type','metric','value')
 if(!all(req %in% names(x))) return(NULL)
 x<-x %>% filter(toupper(trimws(org_code))==provider,tolower(level)=='provider',tolower(trimws(type))=='type 1')
 if(!nrow(x))return(NULL)
 x<-x %>% mutate(month=month_file(f), field=unname(bed_metric[match(metric,names(bed_metric))]), val=to_num(value)) %>% filter(!is.na(field),!is.na(month))
 if(!nrow(x)) return(NULL)
 if(anyDuplicated(x$field)) {warning('Duplicate G&A bed metrics: ',basename(f));return(NULL)}
 x %>% select(month,field,val) %>% pivot_wider(names_from=field,values_from=val)
}
B<-bind_rows(purrr::map(files_for('beds'),read_bed))
if(nrow(B)) {
 if(anyDuplicated(B$month))stop('Duplicate monthly beds sources: check file revisions.')
 B<-B %>% mutate(ga_occupancy_pct=100*beds_occupied/beds_available,
   los21_occupied_share_pct=if('los21_beds' %in% names(B)) 100*los21_beds/beds_occupied else NA_real_)
}

# 2. DRD: use ONLY exact provider-level measure, avoid mixing other measures.
read_drd <- function(f){
 x<-read_source(f)
 req<-c('code','data_type','data_source','measure','value')
 if(!all(req %in% names(x)))return(NULL)
 x<-x %>% filter(toupper(trimws(code))==provider,tolower(trimws(data_type))=='provider')
 if(!nrow(x)) return(NULL)
 # Source documented by user's sample as SUS. Other versions are not silently combined.
 x<-x %>% filter(toupper(trimws(data_source))=='SUS')
 x<-x %>% filter(trimws(measure)=='Total bed days lost due to delayed discharge')
 if(nrow(x)!=1){
   warning('DRD not unique for ',basename(f),' (n=',nrow(x),'). Not imported.')
   return(NULL)
 }
 tibble(month=month_file(f),drd_delayed_bed_days=to_num(x$value))
}
D<-bind_rows(purrr::map(files_for('drd'),read_drd))
if(nrow(D) && anyDuplicated(D$month))stop('Duplicate DRD months in downloaded files.')

# 3. Daily Acute Discharge SitRep: identify exact total daily discharges first.
# Diagnostics permit review if publication wording differs.
acute_files<-files_for('acute')
read_acute <- function(f){
 x<-read_source(f)
 req<-c('period','org_code','level','metric','value')
 if(!all(req %in% names(x)))return(NULL)
 x<-x %>% filter(toupper(trimws(org_code))==provider,tolower(trimws(level))=='provider')
 if(!nrow(x))return(NULL)
 x %>% transmute(source_file=basename(f),period=as.character(period),metric=trimws(metric),
     metric_type=if('metric_type'%in%names(x))as.character(metric_type) else NA_character_,
     metric_group=if('metric_group'%in%names(x))as.character(metric_group) else NA_character_,
     value=to_num(value))
}
AC<-bind_rows(purrr::map(acute_files,read_acute))
if(nrow(AC)) {
 write_csv(AC %>% count(metric,metric_type,metric_group,sort=TRUE),file.path(out,'acute_metric_dictionary.csv'))
}
# Exactly one daily total-discharge record per date. Ignore historical/summary
# rows with the same metric label but no daily metric type/group.
W <- tibble(month = as.Date(character()))
if (nrow(AC)) {
  candidate <- AC %>%
    filter(
      metric == 'Number of patients discharged',
      str_to_lower(str_trim(metric_type)) == 'daily metric',
      str_to_lower(str_trim(metric_group)) == 'discharges'
    ) %>%
    mutate(
      date = as.Date(period, format = '%d/%m/%Y'),
      month = as.Date(format(date, '%Y-%m-01'))
    )

  if (!nrow(candidate)) {
    warning('No daily total discharges with exact type/group found.')
  } else {
    bad_dates <- candidate %>% filter(is.na(date))
    if (nrow(bad_dates)) stop('Unparsed daily discharge dates: ', nrow(bad_dates))

    # Overlapping publications occasionally report different values for a date.
    # Prefer a row from the release whose YYYYMM matches the observation date,
    # but only if that source gives exactly one distinct value. If conflicting
    # values remain, EXCLUDE the date and flag its month incomplete; never
    # sum or silently choose an arbitrary value.
    candidate <- candidate %>%
      mutate(
        release_ym = stringr::str_extract(source_file, "^20[0-9]{2}(0[1-9]|1[0-2])"),
        observation_ym = format(date, "%Y%m"),
        matching_release = !is.na(release_ym) & release_ym == observation_ym
      )
    duplicates <- candidate %>%
      group_by(date) %>%
      summarise(
        n_rows = n(), n_values = n_distinct(value, na.rm = FALSE),
        files = paste(sort(unique(source_file)), collapse = "; "),
        values = paste(sort(unique(as.character(value))), collapse = "; "),
        .groups = "drop"
      ) %>% filter(n_rows > 1)
    if (nrow(duplicates)) {
      write_csv(duplicates, file.path(out, "acute_duplicate_dates_qa.csv"))
      write_csv(candidate %>% semi_join(duplicates, by = "date") %>% arrange(date, source_file),
                file.path(out, "acute_duplicate_rows_qa.csv"))
    }
    resolve_date <- function(df) {
      # A publication belonging to the observation month takes precedence.
      in_month <- df[df$matching_release, , drop = FALSE]
      if (nrow(in_month)) df <- in_month
      unique_values <- unique(df$value)
      if (length(unique_values) != 1L || is.na(unique_values[[1]])) {
        return(tibble(value = NA_real_,
                      resolution = "conflict_excluded",
                      source_file = paste(sort(unique(df$source_file)), collapse = "; ")))
      }
      tibble(value = unique_values[[1]],
             resolution = if (nrow(df) > 1L) "duplicate_agreed" else "selected_matching_release",
             source_file = paste(sort(unique(df$source_file)), collapse = "; "))
    }
    resolved <- candidate %>% group_by(date) %>%
      group_modify(~ resolve_date(.x)) %>% ungroup()
    # group_modify adds the grouping date, so resolve_date returns no date itself.
    write_csv(resolved %>% filter(resolution == "conflict_excluded"),
              file.path(out, "acute_unresolved_dates_qa.csv"))
    n_excluded <- sum(resolved$resolution == "conflict_excluded")
    if (n_excluded) warning(n_excluded, " conflicting discharge date(s) excluded; their months will be flagged incomplete. Inspect acute_unresolved_dates_qa.csv")
    candidate <- resolved %>% filter(!is.na(value)) %>%
      mutate(month = as.Date(format(date, "%Y-%m-01")))
    if (any(is.na(candidate$value))) {
      write_csv(candidate %>% filter(is.na(value)), file.path(out, 'acute_missing_values_qa.csv'))
      stop('Non-numeric discharge counts; inspect acute_missing_values_qa.csv')
    }

    # Coverage check: do not present a monthly weekday/weekend rate as
    # complete unless every calendar date for that month was submitted.
    W <- candidate %>%
      mutate(weekend = lubridate::wday(date, week_start = 1) >= 6) %>%
      group_by(month) %>%
      summarise(
        weekday_discharges = sum(value[!weekend]),
        weekend_discharges = sum(value[weekend]),
        weekday_days = sum(!weekend),
        weekend_days = sum(weekend),
        days_reported = n(),
        calendar_days = lubridate::days_in_month(first(month)),
        .groups = 'drop'
      ) %>%
      mutate(
        complete_month = days_reported == calendar_days,
        weekday_discharges_per_day = if_else(complete_month & weekday_days > 0,
          weekday_discharges / weekday_days, NA_real_),
        weekend_discharges_per_day = if_else(complete_month & weekend_days > 0,
          weekend_discharges / weekend_days, NA_real_),
        weekend_share_pct = if_else(complete_month &
          (weekday_discharges + weekend_discharges) > 0,
          100 * weekend_discharges / (weekday_discharges + weekend_discharges), NA_real_),
        weekend_weekday_daily_ratio = if_else(complete_month &
          weekday_discharges_per_day > 0,
          weekend_discharges_per_day / weekday_discharges_per_day, NA_real_)
      )
    write_csv(W, file.path(out, 'acute_discharge_monthly_coverage.csv'))
    message('Acute discharge: ', nrow(candidate), ' daily observations over ',
      nrow(W), ' months; ', sum(W$complete_month), ' complete months.')
  }
}

# Merge with existing monthly A&E panel. Keep previous A&E values exactly as generated.
panel_path<-file.path(out,'george_eliot_panel.csv')
P<-read_csv(panel_path,show_col_types=FALSE) %>% mutate(month=as.Date(month))
for(extra in list(B,D,W)){
 if(!nrow(extra))next
 # Remove older fields of the same names before joining verified replacements.
 cols<-setdiff(names(extra),'month')
 P<-P %>% select(-any_of(cols)) %>% left_join(extra,by='month')
}
if(all(c('drd_delayed_bed_days','beds_available') %in% names(P))) {
 P<-P %>% mutate(drd_delayed_days_share_pct=100*drd_delayed_bed_days/(beds_available*lubridate::days_in_month(month)))
}
write_csv(P,panel_path)
qa<-tibble(metric=setdiff(names(P),'month'),n_months=vapply(P[setdiff(names(P),'month')],function(z)sum(!is.na(z)),integer(1)))
write_csv(qa,file.path(out,'coverage_qa.csv'))
write_csv(tibble(collection=c('beds','drd','acute'),files=c(length(files_for('beds')),length(files_for('drd')),length(acute_files)),
  parsed_months=c(nrow(B),nrow(D),nrow(W))),file.path(out,'flow_parse_summary.csv'))
message('Flow parser finished: ',nrow(B),' bed months, ',nrow(D),' DRD months, ',nrow(W),' weekend discharge months.')
print(qa,n=Inf)
