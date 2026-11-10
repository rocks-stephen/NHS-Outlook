# George Eliot: reproducible patient-flow panel
# Run: Rscript build_george_eliot_flow.R [path/to/existing/data]
# Data: official NHS England provider files or existing local downloads.
# Outputs: output/george_eliot_panel.csv, source QA and four charts.
# Optional supplementary module, using validated local provider data.

pkgs <- c('dplyr','tidyr','purrr','stringr','readr','readxl','janitor','lubridate','ggplot2','rvest','xml2','tibble')
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs)) stop('Install these R packages to run the deep dive: ', paste(missing_pkgs, collapse=', '))
invisible(lapply(pkgs, library, character.only=TRUE))
source("R/utils.R")  # bounded percentage chart axes

ROOT <- normalizePath('.', winslash='/', mustWork=TRUE)
DATA_DIR <- getOption('nhs.outlook.deep_dive.data_dir', file.path(ROOT,'data','flow'))
OUT <- file.path(ROOT,'output','deep-dives','george-eliot'); CACHE <- file.path(ROOT,'data','flow','downloaded')
dir.create(DATA_DIR, recursive=TRUE,showWarnings=FALSE)
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)
dir.create(CACHE,recursive=TRUE,showWarnings=FALSE)
START <- as.Date('2024-04-01'); END <- as.Date('2026-09-01')
PROVIDER <- 'RLT' # George Eliot Hospital NHS Trust; verify against source organisation names
AUTO_DOWNLOAD <- isTRUE(getOption('nhs.outlook.deep_dive.download', FALSE))

pages <- c(
  ae_2425='https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2024-25/',
  ae_2526='https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2025-26/',
  ae_2627='https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2026-27/',
  bed_2425='https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2024-25/',
  bed_2526='https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2025-26/',
  bed_2627='https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/',
  acute='https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/acute-discharge-situation-report/',
  drd='https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/discharge-ready-date/'
)

# Discover downloads. No guessed file addresses; keep complete link register for manual review.
get_links <- function(url, label) {
  tryCatch({
    h <- rvest::read_html(url)
    a <- rvest::html_elements(h,'a[href]')
    tibble(source=label, page=url, label=rvest::html_text2(a),
           url=xml2::url_absolute(rvest::html_attr(a,'href'),url)) %>%
      filter(str_detect(url,'\\.(csv|xls|xlsx|zip)(\\?|$)'))
  }, error=function(e){message('Cannot read page: ',url,' : ',conditionMessage(e));tibble(source=character(),page=character(),label=character(),url=character())})
}
links <- purrr::imap_dfr(pages, ~get_links(.x,.y)) %>% distinct(url,.keep_all=TRUE)
write_csv(links,file.path(OUT,'official_download_link_register.csv'))

# Download bounded number of provider-bearing monthly files. Use original local files first.
# Files from other projects can be passed as argument. Review link register if site changes.
download_one <- function(url) {
  base <- basename(sub('\\?.*','',url)); if (!nzchar(base)) return(NA_character_)
  base <- gsub('[^A-Za-z0-9._-]','_',base)
  f <- file.path(CACHE,base)
  if (!file.exists(f)) tryCatch({
    old <- getOption('timeout');options(timeout=100)
    on.exit(options(timeout=old),add=TRUE)
    suppressWarnings(download.file(url,f,mode='wb',quiet=TRUE))
  },error=function(e)message('Download failed: ',base))
  if (file.exists(f) && file.size(f)>500) f else NA_character_
}
if (AUTO_DOWNLOAD && nrow(links)) {
  wanted <- links %>% filter(
    (str_detect(source,'^ae_') & str_detect(paste(label,url),regex('monthly.*(csv|data)|\\.csv',ignore_case=TRUE))) |
    (str_detect(source,'^bed_') & str_detect(paste(label,url),regex('beds.*(monthly|publication)|beds-sitrep|sitrep-data',ignore_case=TRUE))) |
    (source=='acute' & str_detect(paste(label,url),regex('monthly-data.*csv|monthly.*csv',ignore_case=TRUE))) |
    (source=='drd' & str_detect(paste(label,url),regex('monthly-data.*csv|monthly.*csv',ignore_case=TRUE)))
  ) %>% filter(!str_detect(url,regex('timeseries',ignore_case=TRUE)))
  # Date and version checks performed after download. Prevent enormous indiscriminate download.
  if(nrow(wanted)>170) wanted <- wanted %>% slice_tail(n=170)
  message('Downloading up to ',nrow(wanted),' official files (existing files reused)')
  invisible(purrr::map(wanted$url,download_one))
}

paths <- c(list.files(DATA_DIR,recursive=TRUE,full.names=TRUE,pattern='\\.(csv|xlsx|xls|zip)$',ignore.case=TRUE),
           list.files(CACHE,recursive=TRUE,full.names=TRUE,pattern='\\.(csv|xlsx|xls|zip)$',ignore.case=TRUE))
# Unzip CSV archives into own subfolders.
for (z in paths[str_detect(tolower(paths),'\\.zip$')]) {
  target <- file.path(CACHE,paste0('unzip_',tools::file_path_sans_ext(basename(z))))
  dir.create(target,showWarnings=FALSE)
  try(utils::unzip(z,exdir=target),silent=TRUE)
}
paths <- unique(c(paths,list.files(CACHE,recursive=TRUE,full.names=TRUE,pattern='\\.(csv|xlsx|xls)$',ignore.case=TRUE)))
paths <- paths[!str_detect(tolower(paths),'\\.zip$')]
message('Local/source data files detected: ',length(paths))
if (!length(paths)) message(
  'No supplementary flow files under ', DATA_DIR,
  '. Will reuse NHS Outlook staged A&E data where available. ',
  'Set options(nhs.outlook.deep_dive.data_dir = "C:/path/to/other/data") ',
  'before sourcing this script to include bed and discharge files.'
)

write_csv(tibble(file=paths),file.path(OUT,'files_found.csv'))

# Read flat provider files; many NHS files begin with metadata rows.
read_tables <- function(f) {
  ext <- tolower(tools::file_ext(f)); result <- list()
  if (ext=='csv') {
    for (skip in c(0,1,2,3,4,5)) {
      x <- tryCatch(suppressMessages(readr::read_csv(f,skip=skip,show_col_types=FALSE,guess_max=2000)),error=function(e)NULL)
      if(!is.null(x) && ncol(x)>=3) result[[paste0('csv_skip',skip)]] <- janitor::clean_names(x)
    }
  } else if(ext %in% c('xlsx','xls')) {
    sheets <- tryCatch(readxl::excel_sheets(f),error=function(e)character())
    for(s in head(sheets,12)) for (skip in c(0,2,4)) {
      x <- tryCatch(suppressMessages(readxl::read_excel(f,sheet=s,skip=skip,.name_repair='unique')),error=function(e)NULL)
      if (!is.null(x)&&ncol(x)>=3) result[[paste0(s,'_',skip)]] <- janitor::clean_names(x)
    }
  }; result
}
find_col <- function(nms, patterns) {
  hits <- which(vapply(nms,function(n) any(vapply(patterns,function(p)str_detect(n,regex(p,ignore_case=TRUE)),logical(1))),logical(1)))
  if(length(hits)) nms[hits[1]] else NA_character_
}
num <- function(x) readr::parse_number(as.character(x),na=c('NA','-','..','[x]','[c]',''))
month_from_filename <- function(f) {
  n<-tolower(basename(f))
  yymm<-str_match(n,'(20[12][0-9])[-_]?([01][0-9])')
  if(!is.na(yymm[1,1])) return(as.Date(sprintf('%s-%s-01',yymm[1,2],yymm[1,3])))
  m <- str_match(n,'(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[-_ ]?(20[0-9]{2}|[0-9]{2})')
  if(!is.na(m[1,1])) {y<-as.integer(m[1,3]);if(y<100)y<-y+2000;return(as.Date(sprintf('%04d-%02d-01',y,match(m[1,2],tolower(month.abb)))))}
  as.Date(NA)
}
get_month <- function(x,f) {
  n <- names(x); mc <- find_col(n,c('^month$','reporting_month','^period$','^date$'))
  if(!is.na(mc)) {
    z<-as.character(x[[mc]])
    d<-suppressWarnings(as.Date(z))
    if(all(is.na(d))) d<-suppressWarnings(lubridate::parse_date_time(z,orders=c('my','bY','ym','Ymd','dmy','mdy')))
    if(any(!is.na(d)))return(as.Date(format(as.Date(d),'%Y-%m-01')))
  }
  rep(month_from_filename(f),nrow(x))
}
subset_provider <- function(x) {
  n<-names(x); oc<-find_col(n,c('^org_code$','^organisation_code$','^provider_code$','^trust_code$','^org_ods_code$','^ods_code$','^org$'))
  if(!is.na(oc)) return(x[str_to_upper(trimws(as.character(x[[oc]])))==PROVIDER,,drop=FALSE])
  nm<-find_col(n,c('^org_name$','organisation_name','provider_name','trust_name','^organisation$'))
  if(!is.na(nm))return(x[str_detect(as.character(x[[nm]]),regex('George Eliot',ignore_case=TRUE)),,drop=FALSE])
  x[0,,drop=FALSE]
}

# Normalise the monthly Beds SitRep long format used in your earlier master script.
bed_keys <- c('G&A beds available'='beds_available','G&A beds occupied'='beds_occupied',
 'G&A core beds available'='core_beds_available','G&A escalation beds available'='escalation_beds_available',
 'Number of G&A beds occupied by patients with a length of stay of - 7 or more days'='los7_beds',
 'Number of G&A beds occupied by patients with a length of stay of - 14 or more days'='los14_beds',
 'Number of G&A beds occupied by patients with a length of stay of - 21 or more days'='los21_beds')
normalise_beds <- function(x,f) {
  if(!all(c('metric','value') %in% names(x)))return(NULL)
  p<-subset_provider(x);if(!nrow(p))return(NULL)
  if('level'%in%names(p))p<-p %>% filter(str_to_lower(level)=='provider')
  if('type'%in%names(p))p<-p %>% filter(str_detect(type,regex('type 1',ignore_case=TRUE)))
  if(!nrow(p))return(NULL)
  p %>% mutate(metric_name=unname(bed_keys[match(metric,names(bed_keys))]),month=get_month(p,f),value=num(value)) %>%
    filter(!is.na(metric_name),!is.na(month),month>=START,month<=END) %>%
    group_by(month,metric_name) %>% summarise(value=mean(value,na.rm=TRUE),.groups='drop') %>%
    pivot_wider(names_from=metric_name,values_from=value) %>% mutate(source_file=basename(f))
}

# General wide A&E data parser: strict identification; do not silently infer metric columns.
normalise_ae <- function(x,f) {
  p<-subset_provider(x);if(!nrow(p))return(NULL)
  n<-names(p)
  ac<-find_col(n,c('^attendances_type_1$','^type_1_attendances$','^a_e_attendances_type_1$'))
  allc<-find_col(n,c('^total_attendances$','^total_a_e_attendances$','^attendances_all_types$','^total_attendances_all_types$'))
  if(is.na(ac)&&is.na(allc))return(NULL)
  fc<-find_col(n,c('within_4_hours','under_4_hours','less_than_4_hours'))
  month<-get_month(p,f)
  tibble(month=month, ae_type1_attendances=if(!is.na(ac))num(p[[ac]]) else NA_real_,
    ae_all_attendances=if(!is.na(allc))num(p[[allc]]) else NA_real_,
    ae_within4_n=if(!is.na(fc))num(p[[fc]]) else NA_real_,source_file=basename(f)) %>%
    filter(!is.na(month),month>=START,month<=END)
}

# Generic long metric handler for provider discharge data. Exports candidates for exact QA.
normalise_discharge_long <- function(x,f) {
  if(!all(c('metric','value')%in%names(x)))return(NULL)
  p<-subset_provider(x);if(!nrow(p))return(NULL)
  p %>% mutate(month=get_month(p,f), numeric_value=num(value), source_file=basename(f)) %>%
    filter(!is.na(month),month>=START,month<=END) %>%
    select(month,metric,numeric_value,source_file)
}

beds<-list();ae<-list();dc<-list();inventory<-list()
for(f in paths) {
  tables<-read_tables(f)
  if(!length(tables))next
  for (s in names(tables)) {
    x<-tables[[s]]
    b<-tryCatch(normalise_beds(x,f),error=function(e)NULL)
    a<-tryCatch(normalise_ae(x,f),error=function(e)NULL)
    d<-tryCatch(normalise_discharge_long(x,f),error=function(e)NULL)
    if(!is.null(b)&&nrow(b))beds[[length(beds)+1]]<-b
    if(!is.null(a)&&nrow(a))ae[[length(ae)+1]]<-a
    if(!is.null(d)&&nrow(d))dc[[length(dc)+1]]<-d
    if((!is.null(b)&&nrow(b))||(!is.null(a)&&nrow(a))||(!is.null(d)&&nrow(d)))
      inventory[[length(inventory)+1]]<-tibble(file=basename(f),sheet=s,rows=nrow(x),columns=paste(names(x),collapse=' | '),bed=!is.null(b)&&nrow(b)>0,ae=!is.null(a)&&nrow(a)>0,discharge=!is.null(d)&&nrow(d)>0)
  }
}
bind_or_empty<-function(l)if(length(l))dplyr::bind_rows(l)else tibble()
B<-bind_or_empty(beds);A<-bind_or_empty(ae);D<-bind_or_empty(dc);I<-bind_or_empty(inventory)
# Reuse the existing NHS Outlook provider-month stage (no repeated downloads).
staged_ae <- file.path(ROOT, 'data-interim', 'provider_month_ae4h_unharmonised.csv')
if (file.exists(staged_ae)) {
  stage <- readr::read_csv(staged_ae, show_col_types=FALSE)
  required <- c('source_org_code', 'calendar_month', 'ae4h_attendances_n',
                'ae4h_within_4h_n', 'type1_attendances_n')
  if (all(required %in% names(stage))) {
    stage <- stage %>%
      filter(toupper(source_org_code)==PROVIDER) %>%
      mutate(month=as.Date(calendar_month)) %>%
      filter(month >= START, month <= END)
    if ('is_current_revision' %in% names(stage)) {
      stage <- stage %>% filter(is.na(is_current_revision) | is_current_revision)
    }
    staged <- stage %>%
      transmute(month, ae_type1_attendances=as.numeric(type1_attendances_n),
                ae_all_attendances=as.numeric(ae4h_attendances_n),
                ae_within4_n=as.numeric(ae4h_within_4h_n),
                source_file='NHS Outlook staged A&E data') %>%
      distinct(month, .keep_all=TRUE)
    if (nrow(staged)) {
      A <- staged
      message('Reused ', nrow(staged), ' monthly George Eliot observations from NHS Outlook staged A&E data.')
    }
  } else warning('Staged A&E file present, but expected columns are missing: ', paste(setdiff(required,names(stage)),collapse=', '))
}

write_csv(I,file.path(OUT,'parsed_sheet_inventory.csv'))
if(nrow(D))write_csv(D,file.path(OUT,'discharge_metric_candidates.csv'))
if(nrow(A))write_csv(A,file.path(OUT,'ae_candidates.csv'))
if(nrow(B))write_csv(B,file.path(OUT,'bed_candidates.csv'))

# Monthly panel: only correctly identified measures are merged. Duplicated sources are
# resolved by most complete row, then file version ('revised' preferred).
resolve <- function(x) {
 if(!nrow(x))return(tibble(month=as.Date(character())))
 metric<-setdiff(names(x),c('month','source_file'))
 x %>% mutate(nonmissing=rowSums(!is.na(across(all_of(metric)))),
   priority=as.integer(str_detect(tolower(source_file),'revis'))*10+as.integer(str_detect(tolower(source_file),'final'))*5) %>%
   arrange(month,desc(nonmissing),desc(priority)) %>% distinct(month,.keep_all=TRUE) %>%
   select(-nonmissing,-priority,-source_file)
}
months<-tibble(month=seq(START,END,by='month'))
panel<-months %>% left_join(resolve(A),by='month') %>% left_join(resolve(B),by='month')
if(all(c('beds_available','beds_occupied') %in% names(panel)))
  panel<-panel %>% mutate(ga_occupancy_pct=100*beds_occupied/beds_available,
    los21_occupied_share_pct=if('los21_beds'%in%names(panel))100*los21_beds/beds_occupied else NA_real_)
if(all(c('ae_all_attendances','ae_within4_n')%in% names(panel)))
  panel<-panel %>% mutate(ae_within4_pct=100*ae_within4_n/ae_all_attendances)
write_csv(panel,file.path(OUT,'george_eliot_panel.csv'))

# Monthly acute discharge and DRD spreadsheets can contain multi-row headers and dozens
# of similarly named metrics. Do not mistake 'discharged' for weekend discharges or DRD
# for criteria-to-reside. Export explicit diagnostics; use the canonical extraction template.
metrics <- tibble(
  standard_field=c('drd_delayed_bed_days','drd_delayed_days_share_pct',
    'weekday_discharges','weekend_discharges','weekend_share_pct'),
  definition=c('DRD: sum of additional days after discharge ready date',
    'DRD delayed bed days divided by adult G&A available bed days',
    'Acute discharge SitRep daily discharges Monday–Friday',
    'Acute discharge SitRep daily discharges Saturday–Sunday',
    'Weekend total / all seven days discharges * 100'),
  status='NEEDS_EXACT_HEADER_MAPPING_IF_NOT_ALREADY_IN_PANEL')
write_csv(metrics,file.path(OUT,'discharge_mapping_template.csv'))

# Allow validated manual mapping from local existing projects to be merged without
# guessing a metric or silently mixing weekly and monthly definitions.
validated<-file.path(DATA_DIR,'validated_discharge_monthly.csv')
if(file.exists(validated)) {
  v<-read_csv(validated,show_col_types=FALSE) %>% mutate(month=as.Date(month))
  stopifnot('month'%in%names(v),!anyDuplicated(v$month))
  panel<-panel %>% left_join(v,by='month')
  if(all(c('weekday_discharges','weekend_discharges')%in% names(panel)))
    panel<-panel %>% mutate(weekend_share_pct=100*weekend_discharges/(weekday_discharges+weekend_discharges))
  write_csv(panel,file.path(OUT,'george_eliot_panel.csv'))
}

# Chart whatever has been verified, never zero-fill missing months.
plot_metric <- function(v,label,name) {
 if(!v %in% names(panel) || sum(!is.na(panel[[v]]))<3)return(invisible(NULL))
 p<-ggplot(panel,aes(x=month,y=.data[[v]]))+geom_line(linewidth=.9,na.rm=TRUE)+geom_point(na.rm=TRUE,size=1)+
   geom_vline(xintercept=as.Date('2026-03-01'),linetype=2)+
   labs(title=paste('George Eliot:',label),x=NULL,y=label,caption='Dashed line: March 2026. Source: NHS England; see source QA.')+
   theme_minimal(base_size=12)
 # Shares and proportions cannot fall below zero or exceed 100%.
 # Bed occupancy is excluded because reporting can exceed nominal capacity.
 if (grepl('_pct$', v) && v != 'ga_occupancy_pct') {
   p <- p + scale_y_continuous(
     limits=nhs_percentage_axis_limits,
     breaks=nhs_percentage_axis_breaks,
     oob=scales::oob_keep,
     expand=expansion(mult=0)
   )
 }
 ggsave(file.path(OUT,paste0(name,'.png')),p,width=9,height=4.5,dpi=160)
}
plot_metric('ae_all_attendances','All A&E attendances','01_attendances')
plot_metric('ae_within4_pct','A&E within four hours (%)','02_ae_four_hour')
plot_metric('ga_occupancy_pct','G&A occupancy (%)','03_occupancy')
plot_metric('los21_occupied_share_pct','21+ day patients / occupied G&A beds (%)','04_long_stay')
plot_metric('drd_delayed_days_share_pct','DRD delayed days / G&A available days (%)','05_delayed_discharges')
plot_metric('weekend_share_pct','Weekend share of weekly discharges (%)','06_weekend_discharge')

measure_cols <- setdiff(names(panel), 'month')
if (length(measure_cols)) {
  qa <- tibble(metric=measure_cols,
               n_months=vapply(panel[measure_cols], function(x) sum(!is.na(x)), integer(1)))
} else {
  qa <- tibble(metric=character(), n_months=integer())
  message('No metrics parsed: inspect files_found.csv and the staged A&E file. Monthly panel contains dates only.')
}
write_csv(qa,file.path(OUT,'coverage_qa.csv'))
message('DONE: ',file.path(OUT,'george_eliot_panel.csv'))
print(qa,n=Inf)
if(!nrow(D)) message('No provider-level discharge metric rows parsed: inspect source files and manual mapping template.')
message('Important: discharge variables are NOT inferred from ambiguous headers. Supply validated_discharge_monthly.csv from existing project after verifying definitions.')
