# NHS Outlook: refresh provider-level patient-flow data from NHS England.
# Run from project root: source('scripts/20_download_flow_sources.R')
# Requires rvest, xml2, dplyr, stringr, readr, tibble (managed by project renv).
# NHS England may change link labels/formats: download inventory flags missing months.

suppressPackageStartupMessages({
  library(rvest); library(xml2); library(dplyr); library(stringr)
  library(readr); library(tibble)
})
root <- normalizePath('.', winslash = '/', mustWork = TRUE)
out <- file.path(root, 'data', 'flow', 'downloaded')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
start_month <- as.Date(getOption('nhs.outlook.flow.start_month', '2024-04-01'))
end_month <- as.Date(getOption('nhs.outlook.flow.end_month', format(Sys.Date(), '%Y-%m-01')))
refresh <- isTRUE(getOption('nhs.outlook.flow.force_refresh', FALSE))
base <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/'
pages <- c(
  beds_2425 = paste0(base, 'bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports-2024-25/'),
  beds_2526 = paste0(base, 'bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports-25-26/'),
  beds_2627 = paste0(base, 'bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports-2026-27/'),
  acute = paste0(base, 'discharge-delays/acute-discharge-situation-report/'),
  drd = paste0(base, 'discharge-delays/discharge-ready-date/')
)
get_links <- function(page, source) {
  tryCatch({
    h <- rvest::read_html(page)
    a <- rvest::html_elements(h, 'a[href]')
    tibble(source = source,
           title = trimws(rvest::html_text2(a)),
           url = xml2::url_absolute(rvest::html_attr(a, 'href'), page)) %>%
      filter(grepl('\\.(csv|xlsx?|zip)([?#].*)?$', url, ignore.case=TRUE))
  }, error=function(e) {
    warning('Failed to read ', source, ': ', conditionMessage(e), call.=FALSE)
    tibble(source=character(),title=character(),url=character())
  })
}
links <- dplyr::bind_rows(Map(get_links, unname(pages), names(pages))) %>%
  distinct(source, url, .keep_all=TRUE)

# Month is taken from the official link label/filename, never inferred from download order.
month_of <- function(z) {
  z <- tolower(z)
  y <- stringr::str_match(z, '(20[12][0-9])[-_ ]?(0[1-9]|1[0-2])')
  if (!is.na(y[1,1])) return(as.Date(paste0(y[1,2],'-',y[1,3],'-01')))
  mon <- paste(tolower(month.name), collapse='|')
  abb <- paste(tolower(month.abb), collapse='|')
  x <- str_match(z, paste0('(',mon,'|',abb,')[-_ ]*(20[12][0-9]|[0-9]{2})'))
  if (!is.na(x[1,1])) {
    yy <- as.integer(x[1,3]); if (yy<100) yy<-2000+yy
    mm <- match(substr(x[1,2],1,3),tolower(month.abb))
    return(as.Date(sprintf('%04d-%02d-01',yy,mm)))
  }
  as.Date(NA)
}
links$month <- as.Date(vapply(seq_len(nrow(links)),function(i) {
  z <- month_of(paste(links$title[i],basename(links$url[i])))
  if(is.na(z)) NA_character_ else as.character(z)
}, character(1)))
links$collection <- ifelse(grepl('^beds',links$source),'beds', links$source)
# Strictly restrict to monthly provider CSVs, not national timeseries, guides or data dictionaries.
links <- links %>%
  filter(!is.na(month), month >= start_month, month <= end_month) %>%
  filter(grepl('\\.csv([?#].*)?$', url, ignore.case=TRUE)) %>%
  filter(case_when(
    collection=='beds' ~ str_detect(paste(title,url),regex('sitrep.data',ignore_case=TRUE)),
    collection=='acute' ~ str_detect(paste(title,url),regex('daily.discharge.sitrep.monthly.data',ignore_case=TRUE)),
    collection=='drd' ~ str_detect(paste(title,url),regex('discharge.ready.date.monthly.data',ignore_case=TRUE)),
    TRUE ~ FALSE
  )) %>%
  filter(!str_detect(paste(title,url),regex('timeseries|guidance|national',ignore_case=TRUE))) %>%
  mutate(preference=as.integer(str_detect(paste(title,url),regex('revised',ignore_case=TRUE)))*100 +
           as.integer(str_detect(paste(title,url),regex('final',ignore_case=TRUE)))*20 +
           as.integer(str_detect(paste(title,url),regex('v2|v3',ignore_case=TRUE)))*5) %>%
  arrange(collection,month,desc(preference),desc(url)) %>%
  group_by(collection,month) %>% slice(1) %>% ungroup()

file_for <- function(collection,month) file.path(out,collection,paste0(format(month,'%Y%m'),'-',collection,'.csv'))
links$local_path <- mapply(file_for,links$collection,links$month, USE.NAMES=FALSE)
links$status <- 'pending'
for(i in seq_len(nrow(links))) {
  p <- links$local_path[i]; dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  if(file.exists(p) && file.size(p)>500 && !refresh) {links$status[i] <- 'cached';next}
  tmp <- paste0(p,'.part')
  tryCatch({
    utils::download.file(links$url[i],tmp,mode='wb',quiet=TRUE, method='libcurl')
    head <- readBin(tmp,'raw',n=100)
    if(file.size(tmp)<500 || grepl('<html|<!doctype',tolower(rawToChar(head)),fixed=FALSE)) stop('Response not a CSV')
    if(file.exists(p))unlink(p)
    if(!file.rename(tmp,p))stop('Could not move downloaded file')
    links$status[i] <- 'downloaded'
  },error=function(e) {
    links$status[i]<-paste0('FAILED: ',conditionMessage(e))
    if(file.exists(tmp))unlink(tmp)
  })
}
readr::write_csv(links,file.path(out,'download_manifest.csv'))
expected <- expand.grid(collection=c('beds','acute','drd'), month=seq(start_month,end_month,by='month'))
coverage <- as_tibble(expected) %>% left_join(select(links,collection,month,status,local_path),by=c('collection','month')) %>%
  mutate(status=ifelse(is.na(status),'NO PUBLISHED CSV FOUND',status))
readr::write_csv(coverage,file.path(out,'download_coverage.csv'))
message('Flow downloads: ',sum(links$status %in% c('cached','downloaded')),' available; ',
        sum(grepl('^FAILED',links$status)),' failed; ',sum(coverage$status=='NO PUBLISHED CSV FOUND'),' missing month/collection combinations.')
message('Inspect ',file.path(out,'download_coverage.csv'),' for missing releases.')
