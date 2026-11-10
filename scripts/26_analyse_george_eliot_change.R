# NHS Outlook | George Eliot: exploratory explanation of A&E four-hour improvement
# Run from ae-trust-forecast project root after scripts/23_refresh_ae_flow_deep_dive.R
# No package installation here. Existing renv environment supplies dplyr/readr/ggplot2/tidyr.
required <- c('dplyr','readr','tidyr','ggplot2')
missing <- setdiff(required, rownames(installed.packages()))
if(length(missing)) stop('Install project dependencies using the project setup script: ', paste(missing,collapse=', '))
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr);library(ggplot2)})
source("R/utils.R")  # bounded percentage chart axes
input <- 'output/deep-dives/george-eliot/george_eliot_panel.csv'
out <- 'output/deep-dives/george-eliot/change-analysis'
if(!file.exists(input)) stop('Run the flow refresh first; missing ',input)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
p <- read_csv(input,show_col_types=FALSE)
if(!'month'%in%names(p)) stop('Missing month column')
p$month <- as.Date(p$month)
if(anyNA(p$month)||anyDuplicated(p$month)) stop('Invalid or duplicate monthly date(s)')
p <- arrange(p,month)
need <- c('ae_within4_pct','ae_all_attendances','ga_occupancy_pct','drd_delayed_days_share_pct','los21_occupied_share_pct','weekend_share_pct','weekend_weekday_daily_ratio','complete_month')
for(v in setdiff(need,names(p)))p[[v]]<-NA_real_
# Standardise percentage metrics only where they have been encoded as proportions.
for(v in c('ae_within4_pct','ga_occupancy_pct','drd_delayed_days_share_pct','los21_occupied_share_pct','weekend_share_pct')) {
  x <- suppressWarnings(as.numeric(p[[v]]))
  if(any(is.finite(x)) && max(x,na.rm=TRUE)<=1.01 && min(x,na.rm=TRUE)>=0) x<-x*100
  p[[v]]<-x
}
for(v in c('ae_all_attendances','weekend_weekday_daily_ratio'))p[[v]]<-as.numeric(p[[v]])
# Only complete reporting months should enter weekend comparisons.
p$weekend_share_pct[is.na(p$complete_month) | !(as.character(p$complete_month)%in%c('TRUE','1','T'))] <- NA_real_
p$weekend_weekday_daily_ratio[is.na(p$complete_month) | !(as.character(p$complete_month)%in%c('TRUE','1','T'))] <- NA_real_
p$phase <- case_when(p$month < as.Date('2025-07-01') ~ 'Pre-Jul-2025',
                     p$month < as.Date('2026-03-01') ~ 'Jul-2025–Feb-2026',
                     TRUE ~ 'Mar-2026 onward')
metrics <- c('ae_within4_pct','ae_all_attendances','ga_occupancy_pct','drd_delayed_days_share_pct','los21_occupied_share_pct','weekend_share_pct','weekend_weekday_daily_ratio')
# Overview: coverage, three phases, and matched calendar-month change.
coverage <- tibble(metric=metrics,n_months=vapply(metrics,function(v)sum(is.finite(p[[v]])),integer(1)),first_month=as.Date(vapply(metrics,function(v)if(all(is.na(p[[v]]))) NA_character_ else as.character(min(p$month[is.finite(p[[v]])])),character(1))),last_month=as.Date(vapply(metrics,function(v)if(all(is.na(p[[v]]))) NA_character_ else as.character(max(p$month[is.finite(p[[v]])])),character(1))))
write_csv(coverage,file.path(out,'01_coverage.csv'))
phase_summary <- p %>% select(month,phase,all_of(metrics)) %>% pivot_longer(all_of(metrics),names_to='metric',values_to='value') %>% group_by(phase,metric) %>% summarise(n=sum(!is.na(value)),mean=if(all(is.na(value))) NA_real_ else mean(value,na.rm=TRUE),.groups='drop')
write_csv(phase_summary,file.path(out,'02_period_means.csv'))
# Like-for-like March–September comparison, 2025 versus 2026, excluding unavailable values.
matched <- p %>% filter(format(month,'%m')%in%sprintf('%02d',3:9),format(month,'%Y')%in%c('2025','2026')) %>% mutate(year=format(month,'%Y'),calendar_month=format(month,'%m')) %>% select(year,calendar_month,all_of(metrics)) %>% pivot_longer(all_of(metrics),names_to='metric',values_to='value') %>% pivot_wider(names_from=year,values_from=value) %>% mutate(change=`2026`-`2025`)
write_csv(matched,file.path(out,'03_month_matched_changes.csv'))
matched_summary <- matched %>% group_by(metric) %>% summarise(n_matched=sum(complete.cases(`2025`,`2026`)),mean_change=if(all(is.na(change)))NA_real_ else mean(change,na.rm=TRUE),.groups='drop')
write_csv(matched_summary,file.path(out,'04_matched_summary.csv'))
# Charts: each facet separately scaled, displayed in separate image output.
labels<-c(ae_within4_pct='A&E four-hour performance (%)',ae_all_attendances='Monthly A&E attendances',ga_occupancy_pct='G&A occupancy (%)',drd_delayed_days_share_pct='DRD delayed bed days / available bed days (%)',los21_occupied_share_pct='21+ day stays / occupied beds (%)',weekend_share_pct='Weekend share of discharges (%)',weekend_weekday_daily_ratio='Weekend / weekday discharge per-day ratio')
for(v in metrics){
  q<-ggplot(p,aes(x=month,y=.data[[v]]))+geom_line(na.rm=TRUE)+geom_point(size=1.4,na.rm=TRUE)+geom_vline(xintercept=as.Date('2025-07-01'),linetype=3)+geom_vline(xintercept=as.Date('2026-03-01'),linetype=2)+labs(title=paste('George Eliot:',labels[[v]]),subtitle='Dotted: July 2025; dashed: March 2026',x=NULL,y=NULL)+theme_minimal(base_size=12)
  # Percentage shares have physical limits; occupancy can legitimately be
  # reported above 100% against nominal bed availability, so do not cap it.
  if (grepl('_pct$', v) && v != 'ga_occupancy_pct') {
    q <- q + scale_y_continuous(
      limits=nhs_percentage_axis_limits,
      breaks=nhs_percentage_axis_breaks,
      oob=scales::oob_keep,
      expand=expansion(mult=0)
    )
  }
  ggsave(file.path(out,paste0('chart_',v,'.png')),q,width=9,height=4,dpi=160)
}
# Exploratory first-difference and one-month-lag correlations; descriptive only, no causal interpretation.
# delta(flow at t) vs delta(A&E at t or t+1): n can be small and seasonal.
cor_rows <- lapply(setdiff(metrics,'ae_within4_pct'),function(v){
  a<-diff(p$ae_within4_pct); b<-diff(p[[v]])
  calc<-function(x,y){z<-complete.cases(x,y);c(n=sum(z),r=if(sum(z)>=8 && sd(x[z])>0 && sd(y[z])>0) cor(x[z],y[z]) else NA_real_)}
  now<-calc(a,b)
  lead<-if(length(a)>1)calc(a[-1],b[-length(b)]) else c(n=0,r=NA_real_)
  tibble(metric=v,same_month_n=unname(now[1]),same_month_r=unname(now[2]),flow_leads_ae_one_month_n=unname(lead[1]),flow_leads_ae_one_month_r=unname(lead[2]))
})
write_csv(bind_rows(cor_rows),file.path(out,'05_exploratory_first_difference_correlations.csv'))
# March 2026 decomposition: rate vs volume.
m2<-p%>%filter(month%in%as.Date(c('2026-02-01','2026-03-01','2025-03-01')))%>%select(month,ae_within4_pct,ae_all_attendances,all_of(setdiff(metrics,c('ae_within4_pct','ae_all_attendances'))))
write_csv(m2,file.path(out,'06_key_months.csv'))
# Lightweight narrative with no causal claims.
writeLines(c('GEORGE ELIOT | EXPLORATORY FLOW ANALYSIS',
 'Source: local NHS Outlook merged panel; analysis generated after download and parser validation.',
 'Use 01_coverage to confirm available data; 02 and 04 report period comparisons.',
 'Inspect plots for shifts preceding March 2026. Compare seasonal matched-month results before drawing conclusions.',
 'Weekend measures are excluded in months that fail daily completeness QA.',
 'Correlations in 05 are exploratory and vulnerable to seasonality, confounding, serial correlation, and reverse causality.',
 'Neither a provider time series nor a forecast residual identifies a causal intervention effect.',
 'Next steps: validate source definitions, investigate board paper chronology, compare matched providers, assess Type 1 mix, acuity and admission conversion.'),file.path(out,'README_findings.txt'))
message('Completed analysis in: ',normalizePath(out,winslash='/'))
print(matched_summary,n=Inf)
