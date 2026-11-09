# NHS Outlook monthly flow deep dive: download, build, parse, publish.
source('scripts/20_download_flow_sources.R')
options(nhs.outlook.deep_dive.data_dir=file.path(getwd(),'data','flow'),
        nhs.outlook.deep_dive.download=FALSE)
source('scripts/21_build_ae_flow_deep_dive.R')
source('scripts/21b_parse_flow_downloads.R')
source('scripts/22_publish_ae_flow_deep_dive.R')
