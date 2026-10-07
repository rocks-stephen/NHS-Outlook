if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org")
if (!file.exists("renv.lock")) renv::init(bare = TRUE, restart = FALSE)
renv::install(c(
  "data.table", "ggplot2", "readxl", "curl", "xml2", "openssl", "pagedown"
))
message("Review package versions, then run renv::snapshot().")
