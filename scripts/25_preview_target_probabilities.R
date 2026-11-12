# Run without altering existing forecast archives, editions, or website.
source("R/target_probability.R")
path <- "output/performance/overview_metric_rows.csv"
if (!file.exists(path)) stop("Missing ", path, "; run the modelling pipeline first.")
rows <- data.table::fread(path)
required <- c("metric_id", "display_name", "target_month", "target_value",
              "projected_target_value", "higher_is_better")
if (!all(required %in% names(rows))) stop("Overview rows are missing target fields.")
rows[, higher_is_better := as.logical(higher_is_better)]
z <- nhs_augment_target_probabilities(rows)
z[, target_chance_label := vapply(target_probability,
                                 nhs_format_target_probability, character(1))]
show <- z[, .(display_name, target_month, target_value,
              projected_target_value, target_chance_label,
              target_probability_method)]
print(show)
dir.create("output/preview", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(show, "output/preview/target_probabilities.csv")
message("Wrote output/preview/target_probabilities.csv. No edition or forecast archive changed.")
