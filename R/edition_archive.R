edition_archive_copy_files <- function(files, project_root, destination_root) {
  files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(project_root, "/")
  if (any(!startsWith(files, prefix))) {
    stop("Edition archive inputs must be inside the project root.")
  }
  relative <- substring(files, nchar(prefix) + 1L)
  destinations <- file.path(destination_root, relative)
  invisible(mapply(function(source, destination) {
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, destination, overwrite = TRUE, copy.date = TRUE)) {
      stop("Could not freeze edition input: ", source)
    }
    TRUE
  }, files, destinations, USE.NAMES = FALSE))
  data.table::data.table(
    relative_path = gsub("\\\\", "/", relative),
    size_bytes = as.numeric(file.info(files)$size),
    md5 = unname(tools::md5sum(files))
  )
}

edition_archive_input_files <- function(project_root = getwd()) {
  fixed <- c(
    "config/performance_metrics.csv",
    "config/national_model.csv",
    "config/provider_model.csv",
    "config/core_model.csv",
    "config/core_targets.csv",
    "config/editorial_commentary.csv",
    "data-interim/national_month_stage1.csv",
    "data-interim/core/national_panel.csv",
    "data-interim/core/community_waits_service_summary.csv",
    "data-interim/core/downloaded_source_manifest.csv",
    "output/performance/latest_published_forecast_rows.csv"
  )
  trees <- c("output/national", "output/provider", "output/core")
  tree_files <- unlist(lapply(file.path(project_root, trees), function(path) {
    if (!dir.exists(path)) return(character())
    list.files(path, pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)
  fixed_paths <- file.path(project_root, fixed)
  missing <- fixed[!file.exists(fixed_paths)]
  # Editorial commentary is optional; every other fixed input is required.
  missing_required <- setdiff(missing, "config/editorial_commentary.csv")
  if (length(missing_required)) {
    stop("Cannot freeze edition; missing inputs: ",
         paste(missing_required, collapse = ", "), ".")
  }
  unique(c(fixed_paths[file.exists(fixed_paths)], tree_files))
}

normalise_outlook_edition_revision <- function(revision = "") {
  revision <- trimws(tolower(as.character(revision)[1L]))
  if (is.na(revision) || !nzchar(revision)) return("")
  if (!grepl("^r[1-9][0-9]*$", revision)) {
    stop(
      "Invalid analytical edition revision '", revision,
      "'. Use an empty value for the original edition, or r1, r2, and so on."
    )
  }
  revision
}

outlook_edition_root <- function(issue_date, publication_mode,
                                 edition_revision = "",
                                 project_root = getwd()) {
  issue_day <- format(as.Date(issue_date), "%Y-%m-%d")
  mode <- tolower(as.character(publication_mode)[1L])
  if (!mode %in% c("forecast", "outturn")) stop("Invalid publication mode.")
  revision <- normalise_outlook_edition_revision(edition_revision)
  base <- file.path(project_root, "output", "editions", issue_day, mode)
  if (nzchar(revision)) file.path(base, "revisions", revision) else base
}

assert_outlook_edition_available <- function(issue_date, publication_mode,
                                             edition_revision = "",
                                             project_root = getwd()) {
  issue_day <- format(as.Date(issue_date), "%Y-%m-%d")
  mode <- tolower(as.character(publication_mode)[1L])
  revision <- normalise_outlook_edition_revision(edition_revision)
  edition_root <- outlook_edition_root(
    issue_date, mode, revision, project_root
  )
  edition_manifest <- file.path(edition_root, "edition_manifest.csv")
  if (file.exists(edition_manifest)) {
    verify_frozen_outlook_edition(edition_root)
    edition_label <- if (nzchar(revision)) {
      paste0(issue_day, " (", mode, ", revision ", revision, ")")
    } else {
      paste0(issue_day, " (", mode, ")")
    }
    stop(
      "Edition ", edition_label, " already has a locked analytical archive. ",
      "Use scripts/24_render_frozen_edition.R for a presentation-only ",
      "correction, or choose the next unused analytical revision (for example r2)."
    )
  }
  invisible(edition_root)
}

freeze_outlook_edition <- function(issue_date, publication_mode,
                                   publication_status = "pilot",
                                   source_forecast_issue_date = issue_date,
                                   edition_revision = "",
                                   project_root = getwd()) {
  issue_day <- format(as.Date(issue_date), "%Y-%m-%d")
  mode <- tolower(as.character(publication_mode))
  if (!mode %in% c("forecast", "outturn")) stop("Invalid publication mode.")
  revision <- normalise_outlook_edition_revision(edition_revision)
  edition_root <- assert_outlook_edition_available(
    issue_date, mode, revision, project_root
  )
  input_root <- file.path(edition_root, "inputs")
  dir.create(input_root, recursive = TRUE, showWarnings = FALSE)

  edition_archive_copy_files(
    edition_archive_input_files(project_root), project_root, input_root
  )
  # RTT diagnostics use the latest two raw monthly extracts. Freeze those bytes
  # and make the archived manifest portable rather than retaining a machine-
  # specific absolute path.
  archived_download_manifest <- file.path(
    input_root, "data-interim", "core", "downloaded_source_manifest.csv"
  )
  if (file.exists(archived_download_manifest)) {
    downloads <- data.table::fread(archived_download_manifest, encoding = "UTF-8")
    if (all(c("metric_id", "dataset_role", "selected", "is_current",
              "activity_month", "local_path") %in% names(downloads))) {
      selected <- which(
        downloads$metric_id == "rtt_18w" &
          downloads$dataset_role == "provider_monthly" &
          downloads$selected %in% c(TRUE, "TRUE", "true", 1, "1") &
          downloads$is_current %in% c(TRUE, "TRUE", "true", 1, "1")
      )
      if (length(selected)) {
        source_paths <- as.character(downloads$local_path[selected])
        missing_sources <- source_paths[!file.exists(source_paths)]
        if (length(missing_sources)) {
          stop("Cannot freeze RTT diagnostic source(s): ",
               paste(missing_sources, collapse = ", "))
        }
        frozen_source_dir <- file.path(
          input_root, "data-interim", "core", "frozen_sources"
        )
        dir.create(frozen_source_dir, recursive = TRUE, showWarnings = FALSE)
        for (j in seq_along(selected)) {
          month_tag <- gsub("[^0-9]", "", as.character(
            downloads$activity_month[selected[j]]
          ))
          frozen_name <- paste0(
            "rtt_", month_tag, "_", basename(source_paths[j])
          )
          frozen_path <- file.path(frozen_source_dir, frozen_name)
          if (!file.copy(source_paths[j], frozen_path, overwrite = TRUE,
                         copy.date = TRUE)) {
            stop("Could not freeze RTT diagnostic source: ", source_paths[j])
          }
          downloads$local_path[selected[j]] <- gsub(
            "\\\\", "/", file.path(
              "data-interim", "core", "frozen_sources", frozen_name
            )
          )
        }
        data.table::fwrite(downloads, archived_download_manifest)
      }
    }
  }
  archived_inputs <- list.files(
    input_root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE
  )
  archived_inputs <- archived_inputs[!file.info(archived_inputs)$isdir]
  input_prefix <- paste0(
    normalizePath(input_root, winslash = "/", mustWork = TRUE), "/"
  )
  inventory <- data.table::data.table(
    relative_path = substring(
      normalizePath(archived_inputs, winslash = "/", mustWork = TRUE),
      nchar(input_prefix) + 1L
    ),
    size_bytes = as.numeric(file.info(archived_inputs)$size),
    md5 = unname(tools::md5sum(archived_inputs))
  )
  template_files <- list.files(
    file.path(project_root, "reports"), pattern = "[.]html$",
    full.names = TRUE
  )
  template_inventory <- edition_archive_copy_files(
    template_files, project_root, file.path(edition_root, "original_templates")
  )
  template_inventory[, relative_path := sub("^reports/", "", relative_path)]

  data.table::fwrite(inventory, file.path(edition_root, "input_manifest.csv"))
  data.table::fwrite(
    template_inventory,
    file.path(edition_root, "original_template_manifest.csv")
  )
  manifest <- data.table::data.table(
    issue_date = issue_day,
    publication_mode = mode,
    edition_revision = revision,
    publication_status = as.character(publication_status),
    source_forecast_issue_date = format(
      as.Date(source_forecast_issue_date), "%Y-%m-%d"
    ),
    frozen_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    input_files = nrow(inventory),
    input_bytes = sum(inventory$size_bytes),
    archive_version = 2L
  )
  data.table::fwrite(manifest, file.path(edition_root, "edition_manifest.csv"))
  invisible(edition_root)
}

verify_frozen_outlook_edition <- function(edition_root) {
  manifest_path <- file.path(edition_root, "input_manifest.csv")
  if (!file.exists(manifest_path)) stop("Frozen edition manifest not found: ", manifest_path)
  inventory <- data.table::fread(manifest_path, encoding = "UTF-8")
  assert_columns(inventory, c("relative_path", "size_bytes", "md5"),
                 "frozen edition input manifest")
  paths <- file.path(edition_root, "inputs", inventory$relative_path)
  missing <- inventory$relative_path[!file.exists(paths)]
  if (length(missing)) stop("Frozen edition is incomplete: ", paste(missing, collapse = ", "))
  current_md5 <- unname(tools::md5sum(paths))
  changed <- inventory$relative_path[current_md5 != inventory$md5]
  if (length(changed)) stop("Frozen edition inputs have changed: ", paste(changed, collapse = ", "))
  invisible(TRUE)
}

