clean_cache_files <- function(cache_dir, n_days = NULL, area_name = NULL, verbose = TRUE) {
  # If area_name specified, target sub-directory
  if (!is.null(area_name)) {
    cache_dir <- file.path(cache_dir, area_name)
  } else {
    stop("'area_name' must be specified!")
  }
  
  if (!dir.exists(cache_dir)) {
    if (verbose) message(paste("Cache directory not found:", cache_dir))
    return(invisible(NULL))
  }
  
  # List files
  files <- list.files(cache_dir, full.names = TRUE)
  files <- files[file.info(files)$isdir == FALSE]
  
  if (length(files) == 0) {
    if (verbose) message("No files found in cache to clean.")
    return(invisible(NULL))
  }
  
  # Either delete all
  if (is.null(n_days)) {
    file.remove(files)
    if (verbose) message(paste("Deleted all", length(files), "files in", cache_dir))
  } else {
    file_info <- file.info(files)
    age_days <- as.numeric(difftime(Sys.time(), file_info$ctime, units = "days"))
    old_files <- files[age_days > n_days]
    file.remove(old_files)
    if (verbose) {
      message(paste("Deleted", length(old_files), "files older than", n_days, "days in", cache_dir))
    }
  }
}
