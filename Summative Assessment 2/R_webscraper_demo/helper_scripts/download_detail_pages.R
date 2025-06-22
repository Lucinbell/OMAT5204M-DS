require(httr)
require(lubridate)
require(dplyr)

# 
helper_dir <- "helper_scripts"
source(file.path(helper_dir, "clean_cache_files.R"))

# Takes in one or a vector of urls and attempt to retrieve the response and cache the result
download_detail_pages <- function(urls, cache_dir = "cache_details_html", area = "DEFAULT",
                                  overwrite = FALSE, sys_wait = 1, timeout_sec = 10,
                                  log_name = NULL, hide_success = TRUE, read_line_notice = 100,
                                  job_id = NULL, area_code = NULL, clean_cache = TRUE, clean_nday_old = 7) {
  # First run cleaner function if activated
  if(clean_cache){
    clean_cache_files(cache_dir = cache_dir,
                      area_name = area,
                      n_days = clean_nday_old)
  }
  
  # timer
  start_time <- Sys.time()

  # Create root and sub-directory for area
  area_dir <- file.path(cache_dir, area)
  dir.create(area_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Initialize data.frame to collect response status
  res_log <- tibble(listing_id = character(),
                    url = character(),
                    status = integer(),
                    condition = character(),
                    timestamp = POSIXct(),
                    elapse_time_sec = numeric())
  
  
  counter <- 0
  
  for (i in seq_along(urls)) {
    id <-  str_extract(urls[i], "(?<=\\?bc=)\\d{12}")
    url <- urls[i]
    file_path <- file.path(cache_dir, area, paste0("detail_", id, ".html"))
    counter <- counter + 1
    # Skip download if already cached
    if (!overwrite && file.exists(file_path)) {
      message(paste("Cached:", id))
      res_log <- add_row(res_log,
                         listing_id = id,
                         url = url,
                         status = 0,
                         condition = "Cached",
                         timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                         elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    }
    
    # Check HTTP status
    res <- tryCatch(httr::GET(url, timeout(timeout_sec)), error = function(e) {
      message(paste0("Request failed for ", id, "-", e$message))
    })
    
    Sys.sleep(abs(rnorm(1, sys_wait, 0.5)))
    
    if (is.null(res)) {
      res_log <- add_row(res_log,
                         listing_id = id,
                         url = url,
                         status = 0,
                         condition = "Request failed",
                         timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                         elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    }
    
    
    status <- httr::status_code(res)
    
    if (status == 404) {
      message(paste("404 Not Found:", id))
      res_log <- add_row(res_log,
                         listing_id = id,
                         url = url,
                         status = status,
                         condition = "URL not found",
                         timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                         elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    } else if (status != 200) {
      message(paste("HTTP", status, "for", id))
      res_log <- add_row(res_log,
                         listing_id = id,
                         url = url,
                         status = status,
                         condition = "Response failed",
                         timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                         elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    }
    
    # If successful... write it down
    res_log <- add_row(res_log,
                       listing_id = id,
                       url = url,
                       status = status,
                       condition = "Read success",
                       timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                       elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
    
    # Attempt to read and write page
    tryCatch({
      page <- read_html(res)
      writeLines(as.character(page), con = file_path)
      if(!hide_success){
        message(paste("Downloaded:", id))  
      }
    }, error = function(e) {
      # Update the condition field of ID list
      res_log[res_log$listing_id == id,]$condition <- "Parse failed"
      message(paste("Failed to parse/save", id, "-", e$message))
    })
    
    # Counter Readout
    
    if(!is.null(read_line_notice) & is.numeric(read_line_notice) == TRUE) {
      if(counter %% read_line_notice == 0) {
        message(paste0("Processed ", counter, " urls"))
      }
    }
    
  }
  
  time_elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  message(paste("Completed", length(urls), "requests for area:", area))
  message(paste("Success:", sum(res_log$condition == "Read success")))
  message(paste("Failures:", sum(res_log$condition != "Read success")))
  message(paste("Total time elapsed:", round(time_elapsed/60, digits = 2), "minute(s)"))
  message(paste0("Average ", time_elapsed/length(urls), " seconds per download"))
  
  
  if(nrow(res_log) > 0) {
    dir.create(file.path("res_detail_page_log"), recursive = TRUE, showWarnings = FALSE)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    if(is.null(log_name)){
      log_name <- paste0("res_log_detail_", timestamp, "_", area_code, "_", area, "_", job_id, ".csv")
    } else {
      log_name <- paste0("res_log_detail_", timestamp, "_", area_code, "_", area, "_", log_name, "_", job_id, ".csv")
    }
    
    log_path <- file.path("res_detail_page_log", log_name)
    write_csv(res_log, file = log_path)
    message(paste("Saved download log to:", log_path))
  }

  return(res_log)  
}



