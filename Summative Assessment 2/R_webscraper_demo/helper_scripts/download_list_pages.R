# Loading require library
require(rvest)
require(dplyr)
require(purrr)
require(stringr)
require(lubridate)
require(httr)
require(stringr)

# Load the called custom function
helper_dir <- "helper_scripts"
source(file.path(helper_dir, "clean_cache_files.R"))
source(file.path(helper_dir, "get_max_page.R"))


# Download the html and cached to local directory
download_list_page <- function(page_num = 1, area_code = "13225", area_name = "inagi", overwrite = FALSE,
                               cache_dir = "cache_list_html", sys_wait = 0.5, log_name = NULL, job_id = NULL,
                               read_max = TRUE, clean_cache = TRUE, clean_nday_old = 7) {
  
  # First run cleaner function if activated
  if(clean_cache){
    clean_cache_files(cache_dir = cache_dir,
                      area_name = area_name,
                      n_days = clean_nday_old)
  }
  
  
  # timer
  start_time <- Sys.time()
  
  # Create root and sub-directory for area
  area_dir <- file.path(cache_dir, area_name)
  dir.create(area_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Initialize data.frame to collect response status
  res_log <- tibble(area_name = character(),
                    area_code = character(),
                    page = integer(),
                    url = character(),
                    status = integer(),
                    condition = character(),
                    timestamp = POSIXct(),
                    elapse_time_sec = numeric())
  
  base_url <- "https://suumo.jp/jj/chintai/ichiran/FR301FC001/"
  
  # Read the max page
  if(read_max){
    page_1_url <- paste0(base_url,
                         "?ar=030&bs=040&ta=13&sc=", area_code,
                         "&cb=0.0&ct=9999999&et=9999999&cn=9999999",
                         "&mb=0&mt=9999999&shkr1=03&shkr2=03&shkr3=03&shkr4=03",
                         "&fw2=&srch_navi=1&page=1")
    res <- read_html(page_1_url)
    max_page <- get_max_page(res)
    page_to_read <- 1:max_page
    
  } else {
    page_to_read <- page_num
  }
  
  # Primary loop
  for (i in seq_along(page_to_read)) {
    # Step 1: build url, check cache, read url/cache
    url <- paste0(base_url,
                  "?ar=030&bs=040&ta=13&sc=", area_code,
                  "&cb=0.0&ct=9999999&et=9999999&cn=9999999",
                  "&mb=0&mt=9999999&shkr1=03&shkr2=03&shkr3=03&shkr4=03",
                  "&fw2=&srch_navi=1&page=", page_to_read[i])
    # Set up cache file path
    cache_file <- paste0("listings_", area_code, "_", area_name, "_", 
                         str_pad(page_to_read[i], width = 3, pad = "0"), ".html")
    cache_file_path <- file.path(area_dir, cache_file)
    
    # Skip download if already cached, unless overwrite == TRUE
    if (!overwrite && file.exists(cache_file_path)) {
      message(paste("Cached:", cache_file_path))
      next
    }
    
    # Check HTTP status
    res <- tryCatch(GET(url), error = function(e) {
      message(paste0("Request failed for ", area_code, area_name, "page", page_to_read[i]))
    })
    
    Sys.sleep(abs(rnorm(1, sys_wait, 0.5)))
    
    if (is.null(res)) {
      res_log <- add_row(res_log,
                        area_name = area_name,
                        area_code = area_code,
                        page = page_to_read[i],
                        url = url,
                        status = 0,
                        condition = "Request failed",
                        timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                        elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    }
    
    # Read the status
    status <- status_code(res)
  
    if (status == 404) {
      message(paste("404 Not Found", area_name, "page", page_to_read[i]))
      res_log <- add_row(res_log,
                         area_name = area_name,
                         area_code = area_code,
                         page = page_to_read[i],
                         url = url,
                         status = status,
                         condition = "URL not found",
                         timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                         elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    } else if (status != 200) {
      message(paste("HTTP", status, "for", area_name, "page", page_to_read[i]))
      res_log <- add_row(res_log,
                         area_name = area_name,
                         area_code = area_code,
                         page = page_to_read[i],
                         url = url,
                         status = status,
                         condition = "Response failed",
                         timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                         elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
      next
    }
    
    # If successful ... record it
    res_log <- add_row(res_log,
                       area_name = area_name,
                       area_code = area_code,
                       page = page_to_read[i],
                       url = url,
                       status = status,
                       condition = "Read success",
                       timestamp = with_tz(Sys.time() , tzone = "Asia/Tokyo"),
                       elapse_time_sec = as.numeric(Sys.time() - start_time, units = "secs"))
    
    # Attempt to read and write page
    tryCatch({
      page <- read_html(res)
      writeLines(as.character(page), con = cache_file_path)
      message(paste("Download:", area_name, "page", page_to_read[i], " of ", length(page_to_read)))
    }, error = function(e) {
      # Update the condition field of failed page
      res_log[res_log$page == page_to_read[i],]$condition <- "Parse failed"
      message(paste("Failed to parse/save", area_name, "page", page_to_read[i]))
    })
    
  }
  
  time_elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  message(paste("Completed", length(page_to_read), "requests for", area_name))
  message(paste("Success:", sum(res_log$condition == "Read success")))
  message(paste("Failures:", sum(res_log$condition != "Read success")))
  message(paste("Total time elapsed:", round(time_elapsed/60, digits = 2), "minute(s)"))
  message(paste0("Average ", time_elapsed/length(page_to_read), " seconds per download"))
  
  if(nrow(res_log) > 0) {
    dir.create(file.path("res_listing_page_log"), recursive = TRUE, showWarnings = FALSE)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    if(is.null(log_name)) {
      log_name <- paste0("res_log_list_", timestamp, "_", area_code, "_", area_name, "_", job_id, ".csv")
    } else {
      log_name <- paste0("res_log_list_", timestamp, "_", area_code, "_", area_name, "_", log_name, "_", job_id, ".csv")
    }

    log_path <- file.path("res_listing_page_log", log_name)
    write_csv(res_log, file = log_path)
    message(paste("Saved download log to:", log_path))
  }
  
  return(res_log)
  
}