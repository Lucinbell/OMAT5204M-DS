# Referenced library
require(readr)
require(dplyr)
require(digest)

# Source the scripts referenced..
helper_dir <- "helper_scripts"
source(file.path(helper_dir, "download_list_pages.R"))
source(file.path(helper_dir, "parse_list_page_folder.R"))
source(file.path(helper_dir, "clean_list_df.R"))
source(file.path(helper_dir, "dedupe_list.R"))
source(file.path(helper_dir, "create_url_list.R"))
source(file.path(helper_dir, "download_detail_pages.R"))
source(file.path(helper_dir, "parse_detail_page_folder.R"))
source(file.path(helper_dir, "clean_detail_df.R"))
source(file.path(helper_dir, "join_results.R"))

area_extract <- function(area_name, area_code_table = "data/tokyo_wards_and_cities_codes.csv",
                         sample_part = 1, parse_part = 1, split_batches = NULL,
                         overwrite = FALSE, sys_wait = 0.5, seed = NULL,
                         cache_dir_list = "cache_list_html",
                         cache_dir_detail = "cache_details_html",
                         timeout_sec = 10, full_process = FALSE,
                         hide_success = TRUE, read_line_notice = 100,
                         clean_cache = TRUE, clean_nday_old = NULL,
                         run_dt_page_section = TRUE) {
  # Set up for logging record
  start_time <- Sys.time()
  timestamp_start <- format(start_time, "%Y%m%d_%H%M%S")
  start_time_date <- as.character(Sys.Date())
  job_id <- substr(digest(Sys.time(), algo = "sha1"), 1, 8)
  
  log_df <- tibble(
    job_id = character(),
    date = character(),
    start_time = POSIXct(),
    end_time = POSIXct(),
    total_time_taken = numeric(),
    area_name = character(),
    area_code = character(),
    sys_wait = numeric(),
    n_list_page_attempted = integer(),
    n_list_page_success = integer(),
    n_list_page_failed = integer(),
    list_dl_time_min = numeric(),
    n_listing_parsed = integer(),
    list_parse_time_min = numeric(),
    n_listing_after_deduped = integer(),
    sample_part = numeric(),
    rand_seed = integer(),
    n_detail_attempted = integer(),
    n_detail_success = integer(),
    n_detail_fail = integer(),
    detail_dl_time_min = numeric(),
    overwrite = logical(),
    full_process = logical(),
    n_detail_parsed = integer(),
    detail_parse_time_min = numeric()
  )
  
  # Step 0: get the area code and relevant URL for list page
  city_info_extract <- read_csv(file = area_code_table, show_col_types = FALSE) %>% 
    mutate(
      area_code = str_extract(page_url, "(?<=sc=)\\d{5}")
    ) %>%
    filter(names_en %in% area_name) %>%
    select(names_en, region_en, area_code, page_url)
  
  
  ## Main body loop
  for (i in 1:length(area_name)) {
    
    start_time_list <- Sys.time()
    
    # Extract the needed area code
    area_code <- city_info_extract[city_info_extract$names_en == area_name[i],]$area_code
    
    message(paste0("--!! Begin List Page Section for ", area_name[i], " !!--"))
    # Download the list page
    message(paste0("-- Begin download list page html --"))
    res_log_list <- download_list_page(area_code = area_code, area_name = area_name[i],
                        overwrite = overwrite, cache_dir = cache_dir_list,
                        sys_wait = sys_wait, read_max = TRUE, job_id = job_id,
                        clean_cache = clean_cache, clean_nday_old = clean_nday_old)
    
    n_list_page <- nrow(res_log_list)
    n_list_page_suc <- nrow(res_log_list[res_log_list$condition == "Read success",])
    n_list_page_fai <- nrow(res_log_list[res_log_list$condition != "Read success",])
    list_dl_time <- as.numeric(Sys.time() - start_time_list, units = "secs")/60
    
    message(paste0("-- Begin parsing, cleaning, de-duplicating list page data from ", area_name[i], "; area code ", area_code))
    message(paste0("- Directory: ", file.path(cache_dir_list, area_name[i])))
    # Parse and clean the data
    
    list_parse_start <- Sys.time()
    
    df_list_raw <- parse_list_page_folder(dir_cache = cache_dir_list,
                                          area_name = area_name[i])
    
    list_parse_time_min <- as.numeric(Sys.time() - list_parse_start, units = "secs")/60
    
    n_listing_parsed <- nrow(df_list_raw)
    df_list_clean <- clean_list_df(df_list_raw)
    
    
    message(paste0("-- Running De-duplication --"))
    df_list_deduped <- dedupe_list(df_list_clean)
    n_listing_after_dd <- nrow(df_list_deduped)
    
    # Save de-duped result

    deduped_file <- paste0("deduped_", area_name[i], "_", area_code, ".csv")
    deduped_folder <- file.path("data_deduped_list", paste0(timestamp_start, "_", job_id))
    deduped_file_path <- file.path(deduped_folder, deduped_file)
    
    if (!file.exists(deduped_folder)) {
      dir.create(deduped_folder, recursive = TRUE, showWarnings = FALSE)
    }

    write_csv(df_list_deduped, file = deduped_file_path)
    message(paste0("-- Saved deduped result to: ", deduped_file_path))
    
    end_time_list <- Sys.time()
    process_time_min <- as.numeric(end_time_list - start_time_list, units = "secs")/60
    message(paste0("--!! List data extraction completed for ", area_name[i], "; total process time: ", process_time_min, " minute(s). !!--"))
    
    # Run detail page section if run_dt_page_section == TRUE
    if(run_dt_page_section){
      message(paste0("\n --!! Begin Details Page Section !!--"))
      start_time_detail <- Sys.time()
      # Create the url lists of detail page to extract
      detail_page_url <- create_url_list(df_list_deduped,
                                         sample_part = sample_part,
                                         drop_bus = TRUE, seed = seed)
      
      start_time_detail_dl <- Sys.time()
      message(paste0("-- Begin download detail page html --"))
      res_log_detail <- download_detail_pages(urls = detail_page_url$detail_url,
                                              cache_dir = cache_dir_detail, area = area_name[i],
                                              overwrite = overwrite, sys_wait = sys_wait, timeout_sec = timeout_sec,
                                              hide_success = hide_success, read_line_notice = read_line_notice,
                                              job_id = job_id, area_code = area_code,
                                              clean_cache = clean_cache, clean_nday_old = clean_nday_old)
      
      n_detail <- nrow(res_log_detail)
      n_detail_suc <- nrow(res_log_detail[res_log_detail$condition == "Read success", ])
      n_detail_fai <- nrow(res_log_detail[res_log_detail$condition != "Read success", ])
      detail_dl_time <- as.numeric(Sys.time() - start_time_detail_dl, units = "secs")/60
      
      # IF to determine whether or not to continue
      if(full_process & sample_part > 0){
        message("--- full_process = TRUE and sample_part > 0; proceed to further parsing and joining ---")
        
        message(paste0("-- Begin parsing, cleaning, and joining detail page data from ", area_name[i]))
        detail_parse_start <- Sys.time()
        df_detail_page_raw <- parse_detail_page_folder(dir_cache = cache_dir_detail,
                                                       area_name = area_name[i],
                                                       parse_prop = parse_part, seed = NULL)
        n_detail_parsed <- nrow(df_detail_page_raw)
        detail_parse_time <- as.numeric(Sys.time() - detail_parse_start, units = "secs")/60
        
        df_detail_page_clean <- clean_detail_df(df_detail_page_raw)
        df_joined <- join_results(df_list_deduped, df_detail_page_clean)
        
        
        # Export and store the data
        file_name_joined <- paste0("final_", area_name[i], "_", area_code, ".csv")
        folder_joined <- file.path("data_final_joined", paste0(timestamp_start, "_", job_id))
        file_name_joined_path <- file.path(folder_joined, file_name_joined)
        
        if (!file.exists(folder_joined)) {
          dir.create(folder_joined, recursive = TRUE, showWarnings = FALSE)
        }
        
        write_csv(df_joined, file = file_name_joined_path)
        
        detail_page_elapsed_time <- as.numeric(Sys.time() - start_time_detail, units = "secs")/60
        
        message(paste0("-- Joining completed, total elapsed time: ", detail_page_elapsed_time, " minute(s) --"))
        
      } else{
        message("--- full_process = FALSE or sample_part = 0; culminate area_extract process ---")
        detail_page_elapsed_time <- as.numeric(Sys.time() - start_time_detail, units = "secs")/60
        
        n_detail_parsed <- NA_integer_
        detail_parse_time <- NA_real_
        
        message(paste0("-- Detail page part completed, total elapsed time: ", detail_page_elapsed_time, " minute(s) --"))
      }
      
      end_time <- Sys.time()
      tot_time <- as.numeric(end_time - start_time_list, units = "secs")/60
      rand_seed <- if(is.null(seed)){(NA_integer_)}else{seed}
      # Make the log
      log_df <- add_row(log_df,
                        job_id = job_id,
                        date = start_time_date,
                        start_time = start_time_list,
                        end_time = end_time,
                        total_time_taken = tot_time,
                        area_name = area_name[i],
                        area_code = area_code,
                        sys_wait = sys_wait,
                        n_list_page_attempted = n_list_page,
                        n_list_page_success = n_list_page_suc,
                        n_list_page_failed = n_list_page_fai,
                        list_dl_time_min = list_dl_time,
                        n_listing_parsed = n_listing_parsed,
                        list_parse_time_min = list_parse_time_min,
                        n_listing_after_deduped = n_listing_after_dd,
                        sample_part = sample_part,
                        rand_seed = rand_seed,
                        n_detail_attempted = n_detail,
                        n_detail_success = n_detail_suc,
                        n_detail_fail = n_detail_fai,
                        detail_dl_time_min = detail_dl_time,
                        overwrite = overwrite,
                        full_process = full_process,
                        n_detail_parsed = n_detail_parsed,
                        detail_parse_time_min = detail_parse_time
      )
      
    } else {
      message("\n --!! Skip Details Page Section !!--")
      
      end_time <- Sys.time()
      tot_time <- as.numeric(end_time - start_time_list, units = "secs")/60
      rand_seed <- if(is.null(seed)){(NA_integer_)}else{seed}
      # Make the log
      log_df <- add_row(log_df,
                        job_id = job_id,
                        date = start_time_date,
                        start_time = start_time_list,
                        end_time = end_time,
                        total_time_taken = tot_time,
                        area_name = area_name[i],
                        area_code = area_code,
                        sys_wait = sys_wait,
                        n_list_page_attempted = n_list_page,
                        n_list_page_success = n_list_page_suc,
                        n_list_page_failed = n_list_page_fai,
                        list_dl_time_min = list_dl_time,
                        n_listing_parsed = n_listing_parsed,
                        list_parse_time_min = list_parse_time_min,
                        n_listing_after_deduped = n_listing_after_dd,
                        sample_part = sample_part,
                        rand_seed = rand_seed,
                        n_detail_attempted = NA_integer_,
                        n_detail_success = NA_integer_,
                        n_detail_fail = NA_integer_,
                        detail_dl_time_min = NA_real_,
                        overwrite = overwrite,
                        full_process = FALSE,
                        n_detail_parsed = NA_integer_,
                        detail_parse_time_min = NA_real_
      )
    }

    
  } # for loop
  
  # Create or update the master log
  master_log_path <- "extraction_job_master_log.csv"
  
  if (file.exists(master_log_path)) {
    existing_log <- read_csv(master_log_path, show_col_types = FALSE,
                             col_types = "ccTTnccniiinininiiiinllin") %>%
      mutate(
        start_time = with_tz(start_time, tzone = "Asia/Tokyo"),
        end_time = with_tz(end_time, tzone = "Asia/Tokyo")
      )
    combined_log <- bind_rows(existing_log, log_df) %>% arrange(desc(start_time))
  } else {
    combined_log <- log_df 
  }
  
  # Save updated master log
  write_csv(combined_log, file = master_log_path)
  
  message(paste0("---- Log file updated: ", master_log_path, " ----"))
  end_time <- Sys.time()
  job_time_elapsed <- as.numeric(end_time - start_time, units = "secs")/60
  
  message(paste0("\n ----- Job completed; total elapsed time: ", job_time_elapsed, " minute(s). -----"))
  
  return(log_df)
  
} # function
