require(furrr)

# Source the key function parse_detail_page
source("helper_scripts/parse_detail_page.R")

# Take a directory path as input, parse all html object in in and return
# a combined table
parse_detail_page_folder <- function(dir_cache = "cache_details_html", area_name = "inagi",
                                     parse_prop = 1, seed = NULL) {
  plan(multisession)
  start_time <- Sys.time()
  
  dir_area <- file.path(dir_cache, area_name)
  file_list <- list.files(dir_area)
  file_paths <- file.path(dir_area, file_list)
  
  set.seed(seed)
  file_paths_to_read <- sample(file_paths, size = length(file_paths)*parse_prop)
  
  output <- future_map_dfr(.x = file_paths_to_read, .f = ~ parse_detail_page(html_path = .x),
                           .progress = TRUE) 
  
  time_elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  message(paste0("\n\n Pages parsed: ", length(file_paths_to_read) ))
  message(paste0("Time Elapsed: ", round(time_elapsed/60, digits = 3), " minute(s)"))
  message(paste0("Process time per page: ", round(time_elapsed/length(file_paths_to_read), digits = 3), " seconds" ))
  message(paste0("Area: ", area_name))
  
  plan(sequential)
  return(output)
}
