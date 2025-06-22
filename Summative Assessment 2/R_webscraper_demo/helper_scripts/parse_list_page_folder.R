require(furrr)

# Source the key function parse_list_page2
source("helper_scripts/parse_list_page2.R")

# take a directory path as input, parse all html objects in it and returned
# a combined table
parse_list_page_folder <- function(dir_cache = "cache_list_html", area_name = "inagi",
                                   parse_page = NULL) {
  plan(multisession)
  start_time <- Sys.time()
  
  dir_area <- file.path(dir_cache, area_name)
  file_list <- list.files(dir_area)
  file_paths <- file.path(dir_area, file_list)
  
  if (is.null(parse_page)) {
    page_to_parse <- 1:length(file_paths)
  } else {
    page_to_parse <- parse_page
  }
  
  output <- future_map_dfr(.x = file_paths[page_to_parse], .f = ~ parse_list_page2(html_path = .x),
                           .progress = TRUE) %>%
    mutate(
      parsed_area = area_name
    )
  
  time_elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  message(paste0("\n\n Pages parsed: ", length(page_to_parse) ))
  message(paste0("Time Elapsed: ", round(time_elapsed/60, digits = 2), " minute(s)"))
  message(paste0("Process time per page: ", round(time_elapsed/length(page_to_parse), digits = 2), " seconds" ))
  message(paste0("Area: ", area_name))
  
  plan(sequential)
    
  return(output)
}

