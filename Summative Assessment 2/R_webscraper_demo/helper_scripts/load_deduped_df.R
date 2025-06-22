require(stringr)
require(purrr)
require(dplyr)
require(readr)
# Take in directory, output the downloaded data frame

load_deduped_df <- function(job_id, select_area_name = NULL,
                         main_dir = "data_deduped_list"){
  
  list_of_folders <- list.files(main_dir)
  target <- str_detect(list_of_folders, job_id)
  target_dir <- list_of_folders[target]
  folder_path <- file.path(main_dir, target_dir)
  
  extracted <- str_match(list.files(folder_path), "^deduped_([^_]+)_([0-9]{5})\\.csv$")
  
  # df for processing
  list_o_df <- data.frame(
    area_name = extracted[,2],
    area_code = extracted[,3],
    file_names = list.files(folder_path),
    stringsAsFactors = FALSE
  )
  
  
  if(is.null(select_area_name)){
    paths <- file.path(folder_path, list_o_df$file_names)
  } else if (length(select_area_name) > 0) {
    target_area_file <- list_o_df %>% 
      filter(area_name %in% select_area_name) %>% select(file_names) %>%
      pull()
    
    paths <- file.path(folder_path, target_area_file)
  }
  
  combined_df <- paths %>%
    map(~ read_csv(., show_col_types = FALSE,
                   col_types = cols(listing_id = col_character()))) %>% bind_rows()
  
  return(combined_df)
  
}