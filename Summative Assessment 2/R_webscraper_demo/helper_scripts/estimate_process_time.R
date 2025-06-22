# Take
estimate_process_time <- function(select_area,
                                  updated_area_info = df_cities_info,
                                  sample_parts = c(0.25, 0.5, 0.75, 1)){
  
  df_process_time <- read_csv("extraction_job_master_log_archived.csv", show_col_types = FALSE) %>%
    mutate(start_time = with_tz(start_time, tzone = "Asia/Tokyo"),
           end_time = with_tz(end_time, tzone = "Asia/Tokyo")) %>%  
    filter(job_id %in% c("812394a8", "b771f7c0")) %>%
    mutate(area_code = as.character(area_code)) %>%
    mutate(
      avg_list_dl_time_sec = list_dl_time_min*60/n_list_page_attempted,
      n_listing_per_list_page = n_listing_parsed / n_list_page_attempted,
      total_list_parsing_time_min = total_time_taken - list_dl_time_min,
      avg_list_parsing_time_per_listing_sec = (total_list_parsing_time_min * 60)/n_listing_parsed,
      perc_dedupe_reduction = 1 - (n_listing_after_deduped / n_listing_parsed)
    ) %>%
    select(area_name, area_code,
           avg_list_dl_time_sec, n_listing_per_list_page,
           avg_list_parsing_time_per_listing_sec, perc_dedupe_reduction)
  
  df_temp <- read_csv("extraction_job_master_log_archived.csv", show_col_types = FALSE) %>%
    mutate(start_time = with_tz(start_time, tzone = "Asia/Tokyo"),
           end_time = with_tz(end_time, tzone = "Asia/Tokyo")) %>%
    select(job_id, date, area_name, area_code, n_detail_attempted:detail_parse_time_min) %>%
    mutate(
      avg_parse_time_per_detail_sec = (detail_parse_time_min * 60) / n_detail_parsed
    )
  
  avg_dl_time_per_detail_page <- (sum(df_temp$detail_dl_time_min * 60, na.rm = TRUE)) / sum(df_temp$n_detail_attempted)
  avg_parse_time_per_detail_page <-(sum(df_temp$detail_parse_time_min, na.rm = TRUE)*60)/sum(df_temp$n_detail_parsed, na.rm = TRUE)
  
  df_estimate_time <- updated_area_info %>% select(names_en, area_code, max_page) %>%
    mutate(area_code = as.character(area_code)) %>%
    left_join(df_process_time, by = join_by(names_en == area_name, area_code)) %>%
    mutate(
      est_list_page_dl_time_min = (avg_list_dl_time_sec/60) * max_page,
      est_total_listings = as.integer(n_listing_per_list_page * max_page),
      est_list_parse_time_min = (avg_list_parsing_time_per_listing_sec/60) * est_total_listings,
      est_n_deduped_listings = as.integer(est_total_listings * (1-perc_dedupe_reduction)),
      est_detail_page_dl_time_min = (avg_dl_time_per_detail_page/60) * est_n_deduped_listings,
      est_detail_parse_time_min = (avg_parse_time_per_detail_page/60) * est_n_deduped_listings
    )
  
  output <- df_estimate_time %>% filter(names_en %in% select_area) %>%
    select(names_en, area_code, max_page,
           est_list_page_dl_time_min:est_detail_parse_time_min) %>%
    mutate(
      est_total_list_proc_time_min = est_list_page_dl_time_min + est_list_parse_time_min,
      est_total_detail_proc_time_min = est_detail_page_dl_time_min + est_detail_parse_time_min,
      est_total_time_min = est_total_list_proc_time_min + est_total_detail_proc_time_min)
  
  total_time <- sum(output$est_total_time_min)
  total_page <- sum(output$max_page)
  
  message(paste0("Number of areas selected: ", length(select_area)))
  message(paste0("Total Pages: ", total_page))
  message(paste0("Total Estimated Time (min): ", total_time))
  
  sample_parts_time <- sum(output$est_total_list_proc_time_min) + 
    (sample_parts * (sum(output$est_detail_page_dl_time_min) + sum(output$est_detail_parse_time_min)))
  
  names(sample_parts_time) <- sample_parts
  
  output_list <- list(
    detail_table = output,
    sample_parts_time = sample_parts_time
  )
  
  return(output_list)
}