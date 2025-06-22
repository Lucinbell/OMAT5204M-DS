# take in the cleaned df, and returns a df containing list of 
# urls to process; option get full df or sample part of the df
create_url_list <- function(df_clean, sample_part = 1,
                            drop_bus = TRUE, seed = NULL) {
  # drop listing w/ no train stations nearby
  if(drop_bus) {
    df_clean <- df_clean %>% filter(!is.na(train_line_1))
  }
  
  # only the col we want
  url_df <- df_clean %>% select(parsed_area, listing_id, detail_url)
  
  # sampling
  col_ind <- 1:nrow(url_df)
  set.seed(seed)
  sample_ind <- sample(col_ind, size = nrow(url_df) * sample_part)
  
  # output
  output <- url_df[sample_ind,]
  
  message(paste0("Full list observations: ", nrow(df_clean)))
  message(paste0("Sampling ", sample_part*100, "% of observations"))
  message(paste0("Output list observations: ", nrow(output)))
  
  return(output)
  
}