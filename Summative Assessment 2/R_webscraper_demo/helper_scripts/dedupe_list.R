# Take in cleaned df and de-duplicate the listing
dedupe_list <- function(cleaned_df) {
  og_nrow <- nrow(cleaned_df)
  
  df_deduped <- cleaned_df %>%
    group_by(building_name, address, building_age, floor, layout) %>%
    arrange(rent, area, desc(!is.na(detail_url))) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(address, building_name)
  
  dd_nrow <- nrow(df_deduped)
  
  message(paste("Original number of rows:", og_nrow))
  message(paste("Deduped number of rows:", dd_nrow))
  message(paste("Rows removed:", og_nrow - dd_nrow))
  message(paste0("Percentage reduction: ", 
                 round((1-(dd_nrow/og_nrow))*100, digits = 2),
                 "%" ))
  
  return(df_deduped)
}