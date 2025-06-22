# Takes in the parsed raw values and clean it up

# Load the needed function
source("helper_scripts/clean_station_group.R")
source("helper_scripts/parse_station_string.R")

# Main function body
clean_list_df <- function(raw_df) {
  df_clean <- raw_df %>%
    bind_cols(
      (
        pmap_dfr(list(raw_df$station_1, raw_df$station_2, raw_df$station_3),
                 clean_station_group)
      ) %>% setNames(c("station_1_cl", "station_2_cl", "station_3_cl"))
    ) %>%
    # Some house cleaning
    mutate(
      station_1 = station_1_cl,
      station_2 = station_2_cl,
      station_3 = station_3_cl
    ) 
  
  # Processes the station values
  df_station_split <- bind_cols(
    df_clean %>%
      pull(station_1_cl) %>%
      map_dfr(parse_station_string) %>%
      rename_with(~ paste0(., "_1")),
    
    df_clean %>%
      pull(station_2_cl) %>%
      map_dfr(parse_station_string) %>%
      rename_with(~ paste0(., "_2")),
    
    df_clean %>%
      pull(station_3_cl) %>%
      map_dfr(parse_station_string) %>%
      rename_with(~ paste0(., "_3"))
  )
  
  df_clean <- bind_cols(df_clean, df_station_split) %>%
    select(-c(station_1_cl, station_2_cl, station_3_cl))
  
  # Cleaning other values
  df_clean <- df_clean %>%
    mutate(
      # Convert full-width yen-style values like "9.3万円" → 93000
      rent = as.numeric(str_remove(rent, "万円")),
      deposit = ifelse(deposit == "-", 0, as.numeric(str_remove(deposit, "万円")) * 10000),
      reikin  = ifelse(reikin == "-", 0, as.numeric(str_remove(reikin, "万円")) * 10000),
      
      # Condo fee might be missing or in plain 円
      condo_fee = ifelse(condo_fee == "-", 0, as.numeric(str_remove(condo_fee, "円"))),
      
      # Area like "42.28m2" → numeric m2
      area = as.numeric(str_remove(area, "m2")),
      
      # Base floor: 
      floor = case_when(
        rent_type %in% c("賃貸一戸建て", "賃貸テラス・タウンハウス") ~ 1, # House & 
        TRUE ~ as.numeric(str_extract(floor, "\\d+"))
      ),
      
      
      # Normalize total floors in building
      floors_total = as.numeric(str_extract(floors_total, "\\d+")),
      
      # Built year like "築7年" or "築24年"  years old (as numeric)
      building_age = str_trim(building_age),
      building_age = if_else(str_detect(building_age, "新築"), 0, parse_number(building_age)),
      
      # Extract listing ID from the url
      listing_id = str_extract(detail_url, "(?<=\\?bc=)\\d{12}")
      
    ) %>%
    select(-c(station_1, station_2, station_3))
  
  message(paste("Cleaned", nrow(raw_df), "listings."))
  
  
  return(df_clean)
    
}

