#
clean_detail_df <- function(df_detail) {
  clean_df <- df_detail %>%
    # Removes whitespaces and escapes
    mutate(
      across(
        c(layout_detail, structure, parking_lot, total_unit_in_building, contract_type),
        ~ str_replace_all(., "[\r\n ]", "")
      )
    ) %>%
    # Formatting, remapping
    mutate(
      room_facing = if_else(room_facing == "-", "not-listed", room_facing),
      layout_detail = if_else(layout_detail == "-", "not-listed", layout_detail),
      listing_area = as.numeric(str_remove(listing_area, "m2")),
      listing_floor = as.integer(str_extract(floor_total_floor, "^-?\\d+")),
      building_total_floor = str_extract(floor_total_floor, "(?<=地上)\\d+(?=階建)") %>%
        ifelse(is.na(.), 
               str_extract(floor_total_floor, "(?<=/)[0-9]+(?=階建)"), 
               .) %>%
        as.integer(),
      
      # Handling Parking Lots
      parking_lot = if_else(parking_lot == "-", "not-listed", parking_lot),
      parking_type = case_when(
        parking_lot == "not-listed" ~ "not-listed",
        str_detect(parking_lot, "敷地内") ~ "on-premise",
        str_detect(parking_lot, "付無料") ~ "on-premise",
        str_detect(parking_lot, "近隣") ~ "nearby",
        .default = NA_character_
      ),
      parking_distance_m = case_when(
        parking_type == "not-listed" ~ NA_real_,
        parking_type == "on-premise" ~ 0,
        parking_type == "nearby" ~ as.numeric(str_extract(parking_lot, "\\d+(?=m)")),
        .default = NA_real_
      ),
      parking_fee = case_when(
        parking_type == "not-listed" ~ NA_real_,
        str_detect(parking_lot, "付無料") ~ 0,
        str_detect(parking_lot, "敷地内") ~ as.numeric(str_extract(parking_lot, "\\d+(?=円)")),
        str_detect(parking_lot, "近隣") ~ as.numeric(str_extract(parking_lot, "\\d+(?=円)")),
        .default = NA_real_
      ),
      
      total_unit_in_building = if_else(str_detect(total_unit_in_building, "-"), 
                                       NA_real_, as.numeric(str_remove(total_unit_in_building, "戸"))),
      build_year = as.integer(str_extract(build_year_month, "\\d+(?=年)")),
      build_month = as.integer(str_extract(build_year_month, "(?<=年)\\d+(?=月)")),
      contract_type = if_else(is.na(contract_type), "no-listed", contract_type)
    ) %>%
    select(
      -c(energy_efficiency_feature, insulation_feature, floor_total_floor, build_year_month, 
         parking_lot, amenities)
    )
  
  return(clean_df)
  
}