# Takes the resulting details page, combine with the original list-page
join_results <- function(df_list, df_detail) {
  target_id <- df_detail$suumo_listing_id
  
  list_info <- df_list %>% filter(listing_id %in% target_id)
  
  combined <- list_info %>% 
    left_join(df_detail, by = join_by(listing_id == suumo_listing_id, building_name == building_name)) %>%
    select(
      parsed_area,
      listing_id, building_name, address, building_type, structure, 
      listing_floor:parking_fee, room_facing,
      layout, layout_detail, rent:reikin, listing_area,
      train_line_1:walk_time_3,
      building_age, build_year,
      sep_bath_toilet:IH_cooktop,
      contract_type, agency, last_update,
      detail_url
    )
  
  return(combined)
    
}