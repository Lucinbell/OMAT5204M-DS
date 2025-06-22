# More optimized version of version 1
parse_list_page2 <- function(html_path) {
  html <- read_html(html_path)
  property_nodes <- html %>% html_elements("#js-bukkenList > ul > li")
  
  map_dfr(property_nodes, function(node) {
    bldg_name   <- node %>% html_element(".cassetteitem_content-title") %>% html_text2()
    rent_type   <- node %>% html_element(".cassetteitem_content-label") %>% html_text2()
    address     <- node %>% html_element(".cassetteitem_detail-col1") %>% html_text2()
    building_age  <- node %>% html_element(".cassetteitem_detail-col3 div:nth-child(1)") %>% html_text2()
    floors_total<- node %>% html_element(".cassetteitem_detail-col3 div:nth-child(2)") %>% html_text2()
    
    station_nodes <- node %>% html_elements(".cassetteitem_detail-col2 div")
    station_vals <- station_nodes %>% html_text2()
    station_vals <- c(station_vals, rep(NA, 3 - length(station_vals)))  # pad if missing
    
    room_table <- node %>% html_element("table")
    room_rows  <- room_table %>% html_elements("tr") %>% .[-1]  # skip header
    
    map_dfr(room_rows, function(row) {
      cells <- row %>% html_elements("td")
      
      detail_url <- cells[9] %>%
        html_element("a") %>%
        html_attr("href") %>%
        paste0("https://suumo.jp", .)
      
      rent_info <- cells[4] %>% html_elements("li span")
      rent      <- rent_info[1] %>% html_text2()
      condo_fee <- rent_info[3] %>% html_text2()
      
      deposit_info <- cells[5] %>% html_elements("li span")
      deposit <- deposit_info[1] %>% html_text2()
      reikin  <- deposit_info[2] %>% html_text2()
      
      layout_info <- cells[6] %>% html_elements("li span")
      layout <- layout_info[1] %>% html_text2()
      area   <- layout_info[2] %>% html_text2()
      
      tibble(
        building_name = bldg_name,
        rent_type,
        address,
        building_age,
        floors_total,
        station_1 = station_vals[1],
        station_2 = station_vals[2],
        station_3 = station_vals[3],
        floor = cells[3] %>% html_text2(),
        rent,
        condo_fee,
        deposit,
        reikin,
        layout,
        area,
        detail_url
      )
    })
  })
}
