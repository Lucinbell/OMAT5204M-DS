# Reads a html file and parse the info we need
parse_list_page <- function(html_path) {
  html <- read_html(html_path)
  
  # Main parsing logic
  property_nodes <- html %>% html_elements("#js-bukkenList > ul > li")
  
  map_df(property_nodes, function(node) {
    bldg_name <- node %>% html_element(".cassetteitem_content-title") %>% html_text2()
    rent_type <- node %>% html_element(".cassetteitem_content-label") %>% html_text2()
    address <- node %>% html_element(".cassetteitem_detail-col1") %>% html_text2()
    building_age <- node %>% html_element(".cassetteitem_detail-col3 div:nth-child(1)") %>% html_text2()
    floors_total <- node %>% html_element(".cassetteitem_detail-col3 div:nth-child(2)") %>% html_text2()
    
    # Get the train station proximity info
    station_1 <- node %>% html_element(".cassetteitem_detail-col2 div:nth-child(1)") %>% html_text2()
    station_2 <- node %>% html_element(".cassetteitem_detail-col2 div:nth-child(2)") %>% html_text2()
    station_3 <- node %>% html_element(".cassetteitem_detail-col2 div:nth-child(3)") %>% html_text2()
    
    room_table <- node %>% html_element("table")
    room_rows <- room_table %>% html_elements("tr") %>% .[-1]
    
    map_df(room_rows, function(row) {
      floor <- row %>% html_element("td:nth-child(3)") %>% html_text2()
      rent <- row %>% html_element("td:nth-child(4) li:nth-child(1) span span") %>% html_text2()
      condo_fee <- row %>% html_element("td:nth-child(4) li:nth-child(2) span") %>% html_text2()
      deposit <- row %>% html_element("td:nth-child(5) li:nth-child(1) span") %>% html_text2()
      reikin <- row %>% html_element("td:nth-child(5) li:nth-child(2) span") %>% html_text2()
      layout <- row %>% html_element("td:nth-child(6) li:nth-child(1) span") %>% html_text2()
      area <- row %>% html_element("td:nth-child(6) li:nth-child(2) span") %>% html_text2()
      detail_url <- row %>%
        html_element("td.ui-text--midium.ui-text--bold a") %>%
        html_attr("href") %>%
        paste0("https://suumo.jp", .)
      
      tibble(
        building_name = bldg_name,
        rent_type,
        address = address,
        building_age = building_age,
        floors_total = floors_total,
        station_1 = station_1,
        station_2 = station_2,
        station_3 = station_3,
        floor = floor,
        rent = rent,
        condo_fee = condo_fee,
        deposit = deposit,
        reikin = reikin,
        layout = layout,
        area = area,
        detail_url = detail_url
      )
    })
  })
}