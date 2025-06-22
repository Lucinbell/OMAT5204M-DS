# Takes in a html object from SUUMO's "details" page and parse for info we want
parse_detail_page <- function(html_path) {
  page <- read_html(html_path)
  
  main <- tibble(
    # ID
    suumo_listing_id = page %>% html_element(xpath = "//th[contains(text(), 'SUUMO')]/following-sibling::td[1]") %>% html_text2(),
    
    building_name = page %>% html_element("#wrapper > div.section_h1 > div.section_h1-header > h1") %>%  html_text(),
    room_facing = page %>% html_element("tr:nth-child(5) > td:nth-child(2)") %>% html_text2(),
    building_type = page %>% html_element("tr:nth-child(5) > td:nth-child(4)") %>% html_text2(),
    
    #//*[@id="contents"]/div[4]/table/tbody/tr[2]/td[1]
    layout_detail = page %>% html_element(xpath = "//th[contains(text(), '間取り詳細')]/following-sibling::td[1]") %>% html_text2(),
    listing_area = page %>% html_element("tr:nth-child(3) > td:nth-child(4)") %>% html_text2(),
    structure = page %>% html_element("tr:nth-child(1) > td:nth-child(4)") %>% html_text2(),
    
    #contents > div:nth-child(6) > table > tbody > tr:nth-child(2) > td:nth-child(2)
    floor_total_floor = page %>% html_element(xpath = "//th[contains(text(), '階建')]/following-sibling::td[1]") %>% html_text2(),
    build_year_month = page %>% html_element("tr:nth-child(2) > td:nth-child(4)") %>% html_text2(),
    
    #contents > div:nth-child(6) > table > tbody > tr:nth-child(3) > td:nth-child(2)
    energy_efficiency_feature = page %>% html_element(xpath = "//th[contains(text(), 'エネルギー')]/following-sibling::td[1]") %>% html_text2(),
    
    #contents > div:nth-child(6) > table > tbody > tr:nth-child(3) > th.data_02
    insulation_feature = page %>% html_element(xpath = "//th[contains(text(), '断熱性能')]/following-sibling::td[1]") %>% html_text2(),
    
    #contents > div:nth-child(6) > table > tbody > tr:nth-child(5) > td:nth-child(4)
    parking_lot = page %>% html_element(xpath = "//th[contains(text(), '駐車場')]/following-sibling::td[1]") %>% html_text2(),
    total_unit_in_building = page %>% html_element("tr:nth-child(8) > td:nth-child(4)") %>% html_text2(),
    last_update = page %>% html_element("tr:nth-child(9) > td:nth-child(2)") %>% html_text2(),
    amenities = page %>% html_elements("#bkdt-option > div > ul > li") %>% html_text2() %>% paste(collapse = "; "),
    #//*[@id="contents"]/div[5]/div[1]/span[1]
    #//*[@id="contents"]/div[6]/div[1]/span[1]
    #//*[@id="contents"]/div[7]/div[1]/span[1]
    agency = page %>% html_element(xpath = "//h2[span[contains(text(), 'この物件を取り扱う店舗')]]/
                          parent::div/
                          descendant::span[contains(@class, 'itemcassette-header-ttl')]") %>% html_text2() %||% NA_character_,
    contract_type = page %>% html_element(xpath = "//th[contains(text(), '契約期間')]/following-sibling::td[1]") %>%html_text2()
    
  )
  
  return(
    main %>%
      mutate(
        sep_bath_toilet = if_else(str_detect(amenities, "バストイレ別"), TRUE, FALSE),
        washlet = if_else(str_detect(amenities, "温水洗浄便座"), TRUE, FALSE),
        reheat_bath = if_else(str_detect(amenities, "追焚機能浴室"), TRUE, FALSE),
        dryer_in_bathroom = if_else(str_detect(amenities, "浴室乾燥機"), TRUE, FALSE),
        sep_bath_sink = if_else(str_detect(amenities, "洗面所独立"), TRUE, FALSE),
        aircon = if_else(str_detect(amenities, "エアコン"), TRUE, FALSE),
        indoor_laundry_slot = if_else(str_detect(amenities, "室内洗濯置"), TRUE, FALSE),
        heated_floor = if_else(str_detect(amenities, "床暖房"), TRUE, FALSE),
        pet_ok = if_else(str_detect(amenities, "ペット相談"), TRUE, FALSE),
        free_internet = if_else(str_detect(amenities, "ネット使用料不要"), TRUE, FALSE),
        furniture_set = if_else(str_detect(amenities, "家具付"), TRUE, FALSE),
        appliance_set = if_else(str_detect(amenities, "家電付"), TRUE, FALSE),
        has_balcony = if_else(str_detect(amenities, "バルコニー"), TRUE, FALSE),
        system_kitchen = if_else(str_detect(amenities, "システムキッチン"), TRUE, FALSE),
        open_kitchen = if_else(str_detect(amenities, "対面式キッチン"), TRUE, FALSE),
        IH_cooktop = if_else(str_detect(amenities, "IHクッキングヒーター"), TRUE, FALSE)
      )
  )
}

