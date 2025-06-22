# Loading require library
require(rvest)
require(dplyr)
require(purrr)
require(stringr)
require(lubridate)


# Scraping info from the "list" page
# Crucially, extracting url to details page of listing

scrape_suumo_page <- function(page_num, area_code = "13225", day_old = 30, sys_wait = 1) {
  
  # Step 1: build url, check cache, read url/cache
  
  base_url <- "https://suumo.jp/jj/chintai/ichiran/FR301FC001/"
  url <- paste0(base_url,
                "?ar=030&bs=040&ta=13&sc=", area_code,
                "&cb=0.0&ct=9999999&et=9999999&cn=9999999",
                "&mb=0&mt=9999999&shkr1=03&shkr2=03&shkr3=03&shkr4=03",
                "&fw2=&srch_navi=1&page=", page_num)
  
  dir.create("cache", showWarnings = FALSE)
  cache_file <- sprintf("cache/suumo_page_%s_%d.html", area_code, page_num)
  
  use_cached <- FALSE
  if (file.exists(cache_file)) {
    modified_time <- file.info(cache_file)$mtime
    age_days <- as.numeric(difftime(Sys.time(), modified_time, units = "days"))
    if (age_days < day_old) {
      use_cached <- TRUE
    }
  }
  
  if (use_cached) {
    html <- read_html(cache_file)
  } else {
    html <- read_html(url)
    writeLines(as.character(html), cache_file)
    Sys.sleep(abs(rnorm(1, sys_wait, 1)))  # use abs to prevent negative wait times
  }
  
  # Parse logic continues here
  property_nodes <- html %>% html_elements("#js-bukkenList > ul > li")
  
  map_df(property_nodes, function(node) {
    bldg_name <- node %>% html_element(".cassetteitem_content-title") %>% html_text2()
    rent_type <- node %>% html_element(".cassetteitem_content-label") %>% html_text2()
    address <- node %>% html_element(".cassetteitem_detail-col1") %>% html_text2()
    built_year <- node %>% html_element(".cassetteitem_detail-col3 div:nth-child(1)") %>% html_text2()
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
        built_year = built_year,
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