require(httr)
require(rvest)

# Main function body

get_area_page_info <- function(data, sys_wait = 0.5) {
  today <- Sys.Date()
  
  area_list <- data %>% select(area_code, page_url) %>%
    mutate(
      # Initialize
      total_listings = NA,
      max_page = NA,
      parsed_date = today
    )
  
  for (i in 1:nrow(area_list)) {
    url <- area_list$page_url[i]
    
    res <- tryCatch(GET(url), error = function(e) {
      message(paste0("Request failed for ", data$names_en[i], " ", data$area_code[i]))
      next
    })
    
    # throttle
    Sys.sleep(abs(rnorm(1, sys_wait, 1)))
    
    # Extract
    html <- read_html(res)
    
    # Get total listings
    area_list$total_listings[i] <- html %>%
      html_elements(xpath = '//*[@id="js-leftColumnForm"]/div[3]/div[2]/div/text()[1]') %>% html_text2() %>% parse_number()
    
    # Get max page
    area_list$max_page[i] <- html %>%
      html_elements("div.pagination_set-nav ol li a") %>%
      html_text2() %>% str_extract("\\d+") %>%
      as.integer() %>%
      suppressWarnings() %>%
      na.omit() %>% max()
    
  }
  
  return(
    data %>% left_join(area_list, by = join_by(area_code, page_url))
  )
}
