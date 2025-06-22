# Get the max page count from SUUMO's list page
get_max_page <- function(html) {
  html %>%
    html_elements("div.pagination_set-nav ol li a") %>%
    html_text2() %>%
    str_extract("\\d+") %>%
    as.integer() %>%
    suppressWarnings() %>%
    na.omit() %>%
    max()
}