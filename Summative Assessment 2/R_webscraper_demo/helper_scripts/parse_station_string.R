# Take the raw station value (Ex: "ＪＲ南武線/稲城長沼駅 歩10分")
# Separate out the train line, train station, and walk time, then return in tibble
parse_station_string <- function(x) {
  # Return NA if empty
  if (is.na(x) || x == "") return(tibble(train_line = NA, train_station = NA, walk_time = NA))
  
  tibble(
    train_line = str_extract(x, "^.+(?=/)"),
    train_station = str_extract(x, "(?<=/).+?(?=駅)") %>% str_remove("駅"),
    walk_time = str_extract(x, "歩\\d+分") %>% str_remove_all("[^\\d]") %>% as.integer()
  )
}
