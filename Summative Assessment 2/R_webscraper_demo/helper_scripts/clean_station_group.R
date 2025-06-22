clean_station_group <- function(st1, st2, st3) {
  stations <- c(st1, st2, st3)
  valid_stations <- stations[
    !is.na(stations) &
      stations != "" &
      !str_detect(stations, "バス")
  ]
  
  # Parse walking time: extract numeric part from "歩10分" etc.
  walk_times <- str_extract(valid_stations, "歩\\d+分") %>%
    str_remove_all("[^\\d]") %>%
    as.integer()
  
  # Sort stations by walking time (if available)
  sorted_stations <- valid_stations[order(walk_times)]
  
  
  length(sorted_stations) <- 3
  
  # Wrap in tibble with named columns
  tibble(
    station_1 = sorted_stations[1],
    station_2 = sorted_stations[2],
    station_3 = sorted_stations[3]
  )
}
