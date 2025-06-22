Column Info

## Meta Data
- `listing_id`: unique listing ID on the listing site
- `contract_type`: type of rental contract
- `agency`: listing real estate agency
- `last_update`: Last date when info got updated on the page
- `detail_url`: URL to the listing details page


## Listing Info
- `parsed_area`: The ward in Tokyo"Minato-ku", "Shinjuku-ku", "Shibuya-ku"
- `building_name`: the name of the building
- `address`: General address, cho-me of JP address system "レジディアタワー麻布十番", "オーパスホームズ三田"
- `building_type`: "マンション", "アパート"
- `structure`: primary material of the building. Exampl: "木造" = wood, "鉄筋コン" = steel reinforced concrete
- `listing_floor`: the floor where the listing is located
- `building_total_floor`: the total number of floors that the building has
- `parking_type`: parking availability ("on-premise", "nearby", "not-listed")
- `parking_distance_m`: distance from the nearest parking (not available if `parking_type` = NA)
- `parking_fee`: monthly parking fee (not available if `parking_type` = NA)
- `room_facing`: direction of the main facade, in 8 directions ("南" = south, "南東" = southeast, etc.)
- `layout`: string of layout codes, e.g "1DK" = "one bedroom with dining and kitchen"; "ワンルーム" = studio apartment
- `layout_detail`: layout with each room's floor size; probably will not use in model
- `rent`: monthly rent in 10,000 JPY
- `condo_fee`: management fee of the apartment/condo
- `deposit`: safety deposit
- `reikin`: key money, sort of a "gift" money to landlord in Japan
- `listing_area`: floor area of the listing
- `train_line_X`: train line of the Xth nearest train station
- `train_station_X`: Xth nearest train station
- `walk_time_X`: walking time to the Xth nearest train station
- `building_age`: age of the building
- `build_year`: year the building was completed
### Amenities - all boolean
- `sep_bath_toilet`: separate bathroom and toilet
- `washlet`: washlet available or not
- `reheat_bath`: bath reheating function
- `dryer_in_bathroom`: built-in laundry dryer in bathroom
- `sep_bath_sink`: sink (vanity) separate from bathroom
- `aircon`: air conditiner installed
- `indoor_laundry_slot`: slot for placing laundry machine
- `heated_floor`: availability of floor heater, usually select area in living room and/or bedroom
- `pet_ok`: pet allow or not
- `free_internet`: complimentary internet/wi-fi
- `furniture_set`: comes with furniture set
- `appliance_set`: comes with appliances
- `has_balcony`: has balcony or not
- `system_kitchen`: built-in gas stoves and other kitchen component
- `open_kitchen`: breakfast-bar style kitchen
- `IH_cooktop`: IH heater for stove


## Calculated column
- `rent_per_sqm`: rent per square meter
- `room_count`: number of rooms/bedrooms
- `w_s`: with storage (usually walk-in closet)
- `w_l`: with living room
- `w_d`: with dining room
- `w_k`: with kitchen
- `total_partitions`: total number of partitions/room
- `area_per_partition`: average area per partition
- `n_station_sub_10min`: number of train stations within 10 minute walk