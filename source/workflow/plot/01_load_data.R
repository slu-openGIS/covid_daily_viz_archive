

  


county_data <- read_csv("data/county/county_full.csv")

kc_detail <- read_csv("data/metro/county_kc.csv")
kc_sf <- st_read("data/metro/daily_snapshot_kc.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003)

covid_race <-  read_csv("data/individual/stl_race_rates.csv")

