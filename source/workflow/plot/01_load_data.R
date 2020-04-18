
state_data <- read_csv("data/state/state_full.csv")
mo_sf <- st_read("data/county/daily_snapshot_mo.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003)

county_data <- read_csv("data/county/county_full.csv")

stl_detail <- read_csv("data/metro/county_stl.csv")
stl_sf <- st_read("data/metro/daily_snapshot_stl.geojson", crs = 4326,
                  stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003)
kc_detail <- read_csv("data/metro/county_kc.csv")
kc_sf <- st_read("data/metro/daily_snapshot_kc.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003)

city_county_zip_sf <- st_read("data/zip/daily_snapshot_city_county.geojson", crs = 4326,
                              stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

metro_data <- read_csv("data/metro_all/metro_full.csv")

covid_race <-  read_csv("data/individual/stl_race_rates.csv")

# create reference object
ref_county <- mo_sf
st_geometry(ref_county) <- NULL
