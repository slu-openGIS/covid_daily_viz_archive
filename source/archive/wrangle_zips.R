# process zip code data

library(dplyr)
library(readr)
library(sf)

df <- read_csv("data/source/stl_daily_zips/stl_city_2020_04_01.csv")
pop <- read_csv("data/source/stl_zips/stl_city_zip/stl_city_zip.csv")
df <- mutate(df,
  report_date = as.Date("2020-04-01"),
  geoid = "29510",
  county = "St. Louis City",
  state = "Missouri",
  last_update = as.POSIXct("2020-04-01 17:00:00", tz = Sys.timezone())
)
df <- select(df, report_date, zip, geoid, county, state, last_update, count)
df <- rename(df, confirmed = count)
df <- left_join(df, pop, by = "zip")
df <- mutate(df, confirmed_rate = confirmed/total_pop*1000)
df <- select(df, -total_pop)

write_csv(df, "data/zip/zip_stl_city.csv")

zip_sf <- st_read("data/source/stl_zips/stl_city_zip/stl_city_zip.geojson", crs = 4326, stringsAsFactors = FALSE)

df %>%
  select(zip, county, state, confirmed, confirmed_rate) %>%
  mutate(zip = as.character(zip)) %>%
  left_join(zip_sf, ., by = "zip") %>%
  select(zip, county, state, pvty_pct, wht_pct, blk_pct, confirmed, confirmed_rate) -> zip_sf

st_write(zip_sf, "data/zip/zip_stl_city.geojson")
