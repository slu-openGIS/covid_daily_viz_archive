# build zip code data set

date <- as.Date("2020-04-02")

# load spatial data
stl_city_zip_sf <- st_read("data/source/stl_zips/stl_city_zip/stl_city_zip.geojson", 
                           crs = 4326, stringsAsFactors = FALSE)

# define days to load
city_dates <- seq(as.Date("2020-04-01"), date, by="days")

# load zip data
city_dates %>%
  unlist() %>%
  map_df(~ wrangle_zip(date = .x, county = 510)) %>%
  mutate(zip = as.character(zip)) -> stl_city_zip

# subset detailed data
stl_city_zip_sub <- filter(stl_city_zip, report_date == date)

# combine with geometry
left_join(stl_city_zip_sf, stl_city_zip_sub, by = "zip") %>%
  select(report_date, zip, geoid, county, state, last_update, 
         pvty_pct, wht_pct, blk_pct, confirmed, confirmed_rate) -> stl_city_zip_sf

# write data
write_csv(stl_city_zip, "data/zip/zip_stl_city.csv")
st_write(stl_city_zip_sf, "data/zip/daily_snapshop_stl_city.geojson",
         delete_dsn = TRUE)

# clean-up
rm(stl_city_zip, stl_city_zip_sub, city_dates, wrangle_zip)
