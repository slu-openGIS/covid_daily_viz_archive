# build zip code data set

# load spatial data
stl_city_zip_sf <- st_read("data/source/stl_zips/stl_city_zip/stl_city_zip.geojson", 
                           crs = 4326, stringsAsFactors = FALSE)
city_county_zip_sf <- st_read("data/source/stl_zips/city_county_zip/city_county_zip.geojson", crs = 4326,
                              stringsAsFactors = FALSE)

# define days to load
city_dates_a <- seq(as.Date("2020-04-01"), as.Date("2020-05-18"), by="days")
city_dates_b <- seq(as.Date("2020-05-20"), date, by="days")
city_dates <- c(city_dates_a, city_dates_b)

# load zip data
city_dates %>%
  unlist() %>%
  map_df(~ wrangle_zip(date = .x, county = 510)) %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate
  ) %>%
  mutate(zip = as.character(zip)) %>% 
  mutate(
    cases = ifelse(is.na(cases) == TRUE, NaN, cases),
    case_rate = ifelse(is.na(case_rate) == TRUE, NaN, case_rate)
  ) -> stl_city_zip

# subset detailed data
stl_city_zip_sub <- filter(stl_city_zip, report_date == date)

# combine with geometry
left_join(stl_city_zip_sf, stl_city_zip_sub, by = "zip") %>%
  select(report_date, zip, geoid, county, state, 
         pvty_pct, wht_pct, blk_pct, cases, case_rate) -> stl_city_zip_sf

# write data
write_csv(stl_city_zip, "data/zip/zip_stl_city.csv")

# clean-up
rm(stl_city_zip, stl_city_zip_sub, city_dates, city_dates_a, city_dates_b)

# load spatial data
stl_county_zip_sf <- st_read("data/source/stl_zips/stl_county_zip/stl_county_zip.geojson", 
                           crs = 4326, stringsAsFactors = FALSE)

# define days to load
county_dates_a <- seq(as.Date("2020-04-06"), as.Date("2020-05-18"), by="days")
county_dates_b <- seq(as.Date("2020-05-20"), date, by="days")
county_dates <- c(county_dates_a, county_dates_b)

# load zip data
county_dates %>%
  unlist() %>%
  map_df(~ wrangle_zip(date = .x, county = 189)) %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate
  ) %>%
  mutate(zip = as.character(zip)) %>% 
  mutate(
    cases = ifelse(is.na(cases) == TRUE, NaN, cases),
    case_rate = ifelse(is.na(case_rate) == TRUE, NaN, case_rate)
  ) -> stl_county_zip

# subset detailed data
stl_county_zip_sub <- filter(stl_county_zip, report_date == date)

# combine with geometry
left_join(stl_county_zip_sf, stl_county_zip_sub, by = "zip") %>%
  select(report_date, zip, geoid, county, state, 
         pvty_pct, wht_pct, blk_pct, cases, case_rate) -> stl_county_zip_sf

# write data
write_csv(stl_county_zip, "data/zip/zip_stl_county.csv")

# clean-up
rm(stl_county_zip, stl_county_zip_sub, county_dates, county_dates_a, county_dates_b)

# load pop
pop_city <- read_csv("data/source/stl_zips/stl_city_zip/stl_city_zip.csv") %>%
  mutate(zip = as.character(zip))
pop_county <- read_csv("data/source/stl_zips/stl_county_zip/stl_county_zip.csv") %>%
  mutate(zip = as.character(zip))

# join pop and geometric data
stl_city_zip_sf <- left_join(stl_city_zip_sf, pop_city, by = "zip")
stl_county_zip_sf <- left_join(stl_county_zip_sf, pop_county, by = "zip")

# subset city less than 5
city_na <- filter(stl_city_zip_sf, zip %in% city_lt5 == TRUE) %>%
  select(zip, total_pop, pvty_pct, wht_pct, blk_pct, cases)
county_na <- filter(stl_county_zip_sf, zip %in% city_lt5 == TRUE) %>%
  select(zip, total_pop, pvty_pct, wht_pct, blk_pct, cases)

city_county_zip_sf <- filter(city_county_zip_sf, zip %in% city_lt5 == FALSE)

# isolate counts
stl_city_zip <- select(stl_city_zip_sf, zip, cases)
st_geometry(stl_city_zip) <- NULL
stl_county_zip <- select(stl_county_zip_sf, zip, cases)
st_geometry(stl_county_zip) <- NULL

stl_zip <- bind_rows(stl_city_zip, stl_county_zip) %>%
  group_by(zip) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  mutate(cases = ifelse(cases == 0, NA, cases)) %>%
  filter(zip %in% city_lt5 == FALSE)

city_county_zip_sf <- left_join(city_county_zip_sf, stl_zip, by = "zip")
city_county_zip_sf <- rbind(city_county_zip_sf, city_na, county_na) %>%
  arrange(zip)

# calculate rate
city_county_zip_sf <- mutate(city_county_zip_sf, case_rate = cases/total_pop*1000)

# clean-up
rm(stl_city_zip, stl_county_zip, pop_city, pop_county,
   city_na, county_na, stl_zip, city_lt5, wrangle_zip)

# write data
st_write(stl_city_zip_sf, "data/zip/daily_snapshot_stl_city.geojson",
         delete_dsn = TRUE)
st_write(stl_county_zip_sf, "data/zip/daily_snapshot_stl_county.geojson",
         delete_dsn = TRUE)
st_write(city_county_zip_sf, "data/zip/daily_snapshot_city_county.geojson",
         delete_dsn = TRUE)

rm(city_county_zip_sf, stl_city_zip_sf, stl_county_zip_sf)
