# create sf objects for mapping

# load full county sf object
counties_sf <- st_read("data/source/mo_county_xl.geojson", crs = 4326, stringsAsFactors = FALSE)

# subset detailed data
county_data %>% 
  filter(report_date == date) %>%
  select(-report_date) -> county_sub

# combine with geometry
counties_sf <- left_join(counties_sf, county_sub, by = c("GEOID" = "geoid"))

# create vector of needed counties
mo_counties <- filter(counties_sf, state == "Missouri")
mo_counties <- as.vector(mo_counties$GEOID)

# create vector of mo counties and adjacent counties in metros
mo_counties_xl <- c(metro_counties$cape_girardeau_il, metro_counties$joplin_ok, metro_counties$kansas_city_ks, 
                    metro_counties$st_joseph_ks, metro_counties$st_louis_il, mo_counties)

# subset
mo_sf <- filter(counties_sf, GEOID %in% mo_counties)
mo_xl_sf <- filter(counties_sf, GEOID %in% mo_counties_xl)
stl_sf <- filter(counties_sf, GEOID %in% metro_counties$st_louis)
kc_sf <- filter(counties_sf, GEOID %in% metro_counties$kansas_city)

mo_detail <- filter(county_data, geoid %in% mo_counties)
stl_detail <- filter(county_data, geoid %in% metro_counties$st_louis)
kc_detail <- filter(county_data, geoid %in% metro_counties$kansas_city)

# add health care data
icu <- read_csv("data/source/kaiser_health_news_icu_bed_counts.csv") %>%
  mutate(GEOID = as.character(GEOID))
mo_xl_sf <- left_join(mo_xl_sf, icu, by = "GEOID")
mo_xl_sf <- left_join(mo_xl_sf, county_pop, by = "GEOID")
mo_xl_sf <- mutate(mo_xl_sf, icu_rate = icu_beds/total_pop*1000)
mo_xl_sf <- select(mo_xl_sf, GEOID, county, state, total_pop, hospitals, 
                   icu_beds, icu_rate, everything())

# clean-up
rm(mo_counties, county_sub, counties_sf, mo_counties_xl, icu,
   county_pop)

# write data
st_write(mo_sf, "data/county/daily_snapshot_mo.geojson", delete_dsn = TRUE)
st_write(mo_xl_sf, "data/county/daily_snapshot_mo_xl.geojson", delete_dsn = TRUE)
st_write(stl_sf, "data/metro/daily_snapshot_stl.geojson", delete_dsn = TRUE)
st_write(kc_sf, "data/metro/daily_snapshot_kc.geojson", delete_dsn = TRUE)
# write_csv(stl_detail, "data/metro/county_stl.csv")
# write_csv(kc_detail, "data/metro/county_kc.csv")

# clean-up
rm(mo_sf, mo_xl_sf, stl_sf, kc_sf, mo_detail, stl_detail, kc_detail, state_data,
   metro_counties, county_data)
