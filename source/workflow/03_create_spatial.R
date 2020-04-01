# create sf objects for mapping

# create full county sf object
## MO geometry with KC as county equivalent
mo_counties <- st_read("data/source/mo_county_plus/mo_county_plus.shp") %>%
  select(GEOID) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  st_transform(crs = 102003)

## download geometry for IL and KS
il_ks_counties <- counties(state = c(17, 20), cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 102003)

## combine
counties_sf <- rbind(mo_counties, il_ks_counties)
counties_sf <- arrange(counties_sf, GEOID)

## clean-up
rm(mo_counties, il_ks_counties)

# subset detailed data
county_data %>% 
  filter(report_date == date) %>%
  select(-report_date, -last_update) -> county_sub

# combine with geometry
counties_sf <- left_join(counties_sf, county_sub, by = c("GEOID" = "geoid"))

## define metro St. Louis counties
stl_metro_counties <- c("17005", "17013", "17027", "17083", "17117", 
                    "17119", "17133", "17163", "29071", "29099", 
                    "29113", "29183", "29189", "29219", "29510")

il_metro_counties <- c("17005", "17013", "17027", "17083", "17117", 
                       "17119", "17133", "17163")

kc_metro_counties <- c("20091", "20103", "20107", "20121", "20209",
                       "29013", "29025", "29037", "29047", "29049", 
                       "29095", "29107", "29165", "29177", "29511")

ks_metro_counties <- c("20091", "20103", "20107", "20121", "20209")

mo_counties <- filter(counties_sf, state == "Missouri")
mo_counties <- as.vector(mo_counties$GEOID)

mo_counties_xl <- c(il_metro_counties, ks_metro_counties, mo_counties)

# subset
mo_sf <- filter(counties_sf, GEOID %in% mo_counties)
mo_xl_sf <- filter(counties_sf, GEOID %in% mo_counties_xl)
stl_sf <- filter(counties_sf, GEOID %in% stl_metro_counties)
kc_sf <- filter(counties_sf, GEOID %in% kc_metro_counties)

mo_detail <- filter(county_data, geoid %in% mo_counties)
stl_detail <- filter(county_data, geoid %in% stl_metro_counties)
kc_detail <- filter(county_data, geoid %in% kc_metro_counties)

# clean-up
rm(stl_metro_counties, il_metro_counties, kc_metro_counties, ks_metro_counties,
   mo_counties, county_sub, counties_sf, county_data, mo_counties_xl)

# write data
st_write(mo_sf, "data/county/daily_snapshot_mo.geojson", delete_dsn = TRUE)
st_write(mo_xl_sf, "data/county/daily_snapshot_mo_xl.geojson", delete_dsn = TRUE)
st_write(stl_sf, "data/metro/daily_snapshot_stl.geojson", delete_dsn = TRUE)
st_write(kc_sf, "data/metro/daily_snapshot_kc.geojson", delete_dsn = TRUE)
write_csv(mo_detail, "data/county/county_mo.csv")
write_csv(stl_detail, "data/metro/county_stl.csv")
write_csv(kc_detail, "data/metro/county_kc.csv")
