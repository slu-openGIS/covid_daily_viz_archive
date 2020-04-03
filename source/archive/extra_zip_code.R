
# geoprocess Franklin County Zips
counties <- counties(state = 29, year = 2018, class = "sf") %>%
  filter(COUNTYFP %in% c("071")) %>%
  st_transform(crs = 26915)

franklin <- st_intersection(zip, counties) %>%
  st_collection_extract(., "POLYGON") %>%
  rename(zip = GEOID10) %>%
  select(zip) %>%
  group_by(zip) %>%
  summarise()

st_write(franklin, "data/source/stl_zips/franklin_county_zip/franklin_county_zip.shp", delete_dsn = TRUE)
st_write(franklin, "data/source/stl_zips/franklin_county_zip/franklin_county_zip.geojson", delete_dsn = TRUE)
