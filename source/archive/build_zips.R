# download demographic data

# dependencies
library(dplyr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)

# download zips
zip <- zctas(year = 2018, class = "sf") %>%
  st_transform(crs = 26915)

# geoprocess City of St. Louis Zips
counties <- counties(state = 29, year = 2018, class = "sf") %>%
  filter(COUNTYFP %in% c("510")) %>%
  st_transform(crs = 26915)

stl_city <- st_intersection(zip, counties) %>%
  st_collection_extract(., "POLYGON") %>%
  rename(zip = GEOID10) %>%
  select(zip) %>%
  group_by(zip) %>%
  summarise() %>%
  filter(zip %in% c("63121", "63155") == FALSE)

st_write(stl_city, "data/source/stl_zips/stl_city_zip/stl_city_zip.shp", delete_dsn = TRUE)
st_write(stl_city, "data/source/stl_zips/stl_city_zip/stl_city_zip.geojson", delete_dsn = TRUE)

# geoprocess St. Louis County Zips
counties <- counties(state = 29, year = 2018, class = "sf") %>%
  filter(COUNTYFP %in% c("189")) %>%
  st_transform(crs = 26915)

stl_county <- st_intersection(zip, counties) %>%
  st_collection_extract(., "POLYGON") %>%
  rename(zip = GEOID10) %>%
  select(zip) %>%
  group_by(zip) %>%
  summarise() %>%
  filter(zip %in% c("63045") == FALSE)

st_write(stl_county, "data/source/stl_zips/stl_county_zip/stl_county_zip.shp", delete_dsn = TRUE)
st_write(stl_county, "data/source/stl_zips/stl_county_zip/stl_county_zip.geojson", delete_dsn = TRUE)

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

# geoprocess City+County Combined Zips
rbind(stl_city, stl_county) %>%
  group_by(zip) %>%
  summarise() -> city_county

st_write(city_county, "data/source/stl_zips/city_county_zip/city_county_zip.shp", delete_dsn = TRUE)
st_write(city_county, "data/source/stl_zips/city_county_zip/city_county_zip.geojson", delete_dsn = TRUE)

# geoprocess Franklin County Zips
rbind(stl_city, stl_county, franklin) %>%
  group_by(zip) %>%
  summarise() -> region

st_write(region, "data/source/stl_zips/region_zip/region_zip.shp", delete_dsn = TRUE)
st_write(region, "data/source/stl_zips/region_zip/region_zip.geojson", delete_dsn = TRUE)
