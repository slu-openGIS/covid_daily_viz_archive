# create st. louis city zip code estimates

# dependencies
library(areal)
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

# get zcta data
total_pop <- get_acs(year = 2018, geography = "zcta", variables = "B01003_001") %>%
  select(GEOID, estimate) %>%
  filter(GEOID %in% stl_city$zip) %>%
  rename(total_pop = estimate)

poverty <- get_acs(year = 2018, geography = "zcta", table = "B17001", output = "wide") %>%
  select(GEOID, B17001_001E, B17001_002E) %>%
  filter(GEOID %in% stl_city$zip) 

race <- get_acs(year = 2018, geography = "zcta", table = "B02001", output = "wide") %>%
  select(GEOID, B02001_001E, B02001_002E, B02001_003E) %>%
  filter(GEOID %in% stl_city$zip) 

# combine zcta data with zcta geometry
zip %>%
  filter(GEOID10 %in% stl_city$zip) %>%
  select(GEOID10) %>%
  rename(GEOID = GEOID10) %>%
  st_transform(crs = 26915) %>%
  left_join(., total_pop, by = "GEOID") %>%
  left_join(., poverty, by = "GEOID") %>%
  left_join(., race, by = "GEOID") -> stl_city_demo

# interpolate
stl_city %>%
  aw_interpolate(tid = "zip", source = stl_city_demo, sid = "GEOID", weight = "total", output = "sf", 
                 extensive = c("total_pop", "B17001_001E", "B17001_002E",
                               "B02001_001E", "B02001_002E", "B02001_003E")) %>%
  mutate(total_pop = round(total_pop)) %>%
  mutate(pvty_pct = B17001_002E/B17001_001E*100) %>%
  mutate(wht_pct = B02001_002E/B02001_001E*100) %>%
  mutate(blk_pct = B02001_003E/B02001_001E*100) %>%
  select(zip, total_pop, pvty_pct, wht_pct, blk_pct) -> stl_city

st_write(stl_city, "data/source/stl_zips/stl_city_zip/stl_city_zip.shp", delete_dsn = TRUE)

stl_city %>%
  st_transform(crs = 4326) %>%
  st_write("data/source/stl_zips/stl_city_zip/stl_city_zip.geojson", delete_dsn = TRUE)

st_geometry(stl_city) <- NULL

stl_city %>%
  select(zip, total_pop) %>%
  write_csv("data/source/stl_zips/stl_city_zip/stl_city_zip.csv")
