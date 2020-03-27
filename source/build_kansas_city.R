# create statewide geometry with Kansas City separated from encompassing counties

# tidyverse packages
library(dplyr)
library(tibble)

# spatial packages
library(areal)
library(sf)
library(tidycensus)
library(tigris)

# get tract data
get_acs(year = 2018, geography = "tract", variables = "B01003_001", 
        state = 29, county = c(37, 47, 95, 165), geometry = TRUE) %>%
  st_transform(crs = 26915) %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate) -> tract_pop

# get KC municipal boundary
places(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID == 2938000) %>%
  select(NAME) -> kc_geo

# get county population data
get_acs(year = 2018, geography = "county", variables = "B01003_001", 
        state = 29)  %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate) -> county_pop

# county geometry data
county_geo <- counties(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  select(GEOID, NAMELSAD) %>%
  rename(NAME = NAMELSAD)

# subset to KC counties
kc_county_geo <- filter(county_geo, GEOID %in% c("29037", "29047", "29095", "29165"))

# intersect KC counties and city boundary
kc <- st_intersection(kc_county_geo, kc_geo) %>%
  st_collection_extract(., "POLYGON") %>%
  rename(KC = NAME.1) %>%
  mutate(KC = TRUE)

kc_0 <- st_difference(kc_county_geo, kc_geo) %>%
  st_collection_extract(., "POLYGON") %>%
  rename(KC = NAME.1) %>%
  mutate(KC = FALSE)

kc <- rbind(kc, kc_0)

rm(kc_0)

# interpolate
kc %>%
  rowid_to_column(var = "id") %>%
  aw_interpolate(tid = "id", source = tract_pop, sid = "GEOID", weight = "sum", output = "sf", extensive = "total_pop") %>%
  mutate(total_pop = round(total_pop)) %>%
  select(-id) %>%
  mutate(NAME = ifelse(KC == TRUE, "Kansas City", NAME)) %>%
  group_by(NAME) %>%
  summarise(
    GEOID = first(GEOID),
    total_pop = sum(total_pop)
  ) %>%
  mutate(GEOID = ifelse(NAME == "Kansas City", "29511", GEOID)) %>%
  select(GEOID, NAME, total_pop) -> kc

rm(kc_geo, kc_county_geo, tract_pop)

# join county geometry and population
county <- left_join(county_geo, county_pop, by = "GEOID")

rm(county_geo, county_pop)

# pull Kansas City counties out
county <- filter(county, GEOID %in% c("29037", "29047", "29095", "29165") == FALSE)

# combine
county <- rbind(county, kc)
county <- st_collection_extract(county, "POLYGON")

# write
st_write(county, "data/mo_county_plus/mo_county_plus.shp")
