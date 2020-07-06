# create statewide geometry with Kansas City separated from encompassing counties

# tidyverse packages
library(dplyr)
library(readr)
library(tibble)

# spatial packages
library(areal)
library(nngeo)
library(sf)
library(tidycensus)
library(tigris)

# read in partial population data
partials <- read_csv("data/source/partial_populations.csv") %>%
  select(GEOID, COUNTY, partial_estimate) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  rename(NAME = COUNTY)

# get KC municipal boundary
places(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID == 2938000) %>%
  select(NAME) -> kc_geo

# get Joplin municipal boundary
places(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID == 2937592) %>%
  select(NAME) -> joplin_geo

# get county population data
get_acs(year = 2018, geography = "county", variables = "B01003_001", 
        state = 29)  %>%
  select(GEOID, NAME, estimate) %>%
  rename(total_pop = estimate) -> county_pop

# county geometry data
county_geo <- counties(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  select(GEOID, NAMELSAD) %>%
  rename(NAME = NAMELSAD)

# subset to KC counties
kc_county_geo <- filter(county_geo, GEOID %in% c("29037", "29047", "29095", "29165"))

# subset to Joplin counties
joplin_county_geo <- filter(county_geo, GEOID %in% c("29097", "29145"))

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

kc %>%
  mutate(NAME = ifelse(KC == TRUE, "Kansas City", NAME)) %>%
  group_by(NAME) %>%
  summarise(
    GEOID = first(GEOID)
  ) %>%
  mutate(GEOID = ifelse(NAME == "Kansas City", "29511", GEOID)) %>%
  select(GEOID, NAME) -> kc

rm(kc_0, kc_county_geo, kc_geo)

# intersect Joplin counties and city boundary
joplin <- st_intersection(joplin_county_geo, joplin_geo) %>%
  st_collection_extract(., "POLYGON") %>%
  rename(JOPLIN = NAME.1) %>%
  mutate(JOPLIN = TRUE)

joplin_0 <- st_difference(joplin_county_geo, joplin_geo) %>%
  # st_collection_extract(., "POLYGON") %>%
  rename(JOPLIN = NAME.1) %>%
  mutate(JOPLIN = FALSE)

joplin <- rbind(joplin, joplin_0)

joplin %>%
  mutate(NAME = ifelse(JOPLIN == TRUE, "Joplin", NAME)) %>%
  group_by(NAME) %>%
  summarise(
    GEOID = first(GEOID)
  ) %>%
  mutate(GEOID = ifelse(NAME == "Joplin", "29512", GEOID)) %>%
  select(GEOID, NAME) -> joplin

joplin <- st_remove_holes(joplin)
joplin <- rename(joplin, geometry = geom)

rm(joplin_0, joplin_county_geo, joplin_geo)

# combine new geometries
county_pop_new_sf <- rbind(joplin, kc)

rm(joplin, kc)

county_pop_new <- county_pop_new_sf
st_geometry(county_pop_new) <- NULL

# calculate revised populations
county_pop_new %>%
  filter(NAME %in% c("Joplin", "Kansas City") == FALSE) %>%
  left_join(., county_pop, by = "GEOID") %>%
  left_join(., partials, by = "GEOID") %>%
  select(GEOID, total_pop, partial_estimate) %>%
  mutate(county_estimate = total_pop-partial_estimate) %>%
  rename(city_estimate = partial_estimate) %>%
  select(-total_pop) %>%
  mutate(city = case_when(
    GEOID %in% c("29097", "29145") ~ "Joplin",
    TRUE ~ "Kansas City"
  )) -> county_pop_new

county_pop_new %>%
  group_by(city) %>%
  summarise(total_pop = sum(city_estimate)) %>%
  mutate(GEOID = case_when(
    city == "Kansas City" ~ "29511",
    city == "Joplin" ~ "29512"
  )) %>%
  rename(NAME = city) %>%
  select(GEOID, total_pop) -> city_pop_new

county_pop_new %>%
  select(GEOID, county_estimate) %>%
  rename(total_pop = county_estimate) -> county_pop_new

county_pop_new <- rbind(county_pop_new, city_pop_new)

county_pop_new_sf <- left_join(county_pop_new_sf, county_pop_new, by = "GEOID")
county_pop_new_sf <- select(county_pop_new_sf, GEOID, NAME, total_pop, geometry)

rm(city_pop_new, county_pop_new, partials)

# join county geometry and population
county_pop %>%
  select(-NAME) %>%
  left_join(county_geo, ., by = "GEOID") -> county

rm(county_geo, county_pop)

# pull Kansas City counties out
county <- filter(county, GEOID %in% c("29037", "29047", "29095", "29165", "29097", "29145") == FALSE)

# combine
county <- rbind(county, county_pop_new_sf)
county <- st_collection_extract(county, "POLYGON")
rm(county_pop_new_sf)

# write
st_write(county, "data/source/mo_county_plus/mo_county_plus.shp", delete_dsn = TRUE)
st_geometry(county) <- NULL
write_csv(county, "data/source/mo_county_plus/mo_county_plus.csv")

