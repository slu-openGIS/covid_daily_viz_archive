# download and store IL, KS, and OK county geometry

# dependencies
library(dplyr)
library(sf)
library(tigris)

# create full county sf object
## MO geometry with KC as county equivalent
mo_counties <- st_read("data/source/mo_county_plus/mo_county_plus.shp") %>%
  select(GEOID) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  st_transform(crs = 4326)

## download geometry for IL and KS
il_ks_ok_counties <- counties(state = c(17, 20, 40), cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 4326)

## combine
counties_sf <- rbind(mo_counties, il_ks_ok_counties)
counties_sf <- arrange(counties_sf, GEOID)

# write data
st_write(counties_sf, "data/source/mo_county_xl.geojson", delete_dsn = TRUE)
