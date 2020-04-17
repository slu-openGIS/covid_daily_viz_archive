# create county master list

library(dplyr)
library(tigris)
library(readr)
library(sf)
library(tigris)
library(tidycensus)

# get county master list
## download
counties <- counties(state = c(17, 20, 29, 40), cb = FALSE, class = "sf")

## remove geometry
st_geometry(counties) <- NULL

## create kansas city row
kc <- tibble(
  geoid = "29511",
  state = "Missouri",
  county = "Kansas City"
)

## tidy
counties %>% 
  select(GEOID, STATEFP, NAME) %>%
  rename(
    geoid = GEOID,
    county = NAME,
    state = STATEFP
  ) %>%
  mutate(state = case_when(
    state == 17 ~ "Illinois",
    state == 20 ~ "Kansas",
    state == 29 ~ "Missouri",
    state == 40 ~ "Oklahoma"
  )) %>%
  bind_rows(., kc) %>%
  arrange(state, geoid) %>%
  mutate(county = ifelse(geoid == "29510", "St. Louis City", county)) -> counties

## write data
write_csv(counties, "data/source/counties.csv")

# get population data
## state populations
state_pop <- get_acs(geography = "state", state = c("Kansas", "Illinois", "Missouri", "Oklahoma"), year = 2018,
                     variables = "B01001_001") %>%
  select(NAME, estimate) %>%
  rename(total_pop = estimate)

## write
write_csv(state_pop, "data/source/state_pop.csv")

## county populations from ACS for IL and KS
county_pop <- get_acs(geography = "county", state = c("Kansas", "Illinois", "Oklahoma"), year = 2018,
                      variables = "B01001_001") %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)

## write
write_csv(county_pop, "data/source/county_pop.csv")
