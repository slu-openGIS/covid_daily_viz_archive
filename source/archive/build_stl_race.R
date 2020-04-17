# create data for rates by race in St Louis City and County

# dependencies
library(dplyr)
library(readr)
library(tidycensus)

# create data
race <- get_acs(year = 2018, geography = "county", table = "B02001") %>%
  filter(GEOID %in% c("29189","29510")) %>%
  filter(variable %in% c("B02001_002", "B02001_003", "B02001_005", "B02001_008")) %>%
  select(GEOID, variable, estimate) %>%
  rename(
    geoid = GEOID,
    value = variable,
    pop = estimate
  ) %>%
  mutate(value = case_when(
    value == "B02001_002" ~ "White",
    value == "B02001_003" ~ "Black",
    value == "B02001_005" ~ "Asian",
    value == "B02001_008" ~ "Two or More"
  ))

# write data
write_csv(race, "data/source/stl_race.csv")
