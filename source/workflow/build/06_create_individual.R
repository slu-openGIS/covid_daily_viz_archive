# create individual data

# load source data
race <- read_csv("data/source/stl_race.csv") %>%
  mutate(geoid = as.character(geoid))

# construct county object
county_race <- tibble(
  geoid = "29189",
  county = "St. Louis",
  value = c("White", "Black", "Asian", "Two or More"),
  count = county_race_counts
)

# construct city object
city_race <- tibble(
  geoid = "29510",
  county = "St. Louis City",
  value = c("White", "Black", "Asian", "Two or More"),
  count = city_race_counts
)

# construct final data object
covid_race <- bind_rows(county_race, city_race) %>%
  left_join(., race, by = c("geoid", "value")) %>%
  mutate(rate = count/pop*1000)

# write data
write_csv(covid_race, "data/individual/stl_race_rates.csv")

# clean-up
rm(race, county_race, county_race_counts, city_race, city_race_counts, covid_race)

