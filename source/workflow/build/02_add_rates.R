# add rates

# get population data
## state populations
state_pop <- get_acs(geography = "state", state = c("Kansas", "Illinois", "Missouri"), year = 2018,
                     variables = "B01001_001") %>%
  select(NAME, estimate) %>%
  rename(total_pop = estimate)

## county populations from ACS for IL
county_pop <- get_acs(geography = "county", state = c("Kansas", "Illinois"), year = 2018,
                      variables = "B01001_001") %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)

## county populations with KC as county equivalent
mo_county_pop <- read_csv("data/source/mo_county_plus/mo_county_plus.csv") %>%
  select(-NAME) %>%
  mutate(GEOID = as.character(GEOID))

## combine county data
county_pop <- bind_rows(county_pop, mo_county_pop)

## clean-up
rm(mo_county_pop)

# join population to counts and calculate rate
left_join(state_data, state_pop, by = c("state" = "NAME")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*100000,
    mortality_rate = deaths/total_pop*100000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(report_date, state, last_update, confirmed, confirmed_rate, new_confirmed, confirmed_avg,
         deaths, mortality_rate, new_deaths, deaths_avg, case_fatality_rate) -> state_data

# join population to counts and calculate rate
left_join(county_data, county_pop, by = c("geoid" = "GEOID")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*1000,
    mortality_rate = deaths/total_pop*1000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(report_date, geoid, county, state, last_update, confirmed, confirmed_rate, new_confirmed, confirmed_avg,
         deaths, mortality_rate, new_deaths, deaths_avg, case_fatality_rate) -> county_data

# clean-up
rm(state_pop)

# export
write_csv(state_data, "data/state/state_full.csv")
write_csv(county_data, "data/county/county_full.csv")
