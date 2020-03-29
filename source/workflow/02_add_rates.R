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
mo_county_pop <- read_csv("data/mo_county_plus/mo_county_plus.csv") %>%
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
  select(report_date, state, last_update, confirmed, confirmed_rate, deaths, 
         mortality_rate, case_fatality_rate) -> state_data

# join population to counts and calculate rate
left_join(county_data, county_pop, by = c("geoid" = "GEOID")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*1000,
    mortality_rate = deaths/total_pop*1000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(report_date, geoid, county, state, last_update, confirmed, 
         confirmed_rate, deaths, mortality_rate, case_fatality_rate) -> county_data

# create days from first confirmed infection data, county-level data
county_data %>%
  filter(confirmed > 0) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, 
         confirmed, confirmed_rate) %>%
  arrange(state, county, day) -> county_confirmed_days

# create days from first death data, county-level data
county_data %>%
  filter(deaths > 0) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, deaths, 
         mortality_rate, case_fatality_rate) %>%
  arrange(state, county, day) -> county_death_days

# create days from first confirmed infection data, state-level data
state_data %>%
  filter(confirmed > 0) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, 
         confirmed, confirmed_rate) %>%
  arrange(state, day) -> state_confirmed_days

# create days from first death data, state-level data
state_data %>%
  filter(deaths > 0) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, deaths, 
         mortality_rate, case_fatality_rate) %>%
  arrange(state, day) -> state_death_days

# clean-up
rm(county_pop, state_pop)

# export
write_csv(state_data, "data/state/state_full.csv")
write_csv(state_confirmed_days, "data/state/state_confirm.csv")
write_csv(state_death_days, "data/state/state_death.csv")

write_csv(county_data, "data/county/county_full.csv")
write_csv(county_confirmed_days, "data/county/county_confirm.csv")
write_csv(county_death_days, "data/county/county_death.csv")
