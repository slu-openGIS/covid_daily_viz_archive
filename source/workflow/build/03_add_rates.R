# add rates

# load state reference data
state_pop <- read_csv("data/source/state_pop.csv")

# load county reference data
## county populations
county_pop <- read_csv("data/source/county_pop.csv") %>%
  mutate(GEOID = as.character(GEOID))

## county populations with KC as county equivalent
mo_county_pop <- read_csv("data/source/mo_county_plus/mo_county_plus.csv") %>%
  select(-NAME) %>%
  mutate(GEOID = as.character(GEOID))

## combine county data
county_pop <- bind_rows(county_pop, mo_county_pop)

## clean-up
rm(mo_county_pop)

## MSA populations
### create vector of MSA GEOIDs
metros_geoid <- c("16020", "17860", "27620", "27900", "28140", "44180", "41140", "41180")

# join population to counts and calculate rate
left_join(state_data, state_pop, by = c("state" = "NAME")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*100000,
    mortality_rate = deaths/total_pop*100000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(report_date, state, confirmed, confirmed_rate, new_confirmed, confirmed_avg,
         deaths, mortality_rate, new_deaths, deaths_avg, case_fatality_rate) -> state_data

# join population to counts and calculate rate
left_join(county_data, county_pop, by = c("geoid" = "GEOID")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*1000,
    mortality_rate = deaths/total_pop*1000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(report_date, geoid, county, state, confirmed, confirmed_rate, new_confirmed, confirmed_avg,
         deaths, mortality_rate, new_deaths, deaths_avg, case_fatality_rate) -> county_data

# clean-up
rm(state_pop, metros_geoid)

# export
write_csv(state_data, "data/state/state_full.csv")
write_csv(county_data, "data/county/county_full.csv")
write_csv(metro_data, "data/metro_all/metro_full.csv")
