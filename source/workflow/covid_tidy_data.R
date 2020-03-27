

# get population data
## state populations
state_pop <- get_acs(geography = "state", state = c("Illinois", "Missouri"), year = 2018,
                     variables = "B01001_001") %>%
  select(NAME, estimate) %>%
  rename(total_pop = estimate)

## county populations from ACS for IL
county_pop <- get_acs(geography = "county", state = c("Illinois"), year = 2018,
                     variables = "B01001_001") %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)

## county populations with KC as county equivalent
mo_pop <- read_csv("data/mo_county_plus/mo_county_plus.csv") %>%
  select(-NAME) %>%
  mutate(GEOID = as.character(GEOID))

## combine county data
county_pop <- bind_rows(county_pop, mo_pop)

## clean-up
rm(mo_pop)

# join population to counts and calculate rate
left_join(summary_data, state_pop, by = c("state_name" = "NAME")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*100000,
    mortality_rate = deaths/total_pop*100000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(state_name, report_date, last_update, confirmed, confirmed_rate, deaths, 
         mortality_rate, case_fatality_rate, recovered) -> summary_data

# join population to counts and calculate rate
left_join(detailed_data, county_pop, by = c("geoid" = "GEOID")) %>%
  mutate(
    confirmed_rate = confirmed/total_pop*1000,
    mortality_rate = deaths/total_pop*1000,
    case_fatality_rate = deaths/confirmed*100
  ) %>% 
  select(-total_pop) %>%
  select(state_name, geoid, name, report_date, last_update, confirmed, 
         confirmed_rate, deaths, mortality_rate, case_fatality_rate, 
         recovered) -> detailed_data

# clean-up
rm(county_pop, state_pop, initial_data, initial_days, detailed_days, get_data)

# export
write_csv(summary_data, "data/summary_data.csv")
write_csv(detailed_data, "data/detailed_data.csv")

# subset detailed data
detailed_sub <- filter(detailed_data, report_date == date)

# create detamiled metro data
## download geometry for IL
il_counties <- counties(state = 17, cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 26915)

## download geometry for MO
mo_counties <- counties(state = 29, cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 26915)

## define metro counties
metro_counties <- c("17005", "17013", "17027", "17083", "17117", 
                    "17119", "17133", "17163", "29071", "29099", 
                    "29113", "29183", "29189", "29219", "29510")

## combine and subset geometric data
rbind(il_counties, mo_counties) %>%
  filter(GEOID %in% metro_counties) -> counties

## clean-up
rm(il_counties, mo_counties)

# combine attributes and mortality
detailed_sub %>%
  filter(geoid %in% metro_counties) %>%
  left_join(counties, ., by = c("GEOID" = "geoid")) %>%
  mutate(report_data = as.character(report_date)) %>%
  select(GEOID, state_name, name, report_date, confirmed_rate, mortality_rate, case_fatality_rate) %>%
  rename(
    state = state_name,
    county = name,
    date = report_date,
    c_rate = confirmed_rate,
    m_rate = mortality_rate,
    cf_rate = case_fatality_rate
   ) -> detailed_metro_sf

# clean-up
rm(counties)

# write data
st_write(detailed_metro_sf, "data/metro_data/metro_data.shp", delete_dsn = TRUE)

# subset detailed data again
detailed_metro <- filter(detailed_data, geoid %in% metro_counties)

# clean-up
rm(metro_counties)
