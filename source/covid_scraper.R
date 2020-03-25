# scrape data from Johns Hopkins

# dependencies
library(dplyr)
library(purrr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)

# values
date <- lubridate::mdy("03-24-2020")

# define download function
get_data <- function(file){
  
  # create values
  url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", file)
  date <- lubridate::mdy(stringr::str_replace(string = file, pattern = ".csv", replacement = ""))
  
  # download data
  response <- RCurl::getURL(url = url)
  
  # read data
  df <- readr::read_csv(response)
  
  # tidy data
  if (date > "2020-03-22"){
    
    df <- dplyr::filter(df, Province_State %in% c("Missouri", "Illinois"))
    df <- dplyr::select(df, FIPS, Admin2, Province_State, Last_Update, Confirmed, Deaths, Recovered, Active)
    df <- dplyr::rename(df,
                        geoid = FIPS,
                        name = Admin2,
                        state_name = Province_State,
                        last_update = Last_Update,
                        confirmed = Confirmed,
                        deaths = Deaths,
                        recovered = Recovered,
                        active = Active)
    df <- dplyr::mutate(df, report_date = date)
    df <- dplyr::select(df, geoid, name, state_name, report_date, dplyr::everything())
    df <- dplyr::arrange(df, as.numeric(geoid))
    
  } else if (date <= "2020-03-22"){
    
    df <- dplyr::rename(df,
                        state_name = `Province/State`,
                        last_update = `Last Update`,
                        confirmed = Confirmed,
                        deaths = Deaths,
                        recovered = Recovered)
    df <- filter(df, state_name %in% c("Missouri", "Illinois"))
    df <- select(df, state_name, last_update, confirmed, deaths, recovered)
    df <- dplyr::mutate(df, report_date = date)
    df <- dplyr::select(df, state_name, report_date, dplyr::everything())
    df <- dplyr::arrange(df, state_name)
    
  }
  
  # return output
  return(df)
  
}

# list file names before county-level data available through Johns Hopkins
initial_days <- c("03-10-2020.csv", "03-11-2020.csv", "03-12-2020.csv", "03-13-2020.csv", "03-14-2020.csv", "03-15-2020.csv",
                  "03-16-2020.csv", "03-17-2020.csv", "03-18-2020.csv", "03-19-2020.csv", "03-20-2020.csv", "03-21-2020.csv",
                  "03-22-2020.csv")

# download initial data
initial_days %>%
  unlist() %>%
  map_df(~ get_data(file = .x)) -> initial_data

# list file names after county-level data available
detailed_days <- c("03-23-2020.csv", "03-24-2020.csv")

# download detailed data
detailed_days %>%
  unlist() %>%
  map_df(~ get_data(file = .x)) -> detailed_data

# summarize detailed data
detailed_data %>%
  group_by(state_name, report_date) %>%
  summarise(
    last_update = first(last_update),
    confirmed = sum(confirmed),
    deaths = sum(deaths),
    recovered = sum(recovered)
  ) %>%
  arrange(report_date, state_name) %>%
  bind_rows(initial_data, .) -> summary_data

# get population data
state_pop <- get_acs(geography = "state", state = c("Illinois", "Missouri"), year = 2018,
                     variables = "B01001_001") %>%
  select(NAME, estimate) %>%
  rename(total_pop = estimate)

county_pop <- get_acs(geography = "county", state = c("Illinois", "Missouri"), year = 2018,
                     variables = "B01001_001") %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)

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
detailed_data <- filter(detailed_data, report_date == date)

# download geometry
il_counties <- counties(state = 17, cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 26915)

mo_counties <- counties(state = 29, cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 26915)

rbind(il_counties, mo_counties) %>%
  filter(GEOID %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) -> counties

# clean-up
rm(il_counties, mo_counties)

# combine attributes and mortality
detailed_data %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) %>%
  left_join(counties, ., by = c("GEOID" = "geoid")) %>%
  mutate(report_data = as.character(report_date)) %>%
  select(GEOID, state_name, name, report_date, confirmed_rate) %>%
  rename(
    state = state_name,
    county = name,
    date = report_date,
    c_rate = confirmed_rate
    ) -> detailed_sf

# clean-up
rm(date, counties)

# write data
st_write(detailed_sf, "data/metro_data/metro_data.shp", delete_dsn = TRUE)

