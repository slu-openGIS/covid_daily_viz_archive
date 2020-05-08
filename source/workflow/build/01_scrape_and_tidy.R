# initial scrape and tidy of Johns Hopkins COVID-19 data

# load county reference data
counties <- read_csv("data/source/counties.csv") %>%
  mutate(geoid = as.character(geoid))

# define days to scrape
hopkins_dates <- seq(as.Date("2020-04-15"), date, by="days")

# download detailed data
hopkins_dates %>%
  unlist() %>%
  map_df(~ get_hopkins(date = .x, ref = counties)) %>%
  rename(cases = confirmed) -> hopkins_data

# detailed_data <- get_hopkins(date = date, ref = counties)

# add historical data to detailed
## load data
times_raw <- get_times(end_date = date)

# create vector of dates
# historic_dates <- seq(as.Date("2020-01-24")-7, date-1, by="days")
times_dates <- seq(as.Date("2020-01-24")-7, as.Date("2020-04-14"), by="days")

# create full counties data set for historic data
times_dates %>%
  unlist() %>%
  map_df(~historic_expand(ref = counties, date = .x)) -> times_dates

# combine county master list with historic data
times_data <- left_join(times_dates, times_raw, by = c("geoid", "report_date"))

# define update date and time value
# update_dateTime <- paste(date, "00:00:01")

# fill in missing data
times_data %>%
  # mutate(last_update = update_dateTime) %>%
  # mutate(last_update = as.POSIXct(last_update)) %>%
  rename(cases = confirmed) %>%
  mutate(cases = ifelse(is.na(cases) == TRUE, 0, cases)) %>%
  mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  select(report_date, geoid, county, state, cases, deaths) -> times_data

# bind
county_data <- as_tibble(bind_rows(times_data, hopkins_data))
# county_data <- as_tibble(historic_data)

# add daily new cases and deaths, county
county_data %>%
  group_by(state, county) %>%
  mutate(
    new_cases = cases - lag(cases),
    new_deaths = deaths - lag(deaths)
  ) %>%
  mutate(
    case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)) %>%
  select(report_date, geoid, county, state, 
         cases, new_cases, case_avg,
         deaths, new_deaths, deaths_avg) -> county_data

# clean-up
rm(counties, times_dates, get_hopkins, get_times, 
   historic_expand, times_raw, times_data, hopkins_data, hopkins_dates)
