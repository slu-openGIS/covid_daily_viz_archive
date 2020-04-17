# initial scrape and tidy of Johns Hopkins COVID-19 data

# load county reference data
counties <- read_csv("data/source/counties.csv") %>%
  mutate(geoid = as.character(geoid))

# define days to scrape
# detailed_dates <- seq(as.Date("2020-03-22"), date, by="days")

# download detailed data
# detailed_dates %>%
#  unlist() %>%
#  map_df(~ get_hopkins(date = .x, ref = counties)) -> detailed_data

detailed_data <- get_hopkins(date = date, ref = counties)

# add historical data to detailed
## load data
historic_raw <- get_times(end_date = date)

# create vector of dates
historic_dates <- seq(as.Date("2020-01-24")-7, date-1, by="days")

# create full counties data set for historic data
historic_dates %>%
  unlist() %>%
  map_df(~historic_expand(ref = counties, date = .x)) -> historic_dates

# combine county master list with historic data
historic_data <- left_join(historic_dates, historic_raw, by = c("geoid", "report_date"))

# define update date and time value
# update_dateTime <- paste(date, "00:00:01")

# fill in missing data
historic_data %>%
  # mutate(last_update = update_dateTime) %>%
  # mutate(last_update = as.POSIXct(last_update)) %>%
  mutate(confirmed = ifelse(is.na(confirmed) == TRUE, 0, confirmed)) %>%
  mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  select(report_date, geoid, county, state, confirmed, deaths) -> historic_data

# bind
county_data <- as_tibble(bind_rows(historic_data, detailed_data))

# add daily new cases and deaths, county
county_data %>%
  group_by(state, county) %>%
  mutate(
    new_confirmed = confirmed - lag(confirmed),
    new_deaths = deaths - lag(deaths)
  ) %>%
  mutate(
    confirmed_avg = rollmean(new_confirmed, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)) %>%
  select(report_date, geoid, county, state, 
         confirmed, new_confirmed, confirmed_avg,
         deaths, new_deaths, deaths_avg) -> county_data

# clean-up
rm(counties, historic_dates, get_hopkins, get_times, 
   historic_expand, historic_data, historic_raw,
   detailed_data)
