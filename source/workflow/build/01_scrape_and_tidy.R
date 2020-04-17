# initial scrape and tidy of Johns Hopkins COVID-19 data

# get county master list
counties <- tigris::counties(state = c(17, 20, 29), cb = FALSE, class = "sf")

# remove geometry
st_geometry(counties) <- NULL

# create kansas city row
kc <- tibble(
  geoid = "29511",
  state = "Missouri",
  county = "Kansas City"
)

# tidy
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
    state == 29 ~ "Missouri"
  )) %>%
  bind_rows(., kc) %>%
  arrange(state, geoid) %>%
  mutate(county = ifelse(geoid == "29510", "St. Louis City", county)) -> counties

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
update_dateTime <- paste(date, "00:00:01")

# fill in missing data
historic_data %>%
  mutate(last_update = update_dateTime) %>%
  # mutate(last_update = as.POSIXct(last_update)) %>%
  mutate(confirmed = ifelse(is.na(confirmed) == TRUE, 0, confirmed)) %>%
  mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  select(report_date, geoid, county, state, last_update, confirmed, deaths) -> historic_data

# bind
county_data <- as_tibble(bind_rows(historic_data, detailed_data))

# summarize detailed data
county_data %>%
  group_by(state, report_date) %>%
  summarise(
    last_update = first(last_update),
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  ) %>%
  arrange(report_date, state) %>%
  select(report_date, everything()) -> state_data

# clean-up
rm(counties, historic_dates, get_hopkins, get_times, 
   historic_expand, update_dateTime, historic_data, historic_raw, kc,
   detailed_data)

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

# add daily new cases and deaths, county
state_data %>%
  group_by(state) %>%
  mutate(
    new_confirmed = confirmed - lag(confirmed),
    new_deaths = deaths - lag(deaths)
  ) %>%
  mutate(
    confirmed_avg = rollmean(new_confirmed, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)) %>%
  filter(report_date >= "2020-01-24") %>%
  select(report_date, state,
         confirmed, new_confirmed, confirmed_avg,
         deaths, new_deaths, deaths_avg) -> state_data
