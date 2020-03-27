# initial scrape and tidy of Johns Hopkins COVID-19 data

# define days to scrape
detailed_dates <- seq(as.Date("2020-03-22"), date, by="days")

# download detailed data
detailed_dates %>%
  unlist() %>%
  map_df(~ get_hopkins(date = .x)) -> detailed_data

# add historical data to detailed
## load data
historic_raw <- get_times(end_date = "2020-03-22")

# get county master list
counties_sf <- tigris::counties(state = c(17, 20, 29), cb = FALSE, class = "sf")

# tidy
counties_sf %>% 
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
  arrange(state, geoid) -> counties_sf

# remove geometry
counties <- counties_sf
st_geometry(counties) <- NULL

# create vector of dates
historic_dates <- seq(as.Date("2020-01-24"), as.Date("2020-03-21"), by="days")

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
  mutate(last_update = as.POSIXct(last_update)) %>%
  mutate(confirmed = ifelse(is.na(confirmed) == TRUE, 0, confirmed)) %>%
  mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  select(report_date, geoid, county, state, last_update, confirmed, deaths) -> historic_data

# bind
detailed_data <- bind_rows(historic_data, detailed_data)

# summarize detailed data
detailed_data %>%
  group_by(state, report_date) %>%
  summarise(
    last_update = first(last_update),
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  ) %>%
  arrange(report_date, state) -> summary_data

# clean-up
rm(counties, historic_dates, detailed_dates, get_hopkins, get_times, 
   historic_expand, update_dateTime, historic_data, historic_raw)
