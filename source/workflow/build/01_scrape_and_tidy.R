# initial scrape and tidy of Johns Hopkins COVID-19 data

# load county reference data
counties <- read_csv("data/source/counties.csv") %>%
  mutate(geoid = as.character(geoid))

# define days to scrape
# hopkins_dates <- seq(as.Date("2020-04-15"), date, by="days")

# download hopkins data data
# hopkins_dates %>%
#  unlist() %>%
#  map_df(~ get_hopkins(date = .x, ref = counties)) %>%
#  rename(cases = confirmed) -> hopkins_data

# download hopkins data
# hopkins_data <- get_hopkins(date = date, ref = counties) %>%
#  rename(cases = confirmed)

# =============================================================================

# fix Moniteau County

# hopkins_rest <- filter(hopkins_data, county != "Moniteau")
# hopkins_moniteau <- filter(hopkins_data, county == "Moniteau")

# hopkins_moniteau <- mutate(hopkins_moniteau, cases = case_when(
#  report_date == "2020-04-21" ~ 20,
#  report_date == "2020-04-22" ~ 34,
#  report_date == "2020-04-23" ~ 41,
#  report_date == "2020-04-24" ~ 41,
#  report_date == "2020-04-25" ~ 45,
#  report_date >= "2020-04-26" & report_date <= "2020-04-30" ~ 49,
#  report_date >= "2020-05-01" & report_date <= "2020-05-02" ~ 50,
#  report_date >= "2020-05-03" & report_date <= "2020-05-06" ~ 51,
#  report_date >= "2020-05-07" & report_date <= "2020-05-08" ~ 52,
#  report_date >= "2020-05-09" ~ 53
# )) -> x

# =============================================================================

# detailed_data <- get_hopkins(date = date, ref = counties)

# add historical data to detailed
## load data
times_raw <- get_times(end_date = date)

# create vector of dates
times_dates <- seq(as.Date("2020-01-24")-7, date, by="days")

# create full counties data set for historic data
times_dates %>%
  unlist() %>%
  map_df(~historic_expand(ref = counties, date = .x)) -> times_dates

# fix Joplin
joplin <- filter(times_dates, geoid == "29512") %>%
  filter(report_date > "2020-06-24")
times_dates <- filter(times_dates, geoid != "29512")
times_dates <- rbind(times_dates, joplin) %>%
  arrange(state, county, report_date)
rm(joplin)

# combine county master list with historic data
times_data <- left_join(times_dates, times_raw, by = c("geoid", "report_date"))

# fill in missing data
times_data %>%
  rename(cases = confirmed) %>%
  mutate(cases = ifelse(is.na(cases) == TRUE, 0, cases)) %>%
  mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  select(report_date, geoid, county, state, cases, deaths) -> times_data

# bind
# county_data <- as_tibble(bind_rows(times_data, hopkins_data))
county_data <- as_tibble(times_data)

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
   historic_expand, times_raw, times_data) # hopkins_data, hopkins_dates
