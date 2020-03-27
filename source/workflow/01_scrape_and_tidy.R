# initial scrape and tidy of Johns Hopkins COVID-19 data

# list file names before county-level data available through Johns Hopkins
initial_days <- c("03-10-2020.csv", "03-11-2020.csv", "03-12-2020.csv", "03-13-2020.csv", 
                  "03-14-2020.csv", "03-15-2020.csv", "03-16-2020.csv", "03-17-2020.csv", 
                  "03-18-2020.csv", "03-19-2020.csv", "03-20-2020.csv", "03-21-2020.csv")

# download initial data
initial_days %>%
  unlist() %>%
  map_df(~ get_data(file = .x)) -> initial_data

# download detailed data
detailed_days %>%
  unlist() %>%
  map_df(~ get_data(file = .x)) %>%
  mutate(geoid = ifelse(name == "Kansas City", "29511", geoid)) -> detailed_data

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

# add historical data to detailed
## load data
# historic <- read_csv("data/detailed_historic_data.csv")

## bind to detailed data
# detailed_data <- bind_rows(historic, detailed_data)

## clean-up
# rm(historic)
