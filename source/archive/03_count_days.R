


# create days from 3rd death data, county-level data
county_data %>%
  filter(deaths >= 3) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, deaths) %>%
  arrange(state, county, day) -> county_death_days

# create days from first day where average deaths were over 3, county-level data
county_data %>%
  filter(deaths_avg >= 3) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, deaths_avg) %>%
  arrange(state, county, day) -> county_avg_death_days


# create days from 3rd death data, state-level data
state_data %>%
  filter(deaths >= 3) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, deaths) %>%
  arrange(state, day) -> state_death_days

# create days from first day where average deaths were over 3, state-level data
state_data %>%
  filter(deaths_avg >= 3) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, deaths_avg) %>%
  arrange(state, day) -> state_avg_death_days
