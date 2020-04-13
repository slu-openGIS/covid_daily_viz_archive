# create days from 10th confirmed infection data, county-level data
county_data %>%
  filter(confirmed >= 10) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, 
         confirmed, confirmed_rate) %>%
  arrange(state, county, day) -> county_confirmed_days

# create days from first day where average confirmed infections were at least 10, county-level data
county_data %>%
  filter(confirmed_avg >= 10) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, confirmed_avg) %>%
  arrange(state, county, day) -> county_avg_confirmed_days

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

# create days from 10th confirmed infection data, state-level data
state_data %>%
  filter(confirmed >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, confirmed) %>%
  arrange(state, day) -> state_confirmed_days

# create days from first day where average confirmed infections were at least 10, state-level data
state_data %>%
  filter(confirmed_avg >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, confirmed_avg) %>%
  arrange(state, day) -> state_avg_confirmed_days

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

# export
write_csv(state_confirmed_days, "data/state/state_confirm.csv")
write_csv(state_avg_confirmed_days, "data/state/state_confirm_avg.csv")
write_csv(state_death_days, "data/state/state_death.csv")
write_csv(state_avg_death_days, "data/state/state_death_avg.csv")

write_csv(county_confirmed_days, "data/county/county_confirm.csv")
write_csv(county_avg_confirmed_days, "data/county/county_confirm_avg.csv")
write_csv(county_death_days, "data/county/county_death.csv")
write_csv(county_avg_death_days, "data/county/county_death_avg.csv")

# clean-up
rm(state_confirmed_days, state_avg_confirmed_days, state_death_days, state_avg_death_days,
   county_confirmed_days, county_avg_confirmed_days, county_death_days, county_avg_death_days)
