library(dplyr)
library(readr)

counties_old <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/county/county_full.csv") %>%
  filter(report_date == as.Date("2020-04-30")) %>%
  filter(state == "Missouri") %>%
  select(geoid, cases) %>%
  rename(cases_old = cases)

counties <- read_csv("data/county/county_full.csv") %>%
  filter(report_date == as.Date("2020-04-30")) %>%
  filter(state == "Missouri") %>%
  select(report_date, geoid, county, cases)

counties <- left_join(counties, counties_old, by = "geoid") %>%
  mutate(delta = cases - cases_old)

rm(counties_old)
