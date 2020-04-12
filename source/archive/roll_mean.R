# UPDATE date value
date <- lubridate::mdy("04-10-2020")

# dependencies
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)

# functions
source("source/functions/get_data.R")
source("source/functions/historic_expand.R")

# get county master list
counties <- tigris::counties(state = c(17, 20, 29), cb = FALSE, class = "sf")

# remove geometry
st_geometry(counties) <- NULL

# load data
nyt_raw <- get_times(end_date = date)

# create vector of dates
nyt_dates <- seq(as.Date("2020-01-24"), date, by="days")

# create full counties data set for nyt data
nyt_dates %>%
  unlist() %>%
  map_df(~historic_expand(ref = counties, date = .x)) -> nyt_dates

# combine county master list with historic data
county_full <- left_join(historic_dates, historic_raw, by = c("geoid", "report_date"))


county_full %>%
  group_by(geoid) %>%
  arrange(report_date) %>%
  mutate(new_confirm = confirmed - lag(confirmed))

county_full %>%
  group_by(state, county) %>%
  mutate(diff = confirmed - lag(confirmed)) %>%
  mutate(avg_3 = rollmean(diff, k = 3, align = "right", fill = NA)) %>%
  mutate(avg_5 = rollmean(diff, k = 5, align = "right", fill = NA)) %>%
  mutate(avg_7 = rollmean(diff, k = 6, align = "right", fill = NA)) -> x

x %>%
  filter(geoid == 29510 | geoid == 29189) %>%
  filter(avg_5 >= 1) %>%
  ggplot(data = ., mapping = aes(x = report_date, y = avg_5, color = county)) +
    geom_line()  +
  scale_y_log10()
  