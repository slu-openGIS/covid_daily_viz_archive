library(dplyr)
library(ggplot2)
library(readr)
library(zoo)

county_full <- read_csv("data/county/county_full.csv")

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
  