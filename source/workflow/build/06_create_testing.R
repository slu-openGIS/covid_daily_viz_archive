library(dplyr)
library(lubridate)
library(readr)
library(zoo)

## read data
test_data <- read_csv("data/source/mo_tests/historic_tests.csv") %>%
  mutate(date = mdy(date)) %>%
  rename(report_date = date)

## subset to relevant range
test_subset <- filter(test_data, report_date >= as.Date("2020-03-27") & report_date <= as.Date("2020-04-30"))

## linear interpolation
test_subset$tests_impute <- na.approx(test_subset$tests)

## combine with full data
test_subset %>%
  select(report_date, tests_impute) %>%
  left_join(test_data, ., by = "report_date") -> test_data
