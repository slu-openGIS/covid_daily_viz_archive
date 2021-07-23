# create descriptives for newsletter ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# dependencies ####
## tidyverse
library(dplyr)
library(lubridate)
library(readr)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####
state_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/state/state_full.csv") %>%
  filter(state == "Missouri")
county_data <- sf::st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo.geojson", crs = 4326,
                 stringsAsFactors = FALSE)
sf::st_geometry(county_data) <- NULL

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# calculate change since last Thursday ####
## subset and reformat
state_data %>%
  slice(tail(row_number(), 8)) %>%
  filter(row_number() == n() | row_number() == 1) %>%
  group_by(state) %>%
  summarise(
    total_cases = last(cases),
    delta_cases = last(cases) - first(cases),
    current_avg_cases = last(case_avg),
    delta_avg_cases = last(case_avg) - first(case_avg),
    total_deaths = last(deaths),
    delta_deaths = last(deaths) - first(deaths),
    current_avg_deaths = last(deaths_avg),
    delta_avg_deaths = last(deaths_avg) - first(deaths_avg)
  ) -> state_summary

## prepare text
sign <- ifelse(state_summary$delta_cases >= 0, "+", "-")
total_cases <- paste0(format(state_summary$total_cases, big.mark=",",scientific=FALSE), " (", sign,
  format(abs(state_summary$delta_cases), big.mark=",",scientific=FALSE), " from last Thursday)")
sign <- ifelse(state_summary$delta_avg_cases >= 0, "+", "-")
new_cases <- paste0(format(round(state_summary$current_avg_cases, digits = 2), big.mark=",",scientific=FALSE), " (", sign,
  format(abs(round(state_summary$delta_avg_cases, digits = 2)), big.mark=",",scientific=FALSE), " from last Thursday)")
sign <- ifelse(state_summary$delta_deaths >= 0, "+", "-")
total_deaths <- paste0(format(state_summary$total_deaths, big.mark=",",scientific=FALSE), " (", sign,
                      format(abs(state_summary$delta_deaths), big.mark=",",scientific=FALSE), " from last Thursday)")
sign <- ifelse(state_summary$delta_avg_deaths >= 0, "+", "-")
new_deaths <- paste0(format(round(state_summary$current_avg_deaths, digits = 2), big.mark=",",scientific=FALSE), " (", sign,
                    format(abs(round(state_summary$delta_avg_deaths, digits = 2)), big.mark=",",scientific=FALSE), " from last Thursday)")
rm(sign)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# calculate top counties ####
## subset and reformat
county_data %>%
  select(county, case_avg_rate) %>%
  arrange(desc(case_avg_rate)) %>%
  slice(head(row_number(), 10)) %>%
  mutate(text = ifelse(row_number() == 1, paste0(county, " (", round(case_avg_rate, digits = 2), " per 100,000),"), NA)) %>%
  mutate(text = ifelse(row_number() == n(), paste0("and ", county, " (", round(case_avg_rate, digits = 2), ")"), text)) %>%
  mutate(text = ifelse(is.na(text) == TRUE, paste0(county, " (", round(case_avg_rate, digits = 2), "),"), text)) %>%
  pull(text) %>%
  paste(., collapse = " ") -> county_summary

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# create output ####
## construct list
output <- list(
  total_cases = total_cases,
  new_cases = new_cases,
  county_summary = county_summary,
  total_deaths = total_deaths,
  new_deaths = new_deaths
)

## clean-up
rm(state_data, state_summary, county_summary, new_cases, new_deaths, total_cases, total_deaths)

## print top 10 county 7-day average rates
county_data %>%
  as_tibble() %>%
  select(county, case_avg_rate) %>%
  arrange(desc(case_avg_rate)) %>%
  print(.)

## print list
print(output)
