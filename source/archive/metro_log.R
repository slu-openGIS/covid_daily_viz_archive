date <- lubridate::mdy("04-13-2020")

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(zoo)

source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

county_full <- read_csv("data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid))

metros <- list(
  cape_girardeau = c("29031", "29017", "17003"),
  columbia = c("29019", "29007", "29175", "29053", "29089"),
  jeff_city = c("29051", "29027", "29135", "29151"),
  joplin = c("29097", "29145"),
  kansas_city = c("20091", "20103", "20107", "20121", "20209",
                   "29013", "29025", "29037", "29047", "29049", 
                   "29095", "29107", "29165", "29177", "29511"),
  springfield = c("29077", "29043", "29225", "29167", "29225"),
  st_joseph = c("29003", "29021", "29063", "20043"),
  st_louis = c("17005", "17013", "17027", "17083", "17117", 
                "17119", "17133", "17163", "29071", "29099", 
                "29113", "29183", "29189", "29219", "29510")
)


county_full %>%
  mutate(msa = case_when(
    geoid %in% metros$jeff_city ~ "Jefferson City",
    geoid %in% metros$columbia ~ "Columbia",
    geoid %in% metros$springfield ~ "Springfield",
    geoid %in% metros$joplin ~ "Joplin",
    geoid %in% metros$st_joseph ~ "St. Joseph",
    geoid %in% metros$cape_girardeau ~ "Cape Girardeau",
    geoid %in% metros$st_louis ~ "St. Louis",
    geoid %in% metros$kansas_city ~ "Kansas City"
  )) %>%
  filter(is.na(msa) == FALSE) %>%
  group_by(msa, report_date) %>%
  summarise(
    confirmed = sum(confirmed),
    new_confirmed = sum(new_confirmed),
    deaths = sum(deaths),
    new_deaths = sum(new_deaths)
  ) %>%
  mutate(
    confirmed_avg = rollmean(new_confirmed, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)
  ) %>%
  filter(report_date >= "2020-03-01") -> metro_full


