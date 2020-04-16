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

metro_full %>%
  filter(confirmed >= 10) %>%
  arrange(report_date) %>%
  group_by(msa) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, msa, confirmed) %>%
  arrange(msa, day) -> msa_confirmed_days

# define top_val
top_val <- round_any(x = max(msa_confirmed_days$day), accuracy = 10, f = ceiling)

ggplot(data = msa_confirmed_days, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = msa), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 10000)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/a_log_confirmed.png", preset = "lg")
save_plots(filename = "results/low_res/metro/a_log_confirmed.png", preset = "lg", dpi = 72)

metro_full %>%
  filter(confirmed_avg >= 10) %>%
  arrange(report_date) %>%
  group_by(msa) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, msa, confirmed_avg) %>%
  arrange(msa, day) -> msa_avg_confirmed_days

# define top_val
top_val <- round_any(x = max(msa_avg_confirmed_days$day), accuracy = 10, f = ceiling)

ggplot(data = msa_avg_confirmed_days, mapping = aes(day, confirmed_avg)) +
  geom_line(mapping = aes(color = msa), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Reached",
    y = "7-day Average of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/b_avg_log_confirmed.png", preset = "lg")
save_plots(filename = "results/low_res/metro/b_avg_log_confirmed.png", preset = "lg", dpi = 72)

metro_full %>%
  filter(deaths >= 5) %>%
  arrange(report_date) %>%
  group_by(msa) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, msa, deaths) %>%
  arrange(msa, day) -> metro_death_days

top_val <- round_any(x = max(metro_death_days$day), accuracy = 10, f = ceiling)

ggplot(data = metro_death_days, mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = msa), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(5, 1000), breaks = c(5, 10, 100, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Deaths by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Fifth Death Confirmed",
    y = "Count of Confirmed Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/c_mortality_log.png", preset = "lg")
save_plots(filename = "results/low_res/metro/c_mortality_log.png", preset = "lg", dpi = 72)

metro_full %>%
  filter(deaths_avg >= 5) %>%
  arrange(report_date) %>%
  group_by(msa) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, msa, deaths_avg) %>%
  arrange(msa, day) -> metro_avg_death_days

top_val <- round_any(x = max(metro_avg_death_days$day), accuracy = 10, f = ceiling)

ggplot(data = metro_avg_death_days, mapping = aes(day, deaths_avg)) +
  geom_line(mapping = aes(color = msa), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(5, 100), breaks = c(5, 10, 100), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Deaths by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Five Deaths Reached",
    y = "7-day Average of Confirmed Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/d_avg_mortality_log.png", preset = "lg")
save_plots(filename = "results/low_res/metro/d_avg_mortality_log.png", preset = "lg", dpi = 72)

