# scrape data from Johns Hopkins, tidy, and plot

# UPDATE values
date <- lubridate::mdy("03-26-2020")

# UPDATE list file names after county-level data available
detailed_days <- c("03-22-2020.csv", "03-23-2020.csv", "03-24-2020.csv", "03-25-2020.csv", "03-26-2020.csv")

# dependencies
library(dplyr)
library(gghighlight)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)

# load get_data function
source("source/covid_get_data.R")

# scrape and tidy data
source("source/covid_tidy_data.R")

# update plots
source("source/covid_plots.R")
