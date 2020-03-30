# scrape data from Johns Hopkins, tidy, and plot

# UPDATE date value
date <- lubridate::mdy("03-29-2020")

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
source("source/functions/get_data.R")
source("source/functions/historic_expand.R")

# scrape and tidy data
source("source/workflow/01_scrape_and_tidy.R")
source("source/workflow/02_add_rates.R")
source("source/workflow/03_create_spatial.R")

# update plots
source("source/workflow/04_summary_plots.R")
source("source/workflow/05_stl_plots.R")
source("source/workflow/06_kc_plots.R")
source("source/workflow/07_log_plots.R")
