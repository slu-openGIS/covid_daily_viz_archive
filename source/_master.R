# scrape data from Johns Hopkins, tidy, and plot

# UPDATE date value
date <- lubridate::mdy("04-01-2020")
mode <- "build"
# mode <- "plot"

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
if (mode == "build"){
  
  source("source/functions/get_data.R")
  source("source/functions/historic_expand.R")
  
}

source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

# scrape and tidy data
if (mode == "build"){
  
  source("source/workflow/01_scrape_and_tidy.R")
  source("source/workflow/02_add_rates.R")
  source("source/workflow/03_create_spatial.R")
  
  # create reference object
  ref_county <- mo_sf
  st_geometry(ref_county) <- NULL
  
}

# re-load data
if (mode == "plot"){
  
  state_data <- read_csv("data/state/state_full.csv")
  mo_sf <- st_read("data/county/daily_snapshot_mo.geojson", crs = 102003,
                   stringsAsFactors = FALSE) 
  
  stl_detail <- read_csv("data/metro/county_stl.csv")
  stl_sf <- st_read("data/metro/daily_snapshot_stl.geojson", crs = 102003,
                    stringsAsFactors = FALSE) 
  kc_detail <- read_csv("data/metro/county_kc.csv")
  kc_sf <- st_read("data/metro/daily_snapshot_kc.geojson", crs = 102003,
                   stringsAsFactors = FALSE) 
  
  state_confirmed_days <- read_csv("data/state/state_confirm.csv")
  county_confirmed_days <- read_csv("data/county/county_confirm.csv")
  
}

# update plots
source("source/workflow/04_summary_plots.R")
source("source/workflow/05_stl_plots.R")
source("source/workflow/06_kc_plots.R")
source("source/workflow/07_log_plots.R")
source("source/workflow/08_zip_plots.R")

# define date
date_str <- paste0("Current as of ", as.character(date))

# update interactive map
rmarkdown::render(input = "docs/index.Rmd",
       params = list(
         date = date_str
       ))

# clean-up
rm(pal, snapshot, date, date_str, mode)
