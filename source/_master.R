# scrape data from Johns Hopkins, tidy, and plot

# UPDATE date value
date <- lubridate::mdy("04-13-2020")
mode <- "build"
# mode <- "plot"
city_lt5 <- c("63105", "63117", "63119", "63123", "63125", "63130", "63133", "63137", "63143")

# dependencies
library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(purrr)
library(readr)
library(scales)
library(sf)
library(tidycensus)
library(tigris)
library(zoo)

# load functions
if (mode == "build"){
  
  source("source/functions/get_data.R")
  source("source/functions/historic_expand.R")
  source("source/functions/wrangle_zip.R")
  
} else if (mode == "plot"){
  
  source("source/functions/map_breaks.R")
  source("source/functions/save_plots.R")
  source("source/functions/sequoia_theme.R")
  
}

round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# scrape and tidy data
if (mode == "build"){
  
  source("source/workflow/01_scrape_and_tidy.R")
  source("source/workflow/02_add_rates.R")
  source("source/workflow/03_create_spatial.R")
  source("source/workflow/04_create_zip.R")
  
}

# re-load data
if (mode == "plot"){
  
  state_data <- read_csv("data/state/state_full.csv")
  mo_sf <- st_read("data/county/daily_snapshot_mo.geojson", crs = 102003,
                   stringsAsFactors = FALSE) 
  
  county_data <- read_csv("data/county/county_full.csv")
  
  stl_detail <- read_csv("data/metro/county_stl.csv")
  stl_sf <- st_read("data/metro/daily_snapshot_stl.geojson", crs = 102003,
                    stringsAsFactors = FALSE) 
  kc_detail <- read_csv("data/metro/county_kc.csv")
  kc_sf <- st_read("data/metro/daily_snapshot_kc.geojson", crs = 102003,
                   stringsAsFactors = FALSE) 
  
  city_county_zip_sf <- st_read("data/zip/daily_snapshot_city_county.geojson", crs = 4326,
                             stringsAsFactors = FALSE)
  
  rm(city_lt5)
  
  # create reference object
  ref_county <- mo_sf
  st_geometry(ref_county) <- NULL

   
}

# plot data
if (mode == "plot"){
  
  # update plots
  source("source/workflow/06_summary_plots.R")
  source("source/workflow/07_stl_plots.R")
  source("source/workflow/08_kc_plots.R")
  source("source/workflow/09_log_confirm_plots.R")
  source("source/workflow/11_log_confirm_avg_plots.R")
  source("source/workflow/13_zip_plots.R")
  
  # clean-up
  rm(save_plots, sequoia_theme)
  
}

# update interactive map
rmarkdown::render(input = "docs/index.Rmd",
       params = list(
         date = paste0("Current as of ", as.character(date)),
         date_val = as.character(date),
         prior_date_val = as.character(date-7)
       ))

# clean-up
rm(pal, snapshot, date, mode, zip_snapshot, map_breaks, map_bins, bins, round_any,
   data_table, state_data, stl_city_data, stl_county_data, kc_city_data)
