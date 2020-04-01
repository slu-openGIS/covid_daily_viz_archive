# scrape data from Johns Hopkins, tidy, and plot

# UPDATE date value
date <- lubridate::mdy("03-31-2020")
# mode <- "build"
mode <- "plot"

# dependencies
library(dplyr)
library(gghighlight)
library(ggplot2)
library(here)
library(lubridate)
library(purrr)
library(readr)
library(rmarkdown)
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

# update plots
source("source/workflow/04_summary_plots.R")
source("source/workflow/05_stl_plots.R")
source("source/workflow/06_kc_plots.R")
source("source/workflow/07_log_plots.R")

# define date
date_str <- paste0("Current as of ", as.character(date))

# update interactive map
render(here("docs", "index.Rmd"),
       params = list(
         date = date_str
       ))
