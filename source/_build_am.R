# Scape Data and Construct Data Sets

# UPDATE date value
# date <- lubridate::mdy("05-16-2020")
date <- Sys.Date()-1

# dependencies
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(tigris)
library(zoo)

# functions
source("source/functions/get_data.R")
source("source/functions/historic_expand.R")

# workflow
source("source/workflow/build/01_scrape_and_tidy.R")
source("source/workflow/build/02_create_state_msa.R")
source("source/workflow/build/03_add_rates.R")
source("source/workflow/build/04_create_spatial.R")

# update README
rmarkdown::render(input = "README.Rmd",
                  params = list(
                    date_val = as.character(date)
                  ))

# delete README.html
fs::file_delete("README.html")

# clean-up
rm(date)
