# Scape Data and Construct Data Sets

# UPDATE date value
date <- lubridate::mdy("04-15-2020")
city_lt5 <- c("63105", "63117", "63119", "63123", "63125", "63130", "63133", "63137", "63143")

# dependencies
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)
library(zoo)

# functions
source("source/functions/get_data.R")
source("source/functions/historic_expand.R")
source("source/functions/wrangle_zip.R")

# workflow
source("source/workflow/01_scrape_and_tidy.R")
source("source/workflow/02_add_rates.R")
source("source/workflow/03_create_spatial.R")
source("source/workflow/04_create_zip.R")

# update interactive map
rmarkdown::render(input = "docs/index.Rmd",
                  params = list(
                    date = paste0("Current as of ", as.character(date)),
                    date_val = as.character(date),
                    prior_date_val = as.character(date-7)
                  ))

# update README
rmarkdown::render(input = "README.Rmd",
                  params = list(
                    date_val = as.character(date)
                  ))

# delete README.html
fs::file_delete("README.html")
