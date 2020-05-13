# Scape Data and Construct Data Sets

# UPDATE date value
date <- lubridate::mdy("05-11-2020")

# UPDATE city zip's with < 5 or 0 cases
city_lt5 <- c("63105", "63117", "63119", "63125", "63130", "63133", "63137", "63143")

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
source("source/functions/wrangle_zip.R")

# workflow
source("source/workflow/build/01_scrape_and_tidy.R")
source("source/workflow/build/02_create_state_msa.R")
source("source/workflow/build/03_add_rates.R")
source("source/workflow/build/04_create_spatial.R")
source("source/workflow/build/05_create_zip.R")
source("source/workflow/build/06_create_testing.R")
source("source/workflow/build/07_create_stl_hospital.R")

# update README
rmarkdown::render(input = "README.Rmd",
                  params = list(
                    date_val = as.character(date)
                  ))

# delete README.html
fs::file_delete("README.html")

# clean-up
