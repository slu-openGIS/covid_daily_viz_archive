# Scape Data and Construct Data Sets, PM only

# UPDATE date value
date <- lubridate::mdy("05-13-2020")

# UPDATE city zip's with < 5 or 0 cases
city_lt5 <- c("63105", "63117", "63119", "63125", "63130", "63133", "63137", "63143")

# dependencies
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(zoo)

# functions
source("source/functions/wrangle_zip.R")

# workflow
source("source/workflow/build/05_create_zip.R")
source("source/workflow/build/06_create_testing.R")
source("source/workflow/build/07_create_stl_hospital.R")

# clean-up
rm(date)
