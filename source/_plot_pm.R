# Plot PM Data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# these plots include:
#   - state, regional, and metro trends
#   - county-level plots in a variety of metro and non-metro regions

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# qa prompts ####

## prompt
q <- usethis::ui_yeah("Have you updated the data submodule?")

## evaluate prompt
if (q == FALSE){
  stop("PM plot build aborted!")
}

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# UPDATE date value
date <- Sys.Date()

# define first date for plotting
test_date_breaks <- "14 days"
total_test_date_breaks <- "14 days"
hosp_breaks <- "14 days"
x_angle <- 25

# create caption
caption_text_tests <- "Plot by Christopher Prener, Ph.D.\nData via The Atlantic COVID-19 Tracking Project and the State of Missouri"
caption_text_tests_census <- "Plot by Christopher Prener, Ph.D.\nData via The Atlantic COVID-19 Tracking Project, the State of Missouri, and and the U.S. Census Bureau"

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# dependencies

## packages
### tidyverse
library(dplyr)          # data wrangling
library(forcats)        # factor tools
library(ggplot2)        # mapping and plotting
library(readr)          # csv file tools
library(tidyr)          # data wrangling

### spatial packages
library(sf)             # mapping tools

### other packages
library(ggrepel)        # map labeling
library(RColorBrewer)   # color palettes

## functions
source("source/functions/get_coords.R")        # convert sf geometry to x,y columns
source("source/functions/map_breaks.R")        # creating map beaks
source("source/functions/round_any.R")         # rounding
source("source/functions/save_plots.R")        # save plots and maps
source("source/functions/sequoia_theme.R")     # theme for plots and maps

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# update plots
## overview plots
# source("source/workflow/02_state_test_plots.R")
source("source/workflow/03_state_individual_plots.R")

# regional plots
# source("source/workflow/18_stl_zip_plots.R")
# source("source/workflow/18_stl_zip_plots_v2.R")
source("source/workflow/19_stl_individual_plots.R")
source("source/workflow/20_stl_hospital_plots.R")
source("source/workflow/22_mo_deaths.R")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up
rm(map_bins, map_breaks, round_any, save_plots, sequoia_theme, x_angle)
