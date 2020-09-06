# plot data, PM only

# =============================================================================

# UPDATE date value
date <- Sys.Date()

# UPDATE remove last row value
remove_last <- FALSE

# define first date for plotting
test_date_breaks <- "14 days"
total_test_date_breaks <- "14 days"
hosp_breaks <- "14 days"
x_angle <- 25

# create caption
caption_text_tests <- "Plot by Christopher Prener, Ph.D.\nData via The Atlantic COVID-19 Tracking Project and the State of Missouri"
caption_text_tests_census <- "Plot by Christopher Prener, Ph.D.\nData via The Atlantic COVID-19 Tracking Project, the State of Missouri, and and the U.S. Census Bureau"

# =============================================================================

# dependencies
library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(readr)
library(sf)
library(tidyr)

# functions
source("source/functions/map_breaks.R")
source("source/functions/get_coords.R")
source("source/functions/round_any.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

# =============================================================================

# update plots
## overview plots
source("source/workflow/02_state_test_plots.R")
source("source/workflow/03_state_individual_plots.R")

# regional plots
source("source/workflow/17_stl_zip_plots.R")
source("source/workflow/18_stl_individual_plots.R")
source("source/workflow/19_stl_hospital_plots.R")

# =============================================================================

# clean-up
rm(map_bins, map_breaks, round_any, save_plots, sequoia_theme, x_angle)
