# plot data, PM only

# =============================================================================

# UPDATE date value
date <- lubridate::mdy("05-16-2020")

# define first date for plotting
test_date_breaks <- "4 days"
new_pt_breaks <- "5 days"

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
# source("source/functions/calculate_days.R")
source("source/functions/map_breaks.R")
source("source/functions/round_any.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

# =============================================================================

# update plots
## overview plots
source("source/workflow/plot/02_state_test_plots.R")

# regional plots
source("source/workflow/plot/09_stl_zip_plots.R")
source("source/workflow/plot/10_stl_individual_plots.R")
source("source/workflow/plot/11_stl_hospital_plots.R")

# =============================================================================

# clean-up
rm(map_bins, map_breaks, round_any, save_plots, sequoia_theme)

# =============================================================================

# update interactive map
rmarkdown::render(input = "docs/index.Rmd",
                  params = list(
                    date = paste0("Last updated on ", as.character(date)),
                    date_val = as.character(date),
                    prior_date_val = as.character(date-7)
                  ))

# =============================================================================

# clean-up
rm(pal, snapshot, date, zip_snapshot, map_breaks, map_bins, bins, round_any,
   data_table, state_data, stl_city_data, stl_county_data, kc_city_data,
   kc_metro_data, stl_metro_data, state_test_data, stl_hosp)
