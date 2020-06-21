# plot data

# =============================================================================

# UPDATE date value
# date <- lubridate::mdy("05-16-2020")
date <- Sys.Date()-1

# define first date for plotting
plot_date <- "2020-03-10"
date_breaks <- "9 days"
date_breaks_alt <- "9 days"
x_angle <- 25

# create caption
# Johns Hopkins University CSSE
caption_text <- "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project\nThe 15 Apr reporting change refers to the inclusion of probable cases in data per CDC guidance"
caption_text_census <- "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project and the U.S. Census Bureau"
caption_text_census_map <- "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project and the U.S. Census Bureau"
caption_text_census_map2 <- "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project"

# =============================================================================

# dependencies
library(dplyr)
library(forcats)
library(ggplot2)
library(gghighlight)
library(ggrepel)
library(purrr)
library(RColorBrewer)
library(readr)
library(scales)
library(sf)
library(tibble)
library(tidyr)
library(zoo)

# functions
source("source/functions/calculate_days.R")
source("source/functions/map_breaks.R")
source("source/functions/round_any.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

# =============================================================================

# update plots
## overview plots
source("source/workflow/plot/01_state_plots.R")
source("source/workflow/plot/04_regional_plots.R")
source("source/workflow/plot/05_metro_plots.R")
source("source/workflow/plot/06_county_plots.R")

## regional plots
source("source/workflow/plot/07_county_plots_semo.R")
source("source/workflow/plot/08_county_plots_midmo.R")
source("source/workflow/plot/09_county_plots_stjo.R")
source("source/workflow/plot/10_county_plots_nomo.R")
source("source/workflow/plot/11_county_plots_ozark.R")
source("source/workflow/plot/12_county_plots_swmo.R")
source("source/workflow/plot/13_county_plots_cape.R")
source("source/workflow/plot/14_stl_county_plots.R")
source("source/workflow/plot/18_kc_county_plots.R")

# =============================================================================

# clean-up
rm(plot_date, save_plots, sequoia_theme, date_breaks, calculate_days, filter_date,
   map_breaks, map_bins, round_any, caption_text, caption_text_census,
   caption_text_census_map, caption_text_census_map2, x_angle,
   start_date, date_breaks_alt)

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
