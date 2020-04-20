# plot data

# =============================================================================

# UPDATE date value
date <- lubridate::mdy("04-18-2020")

# define first date for plotting
plot_date <- "2020-03-10"
date_breaks <- "5 days"

# =============================================================================

# dependencies
library(dplyr)
library(forcats)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(readr)
library(scales)
library(sf)

# functions
source("source/functions/map_breaks.R")
source("source/functions/round_any.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

# =============================================================================

# load data
source("source/workflow/plot/01_load_data.R")

# =============================================================================

# update plots
source("source/workflow/plot/01_state_plots.R")
source("source/workflow/plot/02_metro_plots.R")
source("source/workflow/plot/03_metro_plots.R")

source("source/workflow/plot/07_stl_plots.R")
source("source/workflow/plot/08_kc_plots.R")
source("source/workflow/plot/09_log_confirm_plots.R")
source("source/workflow/plot/09_perry_plots.R")
source("source/workflow/plot/11_log_confirm_avg_plots.R")
source("source/workflow/plot/13_zip_plots.R")
source("source/workflow/plot/14_individual.R")

# =============================================================================

# update interactive map
rmarkdown::render(input = "docs/index.Rmd",
                  params = list(
                    date = paste0("Current as of ", as.character(date)),
                    date_val = as.character(date),
                    prior_date_val = as.character(date-7)
                  ))

# =============================================================================

# update README
rmarkdown::render(input = "README.Rmd",
                  params = list(
                    date_val = as.character(date)
                  ))

# delete README.html
fs::file_delete("README.html")

# =============================================================================

# clean-up
rm(pal, snapshot, date, zip_snapshot, map_breaks, map_bins, bins, round_any,
   data_table, state_data, stl_city_data, stl_county_data, kc_city_data)
