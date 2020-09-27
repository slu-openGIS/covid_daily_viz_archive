# plot data ####

# =============================================================================

# manual values ####

## define color palettes
# define colors
pal_a <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
pal_a[6] <- "#FFD60C"
pal_b <- RColorBrewer::brewer.pal(n = 6, name = "Reds")
pal_b <- pal_b[c(6)]
pal_c <- RColorBrewer::brewer.pal(n = 6, name = "Blues")
pal_c <- pal_c[c(6)]
pal_d <- RColorBrewer::brewer.pal(n = 6, name = "Greens")
pal_d <- pal_d[c(6)]

## create list
values <- list(
  date = Sys.Date()-1,
  
  plot_date = "2020-03-10",
  date_breaks = "14 days",
  date_breaks_long = "1 month",
  date_breaks_log = 10,
  date_breaks_facet = "2 months",
  x_angle = 25,
  
  county_log_max = 30000,
  county_rate_val = 5,
  county_rate_pos = 30,
  county_rate_x = 2,
  county_rate_y = 25,
  county_log_x = -3,
  county_log_y = .4,
  
  caption_text = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project",
  caption_text_census = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project and the U.S. Census Bureau",
  caption_text_census_map = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project and the U.S. Census Bureau",
  caption_text_census_map2 = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project",
  
  pal = c(pal_a, pal_b, pal_c, pal_d)
  
)

## clean-up
rm(pal_a, pal_b, pal_c, pal_d)

# =============================================================================

# UPDATE date value
# date <- lubridate::mdy("05-16-2020")
date <- Sys.Date()-1

# define first date for plotting
plot_date <- "2020-03-10"
date_breaks <- "14 days"
date_breaks_alt <- "14 days"
date_breaks_log <- 10
x_angle <- 25

# county region plot values
county_log_max <- 30000
county_rate_val <- 5
county_rate_pos <- 30
county_rate_x <- 2
county_rate_y <- 25
county_log_x <- -3
county_log_y <- .4

# create caption
# Johns Hopkins University CSSE
caption_text <- "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project"
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
source("source/functions/plots.R")
source("source/functions/round_any.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

# =============================================================================

# update plots
## overview plots
source("source/workflow/01_state_plots.R")
source("source/workflow/04_regional_plots.R")
source("source/workflow/05_metro_plots.R")
source("source/workflow/06_county_plots.R")

## regional plots
source("source/workflow/07_county_plots_semo.R")
source("source/workflow/08_county_plots_midmo.R")
source("source/workflow/09_county_plots_stjo.R")
source("source/workflow/10_county_plots_nomo.R")
source("source/workflow/11_county_plots_ozark.R")
source("source/workflow/12_county_plots_swmo.R")
source("source/workflow/13_county_plots_cape.R")
source("source/workflow/14_county_plots_west.R")
source("source/workflow/15_county_plots_spring.R")
source("source/workflow/16_county_plots_ozkmtns.R")
source("source/workflow/17_stl_county_plots.R")
source("source/workflow/21_kc_county_plots.R")

# =============================================================================

# clean-up
rm(plot_date, save_plots, sequoia_theme, date_breaks, calculate_days, filter_date,
   map_breaks, map_bins, round_any, caption_text, caption_text_census,
   caption_text_census_map, caption_text_census_map2, x_angle,
   start_date, date_breaks_alt, county_rate_val)
