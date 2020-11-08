# plot data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# prompt for submodule update ####

## prompt
q <- usethis::ui_yeah("Have you updated the data submodule?")

## evaluate prompt
if (q == FALSE){
  stop("AM plot build aborted!")
}

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# manual values ####

## define color palettes
# define colors
pal <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
pal[6] <- "#FFD60C"

## create list
values <- list(
  date = Sys.Date()-1,
  
  plot_date = "2020-03-10",
  date_breaks = "1 month",
  date_breaks_long = "1 month",
  date_breaks_log = 15,
  date_breaks_facet = "2 months",
  x_angle = 25,
  
  county_log_max = 100000,
  county_log_breaks = c(5,10,30,100,300,1000,3000,10000,30000,100000),
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
  
  pal = c(pal, 
          RColorBrewer::brewer.pal(n = 6, name = "Reds")[c(6)], 
          RColorBrewer::brewer.pal(n = 6, name = "Blues")[c(6)], 
          RColorBrewer::brewer.pal(n = 6, name = "Greens")[c(6)], 
          RColorBrewer::brewer.pal(n = 6, name = "Purples")[c(6)])
)

## clean-up
rm(pal)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

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

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# construct regional objects ####
## build
regional_geoids <- list(
  cape = c("29031", "29157", "29186",  "29187", "29017", "29221", "29123", "29055"),
  mid_mo = c("29135", "29019", "29051", "29007", "29027", "29053", "29151", "29073", "29139"),
  ozark = c("29029", "29131", "29141", "29015", "29169", "29105", "29125", "29161"),
  ozark_mtns = c("29153", "29215", "29091", "29229", "29203", "29035", "29181"),
  nomo = c("29211", "29001", "29079", "29127", "29163", "29173", "29117", "29175", "29137"),
  semo = c("29511", "29133", "29155", "29201",  "29207", "29143", "29023", "29069"),
  spring = c("29043", "29077", "29167", "29213", "29209", "29225", "29059", "29085"),
  st_jo = c("29003", "29021", "29063", "29049", "29147", "29075", "29081"),
  swmo = c("29097", "29119", "29145", "29011", "29009", "29512", "29109"),
  west = c("29033", "29101", "29159", "29195")
)

## save for site generation
save(regional_geoids, file = "data/regional_geoids.rda")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

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
rm(values, calculate_days, facet_rate, filter_date, map_bins, map_breaks,
   round_any, save_plots, sequoia_theme, regional_geoids, q)

