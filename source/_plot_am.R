# Plot AM Data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# these plots include:
#   - state, regional, and metro trends
#   - county-level plots in a variety of metro and non-metro regions

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# update submodule ####
system("git submodule update --remote")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# manual values ####

# the resulting object contains global values for adjusting plots

## define color palettes
# define colors
pal <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
pal[6] <- "#FFD60C"

## create list
values <- list(
  date = Sys.Date()-1,
  
  plot_date = "2020-03-10",
  date_breaks = "1 month",
  date_breaks_hosp = "2 months",
  date_breaks_long = "1 month",
  date_breaks_log = 50,
  date_breaks_facet = "3 months",
  date_breaks_3days = "1 week", #  7 days
  x_angle = 25,
  
  county_log_max = 100000,
  county_log_breaks = c(5,10,30,100,300,1000,3000,10000,30000,100000),
  county_rate_val = 10,
  county_rate_pos = 30,
  county_rate_x = 2,
  county_rate_y = 25,
  county_log_x = -3,
  county_log_y = .4,
  
  regional_current_x = -330,
  state_current_y = 1500,
  
  caption_text = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project",
  caption_text_census = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project and the U.S. Census Bureau",
  caption_text_census_map = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project and the U.S. Census Bureau",
  caption_text_census_map2 = "Plot by Christopher Prener, Ph.D.\nData via the New York Times COVID-19 Project",
  caption_text_hosp = "Plot by Christopher Prener, Ph.D.\nData via HHS and the U.S. Census Bureau",
  
  pal = c(pal, 
          RColorBrewer::brewer.pal(n = 6, name = "Reds")[c(6)], 
          RColorBrewer::brewer.pal(n = 6, name = "Blues")[c(6)], 
          RColorBrewer::brewer.pal(n = 6, name = "Greens")[c(6)], 
          RColorBrewer::brewer.pal(n = 6, name = "Purples")[c(6)],
          "#000000")
)

## clean-up
rm(pal)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# construct regional objects ####

# the resulting object contains master lists of GEOIDs for regional mapping

## build
regional_geoids <- list(
  cape = c("29031", "29157", "29186",  "29187", "29017", "29221", "29123", "29055", "29093"),
  mid_mo = c("29135", "29019", "29051", "29007", "29027", "29053", "29151", "29073", "29139"),
  ozark = c("29029", "29131", "29141", "29015", "29169", "29105", "29125", "29161", "29065"),
  ozark_mtns = c("29153", "29215", "29091", "29229", "29203", "29035", "29149", "29181", "29179", "29067"),
  nemo = c("29045", "29103", "29111", "29127", "29137", "29163", "29173", "29199", "29205"),
  nomo = c("29211", "29001", "29079", "29121", "29115", "29197", "29117", "29175", "29171", "29129"),
  semo = c("29511", "29133", "29155", "29201",  "29207", "29143", "29023", "29069"),
  spring = c("29043", "29077", "29167", "29213", "29209", "29225", "29059", "29085"),
  st_jo = c("29003", "29021", "29063", "29087", "29147", "29075", "29081", "29005", "29061", "29227"),
  swmo = c("29097", "29119", "29145", "29011", "29009", "29512", "29109", "29057"),
  west = c("29033", "29101", "29159", "29195", "29083", "29185", "29217", "29039", "29041")
)

## save for site generation
save(regional_geoids, file = "data/regional_geoids.rda")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# dependencies ####

## packages
### tidyverse
library(dplyr)          # data wrangling
library(forcats)        # factor tools
library(ggplot2)        # mapping and plotting
library(purrr)          # functional programming
library(readr)          # csv file tools
library(tibble)         # data wrangling
library(tidyr)          # data wrangling

### spatial packages
library(sf)             # mapping tools

### other packages
library(gghighlight)    # highlight trends on plots
library(ggrepel)        # map labeling
library(RColorBrewer)   # color palettes
library(scales)         # plot scales
library(zoo)            # rolling means

## functions
source("source/functions/calculate_days.R")    # calculate days since infections
source("source/functions/map_breaks.R")        # creating map beaks
source("source/functions/plots.R")             # standardized plots
source("source/functions/round_any.R")         # rounding
source("source/functions/save_plots.R")        # save plots and maps
source("source/functions/sequoia_theme.R")     # theme for plots and maps

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# plotting workflow ####

## overview plots
source("source/workflow/01_state_plots.R")        # statewide plots
source("source/workflow/04_regional_plots.R")     # St. Louis, Kansas City, and Outstate
source("source/workflow/05_metro_plots.R")        # metropolitan statistical areas
source("source/workflow/06_county_plots.R")       # county maps for all of MO

## regional plots
source("source/workflow/07_county_plots_semo.R")  # Bootheel
source("source/workflow/08_county_plots_midmo.R") # Mid-Missouri
source("source/workflow/09_county_plots_stjo.R")  # St. Joseph and Northwest MO
source("source/workflow/10_county_plots_nomo.R")  # Northern MO
source("source/workflow/11_county_plots_ozark.R") # Lake of the Ozarks & South-Central MO
source("source/workflow/12_county_plots_swmo.R")  # Joplin and Southwest MO
source("source/workflow/13_county_plots_cape.R")  # Cape Girardeau region
source("source/workflow/14_county_plots_west.R")  # West-Central MO
source("source/workflow/15_county_plots_spring.R") # Springfield
source("source/workflow/16_county_plots_ozkmtns.R") # Ozark Mountains
source("source/workflow/17_county_plots_nemo.R")  # Northeastern Missouri
source("source/workflow/17_stl_county_plots.R")   # St. Louis MSA
source("source/workflow/21_kc_county_plots.R")    # Kansas City MSA

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## check hospitalization metadata for update
load("data/MO_HEALTH_Covid_Tracking/data/source/hhs/last_update.rda")

## rebuild hhs data if there has been an update
if ((values$date+1 == last_update$current_date) == TRUE){
  source("source/workflow/26_hhs_hospital_plots.R") 
}

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up ####
rm(calculate_days, facet_rate, filter_date, map_bins, map_breaks,
   round_any, save_plots, sequoia_theme, regional_geoids, q, regional_count,
   last_update, cumulative_rate, plot_subtype)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

commit <- function(){
  
  system("git add -A")
  system(paste0("git commit -a -m 'build am plots for ", as.character(values$date+1), "'"))
  
}
