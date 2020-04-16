# Plot data

# UPDATE date value
date <- lubridate::mdy("04-15-2020")

# dependencies
library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(scales)
library(sf)

# functions
source("source/functions/map_breaks.R")
source("source/functions/round_any.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")
