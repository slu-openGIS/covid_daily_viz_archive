# plot metro level data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####
metro_hosp <- read_csv("data/MO_HEALTH_Covid_Tracking/data/metro_all/metro_hospital.csv")
region_hosp <- read_csv("data/MO_HEALTH_Covid_Tracking/data/region/region_meso_hospital.csv")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define metro color palette ####
cols <- c("Cape Girardeau" = values$pal[6], "Columbia" = values$pal[3], 
          "Jefferson City" = values$pal[4], "Joplin" = values$pal[7],
          "Kansas City" = values$pal[2], "Springfield" = values$pal[5], 
          "St. Joseph" = values$pal[8], "St. Louis" = values$pal[1])

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# per-capita 7-day average ####

## define top_val
top_val <- round_any(x = max(metro_hosp$covid_per_cap, na.rm = TRUE), accuracy = 50, f = ceiling)

## create factors
metro_hosp <- mutate(metro_hosp, factor_var = fct_reorder2(short_name, report_date, covid_per_cap))

## create plot
p <- facet_rate(metro_hosp, 
                type = "metro HHS", 
                pal = cols, 
                x_breaks = values$date_breaks_hosp,
                y_breaks = 50,
                y_upper_limit = top_val,
                highlight = unique(metro_hosp$geoid),
                plot_date = sort(unique(metro_hosp$report_date))[1],
                date = last_update$last_date,
                title = "Total COVID-19 Hospitalizations by Metro Area",
                caption = values$caption_text_hosp)

## save plot
save_plots(filename = "results/high_res/metro/o_in_pt.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/o_in_pt.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up
rm(metro_hosp)
rm(top_val, p, cols)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define regional color palette ####
cols <- c("St. Louis" = values$pal[1], "Kansas City" = values$pal[2],
          "Outstate" = values$pal[3], "Missouri" = values$pal[4])

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# facet plot ####

## subset data
region_hosp %>%
  filter(region != "Missouri") %>%
  mutate(region = fct_relevel(region, "St. Louis", "Kansas City", "Outstate")) -> region_subset

## define top_val
top_val <- round_any(x = max(region_subset$covid_per_cap), accuracy = 50, f = ceiling)

## construct plot
p <- ggplot() +
  geom_area(region_subset, mapping = aes(x = report_date, y = covid_per_cap, fill = region),
            show.legend = FALSE) +
  scale_fill_manual(values = cols) +
  facet_wrap(vars(region), nrow = 3) +
  scale_x_date(date_breaks = values$date_breaks_long, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 50)) + 
  labs(
    title = "Total COVID-19 Hospitalizations by Region",
    subtitle = paste0(as.character(sort(unique(region_hosp$report_date))[1]), " through ", as.character(last_update$last_date)),
    caption = values$caption_text_hosp,
    x = "Date",
    y = "COVID Patients per 1,000 Staffed Beds"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/regional/o_in_pt.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/o_in_pt.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up
rm(region_hosp, region_subset)
rm(top_val, p, cols, last_update)
