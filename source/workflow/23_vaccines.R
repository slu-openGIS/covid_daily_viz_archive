# vaccine data for Missouri and St. Louis

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####
vaccine_race <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/region_c_race_vaccine.csv")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# plot vaccination data for Region C ####

## store values
vaccine_values <- list(
  unknown = filter(vaccine_race, value == "Unknown") %>%
              pull(vaccine_pct),
  other = filter(vaccine_race, value == "Other") %>%
    pull(vaccine_pct),
  total = sum(vaccine_race$vaccine_est, na.rm = TRUE),
  date = slice(vaccine_race, 1) %>%
              pull(report_date)
)

## subset
vaccine_subset <- filter(vaccine_race, value %in% c("Asian", "Black", "White"))

## define top_val
top_val <- round_any(x = max(vaccine_subset$vaccine_rate, na.rm = TRUE), accuracy = 10, f = ceiling)

## create plot
p <- ggplot(data = vaccine_subset, mapping = aes(x = reorder(value, -vaccine_rate), y = vaccine_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .5, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 10)) +
  labs(
    title = "Vaccinations by Race and Ethnicity, Missouri Region C",
    subtitle = paste0("Current as of ", as.character(vaccine_values$date)),
    x = "Race",
    y = "Estimated Rate per 1,000 Individuals",
    caption = paste0("Plot by Christopher Prener, Ph.D.\nData via the St. Louis Pandemic Task Force and the U.S. Census Bureau",
                     "\n", vaccine_values$unknown,"% of race data are missing, and ", 
                     vaccine_values$other, "% are for 'other' racial identities\nRates for Pacific Islanders and Native Americans are listed as 0% by the Task Force")  
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/stl_individual/e_race_vaccine.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/e_race_vaccine.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(vaccine_race, vaccine_subset, vaccine_values)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up
rm(p, top_val)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data
mo_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo_vaccines.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# map first dose_rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "first_dose_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 3)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "Oranges", name = "Rate per 1,000") +
  labs(
    title = "First Vaccine Doses by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/r_first_dose_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/r_first_dose_map.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# map second dose_rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "second_dose_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 3)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "OrRd", name = "Rate per 1,000") +
  labs(
    title = "Second Vaccine Doses by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/t_second_dose_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/t_second_dose_map.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# map total dose_rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "total_dose_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 3)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "YlOrRd", name = "Rate per 1,000") +
  labs(
    title = "Total Vaccine Doses by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/v_total_dose_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/v_total_dose_map.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up
rm(p, mo_sf)
