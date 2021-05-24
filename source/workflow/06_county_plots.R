# plot county level data

# =============================================================================

# load data
mo_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")

# =============================================================================

# map missouri rates ####
## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 3)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/a_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/a_case_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# map missouri new case rates ####
## clean up NAs
mo_sf <- mutate(mo_sf, case_avg_rate = ifelse(case_avg_rate < 0, NA, case_avg_rate))

## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_avg_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 3)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "RdPu", name = "Rate per 100,000", na.value = "grey70") +
  labs(
    title = "7-day Average of New COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/d_new_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/d_new_case_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# map mortality rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "mortality_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "YlGn", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Mortality by Missouri County",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/g_mortality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/g_mortality_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# map case fatality rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_fatality_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "BuGn", name = "Percent") +
  labs(
    title = "Reported COVID-19 Case Fatality by Missouri County",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map2
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/l_case_fatality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/l_case_fatality_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(mo_sf, p)
