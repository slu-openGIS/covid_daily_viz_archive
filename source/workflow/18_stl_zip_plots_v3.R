# st louis metro zip code level plots ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####
metro_zip_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_metro.geojson", crs = 4326) %>%
  st_transform(crs = 26915) %>%
  mutate(case_avg_rate = ifelse(case_avg_rate < 0, NA, case_avg_rate)) %>%
  mutate(case_rate = ifelse(GEOID_ZCTA == "63628", NA, case_rate)) %>%
  mutate(case_avg_rate = ifelse(GEOID_ZCTA == "63628", NA, case_avg_rate)) %>%
  mutate(case_rate = ifelse(GEOID_ZCTA == "63070", NA, case_rate)) %>%
  mutate(case_avg_rate = ifelse(GEOID_ZCTA == "63070", NA, case_avg_rate))

metro_counties <- st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo_xl.geojson") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) %>%
  select(GEOID, county)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# map case rate ####
## create breaks
metro_zip_sf <- map_breaks(metro_zip_sf, var = "case_rate", newvar = "map_breaks",
                    style = "quantile", classes = 5, dig_lab = 4)

## create map
p <- ggplot() +
  geom_sf(data = metro_counties, fill = "gray", color = NA) +
  geom_sf(data = metro_zip_sf, mapping = aes(fill = map_breaks)) +
  geom_sf(data = metro_counties, fill = NA, color = "black", size = 1.25) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000",
                    na.value = "gray") +
  labs(
    title = "Reported COVID-19 Cases",
    subtitle = paste0("Metro St. Louis ZCTA (Current as of ", as.character(date), ")"),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the included counties and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_metro.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_metro.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# map new case rate ####
## create breaks
metro_zip_sf <- map_breaks(metro_zip_sf, var = "case_avg_rate", newvar = "map_breaks",
                           style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = metro_counties, fill = "gray", color = NA) +
  geom_sf(data = metro_zip_sf, mapping = aes(fill = map_breaks)) +
  geom_sf(data = metro_counties, fill = NA, color = "black", size = 1.25) +
  scale_fill_brewer(palette = "RdPu", name = "Rate per 10,000",
                    na.value = "gray") +
  labs(
    title = "14-day Average of New COVID-19 Cases",
    subtitle = paste0("Metro St. Louis ZCTA (Current as of ", as.character(date), ")"),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the included counties and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/d_avg_map_metro.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/d_avg_map_metro.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up ####
rm(p, metro_counties, metro_zip_sf)

