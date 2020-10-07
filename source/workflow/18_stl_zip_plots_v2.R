# st louis zip code level plots

# =============================================================================

# load data
regional_zip_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_regional.geojson", crs = 4326,
                           stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) %>%
  filter(is.na(case_rate) == FALSE)

regional_counties <- st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo.geojson") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID %in% c("29099", "29183", "29189", "29510")) %>%
  select(GEOID, county)

zip_centroids <- regional_zip_sf %>%
  st_centroid() %>%
  get_coords(crs = 26915) %>%
  select(GEOID_ZCTA)

zip_centroids <- st_intersection(zip_centroids, regional_counties) %>%
  select(GEOID_ZCTA, county)

st_geometry(zip_centroids) <- NULL

regional_zip_sf <- left_join(regional_zip_sf, zip_centroids, by = "GEOID_ZCTA")

## clean-up
rm(zip_centroids)

# =============================================================================

# plot poverty rates by zip

regional_zip_sf <- filter(regional_zip_sf, case_rate < 45)

## define top_val
top_val <- round_any(x = max(regional_zip_sf$case_rate, na.rm = TRUE), accuracy = 5, f = ceiling)

## plot poverty position = "jitter"
p <- ggplot(data = regional_zip_sf, mapping = aes(x = case_rate, pvty_pct, color = county)) +
  geom_smooth(method = "lm", size = 1.5, linetype = "dashed") +
  geom_point(size = 4, position = "jitter") +
  scale_color_brewer(palette = "Set1", name = "County") +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, by = 10)) +
  facet_wrap(~county) +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Reported Rate per 1,000 Residents",
    y = "Residents Below Poverty Line (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the included counties and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/stl_zip/b_poverty_plot.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/b_poverty_plot.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot poverty rates by zip

## plot poverty position = "jitter"
p <- ggplot(data = regional_zip_sf, mapping = aes(x = case_rate, blk_pct, color = county)) +
  geom_smooth(method = "lm", size = 1.5, linetype = "dashed") +
  geom_point(size = 4, position = "jitter") +
  scale_color_brewer(palette = "Set1", name = "County") +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20)) +
  facet_wrap(~county) +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Reported Rate per 1,000 Residents",
    y = "African American Residents (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the included counties and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/stl_zip/c_race_plot.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/c_race_plot.png", plot = p, preset = "lg", dpi = 72)
