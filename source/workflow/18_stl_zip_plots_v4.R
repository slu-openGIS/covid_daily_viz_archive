
city_county_zip_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_city_county.geojson", crs = 4326,
                              stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

regional_counties <- st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo.geojson") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID %in% c("29189", "29510")) %>%
  select(GEOID, county)

## create breaks
city_county_zip_sf <- map_breaks(city_county_zip_sf, var = "case_rate", newvar = "map_breaks",
                     style = "fisher", classes = 5, dig_lab = 3)

## plot
p <- ggplot() +
  geom_sf(data = city_county_zip_sf, mapping = aes(fill = map_breaks), size = .2) +
  geom_sf(data = regional_counties, fill = NA, size = .6, color = "black") +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("St. Louis City and County (Current as of ", as.character(date), ")"),
    caption = "Data via the included counties and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "current_zip.png", plot = p, preset = "lg")
