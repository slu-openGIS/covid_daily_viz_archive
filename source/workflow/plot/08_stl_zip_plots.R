# st louis zip code level plots

# =============================================================================

# load data
city_county_zip_sf <- st_read("data/zip/daily_snapshot_city_county.geojson", crs = 4326,
                              stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

# =============================================================================

## highlight focal zips
focal_zips <- filter(city_county_zip_sf, zip %in% c("63103", "63025"))
non_focal_zips <- filter(city_county_zip_sf, zip %in% c("63103", "63025") == FALSE)

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(city_county_zip_sf, is.na(case_rate) == FALSE)
zip_na <- filter(city_county_zip_sf, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nSt. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date+1)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot poverty rates by zip

## define top_val
top_val <- round_any(x = max(city_county_zip_sf$case_rate, na.rm = TRUE), accuracy = 1, f = ceiling)

## plot poverty position = "jitter"
p <- ggplot() +
  geom_smooth(data = city_county_zip_sf, mapping = aes(x = case_rate, pvty_pct), 
              method = "lm", color = "#D95F02", size = 1.5, linetype = "dashed") +
  geom_point(data = focal_zips, mapping = aes(x = case_rate, pvty_pct), color = "#D95F02", size = 6) +
  geom_point(data = focal_zips, mapping = aes(x = case_rate, pvty_pct), color = "#1B9E77", size = 4) +
  geom_point(data = non_focal_zips, mapping = aes(x = case_rate, pvty_pct), color = "#1B9E77", size = 4,
             position = "jitter") +
  geom_label_repel(data = focal_zips, mapping = aes(x = case_rate, pvty_pct, label = zip),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   nudge_y = -4, 
                   nudge_x = .5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 1)) +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date+1)),
    x = "Reported Rate per 1,000 Residents",
    y = "Residents Below Poverty Line (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau\n63103 and 63025 have significant nursing home outbreaks"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/stl_zip/b_poverty_plot.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/b_poverty_plot.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot race

## plot race
p <- ggplot() +
  geom_smooth(data = city_county_zip_sf, mapping = aes(x = case_rate, blk_pct), 
              method = "lm", color = "#D95F02", size = 1.5, linetype = "dashed") +
  geom_point(data = focal_zips, mapping = aes(x = case_rate, blk_pct), color = "#D95F02", size = 6) +
  geom_point(data = focal_zips, mapping = aes(x = case_rate, blk_pct), color = "#1B9E77", size = 4) +
  geom_point(data = non_focal_zips, mapping = aes(x = case_rate, blk_pct), color = "#1B9E77", size = 4,
             position = "jitter") +
  geom_label_repel(data = focal_zips, mapping = aes(x = case_rate, blk_pct, label = zip),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   nudge_y = -4, 
                   nudge_x = .5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 1)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date+1)),
    x = "Reported Rate per 1,000 Residents",
    y = "African American Residents (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau\n63103 and 63025 have significant nursing home outbreaks"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/stl_zip/c_race_plot.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/c_race_plot.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(city_county_zip_sf, focal_zips, non_focal_zips, zip_na, zip_valid)
rm(p, top_val)
