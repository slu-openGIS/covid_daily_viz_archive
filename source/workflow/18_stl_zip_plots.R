# st louis zip code level plots

# =============================================================================

# load data
regional_zip_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_regional.geojson", crs = 4326,
                           stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

regional_counties <- st_read("data/MO_HEALTH_Covid_Tracking/data/county/daily_snapshot_mo.geojson") %>%
  st_transform(crs = 26915) %>%
  filter(GEOID %in% c("29099", "29183", "29189", "29510")) %>%
  select(GEOID, county)

regional_centroids <- regional_counties %>%
  st_centroid() %>%
  get_coords(crs = 26915)

st_geometry(regional_centroids) <- NULL

city_county_zip_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_city_county.geojson", crs = 4326,
                              stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

jeffco <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_jefferson_county.geojson", crs = 4326,
                           stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

st_charles <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_st_charles_county.geojson", crs = 4326,
                  stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

stl_city <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_stl_city.geojson", crs = 4326,
                      stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

stl_county <- st_read("data/MO_HEALTH_Covid_Tracking/data/zip/daily_snapshot_stl_county.geojson", crs = 4326,
                      stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

# =============================================================================

# store breaks
breaks <- classInt::classIntervals(regional_zip_sf$case_rate, n = 5, style = "quantile")

# identify minimum value and apply to breaks
breaks$brks[1] <- min(c(min(city_county_zip_sf$case_rate, na.rm = TRUE), 
                        min(regional_zip_sf$case_rate, na.rm = TRUE), 
                        min(st_charles$case_rate, na.rm = TRUE), 
                        min(jeffco$case_rate, na.rm = TRUE), 
                        min(stl_city$case_rate, na.rm = TRUE), 
                        min(stl_county$case_rate, na.rm = TRUE)))

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(regional_zip_sf, is.na(case_rate) == FALSE)
zip_na <- filter(regional_zip_sf, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        breaks = breaks, dig_lab = 2)

# create palette
pal <- RColorBrewer::brewer.pal(n = 5, name = "GnBu")
names(pal) <- levels(zip_valid$map_breaks)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  geom_sf(data = regional_counties, fill = NA, color = "black", size = .75) +
  geom_text_repel(data = regional_centroids, mapping = aes(x = x, y = y, label = county),
                  nudge_x = c(-35000, -20000, -40000, 10000),
                  nudge_y = c(-10000, 20000, -20000, -35000),
                  size = 6) +
  scale_fill_manual(values = pal, name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nRegional St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the included counties and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_regional.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_regional.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# map 14-day average rate
## create breaks
regional_zip_sf <- mutate(regional_zip_sf, case_avg_rate = ifelse(case_avg_rate < 0, NA, case_avg_rate))
zip_valid <- filter(regional_zip_sf, is.na(case_avg_rate) == FALSE)
zip_na <- filter(regional_zip_sf, is.na(case_avg_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_avg_rate", newvar = "map_breaks",
                        style = "quantile", classes = 5, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  geom_sf(data = regional_counties, fill = NA, color = "black", size = .75) +
  geom_text_repel(data = regional_centroids, mapping = aes(x = x, y = y, label = county),
                  nudge_x = c(-35000, -20000, -40000, 10000),
                  nudge_y = c(-10000, 20000, -20000, -35000),
                  size = 6) +
  scale_fill_brewer(palette = "RdPu", name = "Rate per 10,000") +
  labs(
    title = "14-day Average of New COVID-19 Cases\nby Regional St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the included counties and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/d_avg_map_regional.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/d_avg_map_regional.png", plot = p, preset = "lg", dpi = 72)


# =============================================================================

# modify regional objects
regional_centroids <- filter(regional_centroids, GEOID %in% c("29510", "29189"))
regional_counties <- filter(regional_counties, GEOID %in% c("29510", "29189"))

# =============================================================================

# clean-up
rm(p, regional_zip_sf)

# =============================================================================

## highlight focal zips
focal_zips <- filter(city_county_zip_sf, GEOID_ZCTA %in% c("63103", "63025"))
non_focal_zips <- filter(city_county_zip_sf, GEOID_ZCTA %in% c("63103", "63025") == FALSE)

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(city_county_zip_sf, is.na(case_rate) == FALSE)
zip_na <- filter(city_county_zip_sf, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        breaks = breaks, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  geom_sf(data = regional_counties, fill = NA, color = "black", size = .75) +
  scale_fill_manual(values = pal, name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nCore St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_core.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_core.png", plot = p, preset = "lg", dpi = 72)

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
  geom_label_repel(data = focal_zips, mapping = aes(x = case_rate, pvty_pct, label = GEOID_ZCTA),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   nudge_y = -4, 
                   nudge_x = .5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 2)) +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
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
  geom_label_repel(data = focal_zips, mapping = aes(x = case_rate, blk_pct, label = GEOID_ZCTA),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   nudge_y = -4, 
                   nudge_x = .5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 2)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  labs(
    title = "Reported COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
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
rm(city_county_zip_sf, focal_zips, non_focal_zips, zip_na, zip_valid,
   regional_centroids, regional_counties)
rm(p, top_val)

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(jeffco, is.na(case_rate) == FALSE)
zip_na <- filter(jeffco, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        breaks = breaks, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  scale_fill_manual(values = pal, name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nJefferson County ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Jefferson County and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_jefferson.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_jefferson.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(jeffco, p, zip_na, zip_valid)

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(st_charles, is.na(case_rate) == FALSE)
zip_na <- filter(st_charles, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        breaks = breaks, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  scale_fill_manual(values = pal, name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nSt. Charles County ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via St. Charles County and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_st_charles.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_st_charles.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(st_charles, p, zip_na, zip_valid)

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(stl_county, is.na(case_rate) == FALSE)
zip_na <- filter(stl_county, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        breaks = breaks, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  scale_fill_manual(values = pal, name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nSt. Louis County ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via St. Louis County and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_stl_county.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_stl_county.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(stl_county, p, zip_na, zip_valid)

# =============================================================================

# map reported rate
## create breaks
zip_valid <- filter(stl_city, is.na(case_rate) == FALSE)
zip_na <- filter(stl_city, is.na(case_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "case_rate", newvar = "map_breaks",
                        breaks = breaks, dig_lab = 2)

## create map
p <- ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = map_breaks)) +
  scale_fill_manual(values = pal, name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by \nSt. Louis City ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via St. Louis City and the U.S. Census Bureau"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/stl_zip/a_case_map_stl_city.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_zip/a_case_map_stl_city.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(stl_city, p, zip_na, zip_valid)

# =============================================================================

## clean-up
rm(breaks, pal)

