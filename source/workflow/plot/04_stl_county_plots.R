# plot st. louis metro level data

# =============================================================================

# load data
stl_sf <- st_read("data/metro/daily_snapshot_stl.geojson", crs = 4326,
                  stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003) %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  ) %>%
  mutate(county = ifelse(GEOID %in% c("29189"), NA, county))

# =============================================================================

# map st. louis rates
## create breaks
stl_sf <- map_breaks(stl_sf, var = "case_rate", newvar = "case_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = case_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/stl_metro/a_confirmed_map.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/a_confirmed_map.png", preset = "lg", dpi = 72)

# =============================================================================


# =============================================================================


# =============================================================================


# =============================================================================

# =============================================================================

# map case fatality rate
## create breaks
stl_sf <- map_breaks(stl_sf, var = "case_fatality_rate", newvar = "case_fatality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = case_fatality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "BuPu", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/stl_metro/i_case_fatality_map.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/i_case_fatality_map.png", preset = "lg", dpi = 72)

# =============================================================================



