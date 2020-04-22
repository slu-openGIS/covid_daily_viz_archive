# plot county level data

# =============================================================================

# load data
mo_sf <- st_read("data/county/daily_snapshot_mo.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003) %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  )

county_data <- read_csv("data/county/county_full.csv") %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  ) %>%
  mutate(geoid = as.character(geoid))

# =============================================================================

# define colors
pal <- brewer.pal(n = 8, name = "Set1")
cols <- c("29201" = pal[6], "29157" = pal[3], "29195" = pal[4],
          "29189" = pal[2], "29135" = pal[5], "29510" = pal[1])

# define focal metros
county_focal <- c("29510", "29189", "29157", "29195", "29135", "29201")

# =============================================================================

# map missouri rates
## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/a_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/a_case_map.png", plot = p, preset = "lg", dpi = 72)

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
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/e_mortality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/e_mortality_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# map case fatality rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_fatality_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "BuGn", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Case Fatality by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/i_case_fatality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/i_case_fatality_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create reference object
ref_county <- mo_sf
st_geometry(ref_county) <- NULL

# =============================================================================

# clean-up
rm(county_data, mo_sf, county_focal)
rm(top_val, pal, cols, p)
