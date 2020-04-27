# map st. louis zip codes

# create breaks
zip_valid <- filter(city_county_zip_sf, is.na(confirmed_rate) == FALSE)
zip_na <- filter(city_county_zip_sf, is.na(confirmed_rate) == TRUE)
zip_valid <- map_breaks(zip_valid, var = "confirmed_rate", newvar = "confirmed_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

# map confirmed rate
ggplot() +
  geom_sf(data = zip_na, fill = "#9d9d9d") +
  geom_sf(data = zip_valid, mapping = aes(fill = confirmed_breaks)) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by \nSt. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and St. Louis County\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mapping"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/zip/a_confirmed_map.png", preset = "lg")
save_plots(filename = "results/low_res/zip/a_confirmed_map.png", preset = "lg", dpi = 72)

# define top_val
top_val <- round_any(x = max(city_county_zip_sf$confirmed_rate, na.rm = TRUE), accuracy = .5, f = ceiling)

# make copy for plotting
city_county_zip_sf2 <- filter(city_county_zip_sf, zip == "63103")
city_county_zip_sf <- filter(city_county_zip_sf, zip != "63103")

# plot poverty
ggplot() +
  geom_point(data = city_county_zip_sf2, mapping = aes(x = confirmed_rate, pvty_pct), color = "#515151", size = 4) +
  geom_smooth(data = city_county_zip_sf, mapping = aes(x = confirmed_rate, pvty_pct), 
              method = "lm", color = "#D95F02", size = 1.5, linetype = "dashed") +
  geom_point(data = city_county_zip_sf, mapping = aes(x = confirmed_rate, pvty_pct), color = "#1B9E77", size = 4) +
  geom_label_repel(data = city_county_zip_sf2, mapping = aes(x = confirmed_rate, pvty_pct, label = zip),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .5)) +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  labs(
    title = "Confirmed COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Confirmed Rate per 1,000 Residents",
    y = "Residents Below Poverty Line (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and St. Louis County\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mapping\nZip 63103 is an outlier due to a nursing home outbreak; it is not included in the trend line"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/zip/b_poverty_plot.png", preset = "lg")
save_plots(filename = "results/low_res/zip/b_poverty_plot.png", preset = "lg", dpi = 72)

# plot race
ggplot() +
  geom_point(data = city_county_zip_sf2, mapping = aes(x = confirmed_rate, blk_pct), color = "#515151", size = 4) +
  geom_smooth(data = city_county_zip_sf, mapping = aes(x = confirmed_rate, blk_pct), 
              method = "lm", color = "#D95F02", size = 1.5, linetype = "dashed") +
  geom_point(data = city_county_zip_sf, mapping = aes(x = confirmed_rate, blk_pct), color = "#1B9E77", size = 4) +
  geom_label_repel(data = city_county_zip_sf2, mapping = aes(x = confirmed_rate, blk_pct, label = zip),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .5)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  labs(
    title = "Confirmed COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Confirmed Rate per 1,000 Residents",
    y = "African American Residents (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and St. Louis County\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mappingn\nZip 63103 is an outlier due to a nursing home outbreak; it is not included in the trend line"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/zip/c_race_plot.png", preset = "lg")
save_plots(filename = "results/low_res/zip/c_race_plot.png", preset = "lg", dpi = 72)

# clean-up
rm(city_county_zip_sf, city_county_zip_sf2, zip_na, zip_valid, top_val)
