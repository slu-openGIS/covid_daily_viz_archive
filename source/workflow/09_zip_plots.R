# map st. louis zip codes

# map confirmed rate
ggplot(data = stl_city_zip_sf) +
  geom_sf(mapping = aes(fill = confirmed_rate)) +
  # geom_sf_label(mapping = aes(label = zip), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "GnBu", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by \nSt. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis\nConfirmed cases are those with a positive test as a proportion\n of the total population\nZCTA is a generalized version of a zip code used for mapping"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/zip/a_confirmed_map.png", preset = "lg")
save_plots(filename = "results/low_res/zip/a_confirmed_map.png", preset = "lg", dpi = 72)

# make copy for plotting
stl_city_zip_sf2 <- filter(stl_city_zip_sf, zip == "63103")
stl_city_zip_sf <- filter(stl_city_zip_sf, zip != "63103")

# plot poverty
ggplot() +
  geom_point(data = stl_city_zip_sf2, mapping = aes(x = confirmed_rate, pvty_pct), color = "#515151", size = 4) +
  geom_smooth(data = stl_city_zip_sf, mapping = aes(x = confirmed_rate, pvty_pct), 
              method = "lm", color = "#D95F02", size = 1.5, linetype = "dashed") +
  geom_point(data = stl_city_zip_sf, mapping = aes(x = confirmed_rate, pvty_pct), color = "#1B9E77", size = 4) +
  geom_label_repel(data = stl_city_zip_sf2, mapping = aes(x = confirmed_rate, pvty_pct, label = zip),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0,4), breaks = c(0,.5,1,1.5,2,2.5,3,3.5,4)) +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  labs(
    title = "Confirmed COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Confirmed Rate per 1,000 Residents",
    y = "Residents Below Poverty Line (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mapping\nZip 63103 is an outlier due to a nursing home outbreak; it is not included in the trend line"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/zip/b_poverty_plot.png", preset = "lg")
save_plots(filename = "results/low_res/zip/b_poverty_plot.png", preset = "lg", dpi = 72)

# plot race
ggplot() +
  geom_point(data = stl_city_zip_sf2, mapping = aes(x = confirmed_rate, blk_pct), color = "#515151", size = 4) +
  geom_smooth(data = stl_city_zip_sf, mapping = aes(x = confirmed_rate, blk_pct), 
              method = "lm", color = "#D95F02", size = 1.5, linetype = "dashed") +
  geom_point(data = stl_city_zip_sf, mapping = aes(x = confirmed_rate, blk_pct), color = "#1B9E77", size = 4) +
  geom_label_repel(data = stl_city_zip_sf2, mapping = aes(x = confirmed_rate, blk_pct, label = zip),
                   size = 6,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_continuous(limits = c(0,4), breaks = c(0,.5,1,1.5,2,2.5,3,3.5,4)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  labs(
    title = "Confirmed COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Confirmed Rate per 1,000 Residents",
    y = "African American Residents (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mappingn\nZip 63103 is an outlier due to a nursing home outbreak; it is not included in the trend line"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/zip/c_race_plot.png", preset = "lg")
save_plots(filename = "results/low_res/zip/c_race_plot.png", preset = "lg", dpi = 72)

# clean-up
rm(stl_city_zip_sf, stl_city_zip_sf2)
