# map st. louis zip codes

# read data
stl_zip <- st_read("data/zip/zip_stl_city.geojson", stringsAsFactors = FALSE)

# map confirmed rate
ggplot(data = stl_zip) +
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

# 
stl_zip2 <- filter(stl_zip, zip != "63103")

# plot poverty
ggplot() +
  geom_point(data = stl_zip, mapping = aes(x = confirmed_rate, pvty_pct), color = "#515151", size = 2) +
  geom_point(data = stl_zip2, mapping = aes(x = confirmed_rate, pvty_pct), color = "#1B9E77", size = 2) +
  geom_smooth(data = stl_zip2, mapping = aes(x = confirmed_rate, pvty_pct), method = "lm", color = "#D95F02", size = 2) +
  scale_x_continuous(limits = c(0,3), breaks = c(0,.5,1,1.5,2,2.5,3)) +
  scale_y_continuous(limits = c(0,60)) +
  labs(
    title = "Confirmed COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Confirmed Rate per 1,000 Residents",
    y = "Residents Below Poverty Line (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mapping"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/zip/b_poverty_plot.png", preset = "lg")
save_plots(filename = "results/low_res/zip/b_poverty_plot.png", preset = "lg", dpi = 72)

# plot race
ggplot() +
  geom_point(data = stl_zip, mapping = aes(x = confirmed_rate, blk_pct), color = "#515151", size = 2) +
  geom_point(data = stl_zip2, mapping = aes(x = confirmed_rate, blk_pct), color = "#1B9E77", size = 2) +
  geom_smooth(data = stl_zip2, mapping = aes(x = confirmed_rate, blk_pct), method = "lm", color = "#D95F02", size = 2) +
  scale_x_continuous(limits = c(0,3), breaks = c(0,.5,1,1.5,2,2.5,3)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(
    title = "Confirmed COVID-19 Cases by St. Louis ZCTA",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Confirmed Rate per 1,000 Residents",
    y = "African American Residents (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis\nConfirmed cases are those with a positive test as a proportion of the total population\nZCTA is a generalized version of a zip code used for mapping"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/zip/c_race_plot.png", preset = "lg")
save_plots(filename = "results/low_res/zip/c_race_plot.png", preset = "lg", dpi = 72)

