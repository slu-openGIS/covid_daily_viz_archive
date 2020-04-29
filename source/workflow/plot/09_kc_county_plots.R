# metro st. louis data

# final data cleaning
plot_date <- "2020-03-10"
kc_detail <- filter(kc_detail, report_date >= plot_date)
kc_sf <- mutate(kc_sf, county = ifelse(GEOID %in% c("29511"), NA, county))

# create breaks
kc_sf <- map_breaks(kc_sf, var = "confirmed_rate", newvar = "confirmed_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)
kc_sf <- map_breaks(kc_sf, var = "case_fatality_rate", newvar = "case_fatality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

# map confirmed rate
ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = confirmed_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by\nKansas City Metro County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population\nKansas City is intentionally not labeled to increase readability of map\nFisher style breaks used to calculate legend categories\nKansas City is treated as a distinct county due to reporting practices"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/kc_metro/a_confirmed_map.png", preset = "lg")
save_plots(filename = "results/low_res/kc_metro/a_confirmed_map.png", preset = "lg", dpi = 72)

# define top_val
top_val <- round_any(x = max(kc_detail$confirmed_rate), accuracy = .25, f = ceiling)

# plot confirmed rate
ggplot(data = kc_detail, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("20209", "20103", "29511", "29107"),
              label_params = list(size = 6),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "County") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .25)) + 
  labs(
    title = "Confirmed COVID-19 Cases by Kansas City Metro County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population\nKansas City is treated as a distinct county due to reporting practices"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/kc_metro/b_confirmed_plot.png", preset = "lg")
save_plots(filename = "results/low_res/kc_metro/b_confirmed_plot.png", preset = "lg", dpi = 72)

# map case fatality rate
ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = case_fatality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "BuPu", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality by\nKansas City Metro County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death\nKansas City is intentionally not labeled to increase readability of map\nFisher style breaks used to calculate legend categories\nKansas City is treated as a distinct county due to reporting practices"
  )  +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/kc_metro/c_case_fatality_map.png", preset = "lg")
save_plots(filename = "results/low_res/kc_metro/c_case_fatality_map.png", preset = "lg", dpi = 72)

# plot case fatality rate
ggplot(data = kc_detail, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29511", "29095", "20091", "20209"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "County") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) + 
  labs(
    title = "COVID-19 Case Fatality by Metro Kansas City County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death\nKansas City is treated as a distinct county due to reporting practices\nInitial values for Wyandotte County not shown to increase readability of plot"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/kc_metro/d_case_fatality_plot.png", preset = "lg")
save_plots(filename = "results/low_res/kc_metro/d_case_fatality_plot.png", preset = "lg", dpi = 72)

# clean-up
rm(kc_sf, kc_detail, top_val)
