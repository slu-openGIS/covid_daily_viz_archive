# metro st. louis data

# final data cleaning
plot_date <- "2020-03-10"
stl_detail <- filter(stl_detail, report_date >= plot_date)
stl_sf <- mutate(stl_sf, county = ifelse(GEOID %in% c("29189", "29510"), NA, county))

# map confirmed rate
ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = confirmed_rate)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "GnBu", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population\nSt. Louis City and County are intentionally not labeled to increase readability of map"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/stl_metro/a_confirmed_map.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/a_confirmed_map.png", preset = "lg", dpi = 72)

# plot confirmed rate
ggplot(data = stl_detail, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29189", "29510", "17027", "17163" , "29183")) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 0.5), breaks = c(0,.1,.2,.3,.4,.5)) + 
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/stl_metro/b_confirmed_plot.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/b_confirmed_plot.png", preset = "lg", dpi = 72)

# map case fatality rate
ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = case_fatality_rate)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "BuPu", trans = "reverse", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality by Metro St. Louis County",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death\nSt. Louis City and County are intentionally not labeled to increase readability of map"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/stl_metro/c_case_fatality_map.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/c_case_fatality_map.png", preset = "lg", dpi = 72)

# plot case fatality rate
ggplot(data = stl_detail, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29189", "29510", "29183", "17163")) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 10), breaks = c(0,2,4,6,8,10)) + 
  labs(
    title = "COVID-19 Case Fatality by Metro St. Louis County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/stl_metro/d_case_fatality_plot.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/d_case_fatality_plot.png", preset = "lg", dpi = 72)

# clean-up
rm(stl_detail, stl_sf, plot_date)
