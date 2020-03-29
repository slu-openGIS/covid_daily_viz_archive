# metro st. louis data

# final data cleaning
plot_date <- "2020-03-10"
kc_detail <- filter(kc_detail, report_date >= plot_date)
kc_sf <- mutate(kc_sf, county = ifelse(GEOID %in% c("29511"), NA, county))

# map confirmed rate
ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = confirmed_rate)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "Oranges", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Metro Kansas City County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population\n Kansas City is intentionally not labeled to increase readability of map\nKansas City is treated as a distinct county due to reporting practices"
  ) +
  theme_void()

ggsave(filename = "results/kc_metro/a_confirmed_map.png", width = 8, height = 6, units = "in", dpi = 500)

# plot confirmed rate
ggplot(data = kc_detail, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county))  +
  gghighlight(geoid %in% c("20107", "20209", "20103", "29511")) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 0.5)) + 
  labs(
    title = "Confirmed COVID-19 Cases by Metro Kansas City County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population\nKansas City is treated as a distinct county due to reporting practices"
  )

ggsave(filename = "results/kc_metro/b_confirmed_plot.png", width = 8, height = 6, units = "in", dpi = 500)

# map case fatality rate
ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = case_fatality_rate)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "Reds", trans = "reverse", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality by Metro Kansas City County",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death\nKansas City is intentionally not labeled to increase readability of map\nKansas City is treated as a distinct county due to reporting practices"
  ) +
  theme_void()

ggsave(filename = "results/kc_metro/c_case_fatality_map.png", width = 8, height = 6, units = "in", dpi = 500)

# plot case fatality rate
ggplot(data = kc_detail, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = county))  +
  gghighlight(geoid %in% c("20209", "29095", "20091")) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(
    title = "COVID-19 Case Fatality by Metro Kansas City County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death\nKansas City is treated as a distinct county due to reporting practices\nInitial values for Wyandotte County not shown to increase readability of plot"
  )

ggsave(filename = "results/kc_metro/d_case_fatality_plot.png", width = 8, height = 6, units = "in", dpi = 500)
