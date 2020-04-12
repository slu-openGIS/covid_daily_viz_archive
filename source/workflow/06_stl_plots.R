# metro st. louis data

# final data cleaning
plot_date <- "2020-03-10"
stl_detail <- filter(stl_detail, report_date >= plot_date) 
stl_sf <- mutate(stl_sf, county = ifelse(GEOID %in% c("29189"), NA, county))

# create breaks
stl_sf <- map_breaks(stl_sf, var = "confirmed_rate", newvar = "confirmed_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)
stl_sf <- map_breaks(stl_sf, var = "case_fatality_rate", newvar = "case_fatality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

# map confirmed rate
ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = confirmed_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population\nSt. Louis County is intentionally not labeled to increase readability of map\nFisher style breaks used to calculate legend categories"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/stl_metro/a_confirmed_map.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/a_confirmed_map.png", preset = "lg", dpi = 72)

# define top_val
top_val <- round_any(x = max(stl_detail$confirmed_rate), accuracy = 1, f = ceiling)

# plot confirmed rate
ggplot(data = stl_detail, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29189", "29510", "29183", "29071", "17133", "17027"), 
              label_params = list(size = 6),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "County") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .25)) + 
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
  geom_sf(mapping = aes(fill = case_fatality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "BuPu", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death\nSt. Louis County is intentionally not labeled to increase readability of map\nFisher style breaks used to calculate legend categories"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/stl_metro/c_case_fatality_map.png", preset = "lg")
save_plots(filename = "results/low_res/stl_metro/c_case_fatality_map.png", preset = "lg", dpi = 72)

# plot case fatality rate
ggplot(data = stl_detail, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29189", "29510", "29183"),
              label_params = list(size = 6),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "County") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 10), breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
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
rm(stl_detail, stl_sf, plot_date, top_val)
