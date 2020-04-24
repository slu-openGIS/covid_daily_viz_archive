# metro st. louis data

# final data cleaning
plot_date <- "2020-03-10"
stl_detail <- filter(stl_detail, report_date >= plot_date) 

# define top_val
top_val <- round_any(x = max(stl_detail$confirmed_rate), accuracy = .25, f = ceiling)

# plot confirmed rate
ggplot(data = stl_detail, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29189", "29510", "29183", "29071", "17133", "17027", "17163"), 
              label_params = list(size = 6),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "County") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b")  +
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

# plot case fatality rate
ggplot(data = stl_detail, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c("29189", "29510", "29183", "29071"),
              label_params = list(size = 6),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "County") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) + 
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
rm(stl_detail, stl_sf, top_val)
