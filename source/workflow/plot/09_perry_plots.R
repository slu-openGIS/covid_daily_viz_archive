

plot_date <- "2020-03-10"

county_data %>%
  filter(state == "Missouri") %>%
  filter(report_date >= plot_date) -> county_subset

# define top_val
top_val <- round_any(x = max(county_subset$confirmed_rate), accuracy = .25, f = ceiling)

# plot confirmed rate
ggplot(data = county_subset, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  gghighlight(geoid %in% c(29189, 29510, 29157, 29195), 
              label_params = list(size = 6),
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .25)) + 
  labs(
    title = "Confirmed COVID-19 Cases by Select MO Counties",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/misc/perry_saline_confirmed_plot.png", preset = "lg")
save_plots(filename = "results/low_res/misc/perry_saline_confirmed_plot.png", preset = "lg", dpi = 72)

# missouri days
county_confirmed_days %>%
  filter(state == "Missouri") -> county_subset

# define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

ggplot(data = county_subset, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29189", "29510", "29511", "29157", "29195"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 10000)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/misc/perry_log_plot.png", preset = "lg")
save_plots(filename = "results/low_res/misc/perry_log_plot.png", preset = "lg", dpi = 72)

rm(county_confirmed_days, top_val)

