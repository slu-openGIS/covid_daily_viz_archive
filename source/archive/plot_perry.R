county_full <- read_csv("data/county/county_full.csv")

plot_date <- "2020-03-10"
county_full <- filter(county_full, report_date >= plot_date) %>%
  filter(geoid %in% c(29189, 29510, 29157))

# plot confirmed rate
ggplot(data = county_full, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = county), size = 2)  +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 1), breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + 
  labs(
    title = "Confirmed COVID-19 Cases by Select MO Counties",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/misc/perry_confirmed_plot.png", preset = "lg")
save_plots(filename = "results/low_res/misc/perry_confirmed_plot.png", preset = "lg", dpi = 72)
