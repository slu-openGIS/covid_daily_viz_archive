# plot data

# dependencies
library(ggplot2)

# plot confirmed rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = state_name)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  labs(
    title = "Confirmed COVID-19 Cases by State",
    subtitle = "2020-03-10 through 2020-03-24",
    x = "Date",
    y = "Rate of Confirmed Infections per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nConfirmed cases are those with a positive test as a proportion of the total population"
  )

ggsave(filename = "results/confirmed_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot case fatality rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = state_name)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = "2020-03-10 through 2020-03-24",
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nCase fatality is the percent of confirmed cases that result in death"
  )

ggsave(filename = "results/case_fatality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot mortality rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = mortality_rate)) +
  geom_line(mapping = aes(color = state_name)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  labs(
    title = "Confirmed COVID-19 Mortality by State",
    subtitle = "2020-03-10 through 2020-03-24",
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nMortality rate is the number of deaths as a proportion of the total population"
  )

ggsave(filename = "results/mortality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# map confirmed rate
ggplot() +
  geom_sf(data = detailed_sf, mapping = aes(fill = c_rate)) +
  scale_fill_distiller(palette = "Reds", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = "2020-03-10 through 2020-03-24",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  theme_void()

ggsave(filename = "results/confirmed_rate_metro_map.png", width = 8, height = 6, units = "in", dpi = 500)


