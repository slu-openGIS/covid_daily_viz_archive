# plot summary data

# plot confirmed rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = state)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  labs(
    title = "Confirmed COVID-19 Cases by State",
    subtitle = paste0("2020-01-24 through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nConfirmed cases are those with a positive test as a proportion of the total population"
  )

ggsave(filename = "results/summary/confirmed_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot case fatality rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = state)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0("2020-01-24 through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nCase fatality is the percent of confirmed cases that result in death"
  )

ggsave(filename = "results/summary/case_fatality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot mortality rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = mortality_rate)) +
  geom_line(mapping = aes(color = state)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  labs(
    title = "Confirmed COVID-19 Mortality by State",
    subtitle = paste0("2020-01-24 through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nMortality rate is the number of deaths as a proportion of the total population"
  )

ggsave(filename = "results/summary/mortality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

