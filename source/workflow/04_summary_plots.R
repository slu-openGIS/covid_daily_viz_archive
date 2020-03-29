# plot summary data

# final data cleaning
plot_date <- "2020-03-01"
state_data <- filter(state_data, report_date >= plot_date)

# plot confirmed rate
ggplot(data = state_data, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = state)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,30), breaks = c(0,5,10,15,20,25,30)) + 
  labs(
    title = "Confirmed COVID-19 Cases by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
  )

ggsave(filename = "results/state/b_confirmed_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot case fatality rate
ggplot(data = state_data, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = state)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death"
  )

ggsave(filename = "results/state/c_case_fatality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot mortality rate
ggplot(data = state_data, mapping = aes(x = report_date, y = mortality_rate)) +
  geom_line(mapping = aes(color = state)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,.4), breaks = c(0,.05,.1, .15,.2,.25,.3,.35,.4)) +
  labs(
    title = "Confirmed COVID-19 Mortality by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nMortality rate is the number of deaths as a proportion of the total population"
  )

ggsave(filename = "results/state/d_mortality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# map missouri rates
ggplot(data = mo_sf, mapping = aes(fill = confirmed_rate)) +
  geom_sf() +
  scale_fill_distiller(palette = "Oranges", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  theme_void()

ggsave(filename = "results/state/a_mo_map.png", width = 8, height = 6, units = "in", dpi = 500)
