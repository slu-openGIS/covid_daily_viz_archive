# plot summary data

# final data cleaning
plot_date <- "2020-03-01"
state_data <- filter(state_data, report_date >= plot_date)

# plot confirmed rate
ggplot(data = state_data, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2", name = "State") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,70), breaks = c(0,10,20,30,40,50,60,70)) + 
  labs(
    title = "Confirmed COVID-19 Cases by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/state/b_confirmed_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/b_confirmed_rate.png", preset = "lg", dpi = 72)

# plot case fatality rate
ggplot(data = state_data, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2", name = "State") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,10,15,20)) +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/state/c_case_fatality_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/c_case_fatality_rate.png", preset = "lg", dpi = 72)

# plot mortality rate
ggplot(data = state_data, mapping = aes(x = report_date, y = mortality_rate)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2", name = "State") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,1.4), breaks = c(0,.2,.4,.6,.8,1,1.2,1.4)) +
  labs(
    title = "Confirmed COVID-19 Mortality by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nMortality rate is the number of deaths as a proportion of the total population"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/state/d_mortality_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/d_mortality_rate.png", preset = "lg", dpi = 72)

# map missouri rates
ggplot(data = mo_sf, mapping = aes(fill = confirmed_rate)) +
  geom_sf() +
  scale_fill_distiller(palette = "GnBu", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nKansas City is treated as a distinct county due to reporting practices\nConfirmed cases are those with a positive test as a proportion of the total population"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/state/a_mo_map.png", preset = "lg")
save_plots(filename = "results/low_res/state/a_mo_map.png", preset = "lg", dpi = 72)

# clean-up
rm(state_data, mo_sf, plot_date)
