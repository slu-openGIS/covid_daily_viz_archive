# plot summary data

# final data cleaning
plot_date <- "2020-03-01"
state_data <- filter(state_data, report_date >= plot_date)

# remove IL
if (date == "2020-04-10"){
  state_data <- mutate(state_data, 
                       confirmed_rate = ifelse(state == "Kansas" & report_date == "2020-04-10", NA, confirmed_rate),
                       mortality_rate = ifelse(state == "Kansas" & report_date == "2020-04-10", NA, mortality_rate),
                       case_fatality_rate = ifelse(state == "Kansas" & report_date == "2020-04-10", NA, case_fatality_rate))
}

# define top_val
top_val <- round_any(x = max(state_data$confirmed_rate), accuracy = 20, f = ceiling)

# plot confirmed rate
ggplot(data = state_data, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2", name = "State") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 20)) + 
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
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b") +
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

# define top_val
top_val <- round_any(x = max(state_data$mortality_rate), accuracy = .5, f = ceiling)

# plot mortality rate
ggplot(data = state_data, mapping = aes(x = report_date, y = mortality_rate)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2", name = "State") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .5)) +
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

# create breaks
mo_sf <- map_breaks(mo_sf, var = "confirmed_rate", newvar = "confirmed_breaks",
                   style = "fisher", classes = 5, dig_lab = 2)

# map missouri rates
ggplot(data = mo_sf, mapping = aes(fill = confirmed_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nKansas City is treated as a distinct county due to reporting practices\nConfirmed cases are those with a positive test as a proportion of the total population\nFisher style breaks used to calculate legend categories"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

save_plots(filename = "results/high_res/state/a_mo_map.png", preset = "lg")
save_plots(filename = "results/low_res/state/a_mo_map.png", preset = "lg", dpi = 72)

# clean-up
rm(mo_sf, plot_date, top_val)
