




# create days from 3rd death data, state-level data
state_data %>%
  filter(deaths >= 5) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, deaths) %>%
  arrange(state, day) -> state_death_days

# define top_val
top_val <- round_any(x = max(state_death_days$day), accuracy = 10, f = ceiling)

# state days
ggplot(data = state_death_days, mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(5, 1000), breaks = c(5, 10, 100, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Fifth Death Confirmed",
    y = "Count of Confirmed Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_mortality/a_state.png", preset = "lg")
save_plots(filename = "results/low_res/log_mortality/a_state.png", preset = "lg", dpi = 72)

# clean-up
rm(state_death_days, top_val)

