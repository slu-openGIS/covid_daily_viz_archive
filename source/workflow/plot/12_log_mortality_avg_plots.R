




# create days from first day where average deaths were over 5, state-level data
state_data %>%
  filter(deaths_avg >= 5) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, last_update, deaths_avg) %>%
  arrange(state, day) -> state_avg_death_days

# define top_val
top_val <- round_any(x = max(state_avg_death_days$day), accuracy = 10, f = ceiling)

# state days
ggplot(data = state_avg_death_days, mapping = aes(day, deaths_avg)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(5, 1000), breaks = c(5, 10, 100, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Confirmed",
    y = "7-day Average of New Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_mortality_avg/a_state.png", preset = "lg")
save_plots(filename = "results/low_res/log_mortality_avg/a_state.png", preset = "lg", dpi = 72)

# clean-up
rm(state_data, county_data, state_avg_death_days, top_val)
