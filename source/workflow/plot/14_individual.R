
covid_race %>%
  mutate(value = as_factor(value)) %>%
  mutate(value = fct_relevel(value, "White", "Black", "Asian", "Two or More")) -> covid_race


ggplot(data = covid_race, mapping = aes(x = value, y = case_rate, fill = county)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Dark2", name = "County") +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = .5)) +
  labs(
    title = "Confirmed Cases by Race, St. Louis City & County",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Rate per 1,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and St. Louis County\nSt. Louis County's data are missing race for ~40% of cases\nSt. Louis City's data are missing race for ~6% of cases"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

save_plots(filename = "results/high_res/individual/a_race_case.png", preset = "lg")
save_plots(filename = "results/low_res/individual/a_race_case.png", preset = "lg", dpi = 72)

covid_race %>%
  filter(value %in% c("White", "Black")) %>%
  ggplot(data = ., mapping = aes(x = value, y = mortality_rate, fill = county)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Dark2", name = "County") +
  scale_y_continuous(limits = c(0, .25), breaks = seq(0, .25, by = .05)) +
  labs(
    title = "Confirmed Deaths by Race, St. Louis City & County",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Rate per 1,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and St. Louis County\nSt. Louis County's data are missing race for ~40% of cases\nSt. Louis City's data are missing race for ~6% of cases"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

save_plots(filename = "results/high_res/individual/a_race_mortality.png", preset = "lg")
save_plots(filename = "results/low_res/individual/a_race_mortality.png", preset = "lg", dpi = 72)

rm(covid_race, plot_date, save_plots, sequoia_theme)
