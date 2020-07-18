# individual data for Missouri

# =============================================================================

# load data
covid_race <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/mo_race_rates.csv")

# =============================================================================

# create factor
covid_race %>%
  mutate(value = as_factor(value)) %>%
  mutate(value = fct_relevel(value, "White", "Black", "Two or More")) -> covid_race

# =============================================================================

# plot morbidity rates, race

## define top_val
top_val <- round_any(x = max(covid_race$case_rate, na.rm = TRUE), accuracy = 100, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = value, y = case_rate, fill = value)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 100)) +
  labs(
    title = "Reported Cases by Race, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Estimated Rate per 100,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

# \nMissouri's data are missing race for ~17% of cases

## save plot
save_plots(filename = "results/high_res/mo_individual/a_race_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/mo_individual/a_race_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rates, race

## define top_val
top_val <- round_any(x = max(covid_race$mortality_rate, na.rm = TRUE), accuracy = 5, f = ceiling)

## create plot
p <- covid_race %>%
  ggplot(data = ., mapping = aes(x = value, y = mortality_rate, fill = value)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Reported Deaths by Race, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Estimated Rate per 100,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

# \nMissouri's data are missing race for ~2.5% of deaths

## save plot
save_plots(filename = "results/high_res/mo_individual/b_race_mortality.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/mo_individual/b_race_mortality.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(covid_race)
rm(top_val, p)
