# individual data for St. Louis City and County

# =============================================================================

# load data
covid_race <-  read_csv("data/individual/stl_race_rates.csv")

# =============================================================================

# create factor
covid_race %>%
  mutate(value = as_factor(value)) %>%
  mutate(value = fct_relevel(value, "White", "Black", "Asian", "Two or More")) -> covid_race

# =============================================================================

# plot morbidity rates

## define top_val
top_val <- round_any(x = max(covid_race$case_rate, na.rm = TRUE), accuracy = .5, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = value, y = case_rate, fill = county)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Dark2", name = "County") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .5)) +
  labs(
    title = "Reported Cases by Race, St. Louis City & County",
    subtitle = paste0("Current as of ", as.character(date+1)),
    x = "Race",
    y = "Rate per 1,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau\nSt. Louis County's data are missing race for ~40% of cases\nSt. Louis City's data are missing race for ~6% of cases"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/stl_individual/a_race_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/a_race_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rates

## define top_val
top_val <- round_any(x = max(covid_race$mortality_rate, na.rm = TRUE), accuracy = .05, f = ceiling)

## create plot
p <- covid_race %>%
  filter(value %in% c("White", "Black")) %>%
  ggplot(data = ., mapping = aes(x = value, y = mortality_rate, fill = county)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Dark2", name = "County") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .05)) +
  labs(
    title = "Reported Deaths by Race, St. Louis City & County",
    subtitle = paste0("Current as of ", as.character(date+1)),
    x = "Race",
    y = "Rate per 1,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau\nSt. Louis County's data are missing race for ~40% of cases\nSt. Louis City's data are missing race for ~6% of cases"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/stl_individual/b_race_mortality.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/b_race_mortality.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(covid_race)
rm(top_val, p)
