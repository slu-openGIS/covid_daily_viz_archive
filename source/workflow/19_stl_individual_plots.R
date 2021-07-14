# individual data for St. Louis City and County

# =============================================================================

# load data
covid_race <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/stl_race_rates.csv")
covid_race_gender <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/stl_race_gender_rates.csv")

# =============================================================================

# add native data
if (nrow(filter(covid_race, geoid == 29189 & value == "Native")) == 0){
  native_county <- dplyr::tibble(
    geoid = 29189,
    county = "St. Louis",
    value = "Native",
    cases = 0,
    case_rate = 0,
    deaths = NA,
    mortality_rate = NA,
    report_date = date
  )
  
  covid_race <- bind_rows(covid_race, native_county)
}

# =============================================================================

# create factor
covid_race %>%
  mutate(value = as_factor(value)) %>%
  mutate(value = fct_relevel(value, "White", "Black", "Latino", "Asian", "Native", "Two or More")) -> covid_race

# create factor
covid_race_gender %>%
  mutate(value = as_factor(value)) %>%
  mutate(value = fct_relevel(value, "White", "Black", "Asian")) -> covid_race_gender

# =============================================================================

# plot morbidity rates, race

## define top_val
top_val <- round_any(x = max(covid_race$case_rate, na.rm = TRUE), accuracy = 5, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = value, y = case_rate, fill = county)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set1", name = "County") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Reported Cases by Race, St. Louis City & County",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Rate per 1,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau\nData on cases among indigenous residents of St. Louis County not available"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

# \nSt. Louis County's data are missing race for ~40% of cases\nSt. Louis City's data are missing race for ~6% of cases

## save plot
save_plots(filename = "results/high_res/stl_individual/a_race_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/a_race_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rates, race

## define top_val
top_val <- round_any(x = max(covid_race$mortality_rate, na.rm = TRUE), accuracy = .2, f = ceiling)

## create plot
p <- covid_race %>%
  filter(value %in% c("White", "Black")) %>%
  ggplot(data = ., mapping = aes(x = value, y = mortality_rate, fill = county)) +
    geom_bar(position = "dodge", stat = "identity", width = .75) +
    scale_fill_brewer(palette = "Set1", name = "County") +
    scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .2)) +
    labs(
      title = "Reported Deaths by Race, St. Louis City & County",
      subtitle = paste0("Current as of ", as.character(date)),
      x = "Race",
      y = "Rate per 1,000 Individuals",
      caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis, St. Louis County, and the U.S. Census Bureau"
    ) +
    sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/stl_individual/b_race_mortality.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/b_race_mortality.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot morbidity rates, race and gender

## define top_val
top_val <- round_any(x = max(covid_race_gender$case_rate, na.rm = TRUE), accuracy = 5, f = ceiling)

## create plot
p <- ggplot(data = covid_race_gender, mapping = aes(x = value, y = case_rate, fill = sex)) +
  geom_bar(position = "dodge", stat = "identity", width = .75) +
  scale_fill_brewer(palette = "Set1", name = "Sex") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Reported Cases by Race and Sex, St. Louis City",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Rate per 1,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/stl_individual/c_race_sex_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/c_race_sex_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot morbidity rates, race and gender

## define top_val
top_val <- round_any(x = max(covid_race_gender$mortality_rate, na.rm = TRUE), accuracy = .1, f = ceiling)

## create plot
p <- covid_race_gender %>%
  filter(value %in% c("White", "Black")) %>%
  ggplot(., mapping = aes(x = value, y = mortality_rate, fill = sex)) +
    geom_bar(position = "dodge", stat = "identity", width = .75) +
    scale_fill_brewer(palette = "Set1", name = "Sex") +
    scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = .1)) +
    labs(
      title = "Reported Deaths by Race and Sex, St. Louis City",
      subtitle = paste0("Current as of ", as.character(date)),
      x = "Race",
      y = "Rate per 1,000 Individuals",
      caption = "Plot by Christopher Prener, Ph.D.\nData via the City of St. Louis and the U.S. Census Bureau"
    ) +
    sequoia_theme(base_size = 22, background = "white", map = FALSE)

# \nSt. Louis City's data are missing race for ~6% of cases

## save plot
save_plots(filename = "results/high_res/stl_individual/d_race_sex_mortality.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_individual/d_race_sex_mortality.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(covid_race, covid_race_gender, native_county)
rm(top_val, p)
