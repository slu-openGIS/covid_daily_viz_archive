# individual data for Missouri

# =============================================================================

# load data
covid_race <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/mo_race_rates.csv")

# =============================================================================

missing <- list(
  race_case = covid_race %>%
    filter(value %in% c("Unknown Race")) %>%
    select(cases_pct) %>%
    pull(),
  race_death = covid_race %>%
    filter(value %in% c("Unknown Race")) %>%
    select(deaths_pct) %>%
    pull(),  
  ethnic_case = covid_race %>%
    filter(value %in% c("Unknown Ethnicity")) %>%
    select(cases_pct) %>%
    pull(),
  ethnic_death = covid_race %>%
    filter(value %in% c("Unknown Ethnicity")) %>%
    select(deaths_pct) %>%
    pull()
)

covid_race <- filter(covid_race, value %in% c("Unknown Race", "Unknown Ethnicity") == FALSE)

# =============================================================================

# create factor
covid_race <- covid_race %>%
  mutate(value = case_when(
    value == "Native" ~ "Native American",
    TRUE ~ as.character(value)
  ))

# covid_race %>%
#  mutate(value = as_factor(value)) %>%
#  mutate(value = fct_relevel(value, "White", "Black", "Asian", "Native American",
#                             "Pacific Islander", "Two or More", "Latino")) -> covid_race

# =============================================================================

# plot morbidity rates, race

## define top_val
top_val <- round_any(x = max(covid_race$case_rate, na.rm = TRUE), accuracy = 500, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = reorder(value, -case_rate), y = case_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(3, "Set1")[2]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 500)) +
  labs(
    title = "Reported Cases by Race and Ethnicity, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Estimated Rate per 100,000 Individuals",
    caption = paste0("Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau",
                     "\nStatewide, ", round(missing$race_case, 2),"% of race data are missing, as are ", 
                     round(missing$ethnic_case, 2), "% of ethnicity data")  
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

# \nMissouri's data are missing race for ~17% of cases

## save plot
save_plots(filename = "results/high_res/mo_individual/a_race_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/mo_individual/a_race_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rates, race

## define top_val
top_val <- round_any(x = max(covid_race$mortality_rate, na.rm = TRUE), accuracy = 10, f = ceiling)

## create plot
p <- covid_race %>%
  ggplot(data = ., mapping = aes(x = reorder(value, -mortality_rate), y = mortality_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(3, "Set1")[3]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 10)) +
  labs(
    title = "Reported Deaths by Race and Ethnicity, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Estimated Rate per 100,000 Individuals",
    caption = paste0("Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau",
                     "\nStatewide, ", round(missing$race_death, 2),"% of race data are missing, as are ", 
                     round(missing$ethnic_death, 2), "% of ethnicity data")
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
