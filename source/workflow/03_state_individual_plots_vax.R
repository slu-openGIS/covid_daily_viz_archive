# =============================================================================

# load data
covid_race <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/mo_vaccine_race_rates.csv")
covid_totals <- read_csv(paste0("data/MO_HEALTH_Covid_Tracking/data/source/mo_daily_vaccines/",
                                "mo_total_vaccines_", date, ".csv"))

# =============================================================================

missing <- list(
  initiated_race = covid_race %>%
    filter(value == "Unknown, Race") %>%
    select(initiated) %>%
    pull(),
  initiated_latino = covid_race %>%
    filter(value == "Unknown, Ethnicity") %>%
    select(initiated) %>%
    pull(),
  initiated_total = covid_totals %>%
    filter(category %in% c("No Address Given", "Out-of-State") == FALSE) %>%
    group_by(report_date) %>%
    summarise(total = sum(initiated)) %>%
    select(total) %>%
    pull(),
  completed_race = covid_race %>%
    filter(value == "Unknown, Race") %>%
    select(completed) %>%
    pull(),
  completed_latino = covid_race %>%
    filter(value == "Unknown, Ethnicity") %>%
    select(completed) %>%
    pull(),
  completed_total = covid_totals %>%
    filter(category %in% c("No Address Given", "Out-of-State") == FALSE) %>%
    group_by(report_date) %>%
    summarise(total = sum(completed)) %>%
    select(total) %>%
    pull()
)

covid_race <- filter(covid_race, value %in% c("Unknown, Ethnicity", "Unknown, Race", "Other Race", "Two or More") == FALSE)

# =============================================================================

# create factor
covid_race <- covid_race %>%
  mutate(value = case_when(
    value == "Native" ~ "Native American",
    TRUE ~ as.character(value)
  ))

# =============================================================================

# plot initiated dose rates, race

## define top_val
top_val <- round_any(x = max(covid_race$initiated_rate, na.rm = TRUE), accuracy = 5000, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = reorder(value, -initiated_rate), y = initiated_rate)) +
      geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
               fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
      scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5000)) +
      labs(
        title = "Initiated Vaccinations by Race and Ethnicity, Missouri",
        subtitle = paste0("Current as of ", as.character(date)),
        x = "Race",
        y = "Vaccinations per 100,000 Individuals",
        caption = paste0("Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau",
                         "\nStatewide, ", round(missing$initiated_race/missing$initiated_total*100, 2),"% of race data and ",
                         round(missing$initiated_latino/missing$initiated_total*100, 2), "% of ethnicity data are missing")) +
      sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/mo_individual/c_race_vaccine_initiated.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/mo_individual/c_race_vaccine_initiated.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot initiated dose rates, race

## define top_val
top_val <- round_any(x = max(covid_race$completed_rate, na.rm = TRUE), accuracy = 2500, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = reorder(value, -completed_rate), y = completed_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 2500)) +
  labs(
    title = "Completed Vaccinations by Race and Ethnicity, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Vaccinations per 100,000 Individuals",
    caption = paste0("Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau",
                     "\nStatewide, ", round(missing$completed_race/missing$completed_total*100, 2),"% of race data and ",
                     round(missing$completed_latino/missing$completed_total*100, 2), "% of ethnicity data are missing")) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/mo_individual/c_race_vaccine_completed.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/mo_individual/c_race_vaccine_completed.png", plot = p, preset = "lg", dpi = 72)

rm(covid_race, missing, p, top_val)