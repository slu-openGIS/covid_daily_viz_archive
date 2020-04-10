
state_death_days <- read_csv("data/state/state_death.csv")
county_death_days <- read_csv("data/county/county_death.csv")

# st. louis metro
county_death_days %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) %>%
  ggplot(data = ., mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = county)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(3, 100)) +
  scale_x_continuous(limits = c(1,20), breaks = c(1,5,10,15,20))  +
  labs(
    title = "Pace of COVID-19 Deaths by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Third Confirmed Death",
    y = "Count of Deaths (Log)"
  )


# kansas city days
county_death_days %>%
  filter(geoid %in% c("20091", "20103", "20107", "20121", "20209",
                      "29013", "29025", "29037", "29047", "29049", 
                      "29095", "29107", "29165", "29177", "29511")) %>%
  ggplot(data = ., mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = county)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10(limits = c(3, 100)) +
  scale_x_continuous(limits = c(1,20), breaks = c(1, 5, 10, 15, 20))  +
  labs(
    title = "Pace of COVID-19 Deaths by Metro Kansas City County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Third Confirmed Death",
    y = "Count of Deaths (Log)"
  )

# kansas city days
county_death_days %>%
  filter(state == "Missouri") %>%
  ggplot(data = ., mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = county)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 100)) +
  scale_x_continuous(limits = c(1,20), breaks = c(1, 5, 10, 15, 20))  +
  labs(
    title = "Pace of COVID-19 Deaths by Metro Kansas City County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Confirmed Death",
    y = "Count of Deaths (Log)"
  )

# state days
ggplot(data = state_death_days, mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 1000)) +
  scale_x_continuous(limits = c(1,20), breaks = c(1, 5, 10, 15, 20))  +
  labs(
    title = "Pace of COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Confirmed Death",
    y = "Count of Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/misc/log_mortality_plot.png", preset = "lg")
save_plots(filename = "results/low_res/misc/log_mortality_plot.png", preset = "lg", dpi = 72)
