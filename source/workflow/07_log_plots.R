# log plots

# st. louis metro
county_confirmed_days %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) %>%
  ggplot(data = ., mapping = aes(day, confirmed)) +
    geom_line(mapping = aes(color = county)) +
    gghighlight(geoid %in% c("29189", "29510", "29183")) +
    scale_color_brewer(palette = "Set1") +
    scale_y_log10(limits = c(1, 1000)) +
    scale_x_continuous(limits = c(1,25), breaks = c(1, 5, 10, 15, 20, 25))  +
    labs(
      title = "Pace of COVID-19 Cases by Metro St. Louis County",
      subtitle = paste0("Current as of ", as.character(date)),
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population",
      x = "Days Since First Case Confirmed",
      y = "Count of Confirmed Cases"
    )

ggsave(filename = "results/log_confirmed/b_st_louis.png", width = 8, height = 6, units = "in", dpi = 500)

# kansas city days
county_confirmed_days %>%
  filter(geoid %in% c("20091", "20103", "20107", "20121", "20209",
                      "29013", "29025", "29037", "29047", "29049", 
                      "29095", "29107", "29165", "29177", "29511")) %>%
  ggplot(data = ., mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = county)) +
  gghighlight(geoid %in% c("29511", "20091", "20209" , "29095")) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10(limits = c(1, 1000)) +
  scale_x_continuous(limits = c(1,25), breaks = c(1, 5, 10, 15, 20, 25))  +
  labs(
    title = "Pace of COVID-19 Cases by Metro Kansas City County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population",
    x = "Days Since First Case Confirmed",
    y = "Count of Confirmed Cases"
  )

ggsave(filename = "results/log_confirmed/c_kansas_city.png", width = 8, height = 6, units = "in", dpi = 500)

# state days
ggplot(data = state_confirmed_days, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = state)) +
  gghighlight(state %in% c("Illinois", "Kansas", "Missouri")) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10(limits = c(1, 10000)) +
  scale_x_continuous(limits = c(1,75), breaks = c(0,15,30,45,60,75))  +
  labs(
    title = "Pace of COVID-19 Cases by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population",
    x = "Days Since First Case Confirmed",
    y = "Count of Confirmed Cases"
  )

ggsave(filename = "results/log_confirmed/a_state.png", width = 8, height = 6, units = "in", dpi = 500)

# st. louis metro
county_death_days %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) %>%
  ggplot(data = ., mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = county)) +
  gghighlight(geoid %in% c("29189", "29510")) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10(limits = c(1, 10)) +
  scale_x_continuous(limits = c(1,10), breaks = c(1, 2, 4, 6, 8, 10))  +
  labs(
    title = "Pace of COVID-19 Deaths by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population",
    x = "Days Since First Confirmed Death",
    y = "Count of Deaths"
  )

ggsave(filename = "results/log_deaths/b_st_louis.png", width = 8, height = 6, units = "in", dpi = 500)

# kansas city days
county_death_days %>%
  filter(geoid %in% c("20091", "20103", "20107", "20121", "20209",
                      "29013", "29025", "29037", "29047", "29049", 
                      "29095", "29107", "29165", "29177", "29511")) %>%
  ggplot(data = ., mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = county)) +
  gghighlight(geoid %in% c("20209")) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10(limits = c(1, 20)) +
  scale_x_continuous(limits = c(1,20), breaks = c(1, 5, 10, 15, 20))  +
  labs(
    title = "Pace of COVID-19 Deaths by Metro Kansas City County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population",
    x = "Days Since First Confirmed Death",
    y = "Count of Deaths"
  )

ggsave(filename = "results/log_deaths/c_kansas_city.png", width = 8, height = 6, units = "in", dpi = 500)

# state days
ggplot(data = state_death_days, mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = state)) +
  gghighlight(state %in% c("Illinois", "Kansas", "Missouri")) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10(limits = c(1, 100)) +
  scale_x_continuous(limits = c(1,20), breaks = c(1, 5, 10, 15, 20))  +
  labs(
    title = "Pace of COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population",
    x = "Days Since First Confirmed Death",
    y = "Count of Deaths"
  )

ggsave(filename = "results/log_deaths/a_state.png", width = 8, height = 6, units = "in", dpi = 500)

  