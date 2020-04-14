# log plots

# create days from 10th confirmed infection data, county-level data
county_data %>%
  filter(confirmed >= 10) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, geoid, county, state, last_update, 
         confirmed, confirmed_rate) %>%
  arrange(state, county, day) -> county_confirmed_days

# st. louis metro
county_confirmed_days %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) -> county_subset

# define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

ggplot(data = county_subset, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29189", "29510", "29183", "17163", "29099"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 10000)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_confirmed/b_st_louis.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed/b_st_louis.png", preset = "lg", dpi = 72)

# kansas city days
county_confirmed_days %>%
  filter(geoid %in% c("20091", "20103", "20107", "20121", "20209",
                      "29013", "29025", "29037", "29047", "29049", 
                      "29095", "29107", "29165", "29177", "29511")) -> county_subset

# define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

ggplot(data = county_subset, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29511", "20091", "20209" , "29095", "20103"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 1000)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Metro Kansas City County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_confirmed/c_kansas_city.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed/c_kansas_city.png", preset = "lg", dpi = 72)

# missouri days
county_confirmed_days %>%
  filter(state == "Missouri") -> county_subset

# define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

ggplot(data = county_subset, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29189", "29510", "29511", "29019", "29095", "29077", "29183", "29099"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 10000)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_confirmed/d_missouri.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed/d_missouri.png", preset = "lg", dpi = 72)

# create days from 10th confirmed infection data, state-level data
state_data %>%
  filter(confirmed >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, last_update, confirmed) %>%
  arrange(state, day) -> state_confirmed_days

# define top_val
top_val <- round_any(x = max(state_confirmed_days$day), accuracy = 10, f = ceiling)

# state days
ggplot(data = state_confirmed_days, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = state), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 100000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_confirmed/a_state.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed/a_state.png", preset = "lg", dpi = 72)

# clean-up
rm(state_confirmed_days, county_subset, top_val)
