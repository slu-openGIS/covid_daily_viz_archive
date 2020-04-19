
metro_data %>%
  filter(confirmed >= 10) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, confirmed) %>%
  arrange(short_name, day) -> msa_confirmed_days

# define top_val
top_val <- round_any(x = max(msa_confirmed_days$day), accuracy = 10, f = ceiling)

ggplot(data = msa_confirmed_days, mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = short_name), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 10000)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/a_log_confirmed.png", preset = "lg")
save_plots(filename = "results/low_res/metro/a_log_confirmed.png", preset = "lg", dpi = 72)

metro_data %>%
  filter(confirmed_avg >= 10) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, confirmed_avg) %>%
  arrange(short_name, day) -> msa_avg_confirmed_days

# define top_val
top_val <- round_any(x = max(msa_avg_confirmed_days$day), accuracy = 10, f = ceiling)

ggplot(data = msa_avg_confirmed_days, mapping = aes(day, confirmed_avg)) +
  geom_line(mapping = aes(color = short_name), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Reached",
    y = "7-day Average of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/b_avg_log_confirmed.png", preset = "lg")
save_plots(filename = "results/low_res/metro/b_avg_log_confirmed.png", preset = "lg", dpi = 72)

metro_data %>%
  filter(deaths >= 5) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, deaths) %>%
  arrange(short_name, day) -> metro_death_days

top_val <- round_any(x = max(metro_death_days$day), accuracy = 10, f = ceiling)

ggplot(data = metro_death_days, mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = short_name), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(5, 1000), breaks = c(5, 10, 100, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Deaths by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Fifth Death Confirmed",
    y = "Count of Confirmed Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/c_mortality_log.png", preset = "lg")
save_plots(filename = "results/low_res/metro/c_mortality_log.png", preset = "lg", dpi = 72)

metro_data %>%
  filter(deaths_avg >= 5) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, deaths_avg) %>%
  arrange(short_name, day) -> metro_avg_death_days

top_val <- round_any(x = max(metro_avg_death_days$day), accuracy = 10, f = ceiling)

ggplot(data = metro_avg_death_days, mapping = aes(day, deaths_avg)) +
  geom_line(mapping = aes(color = short_name), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(5, 100), breaks = c(5, 10, 100), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Deaths by Missouri Metro",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Five Deaths Reached",
    y = "7-day Average of Confirmed Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/d_avg_mortality_log.png", preset = "lg")
save_plots(filename = "results/low_res/metro/d_avg_mortality_log.png", preset = "lg", dpi = 72)

rm(metro_data, msa_avg_confirmed_days, msa_confirmed_days, metro_avg_death_days, metro_death_days,
   top_val)
