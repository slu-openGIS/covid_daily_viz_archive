# log plots

county_data <- read_csv("data/county/county_full.csv")

# create days from first day where average confirmed infections were at least 10, county-level data
county_data %>%
  filter(confirmed_avg >= 10) %>%
  filter(county != "Unassigned") %>%
  arrange(report_date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, geoid, county, state, confirmed_avg) %>%
  arrange(state, county, day) -> county_avg_confirmed_days

# st. louis metro
## subset
county_avg_confirmed_days %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510")) -> county_subset

## define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

## plot
ggplot(data = county_subset, mapping = aes(day, confirmed_avg)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29189", "29510", "29183", "17163", "29099"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 300)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Confirmed",
    y = "7-day Average of New Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/log_confirmed_avg/b_st_louis.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed_avg/b_st_louis.png", preset = "lg", dpi = 72)

# Kansas City Metro
## subset
county_avg_confirmed_days %>%
  filter(geoid %in% c("20091", "20103", "20107", "20121", "20209",
                      "29013", "29025", "29037", "29047", "29049", 
                      "29095", "29107", "29165", "29177", "29511")) -> county_subset

## define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

## plot
ggplot(data = county_subset, mapping = aes(day, confirmed_avg)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29511", "20091", "20209" , "29095", "20103"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 300)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by Kansas City Metro County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Confirmed",
    y = "7-day Average of New Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save
save_plots(filename = "results/high_res/log_confirmed_avg/c_kansas_city.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed_avg/c_kansas_city.png", preset = "lg", dpi = 72)

# missouri days
## subset data
county_avg_confirmed_days %>%
  filter(state == "Missouri") -> county_subset

##  define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 10, f = ceiling)

## plot
ggplot(data = county_subset, mapping = aes(day, confirmed_avg)) +
  geom_line(mapping = aes(color = county), size = 2) +
  gghighlight(geoid %in% c("29189", "29510", "29511", "29019", "29095", "29077", "29183", "29099"),
              label_params = list(size = 6, nudge_x = 1, nudge_y = .1),
              use_group_by = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(10, 300)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Confirmed",
    y = "7-day Average of New Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save 
save_plots(filename = "results/high_res/log_confirmed_avg/d_missouri.png", preset = "lg")
save_plots(filename = "results/low_res/log_confirmed_avg/d_missouri.png", preset = "lg", dpi = 72)

# clean-up
rm(county_subset, county_avg_confirmed_days, top_val)
