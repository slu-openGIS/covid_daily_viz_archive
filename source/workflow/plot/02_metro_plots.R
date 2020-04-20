# plot metro level data

# =============================================================================

# load data
metro_data <- read_csv("data/metro_all/metro_full.csv") %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  )

# =============================================================================

# define colors
pal <- brewer.pal(n = 8, name = "Set1")
cols <- c("Cape Girardeau" = pal[6], "Columbia" = pal[3], "Jefferson City" = pal[4], "Joplin" = pal[7],
          "Kansas City" = pal[2], "Springfield" = pal[5], "St. Joseph" = pal[8], "St. Louis" = pal[1])

# define focal metros
metro_focal <- c("Columbia", "Jefferson City", "Kansas City", "Springfield", "St. Louis")

# subset data
## limit dates included
metro_subset <- filter(metro_data, report_date >= plot_date)

## create end points
metro_points <- filter(metro_data, report_date == date) %>%
  filter(short_name %in% metro_focal)

# =============================================================================

# plot confirmed rate
## define top_val
top_val <- round_any(x = max(metro_subset$case_rate), accuracy = .25, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_rate))

## create plot
p <- ggplot(metro_subset) +
  geom_line(mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .25)) + 
  labs(
    title = "Reported COVID-19 Cases by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/metro/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data
## subset data
metro_data %>%
  filter(cases >= 10) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, cases) %>%
  arrange(short_name, day) -> metro_subset

## define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 5, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") -> metro_points

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, cases))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, day, cases))

## create plot
p <- ggplot(metro_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(10, 10000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Confirmed",
    y = "Count of Confirmed Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/metro/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/c_case_log.png", plot = p, preset = "lg", dpi = 72)

## clean-up state_points
metro_points <- select(metro_points, -day)

# =============================================================================

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

rm(metro_data, metro_subset, metro_points, msa_avg_confirmed_days, msa_confirmed_days, 
   metro_avg_death_days, metro_death_days, top_val)
