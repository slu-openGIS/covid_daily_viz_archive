# plot metro level data

# =============================================================================

# load data
metro_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/metro_all/metro_full.csv")

# =============================================================================

# define colors
pal <- brewer.pal(n = 8, name = "Set1")
pal[6] <- "#FFD60C"
cols <- c("Cape Girardeau" = pal[6], "Columbia" = pal[3], "Jefferson City" = pal[4], "Joplin" = pal[7],
          "Kansas City" = pal[2], "Springfield" = pal[5], "St. Joseph" = pal[8], "St. Louis" = pal[1])

# =============================================================================

# subset data
## create end points
metro_points <- filter(metro_data, report_date == date) 

## create reporting change points
report_points <- filter(metro_data, report_date == as.Date("2020-04-15")) %>%
  mutate(text = ifelse(short_name == "St. Louis", "reporting change on 15 Apr", NA))

# =============================================================================

# create line label
report_line <- tibble(
  date = as.Date("2020-04-15"),
  case_rate = 4,
  mortality_rate = 0.18,
  case_fatality_rate = 11,
  text = "reporting change on 15 Apr"
)

# =============================================================================

# plot confirmed rate

## subset data
metro_subset <- filter(metro_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(metro_subset$case_rate), accuracy = 2, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_rate))

## create plot
p <- ggplot(metro_subset) +
  geom_line(mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text_repel(data = report_line, mapping = aes(x = date, y = case_rate, label = text),
                  nudge_y = 1, nudge_x = -25, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 2)) + 
  labs(
    title = "Reported COVID-19 Cases by Metro Area",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/metro/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data

## subset data
metro_data %>%
  calculate_days(group_var = "geoid", stat_var = "cases", val = 5) %>%
  select(day, report_date, short_name, cases) %>%
  arrange(short_name, day) -> metro_subset

## define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 5, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") -> metro_day_points

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) -> report_day_points

report_label <- filter(report_day_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, cases))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, cases))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(metro_day_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = day, y = cases), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = cases, label = text),
                  nudge_y = .3, nudge_x = -5, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(5, 38000), breaks = c(5,10,30,100,300,1000,3000,10000,30000), 
                labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text,
    x = "Days Since Fifth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/metro/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/c_case_log.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from first day where average confirmed infections were at least 5

## subset data
metro_data %>%
  calculate_days(group_var = "geoid", stat_var = "case_avg", val = 5) %>%
  select(day, report_date, short_name, case_avg) %>%
  arrange(short_name, day) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> metro_subset

# define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 5, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% metro_subset$short_name) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> metro_day_points

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) %>%
  filter(short_name %in% metro_subset$short_name) -> report_day_points

report_label <- filter(report_day_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, case_avg))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, case_avg))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = case_avg, color = factor_var), size = 2) +
  geom_point(metro_day_points, mapping = aes(x = day, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = day, y = case_avg), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = case_avg, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(.1, 1000), breaks = c(.1, .3, 1, 3, 10, 30, 100, 300, 1000), 
                labels = comma_format(accuracy = .2)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = date_breaks_log)) +
  labs(
    title = "Pace of New COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text,
    x = "Days Since Average of Five Cases Reached",
    y = "7-day Average of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/metro/d_case_log_avg.png", preset = "lg")
save_plots(filename = "results/low_res/metro/d_case_log_avg.png", preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rate
## subset data
metro_subset <- filter(metro_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(metro_subset$mortality_rate), accuracy = .04, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, mortality_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, mortality_rate))

## create plot
p <- ggplot() +
  geom_line(metro_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text_repel(data = report_line, mapping = aes(x = date, y = mortality_rate, label = text),
                  nudge_y = .05, nudge_x = -25, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .04)) +
  labs(
    title = "Reported COVID-19 Mortality by Metro Area",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 1,000",
    caption = caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/metro/f_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/f_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot days from 3rd confirmed death data

## subset data
metro_data %>%
  calculate_days(group_var = "geoid", stat_var = "deaths", val = 3) %>%
  select(day, report_date, short_name, deaths) %>%
  arrange(short_name, day) -> metro_subset

## define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 5, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> metro_day_points

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> report_day_points

report_label <- filter(report_day_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, deaths))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, deaths))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = deaths, color = factor_var), size = 2) +
  geom_point(metro_day_points, mapping = aes(x = day, y = deaths, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = day, y = deaths), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = deaths, label = text),
                  nudge_y = .5, nudge_x = -6, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(3, 2000), breaks = c(3, 10, 30, 100, 300, 1000), labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Deaths by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text,
    x = "Days Since Third Death Reported",
    y = "Count of Reported Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/g_mortality_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/g_mortality_log.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from first day where average deaths were over 3, metro-level data

## subset data
metro_data %>%
  calculate_days(group_var = "geoid", stat_var = "deaths_avg", val = 3) %>%
  select(day, report_date, short_name, deaths_avg) %>%
  arrange(short_name, day) %>%
  mutate(deaths_avg = ifelse(deaths_avg < .1, .1, deaths_avg)) -> metro_subset

## define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 5, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% unique(metro_subset$short_name)) %>%
  mutate(deaths_avg = ifelse(deaths_avg < .1, .1, deaths_avg)) -> metro_day_points

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> report_day_points

report_label <- filter(report_day_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, deaths_avg))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, deaths_avg))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = deaths_avg, color = factor_var), size = 2) +
  geom_point(metro_day_points, mapping = aes(x = day, y = deaths_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = day, y = deaths_avg), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = deaths_avg, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(.1, 30), breaks = c(.1, .3, 1, 3, 10, 30), 
                labels = comma_format(accuracy = .2)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = date_breaks_log)) +
  labs(
    title = "Pace of New COVID-19 Deaths by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = caption_text,
    x = "Days Since Average of Three Deaths Reported",
    y = "7-day Average of New Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/h_mortality_log_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/h_mortality_log_avg.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot case fatality rate

## re-subset data
metro_subset <- filter(metro_data, report_date >= plot_date)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_fatality_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(metro_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text_repel(data = report_line, mapping = aes(x = date, y = case_fatality_rate, label = text),
                  nudge_y = .75, nudge_x = 25, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by Metro Area",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = caption_text
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/metro/j_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/j_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(metro_data, metro_subset, metro_points, metro_day_points, report_day_points, report_points, 
   report_label, report_line)
rm(top_val, pal, cols, p)
