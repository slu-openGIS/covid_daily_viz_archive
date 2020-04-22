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

# =============================================================================

# subset data
## create end points
metro_points <- filter(metro_data, report_date == date) %>%
  filter(short_name %in% metro_focal)

## create reporting change points
report_points <- filter(metro_data, report_date == as.Date("2020-04-15")) %>%
  filter(short_name %in% metro_focal) %>%
  mutate(text = ifelse(short_name == "St. Louis", "reporting change on 15 Apr", NA))

# =============================================================================

# plot confirmed rate

## subset data
metro_subset <- filter(metro_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(metro_subset$case_rate), accuracy = .5, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_rate))

## create plot
p <- ggplot(metro_subset) +
  geom_line(mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text(aes(as.Date("2020-04-15"), y = metro_case_rate_y, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .25)) + 
  labs(
    title = "Reported COVID-19 Cases by Metro Area",
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

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) -> report_points

report_label <- filter(report_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, cases))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, day, cases))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = cases), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = cases, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(10, 10000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/metro/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/c_case_log.png", plot = p, preset = "lg", dpi = 72)

## clean-up data objects
metro_points <- select(metro_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# create days from first day where average confirmed infections were at least 10

## subset data
metro_data %>%
  filter(case_avg >= 10) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, case_avg) %>%
  arrange(short_name, day) -> metro_subset

# define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 10, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> metro_points

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> report_points

report_label <- filter(report_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, case_avg))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, day, case_avg))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = case_avg, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = day, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = case_avg), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = case_avg, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(10, 1000), breaks = c(10, 30, 100, 300, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Reached",
    y = "7-day Average of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/metro/d_case_log_avg.png", preset = "lg")
save_plots(filename = "results/low_res/metro/d_case_log_avg.png", preset = "lg", dpi = 72)

## clean-up data objects
metro_points <- select(metro_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# plot mortality rate
## subset data
metro_subset <- filter(metro_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(metro_subset$mortality_rate), accuracy = .02, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, mortality_rate))

metro_data %>%
  filter(report_date == date) %>%
  filter(short_name %in% metro_focal) %>%
  mutate(factor_var = fct_reorder2(short_name, report_date, mortality_rate)) -> metro_points

## create plot
p <- ggplot() +
  geom_line(metro_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text(aes(as.Date("2020-04-15"), y = metro_mortality_rate_y, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .02)) +
  labs(
    title = "Reported COVID-19 Mortality by Metro Area",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/metro/f_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/f_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot days from 3rd confirmed death data

## subset data
metro_data %>%
  filter(deaths >= 3) %>%
  arrange(report_date) %>%
  group_by(short_name) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, short_name, deaths) %>%
  arrange(short_name, day) -> metro_subset

## define top_val
top_val <- round_any(x = max(metro_subset$day), accuracy = 10, f = ceiling)

## identify max day
metro_subset %>%
  group_by(short_name) %>%
  summarise(day = max(day)) %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> metro_points

## re-create reporting change points
report_points <- filter(metro_data, report_date == as.Date("2020-04-15")) %>%
  filter(short_name %in% metro_focal) %>%
  mutate(text = ifelse(short_name == "St. Louis", "reporting change on 15 Apr", NA))

## add day to report points
metro_subset %>%
  select(short_name, report_date, day) %>%
  left_join(report_points, ., by = c("short_name", "report_date")) %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> report_points

report_label <- filter(report_points, short_name == "St. Louis")

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, deaths))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, day, deaths))

## create plot
ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = deaths, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = day, y = deaths, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = deaths), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = deaths, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(3, 1000), breaks = c(3, 10, 30, 100, 300, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Deaths by Metro Area",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Third Death Reported",
    y = "Count of Reported Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/g_mortality_log.png", preset = "lg")
save_plots(filename = "results/low_res/metro/g_mortality_log.png", preset = "lg", dpi = 72)

## clean-up data objects
metro_points <- select(metro_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# plot case fatality rate

## re-subset data
metro_subset <- filter(metro_data, report_date >= plot_date)

## re-create metro points
metro_points <- filter(metro_data, report_date == date) %>%
  filter(short_name %in% metro_focal)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_fatality_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(metro_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(short_name %in% metro_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text(aes(as.Date("2020-04-15"), y = metro_case_fatality_rate_y, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by Metro Area",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/metro/j_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/j_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(metro_data, metro_subset, metro_points, metro_focal, report_points, report_label,
   metro_case_rate_y, metro_mortality_rate_y, metro_case_fatality_rate_y)
rm(top_val, pal, cols, p)

