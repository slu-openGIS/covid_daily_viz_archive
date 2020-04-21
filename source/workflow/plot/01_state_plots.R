# plot state level data

# =============================================================================

# load data

state_data <- read_csv("data/state/state_full.csv") %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  )

# =============================================================================

# define colors
pal <- brewer.pal(n = 4, name = "Set1")
cols <- c("Illinois" = pal[1], "Kansas" = pal[2], "Missouri" = pal[3], "Oklahoma" = pal[4])

# =============================================================================

# create points
## create end points
state_points <- filter(state_data, report_date == date)

## create reporting change points
report_points <- filter(state_data, report_date == as.Date("2020-04-15")) %>%
  mutate(text = ifelse(state == "Illinois", "reporting change on 15 Apr", NA))

# =============================================================================

# plot confirmed rate
## subset data
state_subset <- filter(state_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$case_rate), accuracy = 25, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, case_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text(aes(as.Date("2020-04-15"), y = 225, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 25)) + 
  labs(
    title = "Reported COVID-19 Cases by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data, state-level data
## subset data
state_data %>%
  filter(cases >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, cases) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day)) %>%
  left_join(state_points, ., by = "state") -> state_points

## add day to report points
state_subset %>%
  select(state, report_date, day) %>%
  left_join(report_points, ., by = c("state", "report_date")) -> report_points

report_label <- filter(report_points, state == "Illinois")

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, cases))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, day, cases))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = cases), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = cases, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(10, 100000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Reported",
    y = "Count of Reports Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/state/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/c_case_log.png", plot = p, preset = "lg", dpi = 72)

## clean-up data objects
state_points <- select(state_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# create days from first day where average confirmed infections were at least 10, state-level data
## subset data
state_data %>%
  filter(case_avg >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, case_avg) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day)) %>%
  left_join(state_points, ., by = "state") -> state_points

## add day to report points
state_subset %>%
  select(state, report_date, day) %>%
  left_join(report_points, ., by = c("state", "report_date")) -> report_points

report_label <- filter(report_points, state == "Illinois")

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, case_avg))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, day, case_avg))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = day, y = case_avg, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = day, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = case_avg), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = case_avg, label = text),
                  nudge_y = .2, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(10, 2000), breaks = c(10, 30, 100, 300, 1000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Cases by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Ten Cases Reported",
    y = "7-day Average of New Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/d_case_log_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/d_case_log_avg.png", plot = p, preset = "lg", dpi = 72)

## clean-up data objects
state_points <- select(state_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# plot mortality rate
## subset data
state_subset <- filter(state_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$mortality_rate), accuracy = .5, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, mortality_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, mortality_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text(aes(as.Date("2020-04-15"), y = 9.25, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 1)) +
  labs(
    title = "Reported COVID-19 Mortality by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/f_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/f_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 3rd death data, state-level data
## subset data
state_data %>%
  filter(deaths >= 3) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, deaths) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day)) %>%
  left_join(state_points, ., by = "state") -> state_points

## add day to report points
state_subset %>%
  select(state, report_date, day) %>%
  left_join(report_points, ., by = c("state", "report_date")) -> report_points

report_label <- filter(report_points, state == "Illinois")

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, deaths))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, day, deaths))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = day, y = deaths, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = day, y = deaths, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = deaths), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = deaths, label = text),
                  nudge_y = .2, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(3, 2000), breaks = c(3, 10, 30, 100, 300, 1000), labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Third Death Reports",
    y = "Count of Reported Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/g_mortality_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/g_mortality_log.png", plot = p, preset = "lg", dpi = 72)

## clean-up data objects
state_points <- select(state_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# create days from first day where average deaths were over 3, state-level data
## subset data
state_data %>%
  filter(deaths_avg >= 3) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, deaths_avg) %>%
  arrange(state, day) -> state_subset

# define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day)) %>%
  left_join(state_points, ., by = "state") -> state_points

## add day to report points
state_subset %>%
  select(state, report_date, day) %>%
  left_join(report_points, ., by = c("state", "report_date")) -> report_points

report_label <- filter(report_points, state == "Illinois")

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, deaths_avg))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, day, deaths_avg))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = day, y = deaths_avg, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = day, y = deaths_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = deaths_avg), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = deaths_avg, label = text),
                  nudge_y = .12, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(3, 100), breaks = c(3,10,30,100)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Three Deaths Reported",
    y = "7-day Average of New Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/state/h_mortality_log_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/h_mortality_log_avg.png", plot = p, preset = "lg", dpi = 72)

## clean-up data objects
state_points <- select(state_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# plot case fatality rate

## re-subset data
state_subset <- filter(state_data, report_date >= plot_date)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_fatality_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text(aes(as.Date("2020-04-15"), y = 9, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/j_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/j_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(state_data, state_subset, state_points, report_points, report_label)
rm(top_val, pal, cols, p)

