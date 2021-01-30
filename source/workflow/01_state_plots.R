# plot state level data

# =============================================================================

# load data
state_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/state/state_full.csv") 

# =============================================================================

# define colors
pal <- brewer.pal(n = 5, name = "Set1")
cols <- c("Illinois" = values$pal[1], "Kansas" = values$pal[2], 
          "Missouri" = values$pal[3], "Oklahoma" = values$pal[4],
          "Arkansas" = values$pal[5])

# =============================================================================

# create points
## create end points
state_points <- filter(state_data, report_date == values$date)

# =============================================================================

# plot confirmed rate

## subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$case_rate), accuracy = 1000, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, case_rate))

## create plot
p <- ggplot(state_subset) +
  geom_line(mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 1000)) + 
  labs(
    title = "Reported COVID-19 Cases by State",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Rate per 100,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/state/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "cases", val = 5) %>%
  select(day, report_date, state, cases) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(state_points, ., by = "state") -> state_day_points

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, cases))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, cases))

## create plot
p <- ggplot(data = state_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(5, 3000000), 
                breaks = c(5, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000, 3000000), 
                labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Cases by State",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Fifth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/state/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/c_case_log.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# per-capita 7-day average ####

## subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$case_avg_rate), accuracy = 10, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_avg_rate))

## create plot
p <- facet_rate(state_subset, 
                type = "state", 
                pal = cols, 
                x_breaks = values$date_breaks_facet,
                y_breaks = 10,
                y_upper_limit = top_val,
                highlight = unique(state_subset$state),
                plot_date = values$plot_date,
                date = values$date,
                title = "Pace of New COVID-19 Cases by State",
                caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/state/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/e_new_case.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# create days from first day where average confirmed infections were at least 5

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "case_avg", val = 5) %>%
  select(day, report_date, state, case_avg) %>%
  arrange(state, day) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> state_subset

# define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(state_points, ., by = "state") %>%
  filter(state %in% state_subset$state) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> state_day_points

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, case_avg))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, case_avg))

## create plot
p <- ggplot(data = state_subset) +
  geom_line(mapping = aes(x = day, y = case_avg, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = day, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(1, 30000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000), 
                labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of New COVID-19 Cases by State",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Average of Five Cases Reached",
    y = "7-day Average of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/state/f_new_case_log.png", preset = "lg")
save_plots(filename = "results/low_res/state/f_new_case_log.png", preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rate
## subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$mortality_rate), accuracy = 10, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, mortality_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, mortality_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 10)) +
  labs(
    title = "Reported COVID-19 Mortality by State",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/state/h_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/h_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot days from 3rd confirmed death data

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "deaths", val = 3) %>%
  select(day, report_date, state, deaths) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(state_points, ., by = "state") %>%
  filter(state %in% unique(state_subset$state)) -> state_day_points

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, deaths))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, deaths))

## create plot
p <- ggplot(data = state_subset) +
  geom_line(mapping = aes(x = day, y = deaths, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = day, y = deaths, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(3, 30000), breaks = c(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000), 
                labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Third Death Reported",
    y = "Count of Reported Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/state/i_mortality_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/i_mortality_log.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from first day where average deaths were over 3, metro-level data

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "deaths_avg", val = 3) %>%
  select(day, report_date, state, deaths_avg) %>%
  arrange(state, day) %>%
  mutate(deaths_avg = ifelse(deaths_avg < .1, .1, deaths_avg)) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## identify max day
state_subset %>%
  group_by(state) %>%
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(state_points, ., by = "state") %>%
  filter(state %in% unique(state_subset$state)) %>%
  mutate(deaths_avg = ifelse(deaths_avg < .1, .1, deaths_avg)) -> state_day_points

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, deaths_avg))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, deaths_avg))

## create plot
p <- ggplot(data = state_subset) +
  geom_line(mapping = aes(x = day, y = deaths_avg, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = day, y = deaths_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(.1, 300), breaks = c(.1, .3, 1,3,10,30,100, 300), 
                labels = comma_format(accuracy = .2)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of New COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Average of Three Deaths Reported",
    y = "7-day Average of New Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/state/k_mortality_log_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/k_mortality_log_avg.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot case fatality rate

## re-subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_fatality_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = values$caption_text
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/state/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(state_data, state_subset, state_points, state_day_points)
rm(top_val, pal, cols, p)
