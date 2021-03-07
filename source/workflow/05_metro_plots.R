# plot metro level data

# =============================================================================

# load data
metro_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/metro_all/metro_full.csv")
metro_hosp <- read_csv("data/MO_HEALTH_Covid_Tracking/data/metro_all/metro_hospital.csv")

# =============================================================================

# define colors
cols <- c("Cape Girardeau" = values$pal[6], "Columbia" = values$pal[3], 
          "Jefferson City" = values$pal[4], "Joplin" = values$pal[7],
          "Kansas City" = values$pal[2], "Springfield" = values$pal[5], 
          "St. Joseph" = values$pal[8], "St. Louis" = values$pal[1])

# =============================================================================

# subset data
## create end points
metro_points <- filter(metro_data, report_date == values$date) 

# =============================================================================

# plot confirmed rate

## subset data
metro_subset <- filter(metro_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(metro_subset$case_rate), accuracy = 10, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_rate))

## create plot
p <- ggplot(metro_subset) +
  geom_line(mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 10)) + 
  labs(
    title = "Reported COVID-19 Cases by Metro Area",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

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
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(metro_points, ., by = "short_name") -> metro_day_points

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, cases))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, cases))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(metro_day_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(
    limits = c(5, 300000), 
    breaks = c(5,10,30,100,300,1000,3000,10000,30000,100000,300000), 
    labels = comma_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Fifth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/metro/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/c_case_log.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# per-capita 7-day average ####

## subset data
metro_subset <- filter(metro_data, report_date >= values$plot_date)

## address negative values
metro_subset <- mutate(metro_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## modify Cape Girardeau
metro_subset %>%
  mutate(case_avg_rate = ifelse(short_name == "Cape Girardeau" & 
                                  (report_date == "2020-11-20" | report_date == "2020-11-22"), 160, case_avg_rate),
         case_avg_rate = ifelse(short_name == "Cape Girardeau" & report_date == "2020-11-21", NA, case_avg_rate)
  ) -> metro_subset

## define top_val
top_val <- round_any(x = max(metro_subset$case_avg_rate, na.rm = TRUE), accuracy = 20, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_avg_rate))

## create plot
p <- facet_rate(metro_subset, 
                type = "metro", 
                pal = cols, 
                x_breaks = values$date_breaks_facet,
                y_breaks = 20,
                y_upper_limit = top_val,
                highlight = unique(metro_subset$geoid),
                plot_date = values$plot_date,
                date = values$date,
                title = "Pace of New COVID-19 Cases by Metro Area",
                caption = paste0(values$caption_text_census,"\nValues above 160 for Cape Girardeau truncated to increase readability"))

# values$caption_text_census

## save plot
save_plots(filename = "results/high_res/metro/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/e_new_case.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

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
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% metro_subset$short_name) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> metro_day_points

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, case_avg))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, case_avg))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = case_avg, color = factor_var), 
            size = 2, show.legend = FALSE) +
  geom_point(metro_day_points, mapping = aes(x = day, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(.1, 3000), breaks = c(.1, .3, 1, 3, 10, 30, 100, 300, 1000, 3000, 3000), 
                labels = comma_format(accuracy = .2)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 75)) +
  facet_wrap(~short_name) +
  labs(
    title = "Pace of New COVID-19 Cases by Metro Area",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Average of Five Cases Reached",
    y = "7-day Average of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.y = element_text(size=15))

## save plots
save_plots(filename = "results/high_res/metro/f_new_case_log.png", preset = "lg")
save_plots(filename = "results/low_res/metro/f_new_case_log.png", preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rate
## subset data
metro_subset <- filter(metro_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(metro_subset$mortality_rate), accuracy = .1, f = ceiling)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, mortality_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, mortality_rate))

## create plot
p <- ggplot() +
  geom_line(metro_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .1)) +
  labs(
    title = "Reported COVID-19 Mortality by Metro Area",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Mortality Rate per 1,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/metro/h_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/h_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

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
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% unique(metro_subset$short_name)) -> metro_day_points

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, deaths))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, deaths))

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = deaths, color = factor_var), size = 2) +
  geom_point(metro_day_points, mapping = aes(x = day, y = deaths, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(
    limits = c(3, 10000), 
    breaks = c(3, 10, 30, 100, 300, 1000, 3000, 10000), 
    labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Deaths by Metro Area",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Third Death Reported",
    y = "Count of Reported Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/i_mortality_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/i_mortality_log.png", plot = p, preset = "lg", dpi = 72)

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
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(metro_points, ., by = "short_name") %>%
  filter(short_name %in% unique(metro_subset$short_name)) %>%
  mutate(deaths_avg = ifelse(deaths_avg < .1, .1, deaths_avg)) -> metro_day_points

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, day, deaths_avg))
metro_day_points <- mutate(metro_day_points, factor_var = fct_reorder2(short_name, day, deaths_avg))

## fix counties with anomalies
### remove from primary data
metro_subset %>%
  mutate(deaths_avg = ifelse(short_name == "Kansas City" & (report_date >= "2020-09-30" & 
                                                            report_date <= "2020-10-06"), NA,
                             deaths_avg)) %>%
  mutate(deaths_avg = ifelse(short_name == "St. Louis" & report_date =="2020-10-21", NA,
                             deaths_avg)) -> metro_subset

### create interpolated data
alt_metro_subset <- list(
  kc = filter(metro_subset, short_name == "Kansas City" & (report_date == "2020-09-29" | report_date == "2020-10-07")),
  stl = filter(metro_subset, short_name == "St. Louis" & (report_date == "2020-10-20" | report_date == "2020-10-22"))) %>%
  map_df(bind_rows)

## create plot
p <- ggplot(data = metro_subset) +
  geom_line(mapping = aes(x = day, y = deaths_avg, color = factor_var), 
            size = 2, show.legend = FALSE) +
  geom_line(data = alt_metro_subset, mapping = aes(x = day, y = deaths_avg, color = factor_var), 
            size = 2, show.legend = FALSE) +
  geom_point(metro_day_points, mapping = aes(x = day, y = deaths_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_y_log10(limits = c(.1, 100), breaks = c(.1, .3, 1, 3, 10, 30, 100), 
                labels = comma_format(accuracy = .2)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 75)) +
  facet_wrap(~short_name) +
  labs(
    title = "Pace of New COVID-19 Deaths by Metro Area",
    subtitle = paste0("Current as of ", as.character(values$date)),
    # caption = values$caption_text,
    caption = paste0(values$caption_text, "\nSmall portions of trends for St. Louis and Kansas City interpolated to provide smoother trend lines"),
    x = "Days Since Average of Three Deaths Reported",
    y = "7-day Average of New Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/metro/k_mortality_log_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/k_mortality_log_avg.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot case fatality rate

## re-subset data
metro_subset <- filter(metro_data, report_date >= values$plot_date)

## create factors
metro_subset <- mutate(metro_subset, factor_var = fct_reorder2(short_name, report_date, case_fatality_rate))
metro_points <- mutate(metro_points, factor_var = fct_reorder2(short_name, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(metro_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(metro_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by Metro Area",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = values$caption_text
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/metro/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(metro_data, metro_subset, metro_points, metro_day_points, alt_metro_subset)
rm(top_val, p)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# per-capita 7-day average ####

## define top_val
top_val <- round_any(x = max(metro_hosp$covid_per_cap, na.rm = TRUE), accuracy = 50, f = ceiling)

## create factors
metro_hosp <- mutate(metro_hosp, factor_var = fct_reorder2(short_name, report_date, covid_per_cap))

## create plot
p <- facet_rate(metro_hosp, 
                type = "metro HHS", 
                pal = cols, 
                x_breaks = values$date_breaks,
                y_breaks = 50,
                y_upper_limit = top_val,
                highlight = unique(metro_hosp$geoid),
                plot_date = values$plot_date,
                date = values$date,
                title = "COVID-19 Hospitalizations by Metro Area",
                caption = "Data via HHS and the U.S. Census Bureau")

## save plot
save_plots(filename = "results/high_res/metro/o_in_pt.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/o_in_pt.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

