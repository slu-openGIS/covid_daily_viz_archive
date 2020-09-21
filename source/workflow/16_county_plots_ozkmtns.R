# plot county level data - Ozarks Focus

# =============================================================================

# load data
county_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri")

# =============================================================================

# define cols object
cols <- c("St. Louis City" = values$pal[1], "St. Louis" = values$pal[2], 
          "Kansas City" = values$pal[3], "Ozark" = values$pal[4], 
          "Howell" = values$pal[5], "Texas" = values$pal[6], 
          "Wright" = values$pal[7])

# define focal metros
county_focal <- c("29510", "29189", "29511", "29153", "29215", "29091", "29229")

# =============================================================================

# create points
## create end points
county_points <- filter(county_data, report_date == values$date) %>%
  filter(geoid %in% county_focal)

# =============================================================================

# plot confirmed rate
## subset data
county_subset <- filter(county_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(county_subset$case_rate), accuracy = values$county_rate_val, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_rate))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, case_rate))

## create plot
p <- ggplot() +
  geom_line(county_subset, mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(county_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = values$county_rate_val)) + 
  labs(
    title = "Reported COVID-19 Cases by Select Missouri Counties",
    subtitle = paste0("Ozark Mountains Focus\n", as.character(values$plot_date), " through ", 
                      as.character(values$date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/county_ozkmtns/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_ozkmtns/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data

## subset data
county_data %>%
  calculate_days(group_var = "geoid", stat_var = "cases", val = 5) %>%
  select(day, report_date, geoid, county, cases) %>%
  arrange(county, day) -> county_subset

## define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 5, f = ceiling)

## identify max day
county_subset %>%
  group_by(geoid) %>%
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(county_points, ., by = "geoid") %>%
  filter(county %in% unique(county_subset$county)) -> county_day_points

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, day, cases))
county_day_points <- mutate(county_day_points, factor_var = fct_reorder2(county, day, cases))

## create plot
p <- ggplot(data = county_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(county_day_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_y_log10(limits = c(5, values$county_log_max), breaks = c(5,10,30,100,300,1000,3000,10000,30000), 
                labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Cases by Select Missouri Counties",
    subtitle = paste0("Ozark Mountains Focus\n", "Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Fifth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/county_ozkmtns/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_ozkmtns/c_case_log.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot confirmed rate

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date) %>%
  filter(geoid %in% county_focal)

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate), accuracy = 10, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_avg_rate))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, case_avg_rate))

## create plot
p <- ggplot(county_subset) +
  geom_line(mapping = aes(x = report_date, y = case_avg_rate, color = factor_var), size = 2) +
  geom_point(county_points, mapping = aes(x = report_date, y = case_avg_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 10)) + 
  labs(
    title = "Pace of New COVID-19 Cases in Select Missouri Counties",
    subtitle = paste0("Ozark Mountains Focus\n",as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "7-Day Average Rate per 100,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/county_ozkmtns/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_ozkmtns/e_new_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(county_data, county_subset, county_points, county_day_points, 
   county_focal)
rm(top_val, cols, p)
