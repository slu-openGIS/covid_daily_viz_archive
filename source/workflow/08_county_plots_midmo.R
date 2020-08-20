# plot county level data

# =============================================================================

# load data
county_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri")

# =============================================================================

# define colors
pal <- brewer.pal(n = 8, name = "Set1")
pal[6] <- "#FFD60C"

# define cols object
cols <- c("St. Louis City" = pal[1], "St. Louis" = pal[2], "Kansas City" = pal[3],
          "Moniteau" = pal[5], "Boone" = pal[6], "Cole" = pal[7], "Audrain" = pal[4],
          "Callaway" = pal[8])

# define focal metros
county_focal <- c("29510", "29189", "29511", "29135", "29019", "29051", "29007", "29027")

# =============================================================================

# create points
## create end points
county_points <- filter(county_data, report_date == date) %>%
  filter(geoid %in% county_focal)

## create reporting change points
report_points <- filter(county_data, report_date == as.Date("2020-04-15")) %>%
  filter(geoid %in% county_focal) %>%
  mutate(text = ifelse(geoid == "29189", "reporting change on 15 Apr", NA))

# =============================================================================

# create line label
report_line <- tibble(
  date = as.Date("2020-04-15"),
  case_rate = county_rate_pos,
  mortality_rate = NA,
  case_fatality_rate = NA,
  text = "reporting change on 15 Apr"
)

# =============================================================================

# plot confirmed rate
## subset data
county_subset <- filter(county_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(county_subset$case_rate), accuracy = county_rate_val, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_rate))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, case_rate))

## create plot
p <- ggplot() +
  geom_line(county_subset, mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
  geom_point(county_points, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
  geom_text_repel(data = report_line, mapping = aes(x = date, y = case_rate, label = text),
                  nudge_y = county_rate_x, nudge_x = county_rate_y, size = 5) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = county_rate_val)) + 
  labs(
    title = "Reported COVID-19 Cases by Select Missouri Counties",
    subtitle = paste0("Mid-Missouri Focus\n", as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/county_midmo/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_midmo/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

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
  summarise(day = max(day)) %>%
  left_join(county_points, ., by = "geoid") %>%
  filter(county %in% unique(county_subset$county)) -> county_day_points

## add day to report points
county_subset %>%
  select(county, report_date, day) %>%
  left_join(report_points, ., by = c("county", "report_date")) %>%
  filter(county %in% unique(county_subset$county)) -> report_day_points

report_label <- filter(report_day_points, county == "St. Louis")

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, day, cases))
county_day_points <- mutate(county_day_points, factor_var = fct_reorder2(county, day, cases))

## create plot
p <- ggplot(data = county_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(county_day_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_point(report_day_points, mapping = aes(x = day, y = cases), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = cases, label = text),
                  nudge_y = county_log_y, nudge_x = county_log_x, size = 5) +
  scale_colour_manual(values = cols, name = "County") +
  scale_y_log10(limits = c(5, county_log_max), breaks = c(5,10,30,100,300,1000,3000,10000,30000),
                labels = comma_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Cases by Select Missouri Counties",
    subtitle = paste0("Mid-Missouri Focus\n", "Current as of ", as.character(date)),
    caption = caption_text,
    x = "Days Since Fifth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/county_midmo/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_midmo/c_case_log.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(county_data, county_subset, county_points, county_day_points, report_day_points, report_points, 
   report_label, report_line, county_focal)
rm(top_val, pal, cols, p)
