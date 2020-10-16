# county data, st. louis metro

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####

## spatial data
kc_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/metro/daily_snapshot_kc.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ") %>%
  mutate(county = ifelse(GEOID %in% c("29511"), NA, county))

## longitudinal data
county_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(geoid %in% c("20091", "20103", "20107", "20121", "20209",
                      "29013", "29025", "29037", "29047", "29049", 
                      "29095", "29107", "29165", "29177", "29511"))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# prep data ####

## define cols object
cols <- c("Kansas City" = values$pal[1], "Wyandotte" = values$pal[2], 
          "Leavenworth" = values$pal[3], "Lafayette" = values$pal[4], 
          "Jackson" = values$pal[5], "Johnson" = values$pal[6],
          "Clay" = values$pal[7], "Cass" = values$pal[8], 
          "Platte" = values$pal[9])

## define focal counties
county_focal <- c("20209", "20103", "29511", "29107", "29095", "20091", "29047",
                  "29037", "29165")

## create points
county_points <- filter(county_data, report_date == values$date) %>%
  filter(geoid %in% county_focal)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative case, map ####

## create breaks
kc_sf <- map_breaks(kc_sf, var = "case_rate", newvar = "case_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
p <- ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = case_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases in Metro Kansas City",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/kc_metro/a_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/a_case_map.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative cases, rate

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(county_subset$case_rate), accuracy = 2, f = ceiling)

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
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 2)) + 
  labs(
    title = "Reported COVID-19 Cases in Metro Kansas City",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/kc_metro/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative cases, log ####

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
  left_join(county_points, ., by = "geoid") -> county_day_points

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
  scale_y_log10(
    limits = c(5, values$county_log_max), 
    breaks = c(5,10,30,100,300,1000,3000,10000,30000),
    labels = comma_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Cases in Metro Kansas City",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Fifth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/kc_metro/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/c_case_log.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# 7-day average of new cases, rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate), accuracy = 20, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_avg_rate))

## create plot
p <- facet_rate(county_subset, 
                type = "county", 
                subtype = "Kansas City",
                pal = cols, 
                x_breaks = values$date_breaks_facet,
                y_breaks = 20,
                y_upper_limit = top_val,
                highlight = county_focal,
                plot_date = values$plot_date,
                date = values$date,
                title = "Pace of New COVID-19 Cases in Metro Kansas City",
                caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/kc_metro/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/e_new_case.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# 7-day average of new cases, log ####

## subset data
county_data %>%
  calculate_days(group_var = "geoid", stat_var = "case_avg", val = 5) %>%
  select(day, report_date, geoid, county, case_avg) %>%
  arrange(county, day) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> county_subset

# define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 5, f = ceiling)

## identify max day
county_subset %>%
  group_by(geoid) %>%
  summarise(day = max(day), .groups = "drop_last") %>%
  left_join(county_points, ., by = "geoid") %>%
  filter(county %in% unique(county_subset$county)) %>%
  mutate(case_avg = ifelse(case_avg < .1, .1, case_avg)) -> county_day_points

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, day, case_avg))
county_day_points <- mutate(county_day_points, factor_var = fct_reorder2(county, day, case_avg)) # %>%
  # filter(geoid != "29511")

## fix st louis
# corrected_stl_point <- filter(county_subset, geoid == 29511 & (report_date == "2020-09-29")) # | report_date == "2020-07-02"
# county_subset <- filter(county_subset, (geoid == "29511" & report_date > "2020-09-29") == FALSE)
# x <- filter(county_subset, geoid == 29511 & report_date > "2020-07-01")
# y <- filter(county_subset, geoid != 29511)
# stl_prior <- filter(county_subset, geoid == 29511 & report_date < "2020-09-29")
# county_subset <- bind_rows(x,y)

## create plot
p <- ggplot() +
  geom_line(data = county_subset, mapping = aes(x = day, y = case_avg, color = factor_var), size = 2) +
  # geom_line(data = stl_prior, mapping = aes(x = day, y = case_avg), size = 2, color = values$pal[1]) +
  geom_point(county_day_points, mapping = aes(x = day, y = case_avg, color = factor_var),
             size = 4, show.legend = FALSE) +
  # geom_point(corrected_stl_point, mapping = aes(x = day, y = case_avg), color = values$pal[1], shape = 15,
  #           size = 4, show.legend = FALSE) +
  # geom_line(corrected_stl_point, mapping = aes(x = day, y = case_avg), color = values$pal[1],
  #          size = 2, linetype = "dotted", show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_y_log10(limits = c(.1, 1000), breaks = c(.1, .3, 1, 3, 10, 30, 100, 300, 1000), 
                labels = comma_format(accuracy = .2)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of New COVID-19 Cases in Metro Kansas City",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    # caption = paste0(values$caption_text, "\nKansas City's trend is omitted after 2020-09-29 due to data quality issues"),
    x = "Days Since Average of Five Cases Reached",
    y = "7-day Average of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/kc_metro/f_new_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/f_new_case_log.png", plot = p, preset = "lg", dpi = 72)

## remove extras
rm(x, y, corrected_stl_point)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative mortality, map ####

## create breaks
kc_sf <- map_breaks(kc_sf, var = "mortality_rate", newvar = "mortality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
p <- ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = mortality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "YlGn", name = "Percent") +
  labs(
    title = "COVID-19 Mortality in Metro Kansas City",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/kc_metro/g_mortality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/g_mortality_map.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative mortality, rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(county_subset$mortality_rate), accuracy = .05, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, mortality_rate))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, mortality_rate))

## create plot
p <- ggplot() +
  geom_line(county_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(county_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .05)) +
  labs(
    title = "Reported COVID-19 Mortality in Metro Kansas City",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Mortality Rate per 1,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/kc_metro/h_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/h_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative mortality, log ####

## subset data
county_data %>%
  calculate_days(group_var = "geoid", stat_var = "deaths", val = 3) %>%
  select(day, report_date, geoid, county, deaths) %>%
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
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, day, deaths))
county_day_points <- mutate(county_day_points, factor_var = fct_reorder2(county, day, deaths))

## create plot
p <- ggplot(data = county_subset) +
  geom_line(mapping = aes(x = day, y = deaths, color = factor_var), size = 2) +
  geom_point(county_day_points, mapping = aes(x = day, y = deaths, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_y_log10(
    limits = c(3, 1000), 
    breaks = c(3, 10, 30, 100, 300, 1000), 
    labels = comma_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = values$date_breaks_log)) +
  labs(
    title = "Pace of COVID-19 Deaths in Metro Kansas City",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text,
    x = "Days Since Third Death Reported",
    y = "Count of Reported Deaths (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/kc_metro/i_mortality_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/i_mortality_log.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# case fatality rate, map ####

## create breaks
kc_sf <- map_breaks(kc_sf, var = "case_fatality_rate", newvar = "case_fatality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
p <- ggplot(data = kc_sf) +
  geom_sf(mapping = aes(fill = case_fatality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "BuPu", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality in Metro Kansas City",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map2
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/kc_metro/l_case_fatality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/l_case_fatality_map.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# case fatality rate, longitudinal ####

## re-subset data
county_subset <- filter(county_data, report_date >= values$plot_date)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_fatality_rate))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(county_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(county_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality in Metro Kansas City",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = values$caption_text
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/kc_metro/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up ####
rm(kc_sf, county_focal, county_points, county_subset,
   county_data, county_day_points, stl_prior)
rm(top_val, cols, p)
