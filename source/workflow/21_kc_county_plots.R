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
          "Platte" = values$pal[9], "Clinton" = values$pal[10],
          "Bates" = values$pal[11], "Ray" = values$pal[12]) # Caldwell

## define focal counties
county_focal <- c("20209", "20103", "29511", "29107", "29095", "20091", "29047",
                  "29037", "29165", "29013", "29049", "29177") # 29025

## create points
county_points <- filter(county_data, report_date == values$date) %>%
  filter(geoid %in% county_focal)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative case, map ####

## create breaks
kc_sf <- map_breaks(kc_sf, var = "case_rate", newvar = "case_breaks",
                     style = "fisher", classes = 5, dig_lab = 3)

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
top_val <- round_any(x = max(county_subset$case_rate), accuracy = values$county_rate_val, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_rate))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, case_rate))

## create plot
p <- cumulative_rate(county_subset, 
                     point_data = county_points,
                     type = "county",
                     plot_values = values,
                     highlight = county_focal,
                     y_upper_limit = top_val,
                     pal = cols, 
                     title = "Reported COVID-19 Cases in Metro Kansas City",
                     caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/kc_metro/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# 7-day average of new cases, rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date) %>%
  filter(report_date < as.Date("2021-03-08") | report_date >= as.Date("2021-03-15")) %>%
  filter(report_date < as.Date("2021-04-17") | report_date >= as.Date("2021-04-24"))

## address negative values
county_subset <- mutate(county_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## modify Linn County
county_subset <- mutate(county_subset,
                        case_avg_rate = ifelse(geoid == 20107 & 
                                                 (report_date == "2021-01-06" | 
                                                    report_date == "2021-01-07" |
                                                    report_date == "2021-01-10"), 160, case_avg_rate)
)

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
                caption = paste0(values$caption_text_census,"\nValues above 160 for Linn County (not highlighted) truncated to increase readability"))

## save plot
save_plots(filename = "results/high_res/kc_metro/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/e_new_case.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# plot 7-day average rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$date-20) %>%
  filter(geoid %in% county_focal)

## address negative values
county_subset <- mutate(county_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate, na.rm = TRUE), accuracy = 20, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_avg_rate))

## create plot
p <- facet_rate(county_subset, 
                type = "county", 
                subtype = "Kansas City",
                pal = cols, 
                x_breaks = values$date_breaks_3days,
                y_breaks = 20,
                y_upper_limit = top_val,
                highlight = county_focal,
                plot_date = values$date-20,
                date = values$date,
                title = "Pace of New COVID-19 Cases in Select Missouri Counties",
                caption = values$caption_text_census,
                last3 = TRUE)

## save plot
save_plots(filename = "results/high_res/kc_metro/e_new_case_last21.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/e_new_case_last21.png", plot = p, preset = "lg", dpi = 72)

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
top_val <- round_any(x = max(county_subset$mortality_rate), accuracy = .2, f = ceiling)

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
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .2)) +
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
  geom_vline(xintercept = as.Date("2021-03-08"), lwd = .8) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality in Metro Kansas City",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = paste0(values$caption_text,"\nVertical line represents addition of antigen test data for most Missouri counties on 2021-03-08")
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/kc_metro/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/kc_metro/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up ####
rm(kc_sf, county_focal, county_points, county_subset,
   county_data, county_day_points, alt_county_subset)
rm(top_val, cols, p)
