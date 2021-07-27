# county data, st. louis metro

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####

## spatial data
stl_sf <- st_read("data/MO_HEALTH_Covid_Tracking/data/metro/daily_snapshot_stl.geojson", crs = 4326,
                  stringsAsFactors = FALSE) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ") %>%
  mutate(county = ifelse(GEOID %in% c("29189"), NA, county))

## longitudinal data
county_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(geoid %in% c("17005", "17013", "17027", "17083", "17117", 
                      "17119", "17133", "17163", "29071", "29099", 
                      "29113", "29183", "29189", "29219", "29510"))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# prep data ####

## define cols object
cols <- c("St. Louis City" = values$pal[1], "St. Louis" = values$pal[2], 
          "St. Charles" = values$pal[3], "Monroe" = values$pal[4], 
          "Clinton" = values$pal[5], "St. Clair" = values$pal[6],
          "Jefferson" = values$pal[7], "Madison" = values$pal[8],  
          "Franklin" = values$pal[9], "Lincoln" = values$pal[10],
          "Jersey" = values$pal[11], "Bond" = values$pal[12])

## define focal counties
county_focal <- c("29510", "29189", "29183", "17133", "17027", "17163",
                  "29099", "17119", "29071", "29113", "17083", "17005")

## create points
county_points <- filter(county_data, report_date == values$date) %>%
  filter(geoid %in% county_focal)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative case, map ####

## create breaks
stl_sf <- map_breaks(stl_sf, var = "case_rate", newvar = "case_breaks",
                     style = "fisher", classes = 5, dig_lab = 3)

## create maps
p <- ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = case_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases in Metro St. Louis",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/stl_metro/a_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/a_case_map.png", plot = p, preset = "lg", dpi = 72)

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
                     title = "Reported COVID-19 Cases in Metro St. Louis",
                     caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/stl_metro/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# 7-day average of new cases, rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date) %>%
  filter(report_date < as.Date("2021-01-11") | report_date >= as.Date("2021-01-18")) %>%
  filter(report_date < as.Date("2021-03-08") | report_date >= as.Date("2021-03-15")) %>%
  filter(report_date < as.Date("2021-04-17") | report_date >= as.Date("2021-04-24"))

## address negative values
county_subset <- mutate(county_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## modify Calhoun, Clinton, and Jersey counties
county_subset %>%
  mutate(case_avg_rate = ifelse(geoid == 17027 & report_date == "2020-11-20", 160, case_avg_rate)) %>% 
  mutate(case_avg_rate = ifelse(geoid == 17027 & report_date == "2020-11-26", 160, case_avg_rate)) %>% 
  mutate(case_avg_rate = ifelse(geoid == 17083 & 
                                  (report_date == "2020-11-08" | report_date == "2020-11-12"), 160, case_avg_rate),
         case_avg_rate = ifelse(geoid == 17083 & 
                                  (report_date >= "2020-11-09" & report_date <= "2020-11-11"), NA, case_avg_rate)
  ) %>% 
  mutate(case_avg_rate = ifelse(geoid == 17013 & 
                                  (report_date == "2020-11-20" | report_date == "2020-11-24"), 160, case_avg_rate),
         case_avg_rate = ifelse(geoid == 17013 & 
                                  (report_date >= "2020-11-21" & report_date <= "2020-11-23"), NA, case_avg_rate)
  ) -> county_subset

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate, na.rm = TRUE), accuracy = 20, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_avg_rate))

## create plot
p <- facet_rate(county_subset, 
                type = "county", 
                subtype = "St. Louis",
                pal = cols, 
                x_breaks = values$date_breaks_facet,
                y_breaks = 20,
                y_upper_limit = top_val,
                highlight = county_focal,
                plot_date = values$plot_date,
                date = values$date,
                title = "Pace of New COVID-19 Cases in Metro St. Louis",
                caption = paste0(values$caption_text_census,"\nValues above 160 for Clinton and Jersey counties truncated to increase readability"))

## save plot
save_plots(filename = "results/high_res/stl_metro/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/e_new_case.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# plot 7-day average rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$date-20) %>%
  filter(geoid %in% county_focal)

## address negative values
county_subset <- mutate(county_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate, na.rm = TRUE), accuracy = 10, f = ceiling)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, case_avg_rate))

## create plot
p <- facet_rate(county_subset, 
                type = "county", 
                subtype = "St. Louis",
                pal = cols, 
                x_breaks = values$date_breaks_3days,
                y_breaks = 10,
                y_upper_limit = top_val,
                highlight = county_focal,
                plot_date = values$date-20,
                date = values$date,
                title = "Pace of New COVID-19 Cases in Select Missouri Counties",
                caption = values$caption_text_census,
                last3 = TRUE)

## save plot
save_plots(filename = "results/high_res/stl_metro/e_new_case_last21.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/e_new_case_last21.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# cumulative mortality, map ####

## create breaks
stl_sf <- map_breaks(stl_sf, var = "mortality_rate", newvar = "mortality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
p <- ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = mortality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "YlGn", name = "Percent") +
  labs(
    title = "COVID-19 Mortality in Metro St. Louis",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/stl_metro/g_mortality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/g_mortality_map.png", plot = p, preset = "lg", dpi = 72)

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
    title = "Reported COVID-19 Mortality in Metro St. Louis",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Mortality Rate per 1,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/stl_metro/h_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/h_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# case fatality rate, map ####

## create breaks
stl_sf <- map_breaks(stl_sf, var = "case_fatality_rate", newvar = "case_fatality_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
p <- ggplot(data = stl_sf) +
  geom_sf(mapping = aes(fill = case_fatality_breaks)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines"), size = 6) +
  scale_fill_brewer(palette = "BuPu", name = "Percent") +
  labs(
    title = "COVID-19 Case Fatality in Metro St. Louis",
    subtitle = paste0("Current as of ", as.character(values$date)),
    caption = values$caption_text_census_map2
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/high_res/stl_metro/l_case_fatality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/l_case_fatality_map.png", plot = p, preset = "lg", dpi = 72)

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
    title = "COVID-19 Case Fatality in Metro St. Louis",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = paste0(values$caption_text,"\nVertical line represents addition of antigen test data for most Missouri counties on 2021-03-08")
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/stl_metro/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up ####
rm(stl_sf, county_focal, county_points, county_subset, county_data, stl_prior)
rm(top_val, cols, p)
