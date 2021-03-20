# plot metro level data

# =============================================================================

# load data
metro_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/metro_all/metro_full.csv")

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
p <- cumulative_rate(metro_subset, 
                     point_data = metro_points,
                     type = "metro", 
                     plot_values = values,
                     highlight = unique(metro_subset$geoid),
                     y_upper_limit = top_val,
                     pal = cols, 
                     title = "Reported COVID-19 Cases by Metro Area",
                     caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/metro/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# per-capita 7-day average ####

## subset data
metro_subset <- filter(metro_data, report_date >= values$plot_date) %>%
  filter(report_date < as.Date("2021-03-08") | report_date >= as.Date("2021-03-15"))

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
  geom_vline(xintercept = as.Date("2021-03-08"), lwd = .8) +
  scale_colour_manual(values = cols, name = "Metro Area") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by Metro Area",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = paste0(values$caption_text,"\nVertical line represents addition of antigen test data for most Missouri counties on 2021-03-08")
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/metro/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/metro/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(metro_data, metro_subset, metro_points)
rm(top_val, p)
