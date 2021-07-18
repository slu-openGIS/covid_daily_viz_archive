# plot county level data

# =============================================================================

# load data
county_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri")

# =============================================================================

# define cols object
cols <- c("St. Louis City" = values$pal[1], "St. Louis" = values$pal[2], 
          "Kansas City" = values$pal[3], "Moniteau" = values$pal[5], 
          "Boone" = values$pal[6], "Cole" = values$pal[7], 
          "Audrain" = values$pal[4], "Callaway" = values$pal[8], 
          "Cooper" = values$pal[9], "Osage" = values$pal[10],
          "Gasconade" = values$pal[11], "Montgomery" = values$pal[12])

# define focal metros
county_focal <- c("29510", "29189", "29511", regional_geoids$mid_mo)

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
p <- cumulative_rate(county_subset, 
                     point_data = county_points,
                     type = "county", 
                     subtype = "Mid-Missouri",
                     plot_values = values,
                     highlight = county_focal,
                     y_upper_limit = top_val,
                     pal = cols, 
                     title = "Reported COVID-19 Cases by Select Missouri Counties",
                     caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/county_midmo/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_midmo/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot 7-day average rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$plot_date) %>%
  filter(geoid %in% county_focal) %>%
  filter(report_date < as.Date("2021-03-08") | report_date >= as.Date("2021-03-15")) %>%
  filter(report_date < as.Date("2021-04-17") | report_date >= as.Date("2021-04-24"))

## address negative values
county_subset <- mutate(county_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## modify Audrain, Moniteau, Osage counties
county_subset %>%
  mutate(case_avg_rate = ifelse(geoid == 29007 & 
                                  (report_date == "2020-10-02" | report_date == "2020-10-06"), 160, case_avg_rate),
         case_avg_rate = ifelse(geoid == 29007 & 
                                  (report_date >= "2020-10-03" & report_date <= "2020-10-05"), NA, case_avg_rate)
  ) %>% 
  mutate(case_avg_rate = ifelse(geoid == 29151 & 
                                  (report_date == "2020-10-19" | report_date == "2020-10-20"), 160, case_avg_rate)
  ) %>%
  mutate(case_avg_rate = ifelse(geoid == 29135 & 
                                  (report_date == "2020-11-14" | report_date == "2020-11-16"), 160, case_avg_rate),
         case_avg_rate = ifelse(geoid == 29135 & 
                                  (report_date == "2020-11-15"), NA, case_avg_rate)
  ) -> county_subset

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate, na.rm = TRUE), accuracy = 20, f = ceiling)

## re-order counties
counties <- unique(county_subset$county)
unique_counties <- counties[!counties %in% c("Kansas City", "St. Louis", "St. Louis City")]
unique_counties <- c(unique_counties, c("Kansas City", "St. Louis", "St. Louis City"))

county_subset <- mutate(county_subset, county_fct = fct_relevel(county, unique_counties))

rm(counties, unique_counties)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county_fct, report_date, case_avg_rate))

## create plot
p <- facet_rate(county_subset, 
                type = "county", 
                subtype = "Mid-Missouri",
                pal = cols, 
                x_breaks = values$date_breaks_facet,
                y_breaks = 20,
                y_upper_limit = top_val,
                highlight = county_focal,
                plot_date = values$plot_date,
                date = values$date,
                title = "Pace of New COVID-19 Cases in Select Missouri Counties",
                caption = paste0(values$caption_text_census,"\nValues above 160 for Audrain, Moniteau, and Osage counties truncated to increase readability"))

# 
# values$caption_text_census

## save plot
save_plots(filename = "results/high_res/county_midmo/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_midmo/e_new_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot 7-day average rate ####

## subset data
county_subset <- filter(county_data, report_date >= values$date-20) %>%
  filter(geoid %in% county_focal)

## address negative values
county_subset <- mutate(county_subset, case_avg_rate = ifelse(case_avg_rate < 0, 0, case_avg_rate))

## define top_val
top_val <- round_any(x = max(county_subset$case_avg_rate, na.rm = TRUE), accuracy = 20, f = ceiling)

## re-order counties
counties <- unique(county_subset$county)
unique_counties <- counties[!counties %in% c("Kansas City", "St. Louis", "St. Louis City")]
unique_counties <- c(unique_counties, c("Kansas City", "St. Louis", "St. Louis City"))

county_subset <- mutate(county_subset, county_fct = fct_relevel(county, unique_counties))

rm(counties, unique_counties)

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county_fct, report_date, case_avg_rate))

## create plot
p <- facet_rate(county_subset, 
                type = "county", 
                subtype = "Mid-Missouri",
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
save_plots(filename = "results/high_res/county_midmo/e_new_case_last21.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county_midmo/e_new_case_last21.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(county_data, county_subset, county_points, county_focal)
rm(top_val, cols, p)
