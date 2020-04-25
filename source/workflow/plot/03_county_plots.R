# plot county level data

# =============================================================================

# load data
mo_sf <- st_read("data/county/daily_snapshot_mo.geojson", crs = 4326,
                 stringsAsFactors = FALSE) %>%
  st_transform(crs = 102003) %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  )

county_data <- read_csv("data/county/county_full.csv") %>%
  rename(
    cases = confirmed,
    case_rate = confirmed_rate,
    new_cases = new_confirmed,
    case_avg = confirmed_avg
  ) %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri")

# =============================================================================

# map missouri rates
## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/a_case_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/a_case_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# define colors
pal <- brewer.pal(n = 8, name = "Set1")
pal_a <- pal[c(1:5)]
pal_c <- pal[c(7:8)]
pal_b <- brewer.pal(n = 6, name = "Reds")
pal_b <- pal_b[c(6)]
pal <- c(pal_a, pal_b, pal_c)
cols <- c("St. Louis City" = pal[1], "St. Louis" = pal[2], "St. Charles" = pal[3],
          "Kansas City" = pal[4], "Saline" = pal[5], "Moniteau" = pal[6],
          "Scott" = pal[7], "Perry" = pal[8])

## clean-up
rm(pal_a, pal_b, pal_c)

# define focal metros
county_focal <- c("29510", "29189", "29157", "29195", "29135", "29201",
                  "29183", "29511")

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

# plot confirmed rate
## subset data
county_subset <- filter(county_data, report_date >= plot_date)

## define top_val
top_val <- round_any(x = max(county_subset$case_rate), accuracy = 1, f = ceiling)

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
  geom_text(aes(as.Date("2020-04-15"), y = county_case_rate_y, label = "reporting change on 15 Apr"), 
            angle = 90, vjust = -1, size = 4.5) +
  scale_colour_manual(values = cols, name = "County") +
  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .5)) + 
  labs(
    title = "Reported COVID-19 Cases by Missouri County",
    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
    x = "Date",
    y = "Rate per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/county/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data

## subset data
county_data %>%
  filter(state == "Missouri") %>%
  filter(cases >= 10) %>%
  arrange(report_date) %>%
  group_by(geoid) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, geoid, county, cases) %>%
  arrange(county, day) -> county_subset

## define top_val
top_val <- round_any(x = max(county_subset$day), accuracy = 5, f = ceiling)

## identify max day
county_subset %>%
  group_by(geoid) %>%
  summarise(day = max(day)) %>%
  left_join(county_points, ., by = "geoid") -> county_points

## add day to report points
county_subset %>%
  select(county, report_date, day) %>%
  left_join(report_points, ., by = c("county", "report_date")) -> report_points

report_label <- filter(report_points, county == "St. Louis")

## create factors
county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, day, cases))
county_points <- mutate(county_points, factor_var = fct_reorder2(county, day, cases))

## create plot
p <- ggplot(data = county_subset) +
  geom_line(mapping = aes(x = day, y = cases, color = factor_var), size = 2) +
  geom_point(county_points, mapping = aes(x = day, y = cases, color = factor_var), 
             size = 4, show.legend = FALSE) +
  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
  geom_point(report_points, mapping = aes(x = day, y = cases), size = 4, shape = 18) +
  geom_text_repel(data = report_label, mapping = aes(x = day, y = cases, label = text),
                  nudge_y = .3, nudge_x = -1, size = 5) +
  scale_colour_manual(values = cols, name = "County") +
  scale_y_log10(limits = c(10, 3000), breaks = c(10,30,100,300,1000,3000), labels = comma) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of COVID-19 Cases by County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Tenth Case Reported",
    y = "Count of Reported Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/county/c_case_log.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/c_case_log.png", plot = p, preset = "lg", dpi = 72)

## clean-up data objects
county_points <- select(county_points, -day)
report_points <- select(report_points, -day)

# =============================================================================

# map mortality rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "mortality_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "YlGn", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Mortality by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/e_mortality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/e_mortality_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot confirmed rate
## subset data
# county_subset <- filter(county_data, report_date >= plot_date)

## define top_val
# top_val <- round_any(x = max(county_subset$mortality_rate), accuracy = .2, f = ceiling)

## create factors
# county_subset <- mutate(county_subset, factor_var = fct_reorder2(county, report_date, mortality_rate))
# county_points <- mutate(county_points, factor_var = fct_reorder2(county, report_date, mortality_rate))

## create plot
# p <- ggplot() +
#  geom_line(county_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
#  geom_point(county_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
#             size = 4, show.legend = FALSE) +
#  gghighlight(geoid %in% county_focal, use_direct_label = FALSE, use_group_by = FALSE) +
#  geom_vline(xintercept = as.Date("2020-04-15"), linetype="dotted", size = 1.25) + 
#  geom_text(aes(as.Date("2020-04-15"), y = .15, label = "reporting change on 15 Apr"), 
#            angle = 90, vjust = -1, size = 4.5) +
#  scale_colour_manual(values = cols, name = "County") +
#  scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
#  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .05)) + 
#  labs(
#    title = "COVID-19 Mortality by Missouri County",
#    subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
#    x = "Date",
#    y = "Rate per 1,000",
#    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
#  ) +
#  sequoia_theme(base_size = 22, background = "white")

## save plot
# save_plots(filename = "results/high_res/county/f_mortality_rate.png", plot = p, preset = "lg")
# save_plots(filename = "results/low_res/county/f_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================



# =============================================================================

# map case fatality rate
## create breaks
mo_sf <- map_breaks(mo_sf, var = "case_fatality_rate", newvar = "map_breaks",
                    style = "fisher", classes = 5, dig_lab = 2)

## create map
p <- ggplot(data = mo_sf, mapping = aes(fill = map_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "BuGn", name = "Rate per 1,000") +
  labs(
    title = "Reported COVID-19 Case Fatality by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/county/i_case_fatality_map.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/county/i_case_fatality_map.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# create reference object
ref_county <- mo_sf
st_geometry(ref_county) <- NULL

# =============================================================================

# clean-up
rm(mo_sf, county_focal, county_points, report_points, report_label, county_subset,
   county_data, county_case_rate_y)
rm(top_val, pal, cols, p)
