# plot regional data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####
region_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/region/region_meso.csv",
                        col_types = cols(region = col_character()
                        )) %>%
  filter(report_date < as.Date("2021-01-11") | report_date >= as.Date("2021-01-18")) %>%
  filter(report_date < as.Date("2021-03-08") | report_date >= as.Date("2021-03-15")) %>%
  filter(report_date < as.Date("2021-04-17") | report_date >= as.Date("2021-04-24"))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define color palette ####

cols <- c("St. Louis" = values$pal[1], "Kansas City" = values$pal[2],
          "Outstate" = values$pal[3], "Missouri" = values$pal[4])

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define plotting values, state trend ####
## primary values
state_values <- list(
  top_val = round_any(x = max(region_data$case_avg), accuracy = 500, f = ceiling),
  peak_val = region_data %>% 
    filter(region == "Missouri") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -80, # 0
  peak_y = -140, # 400
  current_x = 0, # -105
  current_y = values$state_current_y, # 1000
  current_display = TRUE
)

## tables
state_values[["peak_tbl"]] <- region_data %>%
  filter(region == "Missouri") %>%
  filter(case_avg == state_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), "\ncases reported on ", format(report_date, format = "%d %b")))

state_values[["current_tbl"]] <- region_data %>%
  filter(region == "Missouri") %>%
  filter(report_date == values$date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), "\ncases reported on ", format(report_date, format = "%d %b")))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define plotting values, st. louis trend ####
## primary values
stl_values <- list(
  peak_val = region_data %>% 
    filter(region == "St. Louis") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -100, 
  peak_y = 500, 
  current_x = values$regional_current_x, 
  current_y = -1000,
  current_display = TRUE
)

## tables
stl_values[["peak_tbl"]] <- region_data %>%
  filter(region == "St. Louis") %>%
  filter(case_avg == stl_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), "\ncases reported on ", format(report_date, format = "%d %b")))

stl_values[["current_tbl"]] <- region_data %>%
  filter(region == "St. Louis") %>%
  filter(report_date == values$date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define plotting values, kansas city trend ####
## primary values
kc_values <- list(
  peak_val = region_data %>% 
    filter(region == "Kansas City") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -125, 
  peak_y = 1200, 
  current_x = values$regional_current_x, 
  current_y = -1000,
  current_display = TRUE
)

## tables
kc_values[["peak_tbl"]]  <- region_data %>%
  filter(region == "Kansas City") %>%
  filter(case_avg == kc_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), "\ncases reported on ", format(report_date, format = "%d %b")))

kc_values[["current_tbl"]] <- region_data %>%
  filter(region == "Kansas City") %>%
  filter(report_date == values$date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# define plotting values, outstate trend ####
## primary values
os_values <- list(
  peak_val = region_data %>% 
    filter(region == "Outstate") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -100, 
  peak_y = 200, 
  current_x = values$regional_current_x, 
  current_y = -2000,
  current_display = TRUE
)

## tables
os_values[["peak_tbl"]] <- region_data %>%
  filter(region == "Outstate") %>%
  filter(case_avg == os_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), "\ncases reported on ", format(report_date, format = "%d %b")))

os_values[["current_tbl"]] <- region_data %>%
  filter(region == "Outstate") %>%
  filter(report_date == values$date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## create end points
region_points <- filter(region_data, report_date == values$date)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# facet plot ####

## subset data
region_data %>%
  filter(region != "Missouri") %>%
  filter(report_date >= values$plot_date) %>%
  mutate(region = fct_relevel(region, "St. Louis", "Kansas City", "Outstate")) -> region_subset

## define top_val
top_val <- round_any(x = max(region_subset$case_avg_rate), accuracy = 20, f = ceiling)

## construct plot
p <- ggplot() +
  geom_area(region_subset, mapping = aes(x = report_date, y = case_avg_rate, fill = region),
            show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2021-03-08"), lwd = .8) +
  scale_fill_manual(values = cols) +
  facet_wrap(vars(region), nrow = 3) +
  scale_x_date(date_breaks = values$date_breaks_long, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 20)) + 
  labs(
    title = "Pace of New COVID-19 Cases in Missouri by Region",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    caption = paste0(values$caption_text, "\nVertical line represents addition of antigen test data for most Missouri counties on 2021-03-08"),
    x = "Date",
    y = "7-day Average of New Cases per 100,000"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/regional/a_avg_all.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/a_avg_all.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# st. louis plot ####

## subset data
region_data %>%
  filter(region %in% c("Missouri", "St. Louis")) %>%
  filter(report_date >= values$plot_date) %>%
  mutate(factor_var = fct_reorder2(region, report_date, case_avg)) -> region_subset

## subset points
region_points %>%
  filter(region %in% c("Missouri", "St. Louis")) %>%
  filter(report_date >= values$plot_date) %>%
  mutate(factor_var = fct_reorder2(region, report_date, case_avg)) -> region_points_subset

## create plot
p <- regional_count(region_subset, region = "St. Louis", 
                    point_data = region_points_subset, 
                    state_data = state_values, 
                    region_data = stl_values, 
                    plot_data = values, 
                    palette = cols)

## save plot
save_plots(filename = "results/high_res/regional/b_avg_stl.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/b_avg_stl.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(stl_values)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# kansas city plot ####

## subset data
region_data %>%
  filter(region %in% c("Missouri", "Kansas City")) %>%
  filter(report_date >= values$plot_date) %>%
  mutate(factor_var = fct_reorder2(region, report_date, case_avg)) -> region_subset

## subset points
region_points %>%
  filter(region %in% c("Missouri", "Kansas City")) %>%
  filter(report_date >= values$plot_date) %>%
  mutate(factor_var = fct_reorder2(region, report_date, case_avg)) -> region_points_subset

## create plot
p <- regional_count(region_subset, region = "Kansas City", 
                    point_data = region_points_subset, 
                    state_data = state_values, 
                    region_data = kc_values, 
                    plot_data = values, 
                    palette = cols)

## save plot
save_plots(filename = "results/high_res/regional/c_avg_kc.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/c_avg_kc.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(kc_values)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# kansas city plot ####

## subset data
region_data %>%
  filter(region %in% c("Missouri", "Outstate")) %>%
  filter(report_date >= values$plot_date) %>%
  mutate(factor_var = fct_reorder2(region, report_date, case_avg)) -> region_subset

## subset points
region_points %>%
  filter(region %in% c("Missouri", "Outstate")) %>%
  filter(report_date >= values$plot_date) %>%
  mutate(factor_var = fct_reorder2(region, report_date, case_avg)) -> region_points_subset

## create plot
p <- regional_count(region_subset, region = "Outstate", 
                    point_data = region_points_subset, 
                    state_data = state_values, 
                    region_data = os_values, 
                    plot_data = values, 
                    palette = cols)

## save plot
save_plots(filename = "results/high_res/regional/d_avg_outstate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/d_avg_outstate.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(state_values, os_values)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# clean-up ####
rm(region_data, region_subset, region_points, region_points_subset)
rm(cols, p)
