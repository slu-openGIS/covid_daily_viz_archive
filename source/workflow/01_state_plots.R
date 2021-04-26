# plot state level data

# =============================================================================

# load data
state_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/state/state_full.csv") 

# =============================================================================

# define colors
pal <- brewer.pal(n = 5, name = "Set1")
cols <- c("Illinois" = values$pal[1], "Kansas" = values$pal[2], 
          "Missouri" = values$pal[3], "Oklahoma" = values$pal[4],
          "Arkansas" = values$pal[5])

# =============================================================================

# create points
## create end points
state_points <- filter(state_data, report_date == values$date)

# =============================================================================

# plot confirmed rate

## subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$case_rate), accuracy = 1000, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, case_rate))

## create plot
p <- cumulative_rate(state_subset, 
                     point_data = state_points,
                     type = "state", 
                     plot_values = values,
                     highlight = unique(state_subset$state),
                     y_upper_limit = top_val,
                     pal = cols, 
                     title = "Reported COVID-19 Cases by State",
                     caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/state/b_case_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/b_case_rate.png", plot = p, preset = "lg", dpi = 72)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# per-capita 7-day average ####

## subset data
state_subset <- filter(state_data, report_date >= values$plot_date) %>%
  filter(report_date < as.Date("2021-03-08") | report_date >= as.Date("2021-03-15")) %>%
  filter(report_date < as.Date("2021-04-17") | report_date >= as.Date("2021-04-24"))

## define top_val
top_val <- round_any(x = max(state_subset$case_avg_rate), accuracy = 10, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_avg_rate))

## create plot
p <- facet_rate(state_subset, 
                type = "state", 
                pal = cols, 
                x_breaks = values$date_breaks_facet,
                y_breaks = 10,
                y_upper_limit = top_val,
                highlight = unique(state_subset$state),
                plot_date = values$plot_date,
                date = values$date,
                title = "Pace of New COVID-19 Cases by State",
                caption = values$caption_text_census)

## save plot
save_plots(filename = "results/high_res/state/e_new_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/e_new_case.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rate
## subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## define top_val
top_val <- round_any(x = max(state_subset$mortality_rate), accuracy = 10, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, mortality_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, mortality_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = mortality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 10)) +
  labs(
    title = "Reported COVID-19 Mortality by State",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = values$caption_text_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/state/h_mortality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/h_mortality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot case fatality rate

## re-subset data
state_subset <- filter(state_data, report_date >= values$plot_date)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, case_fatality_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, case_fatality_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = case_fatality_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2021-03-08"), lwd = .8) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = values$date_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1)) +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0(as.character(values$plot_date), " through ", as.character(values$date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = paste0(values$caption_text,"\nVertical line represents addition of antigen test data for most Missouri counties on 2021-03-08")
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = values$x_angle))

## save plot
save_plots(filename = "results/high_res/state/m_case_fatality_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/m_case_fatality_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(state_data, state_subset, state_points)
rm(top_val, pal, cols, p)
