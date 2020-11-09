# death data for Missouri

# =============================================================================

# load data
mo_deaths <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/state/mo_deaths.csv")

# =============================================================================

# define colors
pal <- brewer.pal(n = 3, name = "Set1")
cols <- c("Deaths, Actual" = pal[1], "Deaths, Reported" = pal[2])

# =============================================================================

## define top_val
top_val <- round_any(x = max(mo_deaths$avg, na.rm = TRUE), accuracy = 5, f = ceiling)

## create points
death_points <- mo_deaths %>%
  group_by(value) %>%
  filter(row_number() == n())

## create factors
mo_deaths <- mutate(mo_deaths, factor_var = fct_reorder2(value, date, avg))
death_points <- mutate(death_points, factor_var = fct_reorder2(value, date, avg))

## plot
p <- ggplot() +
  geom_line(mo_deaths, mapping = aes(x = date, y = avg, color = value), size = 2) +
  geom_point(death_points, mapping = aes(x = date, y = avg, color = value), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Measure") +
  scale_x_date(date_breaks = hosp_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) + 
  labs(
    title = "COVID-19 Mortality, Reported vs. Actual",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Date",
    y = "Deaths",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the New York Times COVID-19 Project"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/state/n_mortality_compare.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/n_mortality_compare.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(death_points, mo_deaths, p, cols, pal, top_val, hosp_breaks)
