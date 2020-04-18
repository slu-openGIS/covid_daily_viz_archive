# plot summary data

# define first date for plotting
plot_date <- "2020-03-10"
date_breaks <- "5 days"

# prep state data
state_data <- filter(state_data, report_date >= plot_date)

# define colors
cols <- c("Illinois" = "#1B9E77", "Kansas" = "#D95F02", "Missouri" = "#7570B3", "Oklahoma" = "#E7298A")

# plot confirmed rate
## define top_val
top_val <- round_any(x = max(state_data$confirmed_rate), accuracy = 20, f = ceiling)

## create plot
state_data %>%
  mutate(factor_var = fct_reorder2(state, report_date, confirmed_rate)) %>%
  ggplot(., mapping = aes(x = report_date, y = confirmed_rate)) +
    geom_line(mapping = aes(color = factor_var), size = 2) +
    scale_colour_manual(values = cols, name = "State") +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
    scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 20)) + 
    labs(
      title = "Confirmed COVID-19 Cases by State",
      subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
      x = "Date",
      y = "Rate of Confirmed Infections per 100,000",
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nConfirmed cases are those with a positive test as a proportion of the total population"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/b_confirmed_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/b_confirmed_rate.png", preset = "lg", dpi = 72)

# plot case fatality rate
## create plot
state_data %>%
  mutate(factor_var = fct_reorder2(state, report_date, case_fatality_rate)) %>%
  ggplot(data = ., mapping = aes(x = report_date, y = case_fatality_rate)) +
    geom_line(mapping = aes(color = factor_var), size = 2) +
    scale_colour_manual(values = cols, name = "State") +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
    scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 1)) +
    labs(
      title = "COVID-19 Case Fatality by State",
      subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
      x = "Date",
      y = "Case Fatality (%)",
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nCase fatality is the percent of confirmed cases that result in death"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/c_case_fatality_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/c_case_fatality_rate.png", preset = "lg", dpi = 72)

# plot mortality rate
## define top_val
top_val <- round_any(x = max(state_data$mortality_rate), accuracy = .5, f = ceiling)

## create plot
state_data %>%
  mutate(factor_var = fct_reorder2(state, report_date, mortality_rate)) %>%
  ggplot(data = ., mapping = aes(x = report_date, y = mortality_rate)) +
    geom_line(mapping = aes(color = factor_var), size = 2) +
    scale_colour_manual(values = cols, name = "State") +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
    scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = .5)) +
    labs(
      title = "Confirmed COVID-19 Mortality by State",
      subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
      x = "Date",
      y = "Mortality Rate per 100,000",
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nMortality rate is the number of deaths as a proportion of the total population"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/d_mortality_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/d_mortality_rate.png", preset = "lg", dpi = 72)

# map missouri rates
## create breaks
mo_sf <- map_breaks(mo_sf, var = "confirmed_rate", newvar = "confirmed_breaks",
                   style = "fisher", classes = 5, dig_lab = 2)

## create map
ggplot(data = mo_sf, mapping = aes(fill = confirmed_breaks)) +
  geom_sf() +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Missouri County",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects\nKansas City is treated as a distinct county due to reporting practices\nConfirmed cases are those with a positive test as a proportion of the total population\nFisher style breaks used to calculate legend categories"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/state/a_mo_map.png", preset = "lg")
save_plots(filename = "results/low_res/state/a_mo_map.png", preset = "lg", dpi = 72)

# clean-up
rm(mo_sf, plot_date, top_val, date_breaks)
