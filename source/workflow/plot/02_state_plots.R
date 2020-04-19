# plot stat level data

# =============================================================================

# define first date for plotting
plot_date <- "2020-03-10"
date_breaks <- "5 days"

# =============================================================================

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
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "results/high_res/state/a_mo_map.png", preset = "lg")
save_plots(filename = "results/low_res/state/a_mo_map.png", preset = "lg", dpi = 72)

# =============================================================================

# define colors
pal <- brewer.pal(n = 4, name = "Set1")
cols <- c("Illinois" = pal[1], "Kansas" = pal[2], "Missouri" = pal[3], "Oklahoma" = pal[4])

# subset state data
state_subset <- filter(state_data, report_date >= plot_date)

# =============================================================================

# plot confirmed rate
## define top_val
top_val <- round_any(x = max(state_subset$confirmed_rate), accuracy = 20, f = ceiling)

## create plot
state_subset %>%
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
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/b_confirmed_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/b_confirmed_rate.png", preset = "lg", dpi = 72)

# =============================================================================

# plot case fatality rate
## create plot
state_subset %>%
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
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/c_case_fatality_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/c_case_fatality_rate.png", preset = "lg", dpi = 72)

# =============================================================================

# plot mortality rate
## define top_val
top_val <- round_any(x = max(state_subset$mortality_rate), accuracy = .5, f = ceiling)

## create plot
state_subset %>%
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
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/state/d_mortality_rate.png", preset = "lg")
save_plots(filename = "results/low_res/state/d_mortality_rate.png", preset = "lg", dpi = 72)

# =============================================================================

# create days from 10th confirmed infection data, state-level data
## subset data
state_data %>%
  filter(confirmed >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, confirmed) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 10, f = ceiling)

## plot data
state_subset %>%
  mutate(factor_var = fct_reorder2(state, day, confirmed)) %>%
  ggplot(data = ., mapping = aes(day, confirmed)) +
    geom_line(mapping = aes(color = factor_var), size = 2) +
    scale_colour_manual(values = cols, name = "State") +
    scale_y_log10(limits = c(10, 100000), labels = comma) +
    scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
    labs(
      title = "Pace of COVID-19 Cases by State",
      subtitle = paste0("Current as of ", as.character(date)),
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
      x = "Days Since Tenth Case Confirmed",
      y = "Count of Confirmed Cases (Log)"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/state/e_confirmed_log.png", preset = "lg")
save_plots(filename = "results/low_res/state/e_confirmed_log.png", preset = "lg", dpi = 72)

# =============================================================================

# create days from first day where average confirmed infections were at least 10, state-level data
## subset data
state_data %>%
  filter(confirmed_avg >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, confirmed_avg) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 10, f = ceiling)

## create plot
state_subset %>%
  mutate(factor_var = fct_reorder2(state, day, confirmed_avg)) %>%
  ggplot(data = ., mapping = aes(day, confirmed_avg)) +
    geom_line(mapping = aes(color = factor_var), size = 2) +
    scale_colour_manual(values = cols, name = "State") +
    scale_y_log10(limits = c(10, 2000), labels = comma) +
    scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
    labs(
      title = "Pace of New COVID-19 Cases by State",
      subtitle = paste0("Current as of ", as.character(date)),
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
      x = "Days Since Average of Ten Cases Confirmed",
      y = "7-day Average of New Cases (Log)"
    ) +
    sequoia_theme(base_size = 22, background = "white")

## sve plot
save_plots(filename = "results/high_res/state/f_confirmed_log_avg.png", preset = "lg")
save_plots(filename = "results/low_res/state/f_confirmed_log_avg.png", preset = "lg", dpi = 72)

# =============================================================================

# create days from 3rd death data, state-level data
## subset data
state_data %>%
  filter(deaths >= 3) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, deaths) %>%
  arrange(state, day) -> state_subset

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 10, f = ceiling)

## plot data
state_subset %>%
  mutate(factor_var = fct_reorder2(state, day, deaths)) %>%
  ggplot(data = ., mapping = aes(day, deaths)) +
    geom_line(mapping = aes(color = factor_var), size = 2) +
    scale_colour_manual(values = cols, name = "State") +
    scale_y_log10(limits = c(3, 2000), breaks = c(3, 10, 30, 100, 300, 1000), labels = comma_format(accuracy = 1)) +
    scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
    labs(
      title = "Pace of COVID-19 Deaths by State",
      subtitle = paste0("Current as of ", as.character(date)),
      caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
      x = "Days Since Third Death Confirmed",
      y = "Count of Confirmed Deaths (Log)"
    ) +
    sequoia_theme(base_size = 22, background = "white")

save_plots(filename = "results/high_res/state/g_mortality_log.png", preset = "lg")
save_plots(filename = "results/low_res/state/g_mortality_log.png", preset = "lg", dpi = 72)

# =============================================================================

# create days from first day where average deaths were over 5, state-level data
## subset data
state_data %>%
  filter(deaths_avg >= 3) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date)) %>%
  select(day, report_date, state, deaths_avg) %>%
  arrange(state, day) -> state_subset

# define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 10, f = ceiling)

## plot data
state_subset %>%
  mutate(factor_var = fct_reorder2(state, day, deaths_avg)) %>%
  ggplot(data = ., mapping = aes(day, deaths_avg)) +
  geom_line(mapping = aes(color = factor_var), size = 2) +
  scale_colour_manual(values = cols, name = "State") +
  scale_y_log10(limits = c(3, 100), breaks = c(3,10,30,100)) +
  scale_x_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) +
  labs(
    title = "Pace of New COVID-19 Deaths by State",
    subtitle = paste0("Current as of ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE and New York Times COVID-19 Projects",
    x = "Days Since Average of Three Cases Confirmed",
    y = "7-day Average of New Cases (Log)"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plots
save_plots(filename = "results/high_res/state/h_mortality_log_avg.png", preset = "lg")
save_plots(filename = "results/low_res/state/h_mortality_log_avg.png", preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(mo_sf, state_data, state_subset, plot_date, top_val, date_breaks, pal, cols)
