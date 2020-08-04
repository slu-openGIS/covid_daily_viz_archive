# plot state level test data

# =============================================================================

# create test_date
test_date <- date

# =============================================================================

# load data
state_test_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/state/state_testing.csv")

# =============================================================================

# define colors
pal <- brewer.pal(n = 5, name = "Set1")
cols <- c("Illinois" = pal[1], "Kansas" = pal[2], "Missouri" = pal[3], "Oklahoma" = pal[4],
          "Arkansas" = pal[5])

# =============================================================================

# plot test rate
## subset
state_subset <- filter(state_test_data, report_date >= as.Date("2020-05-23") & report_date <= test_date)

## re-create end points
state_points <- filter(state_test_data, report_date == test_date)

## define top_val
top_val <- round_any(x = max(state_subset$test_rate), accuracy = 2000, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, test_rate))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, test_rate))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = test_rate, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = test_rate, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = test_date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 2000)) + 
  labs(
    title = "COVID-19 Persons Tested by State",
    subtitle = paste0("2020-05-23 through ", as.character(test_date)),
    x = "Date",
    y = "Cumulative Rate per 100,000 Residents",
    caption = caption_text_tests_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/state/k_test_rate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/k_test_rate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot 7-day average of new tests
## subset
state_subset <- filter(state_test_data, report_date >= as.Date("2020-05-30") & report_date <= test_date) %>%
  filter(state != "Oklahoma")

## re-create end points
state_points <- filter(state_test_data, report_date == test_date)

## define top_val
top_val <- round_any(x = max(state_subset$new_test_rate_avg, na.rm = TRUE), accuracy = 25, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, new_test_rate_avg))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, new_test_rate_avg)) %>%
  filter(state != "Oklahoma")

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = new_test_rate_avg, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = new_test_rate_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = test_date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 25)) + 
  labs(
    title = "New Persons Tested for COVID-19 by State",
    subtitle = paste0("2020-05-30 through ", as.character(test_date)),
    x = "Date",
    y = "7-day Average of New Tests per 100,000 Residents",
    caption = caption_text_tests_census
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/state/l_new_tests_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/l_new_tests_avg.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot 7-day average of new tests
## subset
state_subset <- filter(state_test_data, report_date >= as.Date("2020-05-30") & report_date <= test_date) %>%
  filter(state %in% c("Oklahoma", "Kansas", "Arkansas") == FALSE)

## re-create end points
state_points <- filter(state_test_data, report_date == test_date) %>%
  filter(state %in% c("Oklahoma", "Kansas", "Arkansas") == FALSE)

## define top_val
top_val <- round_any(x = max(state_subset$positive_avg), accuracy = 1, f = ceiling)

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, report_date, positive_avg))
state_points <- mutate(state_points, factor_var = fct_reorder2(state, report_date, positive_avg))

## create plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = positive_avg, color = factor_var), size = 2) +
  geom_point(state_points, mapping = aes(x = report_date, y = positive_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "State") +
  scale_x_date(date_breaks = test_date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0,top_val), breaks = seq(0, top_val, by = 1)) + 
  labs(
    title = "COVID-19 Positive Individuals by State",
    subtitle = paste0("2020-05-30 through ", as.character(test_date)),
    x = "Date",
    y = "7-day Average of Positive Tests (%)",
    caption = caption_text_tests
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/state/m_positive_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/m_positive_avg.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(caption_text_tests, caption_text_tests_census, state_points, state_subset, state_test_data,
   test_date, test_date_breaks)
rm(p, cols, top_val, pal)

# =============================================================================

# load data
state_live_data <- read_csv("data/MO_HEALTH_Covid_Tracking/data/state/state_live_tests.csv")

# =============================================================================

# remove errant last line
if (remove_last == TRUE){
  
  state_live_data <- slice(state_live_data, 1:(n()-1))
  
}

# =============================================================================

# define colors
pal <- brewer.pal(n = 3, name = "Set1")
cols <- c("7-day Average" = pal[1], "Count" = pal[2])

# =============================================================================

# plot average new tests

## define top_val
top_val <- round_any(x = max(state_live_data$pcr_total, na.rm = TRUE), accuracy = 2000, f = ceiling)

## subset
state_live_data %>%
  select(report_date, pcr_total, pcr_avg) %>%
  pivot_longer(cols = c(pcr_total, pcr_avg), names_to = "category", values_to = "value") %>%
  mutate(category = case_when(
    category == "pcr_total" ~ "Count",
    category == "pcr_avg" ~ "7-day Average"
  )) %>%
  mutate(category = fct_relevel(category, "Count", "7-day Average")) -> state_live_subset

avg_line <- filter(state_live_subset, category == "7-day Average")

## create points
if (remove_last == TRUE){
  test_points <- filter(state_live_subset, report_date == date-4)
} else {
  test_points <- filter(state_live_subset, report_date == date-3) 
}

## create factors
state_live_subset <- mutate(state_live_subset, factor_var = fct_reorder2(category, report_date, value))
test_points <- mutate(test_points, factor_var = fct_reorder2(category, report_date, value))

## plot
p <- ggplot() +
  geom_line(state_live_subset, mapping = aes(x = report_date, y = value, color = factor_var), size = 2) +
  geom_line(avg_line, mapping = aes(x = report_date, y = value), color = cols[1], size = 2) +
  geom_point(test_points, mapping = aes(x = report_date, y = value, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Measure") +
  scale_x_date(date_breaks = total_test_date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 2000)) + 
  labs(
    title = "Daily COVID-19 PCR Tests",
    subtitle = paste0(min(state_live_subset$report_date), " through ", 
                      ifelse(remove_last == TRUE, as.character(date-4), as.character(date-3))),
    x = "Date",
    y = "New Tests",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/state/n_total_tests_avg.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/n_total_tests_avg.png", plot = p, preset = "lg", dpi = 72)

## clean-up
rm(remove_last)

