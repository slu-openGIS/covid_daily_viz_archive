# hospitalization data for St. Louis Metro

# =============================================================================

# load data
stl_hosp <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/metro/stl_hospital.csv")

# =============================================================================

# define colors
pal <- brewer.pal(n = 3, name = "Set1")
cols <- c("7-day Average" = pal[1], "Count" = pal[2])

# =============================================================================

# plot new in patient

## define top_val
top_val <- round_any(x = max(stl_hosp$new_in_pt, na.rm = TRUE), accuracy = 10, f = ceiling)

## subset
stl_hosp %>%
  filter(report_date <= date-2) %>%
  select(report_date, new_in_pt, new_in_pt_avg) %>%
  pivot_longer(cols = c(new_in_pt, new_in_pt_avg), names_to = "category", values_to = "value") %>%
  mutate(category = case_when(
    category == "new_in_pt" ~ "Count",
    category == "new_in_pt_avg" ~ "7-day Average"
  )) %>%
  mutate(category = fct_relevel(category, "Count", "7-day Average")) -> stl_subset

avg_line <- filter(stl_subset, category == "7-day Average")

## create points
hosp_points <- filter(stl_subset, report_date == date-2)

## create factors
stl_subset <- mutate(stl_subset, factor_var = fct_reorder2(category, report_date, value))
hosp_points <- mutate(hosp_points, factor_var = fct_reorder2(category, report_date, value))

## plot
p <- ggplot() +
  geom_line(stl_subset, mapping = aes(x = report_date, y = value, color = factor_var), size = 2) +
  geom_line(avg_line, mapping = aes(x = report_date, y = value), color = cols[1], size = 2) +
  geom_point(hosp_points, mapping = aes(x = report_date, y = value, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Measure") +
  scale_x_date(date_breaks = hosp_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 10)) + 
  labs(
    title = "New COVID-19 Hospitalizations in Metro St. Louis",
    subtitle = paste0("St. Louis Metropolitan Pandemic Task Force Hospitals\n", min(stl_subset$report_date), " through ", as.character(date-2)),
    x = "Date",
    y = "New Patients",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the St. Louis Metro Parademic Task Force"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/stl_metro/n_new_in_pt.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/n_new_in_pt.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot in patient

## define top_val
top_val <- round_any(x = max(stl_hosp$in_pt, na.rm = TRUE), accuracy = 50, f = ceiling)

## subset
stl_hosp %>%
  filter(report_date >= as.Date("2020-04-05")) %>%
  select(report_date, in_pt, in_pt_avg) %>%
  pivot_longer(cols = c(in_pt, in_pt_avg), names_to = "category", values_to = "value") %>%
  mutate(category = case_when(
    category == "in_pt" ~ "Count",
    category == "in_pt_avg" ~ "7-day Average"
  )) %>%
  mutate(category = fct_relevel(category, "Count", "7-day Average")) -> stl_subset

avg_line <- filter(stl_subset, category == "7-day Average")

## create points
hosp_points <- filter(stl_subset, report_date == date)

## create factors
stl_subset <- mutate(stl_subset, factor_var = fct_reorder2(category, report_date, value))
hosp_points <- mutate(hosp_points, factor_var = fct_reorder2(category, report_date, value))

## plot
p <- ggplot() +
  geom_line(stl_subset, mapping = aes(x = report_date, y = value, color = factor_var), size = 2) +
  geom_line(avg_line, mapping = aes(x = report_date, y = value), color = cols[1], size = 2) +
  geom_point(hosp_points, mapping = aes(x = report_date, y = value, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Measure") +
  scale_x_date(date_breaks = hosp_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 50)) + 
  labs(
    title = "Total COVID-19 Hospitalizations in Metro St. Louis",
    subtitle = paste0("St. Louis Metropolitan Pandemic Task Force Hospitals\n", min(stl_subset$report_date), " through ", as.character(date)),
    x = "Date",
    y = "Total Patients",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the St. Louis Metro Parademic Task Force"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/stl_metro/o_in_pt.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/o_in_pt.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot icu

## define top_val
top_val <- round_any(x = max(stl_hosp$icu, na.rm = TRUE), accuracy = 25, f = ceiling)

## subset
stl_hosp %>%
  filter(report_date >= as.Date("2020-04-05")) %>%
  select(report_date, icu, icu_avg) %>%
  pivot_longer(cols = c(icu, icu_avg), names_to = "category", values_to = "value") %>%
  mutate(category = case_when(
    category == "icu" ~ "Count",
    category == "icu_avg" ~ "7-day Average"
  )) %>%
  mutate(category = fct_relevel(category, "Count", "7-day Average")) -> stl_subset

avg_line <- filter(stl_subset, category == "7-day Average")

## create points
hosp_points <- filter(stl_subset, report_date == date)

## create factors
stl_subset <- mutate(stl_subset, factor_var = fct_reorder2(category, report_date, value))
hosp_points <- mutate(hosp_points, factor_var = fct_reorder2(category, report_date, value))

## plot
p <- ggplot() +
  geom_line(stl_subset, mapping = aes(x = report_date, y = value, color = factor_var), size = 2) +
  geom_line(avg_line, mapping = aes(x = report_date, y = value), color = cols[1], size = 2) +
  geom_point(hosp_points, mapping = aes(x = report_date, y = value, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Measure") +
  scale_x_date(date_breaks = hosp_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 25)) + 
  labs(
    title = "Total COVID-19 ICU Patients in Metro St. Louis",
    subtitle = paste0("St. Louis Metropolitan Pandemic Task Force Hospitals\n", min(stl_subset$report_date), " through ", as.character(date)),
    x = "Date",
    y = "Total ICU Patients",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the St. Louis Metro Parademic Task Force"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/stl_metro/p_icu.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/p_icu.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot icu

## define top_val
top_val <- round_any(x = max(stl_hosp$vent, na.rm = TRUE), accuracy = 20, f = ceiling)

## subset
stl_hosp %>%
  filter(report_date >= as.Date("2020-04-05")) %>%
  select(report_date, vent, vent_avg) %>%
  pivot_longer(cols = c(vent, vent_avg), names_to = "category", values_to = "value") %>%
  mutate(category = case_when(
    category == "vent" ~ "Count",
    category == "vent_avg" ~ "7-day Average"
  )) %>%
  mutate(category = fct_relevel(category, "Count", "7-day Average")) -> stl_subset

avg_line <- filter(stl_subset, category == "7-day Average")

## create points
hosp_points <- filter(stl_subset, report_date == date)

## create factors
stl_subset <- mutate(stl_subset, factor_var = fct_reorder2(category, report_date, value))
hosp_points <- mutate(hosp_points, factor_var = fct_reorder2(category, report_date, value))

## plot
p <- ggplot() +
  geom_line(stl_subset, mapping = aes(x = report_date, y = value, color = factor_var), size = 2) +
  geom_line(avg_line, mapping = aes(x = report_date, y = value), color = cols[1], size = 2) +
  geom_point(hosp_points, mapping = aes(x = report_date, y = value, color = factor_var), 
             size = 4, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = "Measure") +
  scale_x_date(date_breaks = hosp_breaks, date_labels = "%b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 20)) + 
  labs(
    title = "Total COVID-19 Ventilated Patients in Metro St. Louis",
    subtitle = paste0("St. Louis Metropolitan Pandemic Task Force Hospitals\n", min(stl_subset$report_date), " through ", as.character(date)),
    x = "Date",
    y = "Total Ventilated Patients",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the St. Louis Metro Parademic Task Force"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/stl_metro/q_vent.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/q_vent.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(stl_hosp, stl_subset, hosp_points, avg_line)
rm(top_val, p, cols, pal)
