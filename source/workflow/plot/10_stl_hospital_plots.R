# hospitalization data for St. Louis Metro

# =============================================================================

# load data
stl_hosp <-  read_csv("data/metro/stl_hospital.csv")

# =============================================================================

# plot new in patient

## define top_val
top_val <- round_any(x = max(stl_hosp$new_in_pt, na.rm = TRUE), accuracy = 5, f = ceiling)

## subset
stl_hosp %>%
  select(report_date, new_in_pt, new_in_pt_avg) %>%
  pivot_longer(cols = c(new_in_pt, new_in_pt_avg), names_to = "category", values_to = "value") %>%
  mutate(category = case_when(
    category == "new_in_pt" ~ "Count",
    category == "new_in_pt_avg" ~ "7-day Average"
  )) %>%
  mutate(category = fct_relevel(category, "Count", "7-day Average")) -> stl_subset

## create points
hosp_points <- filter(stl_subset, report_date == date)

## create factors
# stl_subset <- mutate(stl_subset, factor_var = fct_reorder2(category, report_date, value))
# hosp_points <- mutate(hosp_points, factor_var = fct_reorder2(category, report_date, value))

## plot
p <- ggplot() +
  geom_line(stl_subset, mapping = aes(x = report_date, y = value, color = category), size = 2) +
  geom_point(hosp_points, mapping = aes(x = report_date, y = value, color = category), 
             size = 4, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Measure") +
  scale_x_date(date_breaks = test_date_breaks, date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 5)) + 
  labs(
    title = "New COVID-19 In-Patients in Metro St. Louis",
    subtitle = paste0("St. Louis Metropolitan Pandemic Task Force Hospitals\n", min(stl_subset$report_date), " through ", as.character(date)),
    x = "Date",
    y = "New In-Patient Count",
    caption = "Plot by Christopher Prener, Ph.D.\nData via St. Louis Metro Parademic Task Force"
  ) +
  sequoia_theme(base_size = 22, background = "white")

## save plot
save_plots(filename = "results/high_res/stl_metro/n_new_in_pt.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/stl_metro/n_new_in_pt.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# clean-up
rm(stl_hosp, stl_subset, hosp_points, test_date_breaks, top_val)
