# vaccine data for Missouri Highway Patrol Districts

# =============================================================================

# load data
district <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/district/daily_snapshot_mo_vaccines.csv")

# =============================================================================

# plot first dose rates

## define top_val
top_val <- round_any(x = max(district$first_dose_rate, na.rm = TRUE), accuracy = 10, f = ceiling)

## create plot
p <- ggplot(data = district, mapping = aes(x = reorder(district, -first_dose_rate), y = first_dose_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 10)) +
  labs(
    title = "Vaccinations by Highway Patrol District, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Highway Patrol District",
    y = "First Doses per 1,000 Individuals",
    caption = caption_text_census_map  
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/district/a_vaccine_first.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/district/a_vaccine_first.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

rm(p, top_val, caption_text_census_map, district)
