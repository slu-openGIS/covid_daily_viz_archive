# vaccine data for Missouri Highway Patrol Districts

# =============================================================================

# load data
district <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/district/daily_snapshot_mo_vaccines.csv")

# =============================================================================

# plot first dose rates

## define top_val
top_val <- round_any(x = max(district$initiated_rate, na.rm = TRUE), accuracy = 25, f = ceiling)

## create plot
p <- ggplot(data = district, mapping = aes(x = reorder(district, -initiated_rate), y = initiated_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 25)) +
  labs(
    title = "Initiated Vaccinations by MO Highway Patrol District",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Highway Patrol District",
    y = "Vaccinations per 1,000 Individuals",
    caption = paste0(caption_text_census_map,"\nInitiation means a person has recieved at least one dose of the Moderna, Pfizer, or Johnson & Johnson vaccine.")
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/district/a_vaccine_initial.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/district/a_vaccine_initial.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot completed dose rates

## define top_val
top_val <- round_any(x = max(district$complete_rate, na.rm = TRUE), accuracy = 25, f = ceiling)

## create plot
p <- ggplot(data = district, mapping = aes(x = reorder(district, -complete_rate), y = complete_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 25)) +
  labs(
    title = "Completed Vaccinations by MO Highway Patrol District",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Highway Patrol District",
    y = "Vaccinations per 1,000 Individuals",
    caption = paste0(caption_text_census_map,"\nCompleted vaccination means having recieved both doses of the Moderna and Pfizer vaccines, \n  or one dose of the Johnson & Johnson vaccine.")
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/district/b_vaccine_complete.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/district/b_vaccine_complete.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

# plot completed dose rates

## define top_val
top_val <- round_any(x = max(district$last7_rate, na.rm = TRUE), accuracy = 2, f = ceiling)

## create plot
p <- ggplot(data = district, mapping = aes(x = reorder(district, -last7_rate), y = last7_rate)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE,
           fill = RColorBrewer::brewer.pal(4, "Set1")[4]) +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 2)) +
  labs(
    title = "New Vaccinations by MO Highway Patrol District",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Highway Patrol District",
    y = "Vaccinations in Last Week per 1,000 Individuals",
    caption = caption_text_census_map
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

## save plot
save_plots(filename = "results/high_res/district/c_vaccine_last7.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/district/c_vaccine_last7.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

rm(p, top_val, caption_text_census_map, district)
