```{r mo-infections, echo=FALSE, out.width = '100%'}
knitr::include_graphics("img/mo_individual/a_race_case.png")
```

Download: [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/high_res/mo_individual/a_race_case.png" target="_blank">High-res</a>] [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/low_res/mo_individual/a_race_case.png" target="_blank">Low-res</a>]



# =============================================================================

# plot morbidity rates, race

## define top_val
top_val <- round_any(x = max(covid_race$case_rate, na.rm = TRUE), accuracy = 200, f = ceiling)

## create plot
p <- ggplot(data = covid_race, mapping = aes(x = value, y = case_rate, fill = value)) +
  geom_bar(position = "dodge", stat = "identity", width = .65, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 200)) +
  labs(
    title = "Reported Cases by Race, Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "Race",
    y = "Estimated Rate per 100,000 Individuals",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri and the U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

# \nMissouri's data are missing race for ~17% of cases

## save plot
save_plots(filename = "results/high_res/mo_individual/a_race_case.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/mo_individual/a_race_case.png", plot = p, preset = "lg", dpi = 72)