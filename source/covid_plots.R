# plot data

# plot confirmed rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = state_name)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  labs(
    title = "Confirmed COVID-19 Cases by State",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nConfirmed cases are those with a positive test as a proportion of the total population"
  )

ggsave(filename = "results/confirmed_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot case fatality rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = state_name)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  labs(
    title = "COVID-19 Case Fatality by State",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nCase fatality is the percent of confirmed cases that result in death"
  )

ggsave(filename = "results/case_fatality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# plot mortality rate
ggplot(data = summary_data, mapping = aes(x = report_date, y = mortality_rate)) +
  geom_line(mapping = aes(color = state_name)) +
  scale_color_brewer(palette = "Set1", name = "State") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  labs(
    title = "Confirmed COVID-19 Mortality by State",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    x = "Date",
    y = "Mortality Rate per 100,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nMortality rate is the number of deaths as a proportion of the total population"
  )

ggsave(filename = "results/mortality_rate.png", width = 8, height = 6, units = "in", dpi = 500)

# map confirmed rate
detailed_sf <- mutate(detailed_sf, county = ifelse(GEOID %in% c("29189", "29510"), NA, county))

ggplot(data = detailed_sf) +
  geom_sf(mapping = aes(fill = c_rate)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "Oranges", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nConfirmed cases are those with a positive test as a proportion of the total population\nSt. Louis City and County are intentionally not labeled to increase readability of map"
  ) +
  theme_void()

ggsave(filename = "results/confirmed_rate_metro_map.png", width = 8, height = 6, units = "in", dpi = 500)

# plot confirmed rate
ggplot(data = detailed_sub, mapping = aes(x = report_date, y = confirmed_rate)) +
  geom_line(mapping = aes(color = name))  +
  gghighlight(geoid %in% c("29189", "29510", "17027", "17133" , "29183")) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 0.25)) + 
  labs(
    title = "Confirmed COVID-19 Cases by Metro St. Louis County",
    subtitle = paste0("2020-03-22 through ", as.character(date)),
    x = "Date",
    y = "Rate of Confirmed Infections per 1,000",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nConfirmed cases are those with a positive test as a proportion of the total population"
  )

ggsave(filename = "results/confirmed_rate_metro.png", width = 8, height = 6, units = "in", dpi = 500)

# map case fatality rate
ggplot(data = detailed_sf) +
  geom_sf(mapping = aes(fill = cf_rate)) +
  geom_sf_label(mapping = aes(label = county), label.padding = unit(0.15, "lines")) +
  scale_fill_distiller(palette = "Reds", trans = "reverse", name = "Rate per 1,000") +
  labs(
    title = "COVID-19 Case Fatality by Metro St. Louis County",
    subtitle = paste0("2020-03-10 through ", as.character(date)),
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nCase fatality is the percent of confirmed cases that result in death\nSt. Louis City and County are intentionally not labeled to increase readability of map"
  ) +
  theme_void()

ggsave(filename = "results/case_fatality_metro_map.png", width = 8, height = 6, units = "in", dpi = 500)

# plot confirmed rate
detailed_sub <- mutate(detailed_sub, case_fatality_rate = ifelse(is.na(case_fatality_rate) == TRUE, 0, case_fatality_rate))

ggplot(data = detailed_sub, mapping = aes(x = report_date, y = case_fatality_rate)) +
  geom_line(mapping = aes(color = name))  +
  gghighlight(geoid %in% c("29189", "29510", "29183")) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d %b")  +
  scale_y_continuous(limits = c(0, 10), breaks = c(0,2,4,6,8,10)) + 
  labs(
    title = "COVID-19 Case Fatality by Metro St. Louis County",
    subtitle = paste0("2020-03-22 through ", as.character(date)),
    x = "Date",
    y = "Case Fatality (%)",
    caption = "Plot by Christopher Prener, Ph.D.\nData via Johns Hopkins University CSSE COVID-19 Project\nCase fatality is the percent of confirmed cases that result in death"
  )

ggsave(filename = "results/case_fatality_metro.png", width = 8, height = 6, units = "in", dpi = 500)

# clean-up
rm(date)
