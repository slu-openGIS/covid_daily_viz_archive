
## read data
hosp_data <- read_csv("data/source/stl_hospital/historic_data.csv") %>%
  mutate(date = mdy(date)) %>%
  rename(report_date = date)

## calculate rolling averages
hosp_data %>%
  mutate(new_in_pt_avg = rollmean(new_in_pt, k = 7, align = "right", fill = NA)) %>%
  mutate(in_pt_avg = rollmean(in_pt, k = 7, align = "right", fill = NA)) %>%
  mutate(icu_avg = rollmean(icu, k = 7, align = "right", fill = NA)) %>%
  mutate(vent_avg = rollmean(vent, k = 7, align = "right", fill = NA)) %>%
  select(report_date, new_in_pt, new_in_pt_avg, in_pt, in_pt_avg, icu, icu_avg, 
         vent, vent_avg, discharge, new_discharge) -> hosp_data

# write data
write_csv(hosp_data, path = "data/metro/stl_hospital.csv")

# clean-up
rm(hosp_data)
