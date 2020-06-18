# re-format KC data

#### load raw data #### 
orig <- read_excel("data/source/kc_county_breakdowns/KCMO COVID-19 Data by County.xlsx")

#### covert to long #### 
tidy <- pivot_longer(orig, cols = c("Clay", "Jackson", "Platte", "Cass"), names_to = "county", values_to = "cases")

#### clean-up #### 
tidy %>%
  rename(report_date = Date) %>%
  mutate(geoid = case_when(
    county == "Clay" ~ "29047",
    county == "Jackson" ~ "29095",
    county == "Platte" ~ "29165",
    county == "Cass" ~ "29037"
  )) %>%
  mutate(state = "Missouri") %>%
  select(report_date, geoid, county, state, cases) %>%
  mutate(report_date = as.Date(report_date)) -> tidy

#### write data #### 
write_csv(tidy, "data/county/kc_cases_by_county.csv")

#### clean-up #### 
rm(orig, tidy)
