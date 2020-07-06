
# state data
# summarize county data to state
county_data %>%
  group_by(state, report_date) %>%
  summarise(
    cases = sum(cases),
    deaths = sum(deaths)
  ) %>%
  arrange(report_date, state) %>%
  select(report_date, everything()) -> state_data

# add daily new cases and deaths, state
state_data %>%
  group_by(state) %>%
  mutate(
    new_cases = cases - lag(cases),
    new_deaths = deaths - lag(deaths)
  ) %>%
  mutate(
    case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)) %>%
  filter(report_date >= "2020-01-24") %>%
  select(report_date, state,
         cases, new_cases, case_avg,
         deaths, new_deaths, deaths_avg) -> state_data

# create list with vectors of metro counties by GEOID
metro_counties <- list(
  cape_girardeau = c("29031", "29017", "17003"),
  cape_girardeau_il = "17003",
  columbia = c("29019", "29007", "29175", "29053", "29089"),
  jeff_city = c("29051", "29027", "29135", "29151"),
  joplin = c("29097", "29145", "29512", "40115"),
  joplin_ok = "40115",
  kansas_city = c("20091", "20103", "20107", "20121", "20209",
                  "29013", "29025", "29037", "29047", "29049", 
                  "29095", "29107", "29165", "29177", "29511"),
  kansas_city_ks = c("20091", "20103", "20107", "20121", "20209"),
  springfield = c("29077", "29043", "29225", "29167", "29225"),
  st_joseph = c("29003", "29021", "29063", "20043"),
  st_joseph_ks = "20043",
  st_louis = c("17005", "17013", "17027", "17083", "17117", 
               "17119", "17133", "17163", "29071", "29099", 
               "29113", "29183", "29189", "29219", "29510"),
  st_louis_il = c("17005", "17013", "17027", "17083", "17117", 
                  "17119", "17133", "17163")
)

# create MSA object
county_data %>%
  mutate(short_name = case_when(
    geoid %in% metro_counties$cape_girardeau ~ "Cape Girardeau",
    geoid %in% metro_counties$columbia ~ "Columbia",
    geoid %in% metro_counties$jeff_city ~ "Jefferson City",
    geoid %in% metro_counties$joplin ~ "Joplin",
    geoid %in% metro_counties$kansas_city ~ "Kansas City",
    geoid %in% metro_counties$springfield ~ "Springfield",
    geoid %in% metro_counties$st_joseph ~ "St. Joseph",
    geoid %in% metro_counties$st_louis ~ "St. Louis"
  )) %>%
  filter(is.na(short_name) == FALSE) %>%
  group_by(short_name, report_date) %>%
  summarise(
    cases = sum(cases),
    new_cases = sum(new_cases, na.rm = TRUE),
    deaths = sum(deaths),
    new_deaths = sum(new_deaths)
  ) %>%
  mutate(
    case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)
  ) %>%
  filter(report_date >= "2020-01-24") %>%
  ungroup() %>%
  mutate(geoid = case_when(
    short_name == "Cape Girardeau" ~ "16020",
    short_name == "Columbia" ~ "17860",
    short_name == "Jefferson City" ~ "27620",
    short_name == "Joplin" ~ "27900",
    short_name == "Kansas City" ~ "28140",
    short_name == "Springfield" ~ "44180",
    short_name == "St. Joseph" ~ "41140",
    short_name == "St. Louis" ~ "41180"
  )) %>%
  select(report_date, geoid, short_name, cases, new_cases, case_avg, 
         deaths, new_deaths, deaths_avg) -> metro_data

# remove early reports from county data
county_data <- filter(county_data, report_date >= "2020-01-24")
