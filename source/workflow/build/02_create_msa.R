
# create list with vectors of metro counties by GEOID
metro_counties <- list(
  cape_girardeau = c("29031", "29017", "17003"),
  cape_girardeau_il = "17003",
  columbia = c("29019", "29007", "29175", "29053", "29089"),
  jeff_city = c("29051", "29027", "29135", "29151"),
  joplin = c("29097", "29145", "40115"),
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
  mutate(msa = case_when(
    geoid %in% metro_counties$jeff_city ~ "Jefferson City",
    geoid %in% metro_counties$columbia ~ "Columbia",
    geoid %in% metro_counties$springfield ~ "Springfield",
    geoid %in% metro_counties$joplin ~ "Joplin",
    geoid %in% metro_counties$st_joseph ~ "St. Joseph",
    geoid %in% metro_counties$cape_girardeau ~ "Cape Girardeau",
    geoid %in% metro_counties$st_louis ~ "St. Louis",
    geoid %in% metro_counties$kansas_city ~ "Kansas City"
  )) %>%
  filter(is.na(msa) == FALSE) %>%
  group_by(msa, report_date) %>%
  summarise(
    confirmed = sum(confirmed),
    new_confirmed = sum(new_confirmed),
    deaths = sum(deaths),
    new_deaths = sum(new_deaths)
  ) %>%
  mutate(
    confirmed_avg = rollmean(new_confirmed, k = 7, align = "right", fill = NA),
    deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA)
  ) %>%
  filter(report_date >= "2020-01-24") -> metro_data

# remove early reports from county data
county_data <- filter(county_data, report_date >= "2020-01-24")
