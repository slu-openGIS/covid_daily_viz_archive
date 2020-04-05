mo_counties <- st_read("data/source/mo_county_plus/mo_county_plus.shp") %>%
  select(GEOID) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  st_transform(crs = 102003)

## download geometry for IL and KS
il_ks_counties <- counties(state = c(17, 20), cb = FALSE, class = "sf") %>%
  select(GEOID) %>%
  st_transform(crs = 102003)

## combine
counties_sf <- rbind(mo_counties, il_ks_counties)
counties_sf <- arrange(counties_sf, GEOID)

ks_metro_counties <- c("20091", "20103", "20107", "20121", "20209")
il_metro_counties <- c("17005", "17013", "17027", "17083", "17117", 
                       "17119", "17133", "17163")
mo_counties <- as.vector(mo_counties$GEOID)
mo_counties_xl <- c(il_metro_counties, ks_metro_counties, mo_counties)
mo_xl_sf <- filter(counties_sf, GEOID %in% mo_counties_xl)

rm(counties_sf, il_ks_counties, il_metro_counties, ks_metro_counties, mo_counties, mo_counties_xl)

x <- read_excel(path = "data/source/KHN_ICU_bed_county_analysis_2.xlsx") %>%
  select(cnty_fips, hospitals_in_cost_reports, all_icu) %>%
  filter(cnty_fips %in% mo_xl_sf$GEOID) %>%
  rename(
    GEOID = cnty_fips,
    hospitals = hospitals_in_cost_reports,
    icu_beds = all_icu
  )

write_csv(x, "data/source/kaiser_health_news_icu_bed_counts.csv")

