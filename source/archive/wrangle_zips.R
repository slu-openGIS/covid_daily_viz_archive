# process zip code data

library(dplyr)
library(readr)

df <- read_csv("data/source/stl_daily_zips/stl_city_2020_04_01.csv")
df <- mutate(df,
  report_date = as.Date("2020-04-01"),
  geoid = "29510",
  county = "St. Louis City",
  state = "Missouri",
  last_update = as.POSIXct("2020-04-01 17:00:00", tz = Sys.timezone())
)
df <- select(df, report_date, zip, geoid, county, state, last_update, count)
df <- rename(df, confirmed = count)

write_csv(df, "data/zip/zip_stl_city.csv")
