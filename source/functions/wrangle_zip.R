

wrange_zip <- function(date, county){
  
  file <- paste0("data/source/stl_daily_zips/stl_city_", date, ".csv")
  
  df <- readr::read_csv(file)
  
  if (county == 510){
    pop <- readr::read_csv("data/source/stl_zips/stl_city_zip/stl_city_zip.csv") 
  } else if (county == 189){
    pop <- readr::read_csv("data/source/stl_zips/stl_county_zip/stl_city_zip.csv") 
  }
  
}