

wrangle_zip <- function(date, county){
  
  # set county name
  if (county == 510){
    county_name <- "St. Louis City" 
  } else if (county == 189){
    county_name <- "St. Louis"
  }
  
  # construct file path
  if (county == 510){
    file <- paste0("data/source/stl_daily_zips/stl_city_", date, ".csv")
  } else if (county == 189){
    file <- paste0("data/source/stl_daily_zips/stl_county_", date, ".csv")
  }
  
  # read data
  df <- readr::read_csv(file)
  
  # read population
  if (county == 510){
    pop <- readr::read_csv("data/source/stl_zips/stl_city_zip/stl_city_zip.csv") 
  } else if (county == 189){
    pop <- readr::read_csv("data/source/stl_zips/stl_county_zip/stl_county_zip.csv") 
  }
  
  # add new columns, modify existing
  df <- dplyr::mutate(df,
                      report_date = date,
                      geoid = paste0("29", county),
                      county = county_name,
                      state = "Missouri")
  df <- dplyr::select(df, report_date, zip, geoid, county, state, count)
  df <- dplyr::rename(df, confirmed = count)
  
  # calculate rate
  df <- dplyr::left_join(df, pop, by = "zip")
  df <- dplyr::mutate(df, confirmed_rate = confirmed/total_pop*1000)
  df <- dplyr::select(df, -total_pop)
  
  # return output
  return(df)
  
}
