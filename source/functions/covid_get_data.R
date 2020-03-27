# define download function
get_data <- function(file){
  
  # create values
  url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", file)
  date <- lubridate::mdy(stringr::str_replace(string = file, pattern = ".csv", replacement = ""))
  
  # download data
  response <- RCurl::getURL(url = url)
  
  # read data
  df <- readr::read_csv(response)
  
  # tidy data
  if (date >= "2020-03-22"){
    
    df <- dplyr::filter(df, Province_State %in% c("Missouri", "Illinois"))
    df <- dplyr::select(df, FIPS, Admin2, Province_State, Last_Update, Confirmed, Deaths, Recovered, Active)
    df <- dplyr::rename(df,
                        geoid = FIPS,
                        name = Admin2,
                        state_name = Province_State,
                        last_update = Last_Update,
                        confirmed = Confirmed,
                        deaths = Deaths,
                        recovered = Recovered,
                        active = Active)
    df <- dplyr::mutate(df, report_date = date)
    df <- dplyr::select(df, geoid, name, state_name, report_date, dplyr::everything())
    df <- dplyr::arrange(df, as.numeric(geoid))
    
    # problems with 3/22
    if (date == "2020-03-22"){
      
      df <- dplyr::mutate(df, geoid = as.character(geoid))
      df <- dplyr::mutate(df, last_update = mdy_hm(last_update))
      
    }
    
  } else if (date < "2020-03-22"){
    
    df <- dplyr::rename(df,
                        state_name = `Province/State`,
                        last_update = `Last Update`,
                        confirmed = Confirmed,
                        deaths = Deaths,
                        recovered = Recovered)
    df <- dplyr::filter(df, state_name %in% c("Missouri", "Illinois"))
    df <- dplyr::select(df, state_name, last_update, confirmed, deaths, recovered)
    df <- dplyr::mutate(df, report_date = date)
    df <- dplyr::select(df, state_name, report_date, dplyr::everything())
    df <- dplyr::arrange(df, state_name)
    
  }
  
  # return output
  return(df)
  
}