# define download function for Hopkins data
get_hopkins <- function(date, ref){
  
  # create values
  file <- paste0(format(date,'%m-%d-%Y'), ".csv")
  url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", file)
  # date <- lubridate::mdy(stringr::str_replace(string = file, pattern = ".csv", replacement = ""))
  
  # download data
  response <- RCurl::getURL(url = url)
  
  # read data
  df <- readr::read_csv(response)
  
  # tidy data
  if (date >= "2020-03-22"){
    
    df <- dplyr::filter(df, Province_State %in% c("Kansas", "Missouri", "Illinois", "Oklahoma"))
    df <- dplyr::select(df, FIPS, Admin2, Province_State, Last_Update, Confirmed, Deaths)
    df <- dplyr::rename(df,
                        geoid = FIPS,
                        county = Admin2,
                        state = Province_State,
                        last_update = Last_Update,
                        confirmed = Confirmed,
                        deaths = Deaths)
    df <- dplyr::mutate(df, report_date = date)
    df <- dplyr::select(df, report_date, geoid, county, state, dplyr::everything())
    df <- dplyr::arrange(df, as.numeric(geoid))
    df <- dplyr::mutate(df, geoid = ifelse(county == "Kansas City", "29511", geoid))
    
    # problems with 3/22
    if (date == "2020-03-22"){
      
      df <- dplyr::mutate(df, geoid = as.character(geoid))
      df <- dplyr::mutate(df, last_update = mdy_hm(last_update))
      
    # } else if (date == "2020-03-29" | date == "2020-03-30"){
      
    #  df <- dplyr::mutate(df, last_update = mdy_hm(last_update))
      
    } else if (date >= "2020-03-31"){
      
      df <- dplyr::select(df, geoid, confirmed, deaths)
      
      counties <- historic_expand(ref, date = date)
      
      df <- dplyr::left_join(counties, df, by = "geoid")
      
      # define update date and time value
      update_dateTime <- paste(date, "00:00:01")
      
      # fill in missing data
      df <- dplyr::mutate(df, last_update = update_dateTime)
      df <- dplyr::mutate(df, last_update = as.POSIXct(last_update))
      df <- dplyr::mutate(df, confirmed = ifelse(is.na(confirmed) == TRUE, 0, confirmed))
      df <- dplyr::mutate(df, deaths = ifelse(is.na(deaths) == TRUE, 0, deaths))
      df <- dplyr::select(df, report_date, geoid, county, state, last_update, confirmed, deaths)
    
    }
    
  } else if (date < "2020-03-22"){
    
    df <- dplyr::rename(df,
                        state = `Province/State`,
                        last_update = `Last Update`,
                        confirmed = Confirmed,
                        deaths = Deaths)
    df <- dplyr::filter(df, state %in% c("Kansas", "Missouri", "Illinois", "Oklahoma"))
    df <- dplyr::select(df, state, last_update, confirmed, deaths)
    df <- dplyr::mutate(df, report_date = date)
    df <- dplyr::select(df, report_date, state, dplyr::everything())
    df <- dplyr::arrange(df, state)
    
  }
  
  # 
  df <- dplyr::mutate(df, last_update = as.character(last_update))
  
  # return output
  return(df)
  
}

# define function for New york times data
get_times <- function(end_date){
  
  # create values
  url <- c("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  
  # download data
  response <- RCurl::getURL(url = url)
  
  # read data
  df <- readr::read_csv(response)
  
  # tidy data
  df <- dplyr::filter(df, state %in% c("Arkansas", "Kansas", "Missouri", "Illinois", "Oklahoma"))
  df <- dplyr::rename(df,
                      confirmed = cases,
                      report_date = date,
                      geoid = fips)
  df <- dplyr::mutate(df, geoid = ifelse(county == "Kansas City", "29511", geoid))
  df <- dplyr::mutate(df, geoid = ifelse(county == "Joplin", "29512", geoid))
  df <- dplyr::select(df, geoid, report_date, confirmed, deaths)
  
  if (is.null(end_date) == FALSE){
    
    df <- dplyr::filter(df, report_date <= as.Date(end_date))
    
  }
  
  # return output
  return(df)
  
}
