
calculate_days <- function(.data, group_var, stat_var, val){
  
  ## quote
  group_varQ <- rlang::quo(!! rlang::sym(group_var))
  stat_varQ <- rlang::quo(!! rlang::sym(stat_var))
  
  ## create date_tibble object
  date_tibble <- dplyr::filter(.data, !!stat_varQ >= val)
  date_tibble <- dplyr::group_by(date_tibble, !!group_varQ)
  date_tibble <- dplyr::arrange(date_tibble, report_date)
  date_tibble <- dplyr::summarise(date_tibble, first_day = first(report_date), .groups = "drop_last")
  
  ## identify style
  if (group_var == "state"){
    sty <- "state"
  } else {
    sty <- "geoid"
  }
  
  ## pre-process
  if (sty == "geoid"){
    out <- dplyr::filter(.data, geoid %in% date_tibble$geoid) 
  } else{
    out <- .data
  }
  
  ## subset data
  out %>%
    dplyr::group_split(!!group_varQ) %>%
    purrr::map_df(~filter_date(target = .x, ref = date_tibble, style = sty)) -> out
  
  ## calculate days
  out <- dplyr::group_by(out, !!group_varQ)
  out <- dplyr::mutate(out, first_date = first(report_date))
  out <- dplyr::ungroup(out)
  out <- dplyr::mutate(out, day = as.numeric(report_date-first_date))
  
  ## return output
  return(out)
  
}

filter_date <- function(target, ref, style){
  
  if (style == "state"){
    ref_sub <- dplyr::filter(ref, state == first(target$state)) 
  } else if (style == "geoid"){
    ref_sub <- dplyr::filter(ref, geoid == first(target$geoid)) 
  }
  
  filter_date <- ref_sub["first_day"][[1]]
  
  out <- dplyr::arrange(target, report_date)
  out <- dplyr::filter(out, report_date >= filter_date)
  
  return(out)
  
}
