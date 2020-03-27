# create historic counties data
historic_expand <- function(ref, date){
  
  # add date
  out <- dplyr::mutate(ref, report_date = date)
  
  # return output
  return(out)
  
}
