map_breaks <- function(.data, var, newvar, classes, style, breaks, clean_labels = TRUE, dig_lab = 10){
  
  # save parameters to list
  paramList <- as.list(match.call())
  
  # quote input variables
  if (!is.character(paramList$var)) {
    ref <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    ref <- rlang::quo(!! rlang::sym(var))
  }
  
  refQ <- rlang::quo_name(rlang::enquo(ref))
  
  if (!is.character(paramList$newvar)) {
    new <- rlang::enquo(newvar)
  } else if (is.character(paramList$newvar)) {
    new <- rlang::quo(!! rlang::sym(newvar))
  }
  
  newQ <- rlang::quo_name(rlang::enquo(new))
  
  # calculate breaks and categories
  if (missing(breaks) == TRUE){
    breaks <- classInt::classIntervals(.data[[refQ]], n = classes, style = style)
  }
  
  categories <- cut(.data[[refQ]], breaks = c(breaks$brks), include.lowest = TRUE, dig.lab = dig_lab)
  
  # create new variable
  .data <- dplyr::mutate(.data, !!newQ := categories)
  
  # clean labels
  if (clean_labels == TRUE){
    
    .data[[newQ]] %>%
      forcats::fct_relabel(~ gsub(",", " - ", .x)) %>%
      forcats::fct_relabel(~ gsub("\\(", "", .x)) %>%
      forcats::fct_relabel(~ gsub("\\[", "", .x)) %>%
      forcats::fct_relabel(~ gsub("\\]", "", .x)) -> .data[[newQ]]
    
  }
  
  # return result
  return(.data)
  
}

map_bins <- function(.data, var, classes = 5, style = "jenks", round = 0, dig_lab = 10){
  
  # calculate breaks
  breaks <- classInt::classIntervals(.data[[var]], n = classes, style = style)
  categories <- cut(.data[[var]], breaks = c(breaks$brks), include.lowest = TRUE, dig.lab = dig_lab)
  
  # parse categories
  categories <- levels(categories)
  # categories <- unique(categories)
  categories <- gsub("[][()]", "", categories)
  categories <- gsub(",", " ", categories)
  categories <- stringr::word(categories, 2)
  categories <- round(as.numeric(categories), digits = round)
  bins <- c(0, categories)
  
  # return output
  return(bins)
  
}
