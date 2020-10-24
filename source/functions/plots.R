# regional plot b/c/e - 7-day average counts ####

regional_count <- function(.data, region, point_data, state_data, region_data, plot_data, palette){
  
  # create region label
  if (region == "St. Louis"){
    region_label <- "St. Louis Metro Focus (MO Counties Only)\n"
  } else if (region == "Kansas City"){
    region_label <- "Kansas City Metro Focus (MO Counties Only)\n"
  } else if (region == "Outstate"){
    region_label <- "Outstate Focus\n"
  }
  
  # construct plot
  p <- ggplot() +
    geom_line(.data, mapping = aes(x = report_date, y = case_avg, color = factor_var), size = 2) +
    geom_point(data = point_data, mapping = aes(x = report_date, y = case_avg, color = factor_var), 
               size = 4, show.legend = FALSE) +
    geom_point(data = state_data$peak_tbl, mapping = aes(x = report_date, y = case_avg), 
               size = 4, shape = 16) +
    geom_point(data = region_data$peak_tbl, mapping = aes(x = report_date, y = case_avg), 
               size = 4, shape = 16) +
    geom_text_repel(data = state_data$peak_tbl, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = state_data$peak_y, nudge_x = state_data$peak_x, size = 5) +
    geom_text_repel(data = region_data$peak_tbl, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = region_data$peak_y, nudge_x = region_data$peak_x, size = 5) +
    scale_color_manual(values = palette, name = "Region") +
    scale_y_continuous(limits = c(0, state_data$top_val), breaks = seq(0, state_data$top_val, by = 200)) +
    scale_x_date(date_breaks = plot_data$date_breaks, date_labels = "%b") +
    labs(
      title = "Pace of New COVID-19 Cases in Missouri",
      subtitle = paste0(region_label, as.character(plot_data$plot_date), " through ", as.character(plot_data$date)),
      caption = plot_data$caption_text,
      x = "Date",
      y = "7-day Average of New Cases"
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text.x = element_text(angle = plot_data$x_angle))
  
  # add current points
  if (state_data$current_display == TRUE){
    p <- p + geom_text_repel(data = state_data$current_tbl, 
                             mapping = aes(x = report_date, y = case_avg, label = text), 
                             nudge_y = state_data$current_y, nudge_x = state_data$current_x, size = 5) 
  }
  
  if (region_data$current_display == TRUE){
    p <- p + geom_text_repel(data = region_data$current_tbl, 
                             mapping = aes(x = report_date, y = case_avg, label = text), 
                             nudge_y = region_data$current_y, nudge_x = region_data$current_x, size = 5)   
  }
  
  # return output
  return(p)
  
}

# state/county plot e - facet rate ####

facet_rate <- function(.data, type, subtype = NULL, pal, x_breaks, y_breaks, y_upper_limit, highlight, plot_date, date, title, caption){
  
  # address Kansas City
  # if (type == "county"){
  #  .data <- filter(.data, (geoid == "29511" & report_date > "2020-09-29") == FALSE) 
  # }
  
  # create name
  if (type == "metro"){
    scale_name <- "Metro Area"
  } else if (type == "county"){
    scale_name <- "County"
  } else if (type == "state"){
    scale_name <- "State"
  }
  
  # construct plot
  p <- ggplot(.data) +
    geom_line(mapping = aes(x = report_date, y = case_avg_rate, color = factor_var), 
              size = 2, show.legend = FALSE)
  
  # optionally highlight trends
  if (type == "metro" | type == "county"){
    p <- p + gghighlight(geoid %in% highlight, use_direct_label = FALSE, use_group_by = FALSE)
  } 
    
  # finish plot
  p <- p +
    scale_colour_manual(values = pal, name = scale_name) +
    scale_x_date(date_breaks = x_breaks, date_labels = "%b") +
    scale_y_continuous(limits = c(0,y_upper_limit), breaks = seq(0, y_upper_limit, by = y_breaks)) + 
    labs(
      title = title,
      x = "Date",
      y = "7-Day Average Rate per 100,000"
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text=element_text(size = 15))
  
  # add subtitle and captions
  if (is.null(subtype) == TRUE){
    p <- p + labs(
      subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
      caption = caption
    )
  } else if (is.null(subtype) == FALSE){
    
    if (subtype == "Southeast"){
      p <- p + labs(
        subtitle = paste0("Southeast Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Mid-Missouri"){
      p <- p + labs(
        subtitle = paste0("Mid-Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "St. Joseph"){
      p <- p + labs(
        subtitle = paste0("St. Joseph and Northwestern Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Northern"){
      p <- p + labs(
        subtitle = paste0("Northern Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Ozark"){
      p <- p + labs(
        subtitle = paste0("Lake of the Ozarks and South-Central Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Southwest"){
      p <- p + labs(
        subtitle = paste0("Southwest Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Cape"){
      p <- p + labs(
        subtitle = paste0("Cape Girardeau Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "West-Central"){
      p <- p + labs(
        subtitle = paste0("West-Central Missouri Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Springfield"){
      p <- p + labs(
        subtitle = paste0("Springfield Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Ozark Mountains"){
      p <- p + labs(
        subtitle = paste0("Ozark Mountains Focus\n",as.character(values$plot_date), 
                          " through ", as.character(values$date))
      )
    } else if (subtype == "Kansas City"){
      p <- p + labs(
        subtitle = paste0(as.character(plot_date), " through ", as.character(date))
      )
    }
    
    p <- p + labs(caption = caption)
    
  }
  
  # add facet
  if (type == "metro"){
    p <- p + facet_wrap(~short_name)
  } else if (type == "county" & is.null(subtype) == TRUE){
    p <- p + facet_wrap(~county)
  } else if (type == "county" & is.null(subtype) == FALSE){
    
    if (subtype == "Kansas City"){
      p <- p + facet_wrap(~county)
    } else {
      p <- p + facet_wrap(~county_fct)
    }
    
  } else if (type == "state"){
    p <- p + facet_wrap(~state)
  }
  
  # return output
  return(p)
  
}
