facet_rate <- function(.data, type, pal, x_breaks, y_breaks, y_upper_limit, highlight, plot_date, date, title, caption){
  
  # construct plot
  p <- ggplot(.data) +
    geom_line(mapping = aes(x = report_date, y = case_avg_rate, color = factor_var), 
              size = 2, show.legend = FALSE) +
    gghighlight(geoid %in% highlight, use_direct_label = FALSE, use_group_by = FALSE) +
    scale_colour_manual(values = pal, name = "Metro Area") +
    scale_x_date(date_breaks = x_breaks, date_labels = "%b") +
    scale_y_continuous(limits = c(0,y_upper_limit), breaks = seq(0, y_upper_limit, by = y_breaks)) + 
    labs(
      title = title,
      subtitle = paste0(as.character(plot_date), " through ", as.character(date)),
      x = "Date",
      y = "7-Day Average Rate per 100,000",
      caption = caption
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text=element_text(size = 15))
  
  # add facet
  if (type == "metro"){
    p <- p + facet_wrap(~short_name)
  } else if (type == "county"){
    p <- p + facet_wrap(~county)
  }
  
  # return output
  return(p)
  
}
