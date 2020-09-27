# regional plot b/c/e - 7-day average counts ####

regional_count <- function(.data, region, point_data, state_data, region_data){
  
  # construct plot
  p <- ggplot() +
    geom_line(.data, mapping = aes(x = report_date, y = case_avg, color = factor_var), size = 2) +
    geom_point(points, mapping = aes(x = report_date, y = case_avg, color = factor_var), 
               size = 4, show.legend = FALSE) +
    geom_point(peak_point, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
    geom_point(peak_point_nostl, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
    geom_text_repel(data = peak_point, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = peak_point_y, nudge_x = peak_point_x, size = 5) +
    geom_text_repel(data = current_point, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = current_point_y, nudge_x = current_point_x, size = 5) +
    geom_text_repel(data = peak_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = -80, nudge_x = -110, size = 5) +
    geom_text_repel(data = current_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = -900, nudge_x = 0, size = 5) +
    scale_color_brewer(palette = "Dark2", name = "Category") +
    scale_y_continuous(limits = c(0, top_val), breaks = seq(0, top_val, by = 100)) +
    scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
    labs(
      title = "Pace of New COVID-19 Cases in Missouri",
      subtitle = paste0("Outstate Focus\n", as.character(state_subset$report_date[1]), " through ", as.character(date)),
      caption = caption_text,
      x = "Date",
      y = "7-day Average of New Cases"
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text.x = element_text(angle = x_angle))
  
}

# state/county plot e - facet rate ####

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
