# ggplot2 Theme for Sequoia Keynote Template

sequoia_theme <-function(base_size = 28, base_family = "sans", background = c("transparent", "white", "gray"), legend_size = 1.5, map = FALSE) {

  if (map == FALSE) {

    if (background == "gray"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
       + theme(
         line = element_line(colour = "black"),
         rect = element_rect(fill = '#F0F0F0', linetype = 0, colour = NA),
         text = element_text(colour = '#3C3C3C'),
         axis.title = element_text(),
         axis.text = element_text(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.background = element_rect(fill = '#EBEBEB'),
         legend.key = element_rect(fill = '#EBEBEB'),
         legend.key.size = unit(legend_size, units="cm"),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",
         panel.grid = element_line(colour = NULL),
         panel.grid.major =
           element_line(colour = '#D2D2D2'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = '#EBEBEB'),
         plot.background = element_rect(fill = '#EBEBEB'),
         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         plot.caption = element_text(hjust = "0"),
         strip.background = element_rect()))


    } else if (background == "white"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
       + theme(
         line = element_line(colour = "black"),
         rect = element_rect(fill = '#F0F0F0', linetype = 0, colour = NA),
         text = element_text(colour = '#3C3C3C'),
         axis.title = element_text(),
         axis.text = element_text(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.background = element_rect(fill = '#FFFFFF'),
         legend.key = element_rect(fill = '#FFFFFF'),
         legend.key.size = unit(legend_size, units="cm"),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",
         panel.grid = element_line(colour = NULL),
         panel.grid.major =
           element_line(colour = '#D2D2D2'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = '#FFFFFF'),
         plot.background = element_rect(fill = '#FFFFFF'),
         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         plot.caption = element_text(hjust = "0"),
         strip.background = element_rect()))

    } else if (background == "transparent"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
       + theme(
         line = element_line(colour = "black"),
         rect = element_rect(fill = '#F0F0F0', linetype = 0, colour = NA),
         text = element_text(colour = '#3C3C3C'),
         axis.title = element_text(),
         axis.text = element_text(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.background = element_rect(colour = NA, fill = NA),
         legend.key = element_blank(),
         legend.key.size = unit(legend_size, units="cm"),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",
         panel.grid = element_line(colour = NULL),
         panel.grid.major =
           element_line(colour = '#D2D2D2'),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         plot.background = element_blank(),
         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         plot.caption = element_text(hjust = "0"),
         strip.background = element_rect()))

    }

  } else if (map == TRUE){

    if (background == "transparent"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
        theme(
          line = element_line(colour = "black"),
          rect = element_rect(fill = NA, linetype = 1, colour = '#898989'),
          text = element_text(colour = '#3C3C3C'),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.background = element_rect(colour = NA, fill = NA),
          legend.key.size = unit(legend_size, units="cm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.box = "vertical",
          plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
          plot.caption = element_text(hjust = "0")))

    } else if (background == "white"){

      (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
        theme(
          line = element_line(colour = "black"),
          rect = element_rect(fill = NA, linetype = 1, colour = '#898989'),
          text = element_text(colour = '#3C3C3C'),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = '#FFFFFF', color = NA),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_rect(fill = '#FFFFFF', color = NA),
          legend.justification = c(0, 0),
          legend.background = element_rect(fill = '#FFFFFF'),
          legend.key = element_rect(fill = '#FFFFFF'),
          legend.key.size = unit(legend_size, units="cm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.box = "vertical",
          plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
          plot.caption = element_text(hjust = "0")))

    }
  }
}
