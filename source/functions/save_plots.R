# save plots
save_plots <- function(filename, plot, device = "png", preset = c("sm", "med", "lg"), dpi = 300){

  # identify plot object
  if (missing(plot)) {
    plotDef <- ggplot2::last_plot()
  } else {
    plotDef <- plot
  }

  # set small as default preset
  if (missing(preset)){
    preset <- "sm"
  }

  # save plot
  if (preset == "sm"){

    ggplot2::ggsave(filename, plotDef, device = device,
               width = 960 * 0.352778,
               height = 540 * 0.352778,
               units = "mm", dpi = dpi)

  } else if (preset == "med"){

    ggplot2::ggsave(filename, plotDef, device = device,
               width = 960 * 0.352778,
               height = 630 * 0.352778,
               units = "mm", dpi = dpi)

  } else if (preset == "lg"){

    ggplot2::ggsave(filename, plotDef, device = device,
               width = 1024 * 0.352778,
               height = 768 * 0.352778,
               units = "mm", dpi = dpi)

  }
}
