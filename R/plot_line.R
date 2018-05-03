
plot_line <- function(
  line, colour = "", size = 1, plot = "", zoom = -1
) {

  # Check variables
  e <- val_cname(line, "lon", "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_cname(line, "lat", "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_var(size, "numeric"); if (e != "") {stop(e)}
  e <- val_var(zoom, "numeric"); if (e != "") {stop(e)}
  e <- val_var(plot, "ggplot", def = TRUE); if (e != "") {stop(e)}

  # Get map
  if ( (mode(plot) == "character") && (plot == "") ) {
    plot <- psyosphere::plot_map(line, zoom)
  }
  zoom <- plot$zoom

  # Get color

  if (colour == "") {

    if ("track_color" %in% colnames(line)) {
      colour <- line[,"track_color"][1]

      if (is.na(colour)) {
        colour <- "royalblue1"
      }

    } else {
      colour <- "royalblue1"
    }

  }

  # Remove NA
  line <- stats::na.omit(line[,c("lon","lat")])

  # Plot line
  plot <- plot +
    ggplot2::geom_path(
      data = line, colour = colour, size = size
    )

  # Return result
  return(plot)

}
