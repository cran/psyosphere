
plot_polygon <- function(polygon, colour = "blue", plot = "", zoom = -1) {

  # Check variables
  e <- val_cname(polygon, "lon", type = "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_cname(polygon, "lat", type = "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_var(zoom, "numeric"); if (e != "") {stop(e)}
  e <- val_var(plot, "ggplot", def = TRUE); if (e != "") {stop(e)}

  # Get map
  if ( (mode(plot) == "character") && (plot == "") ) {
    plot <- psyosphere::plot_map(polygon, zoom)
  }
  zoom <- plot$zoom

  # Add plygon to plot
  plot <- plot +
    ggplot2::geom_polygon(data = polygon,
                          colour = colour,
                          fill = "blue",
                          alpha = .2)

  # Return result
  return(plot)

}
