
plot_map <- function(
  tracks, zoom = -1, maptype = "terrain", extent = "panel"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_var(zoom, "numeric"); if (e != "") {stop(e)}
  if (zoom < -1 || zoom > 24) { stop("Wrong zoom level") }

  # Prepare variables
  if (zoom == -1) { auto_zoom <- TRUE } else {auto_zoom <- FALSE }
  geomean <- geomedian_private(tracks) # map center
  zoom <- get_zoom_private(tracks, zoom) # zoom factor

  # GGMAP seems to be orphaned. Therefore, ggmap is moved into suggests.
  if (requireNamespace("ggmap", quietly=TRUE)) {

    # Get map with ggmap
    plot <- ggmap::get_googlemap(
      center = c(lon = geomean$lon, lat = geomean$lat),
      zoom = zoom,
      maptype = c("terrain")
    )
    plot <- ggmap::ggmap(plot)
    plot$zoom <- zoom

  } else {

    cat("\n\nInstall package ggmap with Google API key to display map.\n\n")

    # CRAN ggplot2 workaround
    lon <- lat <- NULL

    # # Get dummy map
    box <- get_bounding_box_private(tracks)
    plot <- ggplot2::ggplot(box) + ggplot2::aes(x = lon, y = lat)
    plot$zoom <- zoom

    # # Get map with OpenStreetMap
    # box <- get_bounding_box_private(tracks)
    # upperLeft <- c(box[1,2], box[1,1])
    # lowerRight <- c(box[2,2], box[2,1])
    # map <- OpenStreetMap::openmap(upperLeft, lowerRight)
    # plot <- OpenStreetMap::autoplot.OpenStreetMap(map)
    # plot$zoom <- zoom

  }

  # Return result
  return(plot)

}

all_points_in_polygon_private <- function(points, poly) {

  # Points in polygon?
  in_poly <- sp::point.in.polygon(points$lon,points$lat,poly$lon,poly$lat)

  if ( sum(in_poly) == nrow(poly) ) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}

get_bounding_box_private <- function(tracks, f = 0.05) {

  # Get extreme coordinates
  min_lon <- min(tracks$lon)
  min_lat <- min(tracks$lat)
  max_lon <- max(tracks$lon)
  max_lat <- max(tracks$lat)

  # Save lon and lat
  lon <- c(min_lon, max_lon)
  lat <- c(min_lat, max_lat)

  # Extrand range by fraction
  lon <- grDevices::extendrange( lon, f = f )
  lat <- grDevices::extendrange( lat, f = f )

  # Create bounding box
  bbox <- data.frame(lon, lat)

  # Return result
  return(bbox)

}

get_zoom_private <- function(tracks, zoom) {

  # Check if zoom is set to auto
  if (zoom != -1) {
    return(zoom)
  }

  # Prepare variables
  bbox <- get_bounding_box_private(tracks)

  # Get auto zoom
  zoom <- RgoogleMaps::MaxZoom(bbox$lat, bbox$lon)

  # If zoom is infinite ...
  if (zoom > 23) {
    zoom <- 23
  }

  # Return results
  return(zoom)

}

geomedian_private <- function(tracks) {

  # Prepare variables
  min_lon <- min(tracks$lon)
  min_lat <- min(tracks$lat)
  max_lon <- max(tracks$lon)
  max_lat <- max(tracks$lat)
  center_lon <- (max_lon - min_lon) / 2 + min_lon
  center_lat <- (max_lat - min_lat) / 2 + min_lat
  geomedian <- data.frame(lon = center_lon, lat = center_lat)

  # Return result
  return(geomedian)

}

