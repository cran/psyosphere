
distance_point <- function(
  tracks, point, bind = TRUE, drop = TRUE, cname = "dis_to_point_in_m"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}

  # Get lat and lon from next observation
  coordinates <- subset(tracks, select = c("lon","lat"))
  if (!is.null(nrow(point))) {
    point <- point[1,c("lon","lat")]
  } else if (!is.null(names(point))) {
    point <- c(point["lon"], point["lat"])
  }

  # Get distances
  result <- geosphere::distHaversine(coordinates,point)

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}
