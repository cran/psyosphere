
distance_line <- function(
  tracks, line, bind = TRUE, drop = TRUE, cname = "distances_to_line"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(line, "lon", type = "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_cname(line, "lat", type = "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}

  # Get lat and lon from next observation
  original_coordinates <- subset(tracks, select = c("lon","lat"))

  # Get distances
  result <- geosphere::dist2Line(original_coordinates,line)
  result <- result[,"distance"]

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}
