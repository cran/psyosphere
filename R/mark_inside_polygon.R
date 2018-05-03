
mark_inside_polygon <- function(
  tracks, polygon, bind = TRUE, drop = TRUE, cname = "in_polygon"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(polygon, "lon", type = "numeric", def = FALSE); if (e != "")
    {stop(e)}
  e <- val_cname(polygon, "lat", type = "numeric", def = FALSE); if (e != "")
    {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}

  # calculate list in polygon
  result <- sp::point.in.polygon(
    tracks$lon,
    tracks$lat,
    polygon[,c("lon")],
    polygon[,c("lat")]
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}
