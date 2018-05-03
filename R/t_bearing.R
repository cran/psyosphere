
t_bearing <- function(
  tracks, t_id = "id", bind = TRUE, drop = TRUE, cname = "bearings"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}

  # Add bearings per track
  result <- psyosphere::apply_tracks(
    tracks,
    "bearing_exec_private(eval_track)",
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}


bearing_exec_private <- function(gpx_data) {

  # prepare variables

  # get coordinates for the current and previous position
  current <- subset(gpx_data, select = c("lon","lat"))
  previous <- apply_shift(
    gpx_data, "-1", FALSE, c("lon","lat"), t_id = ""
  )

  names(previous)[names(previous) == "lon_pre_1"] <- "lon"
  names(previous)[names(previous) == "lat_pre_1"] <- "lat"

  # get bearings

  #  bearings
  bearings <- geosphere::bearing(previous, current)

  # set all no movement bearings to NA, otherwise they will be -180
  bearings <- abtg_no_mov_NA_private(
    bearings, previous, current
  )

  # return results
  return(bearings)

}

abtg_no_mov_NA_private <- function(bearings, p1_coordinates,p2_coordinates) {

  n <- nrow(p1_coordinates)

  for (i in 1:n) {

    lon1 <- p1_coordinates[i,c("lon")]
    lon2 <- p2_coordinates[i,c("lon")]
    lat1 <- p1_coordinates[i,c("lat")]
    lat2 <- p2_coordinates[i,c("lat")]

    if (!exists("lon1")) {next()}
    if (!exists("lon2")) {next()}
    if (!exists("lat1")) {next()}
    if (!exists("lat2")) {next()}
    if (is.null(lon1)) {next()}
    if (is.null(lon2)) {next()}
    if (is.null(lat1)) {next()}
    if (is.null(lat2)) {next()}
    if (is.na(lon1)) {next()}
    if (is.na(lon2)) {next()}
    if (is.na(lat1)) {next()}
    if (is.na(lat2)) {next()}

    if ( (lon1 == lon2) && (lat1 == lat2) ) {
      bearings[i] <- NA
    }

  }

  return(bearings)

}
