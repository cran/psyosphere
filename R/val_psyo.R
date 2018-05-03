
val_psyo <- function(tracks, id = 1, p_id = 1, time = 1, lon = 2, lat = 2) {

  # Get call info
  fname <- deparse(sys.call(-1))
  vname <- as.character(sys.call()[2])
  call_info <- paste0("Psyo variable \"", vname,"\" in \"", fname,"\"")

  # Set message variable
  e <- ""

  # Check for existence of dataframe
  if (!exists("tracks")) {
    e <- paste0(e, paste0(call_info, " does not exist\n"))
  }

  # Check if dataframe is empty
  if (is.null(tracks)) {
    e <- paste0(e, paste0(call_info, " is NULL (empty)\n"))
  }

  # Check for columns formatting
  e <- paste0(e, validate_df_id_private(tracks, id, call_info))
  # e <- paste0(e, validate_df_p_id_private(tracks, p_id, call_info))
  e <- paste0(e, validate_df_time_private(tracks, time, call_info))
  e <- paste0(e, validate_df_lon_private(tracks, lon, call_info))
  e <- paste0(e, validate_df_lat_private(tracks, lat, call_info))

  # Return message
  return(e)

}

cexist_private <- function(tracks, cname, force, call_info) {

  # check if cname exists in tracks
  if (!(cname %in% colnames(tracks))) {

    message <- paste0(call_info, " is has no column \"", cname,"\"\n")
    return(val_return_private(message, force))

  }

  return("")

}

validate_df_id_private <- function(tracks, force, call_info) {

  # check if cname exists in tracks
  e <- cexist_private(tracks, "id", force, call_info); if (e != "") {return(e)}
  return("")

}

validate_df_p_id_private <- function(tracks, force, call_info) {

  # check if cname exists in tracks
  e <- cexist_private(tracks, "p_id", force, call_info); if (e != "") {
    return(e)
  }
  return("")

}

validate_df_time_private <- function(tracks, force, call_info) {

  # check if cname exists in tracks
  e <- cexist_private(tracks, "time", force, call_info); if (e != "") {
    return(e)
  }

  # check format
  if (!lubridate::is.POSIXct(tracks$time)) {

    message <- paste0("Column 'time' of ",call_info," has to be 'POSIXct'")
    return(val_return_private(message, force))

  }

  return("")

}

validate_df_lon_private <- function(tracks, force, call_info) {

  # check if cname exists in tracks
  e <- cexist_private(tracks, "lon", force, call_info); if (e != "") {return(e)}

  # check format
  if (mode(tracks$lon) != "numeric") {

    message <- paste0("Column 'lon' of ",call_info," has to be 'numeric'")
    return(val_return_private(message, force))

  }

  return("")

}

validate_df_lat_private <- function(tracks, force, call_info) {

  # check if cname exists in tracks
  e <- cexist_private(tracks, "lat", force, call_info); if (e != "") {return(e)}

  # check format
  if (mode(tracks$lat) != "numeric") {

    message <- paste0("Column 'lat' of ",call_info," has to be 'numeric'")
    return(val_return_private(message, force))

  }

  return("")

}
