
average_duplicates <- function(tracks, t_id = "id") {

  # Check variables
  e <- val_psyo(tracks, 0, 2, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Create geomean of variables with same time
  tracks <- psyosphere::apply_tracks(
    tracks, "correct_time_duplicates_private(eval_track)",
    t_id = t_id
  )

  # Return result
  return(tracks)

}

correct_time_duplicates_private <- function(tracks) {

  # Get all values that have a dublicate once
  time_duplicates <- duplicates_private(tracks)

  # Check for duplicates
  if (NROW(time_duplicates) == 0) {
    return(tracks)
  }

  # Geomean dublicated values by each unique dublicated value
  tracks <- geomean_duplicates_private(tracks, time_duplicates)

  return(tracks)

}

geomean_duplicates_private <- function(tracks, time_duplicates) {

  # Loop through duplicates values
  for (i in 1:nrow(time_duplicates)) {

    dublicated_value <- time_duplicates[i,]
    tracks <- geomean_coordinates_private(tracks, dublicated_value)

  }

  # Return result
  return(tracks)

}


geomean_coordinates_private <- function(tracks, dublicated_value) {

  # Get all coordinates that are dublicated
  dublicated_time <- dublicated_value[,c("time")]
  dublicated_coordinates <- tracks[ tracks[,c("time")] == dublicated_time ,]
  dublicated_coordinates <- dublicated_coordinates[,c("lon","lat")]
  first_p_id <- min(dublicated_value[,c("p_id")])

  # Check if coordinates are the same
  unique_coordinates <- unique(dublicated_coordinates[,c("lon","lat")])

  if (nrow(unique_coordinates) == 1) {

    # Take unique values and skip geomean, otherwise geomean can give wrong
    # position
    merge_point <- c(unique_coordinates$lon, unique_coordinates$lat)

  } else {

    # Get the geomean position
    merge_point <- geosphere::geomean(dublicated_coordinates)

  }

  # Replace first dublicated coordinate
  tracks[ tracks[,c("p_id")] == first_p_id , c("lon") ] <- merge_point[1]
  tracks[ tracks[,c("p_id")] == first_p_id , c("lat") ] <- merge_point[2]

  # Select all tracks that are not dublicated and are not the mean coordinate
  tracks <- tracks[
    tracks[,c("time")] != dublicated_time |
    tracks[,c("p_id")] == first_p_id
  ,]

  # Return results
  return(tracks)

}

duplicates_private <- function(tracks, cduplicated = "time") {

  # Return every item that already exist already once
  data_duplicates <- tracks[duplicated(tracks[,cduplicated]),]

  # Remove all dublications of the values
  data_duplicates <- data_duplicates[!duplicated(data_duplicates[,cduplicated]),]

  # Remove empty rows
  data_duplicates <- data_duplicates[
    rowSums(is.na(data_duplicates)) != ncol(data_duplicates),
    ]

  # Return every dublicated value once no matter how often it's dublicated
  return(data_duplicates)

}
