
average_coordinates <- function(
  tracks, num, units = "minutes", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 2, 2, 2); if (e != "") {stop(e)}
  e <- val_var(num, "numeric"); if (e != "") {stop(e)}
  e <- val_var(units, "character"); if (e != "") {stop(e)}
  unit_types <- c("seconds", "minutes", "hours", "days", "weeks")
  if (!units %in% unit_types) {stop("wrong unit type")}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Prepare variables
  duration <- lubridate::duration(num, units)

  # Compress tracks
  tracks <- psyosphere::apply_tracks(
    tracks,
    "compress_private(eval_track, arg1)",
    arg1 = duration,
    t_id = t_id
  )

  # Return result
  return(tracks)

}

compress_private <- function(tracks, duration) {

  # Prepare variables
  as.POSIXct(tracks$time)
  tracks <- tracks[with(tracks, order(time)), ]
  compressed_data <- utils::head(tracks[1,],-1)

  # Loop through dataframe until it is fully processed

  while (NROW(tracks) > 0) {

    # Get first interval
    first_row <- tracks[1,]
    new_row <- first_row
    interval <- first_row[1,c("time")] + duration
    data_interval <- tracks[tracks$time <= interval,]

    if (NROW(data_interval) > 1) {

      mean_time <- mean.POSIXct(data_interval$time, na.rm = TRUE)

      data_interval_coor <- data_interval[,c("lon","lat")]
      mean_coor <- geosphere::geomean(data_interval_coor)

      # Create the new row
      new_row$lon <- mean_coor[1]
      new_row$lat <- mean_coor[2]
      new_row$time <- mean_time

    }

    compressed_data <- rbind(compressed_data, new_row)

    # Cut down the old data

    interval_size <- NROW(data_interval)
    tracks <- tracks[-(1:interval_size),]
  }

  return(compressed_data)

}
