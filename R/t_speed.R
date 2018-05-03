
t_speed <- function(
  tracks, bind = TRUE, drop = TRUE, cname = "speed", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 2, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}

  # get speed per track
  result <- psyosphere::apply_tracks(
    tracks,
    "speed_track_private(eval_track)",
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

speed_track_private <- function(tracks) {

  # Save attributes of time column
  time_attibutes <- attributes(tracks$time)

  # Get coordinates for the current and next position
  current <- subset(tracks, select = c("lon","lat","time"))
  previous <- apply_shift(
    tracks, "-1", FALSE, c("lon","lat","time"), t_id = ""
  )

  # Reset time attributes
  attributes(previous$time_pre_1) <- time_attibutes

  # Rename columns
  names(previous)[names(previous) == "lon_pre_1"] <- "lon"
  names(previous)[names(previous) == "lat_pre_1"] <- "lat"
  names(previous)[names(previous) == "time_pre_1"] <- "time"

  # Get speed
  speed <- speed_in_kmh_private(previous,current)

  # Set speed attribute
  attributes(speed) <- list(unit = "kmh", type = "speed")

  # Return result
  return(speed)

}

speed_in_kmh_private <- function(p1, p2) {

  # Prepare variables
  coordinates_a <- subset(p1, select = c("lon","lat"))
  coordinates_b <- subset(p2, select = c("lon","lat"))
  time_a <- p1$time
  time_b <- p2$time

  # Get distance in km
  distance_a_b_in_km <- geosphere::distHaversine(
    coordinates_a, coordinates_b
  )/1000

  # Get duration in hours
  duration_a_b_in_hours <- as.numeric(
    difftime(time_b, time_a, units = c("hours"))
  )

  # Avoid infinitave speed by not devigin by 0
  duration_a_b_in_hours[duration_a_b_in_hours == 0] <- NA

  # Caculate kmh
  speed_a_b_in_kmh <- distance_a_b_in_km/duration_a_b_in_hours

  # Return result
  return(speed_a_b_in_kmh)

}
