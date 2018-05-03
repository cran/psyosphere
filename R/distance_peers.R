
distance_peers <- function(
  tracks, cpeer = "", single = FALSE, average = TRUE, cname = "average_dis",
  bind = TRUE, drop = TRUE, t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 2, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(tracks, cpeer); if (e != "") {stop(e)}
  e <- val_var(single, "logical"); if (e != "") {stop(e)}
  e <- val_var(average, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}
  if (single == FALSE && average == FALSE) {
    stop("Either single and/or average have to be TRUE")
  }

  # Add id to restort tracks
  tracks[,"sort_id_6sje94s4s4vs4"] <- 1:nrow(tracks)

  # Calculate peer distances
  result <- psyosphere::apply_tracks(
    tracks,
    "distance_psyo_private(
      eval_track, arg1, eval_track, arg3, arg4, arg5
    )",
    arg1 = t_id,
    arg3 = single,
    arg4 = average,
    arg5 = cname,
    t_id = cpeer
  )

  # Resort result
  result <- result[order(result[,"sort_id_6sje94s4s4vs4"]),]
  tracks[,"sort_id_6sje94s4s4vs4"] <- NULL
  result[,"sort_id_6sje94s4s4vs4"] <- NULL

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)

  return(result)

}

distance_psyo_private <- function(
  tracks, t_id, peer_tracks, single, average, cname
) {

  # get peer distances
  peer_distances <- distance_peers_private(tracks, t_id, peer_tracks)

  # Calculate average distances
  if (average) {
    average_dis <- average_peer_distance_private(peer_distances, cname)
  }

  # Combine results

  if (single && average) { result <- cbind(average_dis, peer_distances) }
  else if (single && (!average)) { result <- peer_distances }
  else if ((!single) && average) { result <- average_dis }

  # Add sort id
  result[,"sort_id_6sje94s4s4vs4"] <- tracks[,"sort_id_6sje94s4s4vs4"]

  return(result)

}

distance_peers_private <- function(tracks, t_id, peer_tracks) {

  # Prepare variables
  result <- data.frame()
  peer_tracks_cols <- c(t_id,"p_id","time","lon","lat", "ele")
  peer_tracks <- peer_tracks[, names(peer_tracks) %in% peer_tracks_cols ]

  # loop through every coordinate
  for (i in 1:NROW(tracks)) {

    # get the current point
    ppn_point <- tracks[i,c("lon","lat","time")]

    # get list of peer positions
    peer_pos <- peer_positions_private(peer_tracks, ppn_point, t_id)

    # skip iteration if no match is found
    if (NROW(peer_pos) == 0) { next() }
    if (!is.data.frame(peer_pos)) { if (peer_pos == FALSE) { next() } }
    if (all(is.na(peer_pos))) { next() }

    # calculate distance to peers
    point_coordinates <- ppn_point[,c("lon","lat")]
    peer_positions_coordinates <- peer_pos[,c("lon","lat")]

    peer_distance_data <- geosphere::distHaversine(
      point_coordinates, peer_positions_coordinates
    )

    # Check if peer
    peer_distance_data[peer_pos[,"id"] == tracks[i,"id"]] <- NA

    # add data to dataframe
    result <- bind_peer_positions_private(
      result, peer_pos, peer_distance_data, i, t_id
    )

  }

  # return result
  return(result)

}

average_peer_distance_private <- function(peer_distances, cname) {

  # Calculate row means
  average <- rowMeans(peer_distances, na.rm = TRUE)
  average <- data.frame(average)
  average <- plyr::rename(average, c("average" = cname))

  # Return result
  return(average)

}

bind_peer_positions_private <- function(
  result, peer_positions, peer_distances, i, t_id
) {

  # Merge peer positions and distances in one dataframe
  peer_distances_df <- cbind(peer_positions,peer_distances)
  peer_distances_df <- peer_distances_df[,c(t_id,"peer_distances")]
  peer_distances_df <- peer_distances_df[!is.na(peer_distances_df[t_id]),]

  # Prepare peer id's to be turned into columns
  peer_distances_df[,c(t_id)] <- sprintf(
    'dis_to_%s_in_m', peer_distances_df[,c(t_id)]
  )

  # Turn the id's into column names
  cnames <- peer_distances_df[,c(t_id)]
  peer_distances_df <- as.data.frame(t(peer_distances_df[,-1]))
  colnames(peer_distances_df) <- cnames

  # add the column names if they don't exist to tracks
  new_cnames <- setdiff(cnames, colnames(result))
  if (NROW(new_cnames) > 1 && NROW(result) != 0) {
    result[,new_cnames] <- list(NA)
  }

  # add the distances to the tracks
  for (cname in cnames) {
    result[i,cname] <- peer_distances_df[1,cname]
  }

  return(result)

}

peer_positions_private <- function(peer_tracks, ppn_point, t_id = "") {

  # Check variables
  e <- val_psyo(peer_tracks); if (e != "") {stop(e)}

  # Get peer posistions
  peer_positions <- psyosphere::apply_tracks(
    peer_tracks,
    "timed_cross_track_position_private(eval_track, arg1, arg2)",
    arg1 = ppn_point,
    arg2 = t_id,
    t_id = t_id
  )

  # Return result
  return(peer_positions)

}

timed_cross_track_position_private <- function(data_tim, point, t_id) {

  # "data_tim" are the coordinates of the peers. We are interested where the
  # peers are at the time of "point". So for each point * t_id combination we
  # want one result.

  # get inclosing points
  peer_position_tim <- inclosing_points_in_time_private(
    point, data_tim, point$time, t_id
  )

  # check if match is found
  if ( (typeof(peer_position_tim) == "logical") && (peer_position_tim == FALSE) ) {

    # Return peer track coordinate the keeps everything but lon, lat, ele and
    # p_id
    not_found <- data_tim[1,]
    not_found[1,"lon"] <- NA
    not_found[1,"lat"] <- NA
    if ("ele" %in% names(not_found)) { not_found[1,"ele"] <- NA }
    if ("p_id" %in% names(not_found)) { not_found[1,"p_id"] <- NA }

    return(not_found)

  }

  # return if perfect match
  if (nrow(peer_position_tim) == 1) {
    return(peer_position_tim)
  }

  # check for no movement
  if (tctp_no_move_private(peer_position_tim)) {
    return(peer_position_tim[1,])
  }

  # get timed peer position between two points
  if (nrow(peer_position_tim) == 2) {

    p1 <- peer_position_tim[1,]
    p2 <- peer_position_tim[2,]

    timed_peer_position <- timed_destination_point_private(
      p1,p2,point, t_id
    )

    return(timed_peer_position)

  }

  return(FALSE)

}

timed_destination_point_private <- function(p1, p2, p3, t_id) {

  duration <- as.numeric(duration_private(p1, p3))
  speed <- speed_in_kmh_private(p1, p2)

  bearing <- geosphere::bearing(p1[,c("lon","lat")],p2[,c("lon","lat")])

  distance <- speed*duration

  coordinates_d <- geosphere::destPoint(p1[,c("lon","lat")], bearing, distance*1000)

  id <- p1[,t_id]
  lon <- coordinates_d[,1]
  lat <- coordinates_d[,2]
  time <- p3$time

  destination_point <- gpx_data_frame_private(id,lon,lat,"",time)

  return(destination_point)

}

duration_private <- function(p1, p2) {

  time_a <- p1$time
  time_b <- p2$time

  duration_a_b_in_hours <- difftime(time_b, time_a, units = c("hours"))

  return(duration_a_b_in_hours)

}

tctp_no_move_private <- function(peer_position) {

  lon1 <- peer_position[1,c("lon")]
  lon2 <- peer_position[2,c("lon")]
  lat1 <- peer_position[1,c("lat")]
  lat2 <- peer_position[2,c("lat")]

  if ( lon1 == lon2 && lat1 == lat2 ) {
    return(TRUE)
  }

  return(FALSE)

}

inclosing_points_in_time_private <- function(p1, coordinates, p1_time, t_id) {

  # Get inclosing points for each seperate track of a peer
  inclosing_coordinates <- psyosphere::apply_tracks(
    coordinates,
    "in_points_exe_private(eval_track, arg1, arg2)",
    arg1 = p1,
    arg2 = p1_time,
    t_id = t_id
  )

  return(inclosing_coordinates)

}

in_points_exe_private <- function(peer_coordinates, p1, p1_time) {

  # test for peer coordinates with same time and coordinates
  match <- check_for_match_private(peer_coordinates, p1, p1_time)
  if (NROW(match) == 1) { return(match) }

  # find inclosing points
  earlier <- subset(peer_coordinates, peer_coordinates$time < p1_time)
  later <- subset(peer_coordinates, peer_coordinates$time > p1_time)

  # return error if peer_coordinates are not inclosing
  if ( (nrow(earlier) == 0) || (nrow(later) == 0) ) {
    return(FALSE)
  }

  # select the closest peer_coordinates
  earlier_last_time <- max(earlier$time, na.rm = TRUE)
  later_first_time <- min(later$time)

  coordinate_earlier <- earlier[earlier$time == earlier_last_time,]
  coordinate_later <- later[later$time == later_first_time,]

  # make sure it is really one coordinate
  coordinate_earlier <- coordinate_earlier[1,]
  coordinate_later <- coordinate_later[1,]

  # return inclosing peer_coordinates
  inclosing_coordinates <- rbind(coordinate_earlier, coordinate_later)

  return(inclosing_coordinates)

}

check_for_match_private <- function(peer_coordinates, p1, p1_time) {

  # Check if there is a peer coordinate with the same time
  time_match <- peer_coordinates[peer_coordinates$time == p1_time,]

  # Return if there is one or no match with same time
  if (nrow(time_match) <= 1) { return(time_match) }

  # If there a multiple coordinates with the same time check for same location
  perf_match <- time_match[ time_match[,c("lon")] == p1[,c("lon")] ,]
  perf_match <- perf_match[ perf_match[,c("lat")] == p1[,c("lat")] ,]

  # If there are one or multiple perfect matches return the first one
  if (nrow(perf_match) != 0) {
    return(perf_match[1,])
  }

  # If there is no perfect match return the first time match
  return(time_match[1,])

}

gpx_data_frame_private <- function(
  id = NA,
  lon = NA,
  lat = NA,
  ele = NA,
  time = NA)
{

  # Create data frame
  data <- data.frame(id, lon, lat, ele, time, stringsAsFactors = FALSE)

  # Add an empty row
  if (nrow(data) == 0) {
    data[1,] <- NA
  }

  # Reformat data frame
  data[,"id"] <- as.character(data[,"id"])
  data[,"lon"] <- as.numeric(data[,"lon"])
  data[,"lat"] <- as.numeric(data[,"lat"])
  data[,"ele"] <- as.numeric(data[,"ele"])
  data[,"time"] <- as.POSIXct(data[,"time"])

  # Return result
  return(data)

}
