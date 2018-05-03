
t_distance <- function(
  tracks, bind = TRUE, drop = TRUE, cname = "distances_in_m", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Add bearings per track
  result <- psyosphere::apply_tracks(
    tracks,
    "distance_exec_private(eval_track)",
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

distance_exec_private <- function(tracks) {

  # Get lat and lon from next observation
  current <- subset(tracks, select = c("lon","lat"))
  previous <- apply_shift(
    tracks, "-1", FALSE, c("lon","lat"), t_id = ""
  )

  # Get distances
  distances_in_m <- geosphere::distHaversine(previous, current)

  return(distances_in_m)

}
