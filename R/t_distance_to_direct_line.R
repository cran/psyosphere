
distance_to_direct_line <- function(
  tracks, line, bind = TRUE, drop = TRUE, cname = "distance_to_direct_line",
  t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(line, "lon", "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_cname(line, "lat", "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Evaluate for each track
  result <- psyosphere::apply_tracks(
    tracks,
    "track_deviation_private(eval_track,arg1, TRUE)",
    line,
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

track_deviation_private <- function(tracks, line, bind = TRUE) {

  # Create a shortest route line
  start_pos <- tracks[1,c("lon","lat")]
  end_pos <- geosphere::dist2Line(start_pos,line)
  end_pos <- end_pos[,c("lon","lat")]
  shortest_line <- rbind(start_pos,end_pos)

  # Calculate deviation from line
  deviation <- psyosphere::distance_line(
    tracks[,c("lon","lat")], shortest_line, bind = FALSE
  )

  # Return result
  return(deviation)

}

