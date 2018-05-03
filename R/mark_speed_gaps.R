
mark_speed_gaps <- function(
  tracks, speed_limit, cspeed = "speed", bind = TRUE, drop = TRUE,
  cname = "speed_gap", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_var(speed_limit, "numeric"); if (e != "") {stop(e)}
  e <- val_cname(tracks, cspeed, "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Mark time gaps
  result <- psyosphere::apply_tracks(
    tracks,
    "mark_speed_gaps_private(eval_track, arg1, arg2, arg3)",
    arg1 = cname,
    arg2 = cspeed,
    arg3 = speed_limit,
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

mark_speed_gaps_private <- function(tracks, cname, cspeed, speed_limit) {

  # Mark speed gaps
  result <- ifelse( tracks[,cspeed] >= speed_limit, TRUE, FALSE )
  result[is.na(result)] <- FALSE

  # Return result
  return(result)

}
