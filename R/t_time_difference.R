
t_time_difference <- function(
  tracks, units = "secs", bind = TRUE, drop = TRUE, cname = "time_difference",
  t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 2, 0, 0); if (e != "") {stop(e)}
  e <- val_var(units, "character"); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  if (units == "auto") {
    warning("'auto' for units can lead to different units per tracks.")
  }

  # Add time difference
  result <- psyosphere::apply_tracks(
    tracks,
    "time_diff_private(eval_track, arg1, arg2)",
    arg1 = units,
    arg2 = cname,
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

time_diff_private <- function(tracks, units, cname) {

  # Prepare variables
  time1 <- tracks[,c("time")]
  time2 <- apply_shift(tracks, "-1", FALSE, c("time"), t_id = "id")
  time2 <- time2[,c("time_pre_1")]

  # Reset time attributes
  attributes(time2) <- attributes(time1)

  # Get time difference
  time_difference <- difftime(time1, time2, units = units)

  # Return result
  return(time_difference)

}
