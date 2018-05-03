
mark_time_gaps <- function(
  tracks, interval = 0, factor = 3, ctime_difference = "time_difference",
  bind = TRUE, drop = TRUE, cname = "time_gap", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 2, 2, 0, 0); if (e != "") {stop(e)}
  e <- val_var(interval, "numeric"); if (e != "") {stop(e)}
  e <- val_var(factor, "numeric"); if (e != "") {stop(e)}
  e <- val_cname(tracks, ctime_difference, "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Get tracker interval setting
  if (interval == 0) {
    interval <- tracker_interval_setting_private(tracks, ctime_difference)
  }

  # Mark time gaps
  result <- psyosphere::apply_tracks(
    tracks,
    "mark_time_gaps_private(eval_track, arg1, arg2, arg3, arg4)",
    arg1 = cname,
    arg2 = ctime_difference,
    arg3 = interval,
    arg4 = factor,
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  colnames(result)[1] <- cname

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

tracker_interval_setting_private <- function(
  tracks, ctime_difference, feedback = TRUE
) {

  # Get interval setting by looking for the most frequent time difference
  interval <- plyr::count(tracks[,ctime_difference])
  max_value <- max(interval[,"freq"])
  interval <- interval[interval[,"freq"] == max_value ,"x"]

  if (feedback) {
    cat(paste0("Tracker interval set to ", interval, "\n"))
  }

  # Return result
  return(interval)

}

mark_time_gaps_private <- function(
  tracks, cname, ctime_difference, interval, factor
) {

  # Mark time gaps
  result <- ifelse(
    tracks[,("time_difference")] > (interval*factor), TRUE, FALSE
  )
  result[is.na(result)] <- FALSE

  # Return result
  return(result)

}
