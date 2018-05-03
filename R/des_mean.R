
des_mean <- function(
  tracks, ctarget, cweight = "", cgaps = "", cname = "mean", drop = TRUE,
  t_id = "id", des_df = ""
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_cname(tracks, ctarget, type = "numeric", def = FALSE); if (e != "") {stop(e)}
  e <- val_cname(tracks, cweight, type = "numeric"); if (e != "") {stop(e)}
  e <- val_cname(tracks, cgaps, type = "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Check variables for merge
  if (is.data.frame(des_df)) {
    e <- val_psyo(des_df, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
    e <- val_cname(tracks, t_id, def = FALSE); if (e != "") {stop(e)}
    e <- val_cname(des_df, t_id, def = FALSE); if (e != "") {stop(e)}
  }

  # Calculate mean for each track
  result <- psyosphere::apply_tracks(
    tracks,
    "mean_private(eval_track, arg1, arg2, arg3, arg4, arg5)",
    arg1 = ctarget,
    arg2 = cweight,
    arg3 = cgaps,
    arg4 = cname,
    arg5 = t_id,
    t_id = t_id
  )

  # Merge descriptives
  result <- des_merge_private(result, des_df, t_id)

  # Check how to return
  result <- des_drop_private(result, drop)

  # return result
  return(result)

}

mean_private <- function(tracks, ctarget, cweight, cgaps, cname, t_id) {

  # Convert difftime weight as numeric
  if (cweight != "" && class(tracks[,cweight]) == "difftime") {
    tracks[,cweight] <- as.numeric(tracks[,cweight])
  }

  # Skip gaps
  tracks <- omit_gaps_private(tracks, ctarget, cgaps)

  # Calculate normal mean
  if (cweight == "") {
    result <- mean( tracks[,ctarget], na.rm = TRUE)
  }

  # Calculate weighted mean
  else {
    result <- SDMTools::wt.mean(tracks[,ctarget],tracks[,cweight])
  }

  # Save as data frame
  if (t_id == "") {
    result <- data.frame(result)
  } else {
    id <- tracks[1,t_id]
    result <- data.frame(id, result)
    names(result)[1] <- t_id
    names(result)[2] <- cname
  }

  return(result)

}
