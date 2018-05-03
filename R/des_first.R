
des_first <- function(
  tracks, ctarget, cgaps = "", cname = "first", drop = TRUE, t_id = "id",
  des_df = ""
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_cname(tracks, ctarget, def = FALSE); if (e != "") {stop(e)}
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

  # Calculate for each track
  result <- psyosphere::apply_tracks(
    tracks,
    "first_private(eval_track, arg1, arg2, arg3, arg4)",
    arg1 = ctarget,
    arg2 = cgaps,
    arg3 = cname,
    arg4 = t_id,
    t_id = t_id
  )

  # Merge descriptives
  result <- des_merge_private(result, des_df, t_id)

  # Check how to return
  result <- des_drop_private(result, drop)

  # return result
  return(result)

}

first_private <- function(tracks, ctarget, cgaps, cname, t_id) {

  # Skip gaps
  tracks <- omit_gaps_private(tracks, ctarget, cgaps)

  # Calculate
  result <- tracks[1,ctarget]

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
