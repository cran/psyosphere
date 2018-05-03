
des_duplicates <- function(
  tracks, cduplicated, cgaps = "", cname = "duplicates", drop = TRUE, t_id = "id",
  des_df = ""
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_cname(tracks, cduplicated, force = 2, def = FALSE); if (e != "") {stop(e)}
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
    "des_duplicates_private(eval_track, arg1, arg2, arg3, arg4)",
    arg1 = cduplicated,
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

des_duplicates_private <- function(tracks, cduplicated, cgaps, cname, t_id) {

  # Skip gaps
  tracks <- select_without_gaps(tracks, cgaps)

  # Calculate
  data_duplicates <- tracks[duplicated(tracks[,cduplicated]),]
  result <- NROW(data_duplicates)

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
