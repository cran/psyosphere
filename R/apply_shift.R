
apply_shift <- function(
  tracks, factor = 1, bind = TRUE, csubset = "", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_cname(tracks, csubset); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Determine direction of copies and remove it from factor
  shift_direction <- set_direction_private(factor)
  factor <- abs(strtoi(factor))

  # Evaluate subtracks
  tracks <- psyosphere::apply_tracks(
    tracks,
    "copy_shift_private(eval_track, arg1, arg2, arg3, arg4)",
    arg1 = shift_direction,
    arg2 = factor,
    arg3 = bind,
    arg4 = csubset,
    t_id = t_id
  )

  # Return result
  return(tracks)

}

copy_shift_private <- function(tracks, shift_direction, factor, bind, csubset) {

  # select subset of the tracks
  selection <- subset(tracks, select = csubset)

  # create all the copies
  for (i in 1:factor ) {

    if (shift_direction$pre) {
      pre_observations <- previous_private(selection, i)
    }

    if (shift_direction$fol) {
      fol_observations <- following_private(selection, i)
    }

    # store results
    if (i == 1) {

      if (shift_direction$pre == TRUE && shift_direction$fol == FALSE) {
        result_data_frame <- pre_observations
      } else if (shift_direction$fol == TRUE && shift_direction$pre == FALSE) {
        result_data_frame <- fol_observations
      } else {
        result_data_frame <- cbind(
          pre_observations, fol_observations
        )
      }

    } else {

      if (shift_direction$pre) {
        result_data_frame <- cbind(result_data_frame,pre_observations)
      }

      if (shift_direction$fol) {
        result_data_frame <- cbind(result_data_frame,fol_observations)
      }
    }

    if (shift_direction$pre) {
      remove(pre_observations)
    }

    if (shift_direction$fol) {
      remove(fol_observations)
    }

  }

  if (bind) {
    result_data_frame <- cbind(tracks, result_data_frame)
  }

  return(result_data_frame)

}

set_direction_private <- function(factor) {

  # By default use both directions for shifted copies
  if (mode(factor) == "numeric") {
    return( data.frame(pre = TRUE, fol = TRUE) )
  }

  # Check if only one direction is selected
  if (grepl(pattern = "+", x = factor, fixed = TRUE)) {
    return( data.frame(pre = FALSE, fol = TRUE) )
  }

  if (grepl(pattern = "-", x = factor, fixed = TRUE)) {
    return( data.frame(pre = TRUE, fol = FALSE) )
  }

  # Return default
  return( data.frame(pre = TRUE, fol = TRUE) )

}

following_private <- function(
  tracks,
  factor = 1
) {

  # create (empty) observations
  emtpy_rows <- create_empty_rows_private(tracks, factor)

  # get every but first x (factor) observations
  without_first_rows <- tracks[(factor + 1):(NROW(tracks)), 1:NCOL(tracks)]

  # Workaround that R destroys one column data frames
  if (NCOL(without_first_rows) == 1) {
    df <- data.frame(fol = without_first_rows)
    names(df)[1] <- names(tracks)[1]
    without_first_rows <- df
  }

  # merge the two subsets
  following_row_data <- rbind(without_first_rows, emtpy_rows)

  # change column names
  suffix <- paste0("_fol_",factor)

  colnames(following_row_data) <- paste(
    colnames(following_row_data),suffix,sep = ""
  )

  # Return result
  return(following_row_data)

}

previous_private <- function(
  tracks,
  factor = 1
) {

  # create first (empty) observations
  emtpy_rows <- create_empty_rows_private(tracks, factor)

  # get every but last x (factor) observations
  without_last_rows <- tracks[
    0:(NROW(tracks) - factor), 1:NCOL(tracks), drop = FALSE
  ]

  # merge the two subsets
  previous_row_data <- rbind(emtpy_rows, without_last_rows)

  # change column names
  suffix <- paste("_pre_",factor,sep = "")
  colnames(previous_row_data) <- paste(
    colnames(previous_row_data),suffix,sep = ""
  )

  return(previous_row_data)

}

create_empty_rows_private <- function(tracks, factor = 1) {

  emtpy_row <- tracks[0:1, 1:NCOL(tracks), drop = FALSE]

  if (NCOL(tracks) == 1) {
    emtpy_row[] <- NA
  } else {
    emtpy_row[,] <- NA
  }

  emtpy_rows <- emtpy_row

  if (factor > 1) {
    for (i in 2:factor ) {
      emtpy_rows <- rbind(emtpy_rows, emtpy_row)
    }
  }

  return(emtpy_rows)

}
