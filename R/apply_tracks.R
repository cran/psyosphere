
apply_tracks <- function(
  tracks,
  exp,
  arg1 = "",
  arg2 = "",
  arg3 = "",
  arg4 = "",
  arg5 = "",
  arg6 = "",
  arg7 = "",
  arg8 = "",
  arg9 = "",
  t_id = "id",
  info = FALSE
) {

  # Check Variables
  # tracks and arg1-6 are not tested to give the function more flexebility
  e <- val_var(exp, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}
  e <- val_var(info, "logical"); if (e != "") {stop(e)}

  # Check if calculation is per track
  if (t_id == "") {

    # Calculation for whole data frame
    result <- whole_dataframe_exe_private(
      tracks, exp, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, t_id,
      info
    )

  } else {

    # Calculation per track
    result <- evaluate_tracks_exe_private(
      tracks, exp, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, t_id ,
      info
    )

  }

  # Return result
  if (exists("result")) {
    return(result)
  }

}

whole_dataframe_exe_private <- function(
  tracks, exp, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, t_id ,info
) {

  # run function on selected tracks
  eval_track <- tracks
  full_exp <- paste0("eval_track = ",exp)
  result <- eval(parse(text = full_exp), eval_track)

  # Return result
  if (exists("result")) {
    return(result)
  }

}

evaluate_tracks_exe_private <- function(
  tracks, exp, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, t_id ,info
) {

  # prepare variables
  result <- NA

  # create a column in tracks to identify unique values and get uniques within it
  tracks <- ebu_add_unique_value_column_private(tracks, t_id)
  unique_values <- ebu_unique_values_private(tracks)
  n <- NROW(unique_values)

  # cycle through tracks by unique value -----------------------------------------

  for (i in 1:n) {

    # get unique value for this iteration
    unique_value <- as.character(unique_values[c(i),c("unique")])

    # select tracks by unique value and remove unique identifier column
    eval_track <- tracks[tracks[,c("unique")] == unique_value,, drop = F]
    eval_track <- eval_track[,!(names(eval_track) %in% c("unique")), drop = F]

    # run function on selected tracks
    full_exp <- paste0("eval_track = ",exp)
    iteration_result <- eval(parse(text = full_exp), eval_track)

    # check if this function produces a result and save them
    if (!is.null(iteration_result)) {
      if (!ggplot2::is.ggplot(iteration_result)) {
        result <- ebu_save_data_private(result, iteration_result)
      } else {
        result <- iteration_result
      }
    }

  }

  # Return result
  if (exists("result")) {
    return(result)
  }

}

ebu_save_data_private <- function(result, iteration_result) {

  # Save new result if result does'nt exist yet
  if ( (NROW(result) == 1) && (is.na(result)) ) {
    return(iteration_result)
  }

  # Match up the columns of result and iteration_result if there are columns
  result <- extend_col_names_private(result, iteration_result)
  iteration_result <- extend_col_names_private(iteration_result, result)

  # Add iteration_result to result
  if (
    !is.data.frame(result) && !is.data.frame(iteration_result) &&
    NCOL(result) == 1 && NCOL(iteration_result) == 1
  ) {
    result <- c(result, iteration_result)
  } else {
    result <- rbind(result,iteration_result)
  }

  # Return result
  return(result)

}

ebu_add_unique_value_column_private <- function(tracks, t_id) {

  if (length(t_id) > 1) {
    tracks$unique <- apply( tracks[,t_id] , 1 , paste , collapse = "-" )
  } else {
    tracks$unique <- tracks[,c(t_id)]
  }

  return(tracks)

}

ebu_unique_values_private <- function(tracks) {

  unique <- "unique"
  unique_column <- subset(tracks, select = c(unique))
  unique_values <- unique(unique_column)

  return(unique_values)

}

extend_col_names_private <- function(df_a, df_b) {

  # prepare variables ----------------------------------------------------------
  if (length(df_a) == 0) {
    df_a[1,] <- NA
  }

  # get unique column names
  cnames_a <- colnames(df_a)
  cnames_b <- colnames(df_b)
  cnames <- c(cnames_a, cnames_b)
  unique_cnames <- unique(cnames)

  # get column names that are missing in dataframe a
  missing_in_df_a <- unique_cnames[!(unique_cnames %in% cnames_a)]

  # add column names
  for (cname in missing_in_df_a) {
    df_a[,cname] <- NA
  }

  return(df_a)

}
