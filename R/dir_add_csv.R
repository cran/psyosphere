dir_add_csv <- function(
  tracks, dir, merge_by = "id", stringsAsFactors = default.stringsAsFactors()
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_var(dir, "character"); if (e != "") {stop(e)}
  e <- val_var(merge_by, "character"); if (e != "") {stop(e)}

  # Read CSV file
  id_info <- utils::read.csv(
    file = dir, header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE,
    comment.char = "", stringsAsFactors = stringsAsFactors
  )

  # Check if tracks exist but have no information in the CSV file
  check_missing_csv_data_private(tracks, id_info)

  # Check if info exist in CSV file but not in tracks
  check_missing_track_data_private(tracks, id_info)

  # Merge the data with the provided dataframe
  tracks <- merge(x = tracks, y = id_info, by = merge_by, all.x = TRUE)

  return(tracks)

}

check_missing_csv_data_private <- function(tracks, id_info) {

  # Compare tracks and CSV file info
  missing_in_id_info <- setdiff(tracks$id, id_info$id)

  # Check if something is missing
  if (NROW(missing_in_id_info) == 0) {
    return()
  }

  # Stop execution
  missing_track_ids <- paste(missing_in_id_info, collapse = ", ")
  message <- paste(
    "The following tracks exist in dataframe but not in CSV file:",
    missing_track_ids
  )

  stop(message)

}

check_missing_track_data_private <- function(tracks, id_info) {

  # Compare tracks and CSV file info
  missing_in_tracks <- setdiff(id_info$id, tracks$id)

  # Check if something is missing
  if ( (length(missing_in_tracks) == 0) || (missing_in_tracks == "") ) {
    return()
  }

  # Inform user
  missing_track_ids <- paste(missing_in_tracks, collapse = ", ")
  message <- paste(
    "The following tracks exist in CSV file but not in dataframe:",
    missing_track_ids
  )

  cat(message)

}
