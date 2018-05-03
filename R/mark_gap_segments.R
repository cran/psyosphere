
mark_gap_segments <- function(
  tracks, cgaps, bind = TRUE, drop = TRUE, cname = "gap_segments", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 2, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_cname(tracks, cgaps, type = "logical", def = FALSE); if (e != "")
  {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Mark segments for each track
  result <- psyosphere::apply_tracks(
    tracks,
    "mark_seperat_segments_private(eval_track, arg1, arg2)",
    arg1 = cgaps,
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

mark_seperat_segments_private <- function(tracks, cgaps, cname) {

  # Select gaps
  gaps <- select_gaps(tracks, cgaps)
  gaps_count <- NROW(gaps)

  # Mark segements
  result <- mark_segment_private(tracks, gaps, gaps_count, cgaps, cname)

  # Return results
  return(result)

}

mark_segment_private <- function(tracks, gaps, gaps_count, cgaps, cname) {

  # Create whole list as one segement
  result <- rep.int(tracks[1,c("p_id")], NROW(tracks))

  # Return list if no tracks are found
  if (gaps_count == 0) {
    return(result)
  }

  # Set begin of first segments
  first_segment_p_id <- tracks[1,c("p_id")]

  # Name all segments
  for (i in 1:(gaps_count + 1)) {

    # Get the current gap p_id
    cur_gap_p_id <- gaps[i,"p_id"]

    # Get end of segment
    if (i != (gaps_count + 1)) {

      # Check if the first coordinate has a gap mark
      after_gap <- tracks[ tracks[,"p_id"] < cur_gap_p_id ,"p_id"]
      if (NROW(after_gap > 0)) {
        last_seg_pid <- max(after_gap)
      }

      # The first coordinate is a gap and stand alone as a point
      else {
        last_seg_pid <- cur_gap_p_id
      }

    # Get the last segement end
    } else {
      last_seg_pid <- max(tracks[,"p_id"])
    }

    # Name segment
    result <- ifelse(
      tracks[,"p_id"] >= first_segment_p_id &
        tracks[,"p_id"] <= last_seg_pid,
      first_segment_p_id,result
    )

    # Set begin of new segment
    remaining_tracks <- tracks[ tracks[,"p_id"] > last_seg_pid ,]
    first_segment_p_id <- remaining_tracks[1,"p_id"]

  }

  # Return result
  return(result)

}
