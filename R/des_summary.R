
des_summary <- function(
  tracks, cweight = "auto", cgaps = "", t_id = "id", des_df = ""
) {

  # check variables
  e <- val_psyo(tracks, 0, 2, 2, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id, force = 2, def = TRUE); if (e != "") {stop(e)}

  # Check variables for merge
  if (is.data.frame(des_df)) {
    e <- val_psyo(des_df, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
    e <- val_cname(tracks, t_id, def = FALSE); if (e != "") {stop(e)}
    e <- val_cname(des_df, t_id, def = FALSE); if (e != "") {stop(e)}
  }

  # Calculate columns
  tracks <- calculate_columns_private(tracks, cweight, t_id)
  if (cweight == "auto") { cweight <- "time_diff_private" }

  # Calculate descriptives
  result <- calculate_descriptives_private(tracks, cweight, cgaps, t_id)

  # Merge descriptives
  result <- des_merge_private(result, des_df, t_id)

  return(result)

}

calculate_columns_private <- function(tracks, cweight, t_id) {

  # Remove essential columns if they already exist
  if ("time_diff_private" %in% names(tracks)) {
    tracks[,"time_diff_private"] <- NULL
  }

  if ("distances_in_m" %in% names(tracks)) {
    tracks[,"distances_in_m"] <- NULL
  }

  if ("speed" %in% names(tracks)) {
    tracks[,"speed"] <- NULL
  }

  # Calculate columns
  tracks <- psyosphere::t_time_difference(tracks, cname = "time_diff_private")
  tracks <- psyosphere::t_distance(tracks, t_id = t_id)
  tracks <- psyosphere::t_speed(tracks, t_id = t_id)

  # Return result
  return(tracks)

}

calculate_descriptives_private <- function(tracks, cweight, cgaps, t_id) {

  # id
  summary <- psyosphere::des_first(
    tracks, ctarget = t_id, cname = "id", drop = F,
    t_id = t_id
  )[,1]

  # ppn
  if ("ppn" %in% names(tracks)) {
    summary <- psyosphere::des_first(
      tracks, ctarget = "ppn", cname = "ppn", drop = F,
      t_id = t_id, des_df = summary
    )
  }

  # team
  if ("team" %in% names(tracks)) {
    summary <- psyosphere::des_first(
      tracks, ctarget = "team", cname = "team", drop = F,
      t_id = t_id, des_df = summary
    )
  }

  # begin
  summary <- psyosphere::des_min(
    tracks, ctarget = "time", cname = "begin_time", drop = F, t_id = t_id,
    des_df = summary
  )

  # end
  summary <- psyosphere::des_max(
    tracks, ctarget = "time", cname = "end_time", drop = F, t_id = t_id,
    des_df = summary
  )

  # duration
  duration <- base::difftime(summary[,"end_time"], summary[,"begin_time"])
  units <- attributes(duration)$units
  cduration <- paste0("duration_in_",units)
  summary[,cduration] <- duration

  # tracker interval
  interval <- tracker_interval_setting_private(
    tracks, "time_diff_private", FALSE
  )
  summary[,"tracker_interval_in_seconds"] <- interval

  # count coordinates
  summary <- psyosphere::des_length(
    tracks, cgaps = cgaps, cname = "coordinates", drop = F, t_id = t_id,
    des_df = summary
  )

  # count gapped coordinates
  if (length(cgaps) > 1 || cgaps != "") {

    gap_tracks <- select_gaps(tracks, cgaps)
    summary <- psyosphere::des_length(
      gap_tracks, cname = "coordinates_gapped", drop = F, t_id = t_id,
      des_df = summary
    )

    summary[,"coordinates_gapped"] <- ifelse(
      is.na(summary[,"coordinates_gapped"]), 0, summary[,"coordinates_gapped"]
    )

    summary <- psyosphere::des_length(
      tracks, cname = "coordinates_all", drop = F, t_id = t_id,
      des_df = summary
    )

  }

  # count duplicates
  summary <- psyosphere::des_duplicates(
    tracks, "time", cgaps = cgaps, cname = "time_duplicates", drop = F, t_id = t_id,
    des_df = summary
  )

  # count gaps per type
  if (length(cgaps) > 1 || cgaps != "") {
    for (i in 1:length(cgaps)) {
      summary <- psyosphere::des_sum(
        tracks, ctarget = cgaps[i], cgaps = "", cname = cgaps[i], drop = F,
        t_id = t_id, des_df = summary
      )
    }
  }

  # distance
  summary <- psyosphere::des_sum(
    tracks, ctarget = "distances_in_m", cname = "sum_km_not_gapped", drop = F,
    t_id = t_id, des_df = summary
  )
  summary[,"sum_km_not_gapped"] <- summary[,"sum_km_not_gapped"]/1000

  # mean kmh all
  summary <- psyosphere::des_mean(
    tracks, "speed", cweight = cweight, cgaps = cgaps, cname = "mean_kmh",
    drop = F, t_id = t_id, des_df = summary
  )

  dont_stop_tracks <- tracks[ which(tracks[,"speed"] > 0) ,]
  only_stop_tracks <- tracks[ which(tracks[,"speed"] == 0) ,]

  # mean kmh all without standing still
  summary <- psyosphere::des_mean(
    dont_stop_tracks, "speed", cweight = cweight, cgaps = cgaps,
    cname = "mean_kmh_no_stop", drop = F, t_id = t_id, des_df = summary
  )

  # add movement time
  summary <- psyosphere::des_sum(
    dont_stop_tracks, "time_diff_private", cgaps = cgaps,
    cname = "movement_time_sum", drop = FALSE, t_id = t_id, des_df = summary
  )

  # add no movement time
  summary <- psyosphere::des_sum(
    only_stop_tracks, "time_diff_private", cgaps = cgaps,
    cname = "no_movement_time_sum", drop = FALSE, t_id = t_id, des_df = summary
  )

  # movement / no movement ratio
  summary[,"move_by_no_move_ratio"] <- (
    as.numeric(summary[,"movement_time_sum"]) /
    as.numeric(summary[,"no_movement_time_sum"])
  )

  # time with good data
  summary <- psyosphere::des_sum(
    tracks, "time_diff_private", cgaps = cgaps, cname = "time_good_sum",
    drop = FALSE, t_id = t_id, des_df = summary
  )
  summary[,"time_missing_sum"] <- (
    summary[,cduration] - summary[,"time_good_sum"]
  )

  # gap time
  if (length(cgaps) > 1 || cgaps != "") {
    gap_tracks <- select_gaps(tracks, cgaps)
    summary <- psyosphere::des_sum(
      gap_tracks, "time_diff_private", cname = "time_gap_sum", drop = FALSE,
      t_id = t_id, des_df = summary
    )

    attr_time_gap_sum <- attributes(summary[,"time_gap_sum"])
    summary[,"time_gap_sum"] <- ifelse(
      is.na(summary[,"time_gap_sum"]), 0, summary[,"time_gap_sum"]
    )
    attributes(summary[,"time_gap_sum"]) <- attr_time_gap_sum

    summary[,"time_missing_sum"] <- (
      summary[,"time_missing_sum"] - summary[,"time_gap_sum"]
    )
  }

  return(summary)

}





