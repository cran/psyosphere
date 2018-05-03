
select_between_polygons <- function(
  tracks, poly1, poly2, t_id = "id", merge_id = TRUE
) {

  # Check variables
  e <- val_psyo(tracks, 0, 2, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(poly1,"lon",type = "numeric",def = FALSE);if (e != "") {stop(e)}
  e <- val_cname(poly1,"lat",type = "numeric",def = FALSE);if (e != "") {stop(e)}
  e <- val_cname(poly2,"lon",type = "numeric",def = FALSE);if (e != "") {stop(e)}
  e <- val_cname(poly2,"lat",type = "numeric",def = FALSE);if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}
  e <- val_var(merge_id, "logical"); if (e != "") {stop(e)}

  # Select between polygons for each tracks
  tracks <- psyosphere::apply_tracks(
    tracks,
    "select_between_start_finish_private(eval_track, arg1, arg2)",
    poly1,
    poly2,
    t_id = t_id
  )

  # Merge track id and rounds
  tracks <- merge_track_ids_private(tracks, t_id, merge_id)

  # Return result
  return(tracks)

}

merge_track_ids_private <- function(tracks, t_id, merge_id) {

  # Check if merge is activated
  if (!merge_id) {
    return(tracks)
  }

  # Merge column track and t_id
  tracks[,c(t_id)] <- apply(
    tracks[,c(t_id,"track")], 1, paste, collapse = "_"
  )

  # Return result
  return(tracks)

}

select_between_start_finish_private <- function(
  tracks, poly1, poly2
) {

  # add in poly1 and in poly2 + prev columns to tracks
  poly_tracks <- mark_start_finish_private(tracks, poly1, poly2)

  # Select all coordinates in first polygon
  in_poly1_t <- poly_tracks[ poly_tracks$in_poly1 == 1,]

  # Select coordinate that is the first in polygon 1
  poly1_entry <- in_poly1_t[ in_poly1_t$in_poly1_pre_1 == 0,]

  # Check if there is a polygon that is first in polygon 1
  if (NROW(poly1_entry) == 0) {
    warnings("No entry in start polygon. No selection made.")
    tracks$track <- 1
    return(tracks)
  }

  # First coordinate in poly1
  poly1_entry$start_finish <- 1

  # Select all coordinates in second polygon
  in_poly2_t <- poly_tracks[ poly_tracks$in_poly2 == 1 ,]

  # Select coordinate that is the first in polygon 2
  in_poly2_entry <- in_poly2_t[ in_poly2_t$in_poly2_pre_1 == 0 ,]

  # First coordinate in poly2
  in_poly2_entry$start_finish <- 2

  start_entry_and_finish_entry <- rbind(poly1_entry, in_poly2_entry)

  start_entry_and_finish_entry <- subset(
    start_entry_and_finish_entry, select = c("p_id","start_finish")
  )
  start_entry_and_finish_entry <- plyr::arrange(
    start_entry_and_finish_entry, start_entry_and_finish_entry$p_id
  )

  start_entry_and_finish_entry <- psyosphere::apply_shift(
    start_entry_and_finish_entry,
    "+1",
    TRUE,
    c("p_id","start_finish"),
    t_id = ""
  )

  start_entry_and_finish_entry <- start_entry_and_finish_entry[((grep(
    "1",
    start_entry_and_finish_entry$start_finish,
    fixed = TRUE))),]

  start_entry_and_finish_entry <- start_entry_and_finish_entry[((grep(
    "2",
    start_entry_and_finish_entry$start_finish_fol_1,
    fixed = TRUE))),]

  start_entry_and_finish_entry <- stats::na.omit(start_entry_and_finish_entry)

  for (i in 1:NROW(start_entry_and_finish_entry)) {

    begin <- start_entry_and_finish_entry[c(i), c("p_id")]
    end <- start_entry_and_finish_entry[c(i), c("p_id_fol_1")]
    track <- subset(tracks,
                    tracks[,c("p_id")] >= begin & tracks[,c("p_id")] <= end
    )

    track <- cbind(track = i, track)

    if (i == 1) {
      result_tracks <- track
    } else {
      result_tracks <- rbind(result_tracks,track)
    }

  }

  return(result_tracks)

}

mark_start_finish_private <- function(
  tracks, poly1, poly2
) {

  # Mark if in first polygon
  tracks <- psyosphere::mark_inside_polygon(tracks, poly1, cname = "in_poly1")

  # Mark if in second polygon
  tracks <- psyosphere::mark_inside_polygon(tracks, poly2, cname = "in_poly2")

  # Copy shift entry in poly2
  tracks <- psyosphere::apply_shift(
    tracks, "+1", TRUE, "in_poly2", t_id = ""
  )

  # Move entry in poly2 in position back, so that the first point is still
  # outside of polygon 2
  tracks[,"in_poly2"] <- ifelse(
    (tracks[,"in_poly2"] + tracks["in_poly2_fol_1"] == 1), 1,
    tracks[,"in_poly2"]
  )
  tracks$in_poly2_fol_1 <- NULL

  # Copy polygon columns for seach for entry in polygons
  tracks <- psyosphere::apply_shift(
    tracks, "-1", TRUE, c("in_poly1","in_poly2"), t_id = ""
  )

  # Return results
  return(tracks)

}
