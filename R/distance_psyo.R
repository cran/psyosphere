
distance_psyo <- function(
  tracks1, tracks2, t_id1 = "id", t_id2 = "id", bind = TRUE, drop = TRUE
) {

  # Check variables
  e <- val_psyo(tracks1, 0, 0, 2, 2, 2); if (e != "") {stop(e)}
  e <- val_psyo(tracks2, 0, 0, 2, 2, 2); if (e != "") {stop(e)}
  e <- val_cname(tracks1, t_id1); if (e != "") {stop(e)}
  e <- val_cname(tracks2, t_id2); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}

  # Add id to restort tracks
  tracks1[,"sort_id_6sje94s4s4vs4"] <- 1:nrow(tracks1)

  # Calculate distances between tracks of the two psyo dataframes
  result <- psyosphere::apply_tracks(
    tracks1,
    "distance_psyo_private(eval_track, arg1, arg2, arg3, arg4, arg5)",
    arg1 = t_id2,
    arg2 = tracks2,
    arg3 = TRUE,
    arg4 = FALSE,
    arg5 = "",
    t_id = t_id1
  )

  # Resort result
  result <- result[order(result[,"sort_id_6sje94s4s4vs4"]),]
  tracks1[,"sort_id_6sje94s4s4vs4"] <- NULL
  result[,"sort_id_6sje94s4s4vs4"] <- NULL

  # Return result
  result <- bind_drop_private(tracks1, result, bind, drop)
  return(result)

}
