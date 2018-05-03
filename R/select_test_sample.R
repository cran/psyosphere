
select_test_sample <- function(tracks, size = 3, t_id = "id") {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_var(size, "numeric"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # select test sample
  test_tracks <- psyosphere::apply_tracks(
    tracks,
    "test_sub_selection_private(eval_track, arg1)",
    size,
    t_id = t_id
  )

  # return test sample
  return(test_tracks)

}

test_sub_selection_private <- function(tracks, size) {

  test_selection <- tracks[c(1:size),]

  return(test_selection)

}


