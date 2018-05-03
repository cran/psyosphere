
des_merge_private <- function(result, des_df, t_id) {

  if (is.data.frame(des_df)) {
    result <- merge(des_df, result, by = t_id, all = TRUE)
  }

  return(result)
}

des_drop_private <- function(result, drop) {

  if ((NROW(result) == 1) && drop) {

    if (NCOL(result) == 2) {
      result <- result[,2, drop = FALSE]
    }

    result <- result[1]

  }

  return(result)

}

omit_gaps_private <- function(tracks, ctarget, cgaps) {

  # save attributes, because ifelse drops attributes
  attributes <- attributes(tracks[,ctarget])

  # check if one cname is provided and that the name is not empty
  if (length(cgaps) == 1 && cgaps != "") {
    tracks[,ctarget] <- ifelse( tracks[,cgaps] == TRUE, NA, tracks[,ctarget])
  }

  # check if there are multiple cnames
  else if (length(cgaps) > 1) {
    tracks[,ctarget] <- ifelse(
      rowSums(tracks[,cgaps],na.rm = TRUE) >= 1 , NA, tracks[,ctarget]
    )
  }

  # restore attributes
  attributes(tracks[,ctarget]) <- attributes

  return(tracks)
}
