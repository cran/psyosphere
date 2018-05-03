
bind_drop_private <- function(tracks, result, bind = TRUE, drop = TRUE) {

  # Prepare variables
  cnames <- names(result)

  # Add column or overwrite column if it already exists
  if (bind) {
    for (i in 1:NROW(cnames)) {
      cname <- cnames[i]
      if (cname %in% names(tracks)) {cat(paste(cname,"is overwritten\n"))}
      tracks[,cname] <- result[,cname]
    }
    result <- tracks
  }

  # Collapse to list or atomic vector
  if ((NROW(result) == 1) && drop) {

    if (NCOL(result) == 2) {
      result <- result[,2, drop = FALSE]
    }

    result <- result[,, drop = TRUE]

  }

  # Return result
  return(result)

}
