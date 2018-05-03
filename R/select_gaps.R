
select_gaps <- function(tracks, cgaps) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 0, 0); if (e != "") {stop(e)}
  e <- val_cname(tracks, cgaps, type = "logical", def = FALSE); if (e != "") {stop(e)}

  # Check if one gaps column
  if (length(cgaps) == 1 && cgaps != "") {
    tracks <- tracks[ which(tracks[,cgaps]) ,]
  }

  # Multiple gap columns
  if (length(cgaps) > 1) {
    tracks <- tracks[ rowSums(tracks[,cgaps],na.rm = TRUE) >= 1 ,]
  }

  # Return result
  return(tracks)

}
