
dir_get_gpx <- function(
  dir, tz = "", stringsAsFactors = default.stringsAsFactors()
) {

  # Check if dir exists
  if (!file.exists(dir)) {
    stop(paste("Directory '",dir,"' not found.", sep = ""))
  }

  # Get GPX file names
  gpx_file_names <- list.files(
    path = dir, pattern = ".gpx", full.names = TRUE)

  # Check if files are found
  if (length(gpx_file_names) == 0L) {
    stop(paste("No gpx files found in directory '",dir,"'.", sep = ""))
  }

  # Loop through files names and append gpx data to "tracks"
  for (i in 1:NROW(gpx_file_names)) {

    file_name <- gpx_file_names[i]
    iteration <- read_gpx_file_private(file_name, tz, stringsAsFactors)

    if (i == 1) {
      tracks <- iteration
    } else {
      tracks <- rbind(tracks, iteration)
    }

  }

  # Return result
  return(tracks)

}

read_gpx_file_private <- function(file_name, tz, stringsAsFactors) {

  # Extract layers from gpx file
  layers <- rgdal::ogrListLayers(file_name)

  # Read layer 5 from gpx file
  gpx <- rgdal::readOGR(
    file_name, layer = layers[5], stringsAsFactors = stringsAsFactors
  )

  # add filename withouth path as id variable
  id <- basename(file_name)
  p_id <- NA
  time <- gpx$time
  lon <- gpx$coords.x1
  lat <- gpx$coords.x2
  ele <- gpx$ele

  # Save as data frame
  tracks <- data.frame(
    id, p_id, time, lon, lat, ele, stringsAsFactors = stringsAsFactors
  )

  tracks$time <- as.POSIXct(tracks$time, tz = tz)
  tracks <- plyr::arrange(tracks,tracks$time)
  tracks$p_id <- c(1:NROW(tracks))

  # Check for duplicates
  if (base::any(base::duplicated(gpx$track_seg_point_id))) {
    cat(paste0("Multiple track segments ignored in: '",id,"'. "))
  }

  # return data
  return(tracks)

}





