
export_gpx <- function(tracks, t_id = "id") {

  if (!"file_name" %in% names(tracks)) {
    stop("Column 'file_name' must be specefied.")
  }

  if (!"track_name" %in% names(tracks)) {
    stop("Column 'track_name' must be specefied.")
  }

  psyosphere::apply_tracks(
    tracks, "export_gpx_private(eval_track)"
  )

  return(NULL)

}

export_gpx_private <- function(track) {

  # Get track settings

  track_name <- track[,"track_name"][1]
  file_name <- track[,"file_name"][1]

  # Create gpx file

  gpx_header <- export_gpx_header_private(track_name)
  gpx_track <- export_gpx_track_private(track)
  gpx_footer <- export_gpx_footer_private()

  gpx <- paste(gpx_header, gpx_track, gpx_footer, sep = "\n")

  cat(gpx, file = file_name, sep = "\n", append = FALSE)

}

export_gpx_header_private <- function(track_name) {

  gpx_header <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>\n',
    '<gpx ',
      'creator="http://analyse-gps.com" ',
      'version="1.0" ',
      'xmlns="http://www.topografix.com/GPX/1/0" ',
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
      'xsi:schemaLocation="http://www.topografix.com/GPX/1/0 ',
      'http://www.topografix.com/GPX/1/0/gpx.xsd"',
    '>\n',
    '  <trk>\n',
    '    <name>',track_name,'</name>\n',
    '    <trkseg>'
  )

  return(gpx_header)

}

export_gpx_track_private <- function(track) {

  # Latitude and Longitude

  track[,"lat"] <- paste0('      <trkpt lat="', track[,"lat"], '" ')
  track[,"lon"] <- paste0('lon="', track[,"lon"], '">\n')

  # Elevation

  if ("ele" %in% colnames(track)){
    track[,"ele"] <- paste0('        <ele>', track[,"ele"], '</ele>\n')
  } else {
    track[,"ele"] <- ""
  }

  # Time

  track[,"time"] <- as.character(track[,"time"])
  track[,"time"] <- unlist(lapply(track[,"time"], function(x) {
    gsub(pattern = "[[:space:]]", replacement = "T", x = x)
  }))
  track[,"time"] <- paste0("        <time>", track[,"time"], "Z</time>\n")
  track[,"time"] <- paste0(track[,"time"], "      </trkpt>")

  # Merge track point

  gpx_track <- paste0(
    track[,"lat"], track[,"lon"], track[,"ele"], track[,"time"]
  )

  gpx_track <- paste0(gpx_track, collapse = "\n")

  return(gpx_track)

}

export_gpx_footer_private <- function() {

  gpx_footer <- paste0(
    '    </trkseg>\n',
    '  </trk>\n',
    '</gpx>\n'
  )

  return(gpx_footer)

}
