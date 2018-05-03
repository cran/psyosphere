
export_kml <- function(tracks, t_id = "id") {

  if (!"file_name" %in% names(tracks)) {
    stop("Column 'file_name' must be specefied.")
  }

  if (!"track_name" %in% names(tracks)) {
    stop("Column 'track_name' must be specefied.")
  }

  psyosphere::apply_tracks(
    tracks, "export_kml_private(eval_track)"
  )

  return(NULL)

}

export_kml_private <- function(track) {

  # Get track settings

  track_name <- track[,"track_name"][1]
  file_name <- track[,"file_name"][1]
  color <- kml_format_color_private(track[,"track_color"][1])

  # Create kml file

  kml_header <- export_kml_header_private(track_name, color)
  kml_track <- export_kml_track_private(track)
  kml_footer <- export_kml_footer_private()

  kml <- paste(kml_header, kml_track, kml_footer, sep = "\n")

  cat(kml, file = file_name, sep = "\n", append = FALSE)

}

kml_format_color_private <- function(color) {

  color_new <- unlist(lapply(color, function(x){

    x <- gsub("#","",x)

    v1 <- substr(x, 1, 2)
    v2 <- substr(x, 3, 4)
    v3 <- substr(x, 5, 6)
    v4 <- substr(x, 7, 8)

    y <- paste0(v4, v3, v2, v1, collapse = "")

    return(y)

  }))

  return(color_new)

}

export_kml_header_private <- function(name, color) {

  kml_xml <- '<?xml version="1.0" encoding="UTF-8"?>'
  kml_kml <- paste(
    '<kml xmlns="http://www.opengis.net/kml/2.2"',
    'xmlns:gx="http://www.google.com/kml/ext/2.2"',
    'xmlns:kml="http://www.opengis.net/kml/2.2"',
    'xmlns:atom="http://www.w3.org/2005/Atom">'
  )

  kml_style <- paste0('
  <Document>

    <name>',name,'</name>

    <StyleMap id="multiTrack">
			  <Pair>
          <key>normal</key>
          <styleUrl>#multiTrack_n</styleUrl>
        </Pair>
        <Pair>
          <key>highlight</key>
          <styleUrl>#multiTrack_h</styleUrl>
        </Pair>
    </StyleMap>

    <Style id="multiTrack_n">
      <IconStyle>
        <Icon>
          <href>',
'http://earth.google.com/images/kml-icons/track-directional/track-0.png','
          </href>
        </Icon>
      </IconStyle>
      <LineStyle>
        <color>',color,'</color>
        <width>6</width>
      </LineStyle>
    </Style>

    <Style id="multiTrack_h">
      <IconStyle>
        <scale>1.2</scale>
        <Icon>
          <href>',
'http://earth.google.com/images/kml-icons/track-directional/track-0.png','
          </href>
        </Icon>
      </IconStyle>
      <LineStyle>
        <color>',color,'</color>
        <width>8</width>
      </LineStyle>
    </Style>

    <Folder>
      <name>Tracks</name>
      <open>1</open>
      <Placemark>
        <name>',name,'</name>
        <styleUrl>#multiTrack</styleUrl>
        <gx:Track>
')

  kml_header <- paste(kml_xml, kml_kml, kml_style, sep = "\n")

  return(kml_header)

}

export_kml_track_private <- function(track) {

  # Create track

  when <- as.character(track[,"time"])
  when <- lapply(when, function(x) {
    gsub(pattern = "[[:space:]]", replacement = "T", x = x)
  })
  when <- paste0("          <when>", when, "Z</when>")
  when <- paste(when, collapse = "\n")

  coord <- paste( track[,"lon"], track[,"lat"])
  coord <- paste0("          <gx:coord>", coord, "</gx:coord>")
  coord <- paste(coord, collapse = "\n")

  kml_track <- paste(when, coord, sep = "\n")

  return(kml_track)

}

export_kml_footer_private <- function() {

  # Create footer

  kml_footer <- '
        </gx:Track>
      </Placemark>
    </Folder>
  </Document>
</kml>'

  return(kml_footer)

}
