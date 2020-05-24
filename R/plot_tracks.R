
plot_tracks <- function(
  tracks, single = FALSE, line = TRUE, dots = TRUE, plot = "", zoom = -1,
  save_dir = "", cgaps = "", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_var(single, "logical"); if (e != "") {stop(e)}
  e <- val_var(line, "logical"); if (e != "") {stop(e)}
  e <- val_var(plot, "ggplot", def = TRUE); if (e != "") {stop(e)}
  e <- val_var(zoom, "numeric"); if (e != "") {stop(e)}
  e <- val_var(save_dir, "character", def = TRUE); if (e != "") {stop(e)}
  e <- val_cname(tracks, cgaps, type = "logical"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Get the google map
  if ( (mode(plot) == "character") && (plot == "") ) {
    plot <- psyosphere::plot_map(tracks, zoom)
  }
  zoom <- plot$zoom

  # Check if save_dir exists and otherwise create it
  create_save_dir_private(save_dir)

  # Plot the each track seperatly
  if (single || save_dir != "") {

    plot <- psyosphere::apply_tracks(
      tracks,
      "plot_tracks_private(
        eval_track, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8
      )",
      arg1 = zoom,
      arg2 = plot,
      arg3 = single,
      arg4 = save_dir,
      arg5 = t_id,
      arg6 = line,
      arg7 = dots,
      arg8 = cgaps,
      t_id = t_id
    )

  } else {

    # Create one plot with all tracks

    plot <- plot_singel_tracks_private(
      tracks, plot, t_id, line, dots, cgaps
    )

  }

  return(plot)

}

plot_singel_tracks_private <- function(tracks, plot, t_id, line, dots, cgaps) {

  # Onle one track
  if (t_id == "") {
    plot <- plot_single_private(tracks, plot, line, dots, cgaps)
    return(plot)
  }

  # Multiple tracks

  # Prepare variables
  tracks_ids <- unique(tracks[,t_id])

  # Plot seperate tracks
  for (id in tracks_ids) {

    track <- tracks[ tracks[,t_id] == id ,]
    plot <- plot_single_private(track, plot, line, dots, cgaps)

  }

  # Return result
  return(plot)

}

plot_single_private <- function(tracks, plot, line, dots, cgaps) {

  # Plot lines for seperate tracks
  if (line) {
    plot <- plot_lines_private(plot, tracks, cgaps)
  }

  # Plot coordinates
  if (dots) {
    plot <- plot_coordinates_private(plot, tracks)
  }

  return(plot)

}

create_save_dir_private <- function(save_dir) {

  # Check if save_dir is set
  if (save_dir == "") {
    return()
  }

  # Create dir if it doesn't exist
  dir.create(save_dir, showWarnings = FALSE)

}

plot_tracks_private <- function(
  gpx_data, zoom, plot, single, save_dir, t_id, line, dots, cgaps
) {

  # Prepare variables
  if (save_dir != "") { save_dir <- paste0(save_dir,"/") }
  current_id <- as.character(gpx_data[1,c(t_id)])

  # Plot lines
  if (line) {
    plot <- plot_lines_private(plot, gpx_data, cgaps)
  }

  # Plot coordinates
  if (dots) {
    plot <- plot_coordinates_private(plot, gpx_data)
  }

  # # Add plot labels
  # plot_label <- plot_labels_private(gpx_data, t_id, zoom)
  # plot <- plot + ggplot2::geom_label(
  #   hjust = 0,
  #   vjust = 1,
  #   label = plot_label,
  #   alpha = 0.5
  # )

  # Display plot and Wait
  if (single) {
    plot(plot)
    if (t_id != "") {
      wait_for_input_private()
    }
  }

  # Save plot in file
  if (save_dir != "") {
    cat(" ",current_id," ")
    png_file <- paste0(current_id, ".png")
    ggplot2::ggsave(filename = paste0(save_dir,png_file), plot = plot)
  }

  return(plot)

}

plot_lines_private <- function(plot, tracks, cgaps = "") {

  # Plot lines without gaps
  if (length(cgaps) == 1 && cgaps == "") {

    if (NROW(tracks) > 1) {
      plot <- psyosphere::plot_line(tracks, plot = plot)
    }
    return(plot)
  }

  # Plot lines with gaps
  plot <- plot_good_segments_private(plot, tracks, cgaps)
  plot <- plot_gaps_private(plot, tracks, cgaps)

  # Return result
  return(plot)

}

plot_good_segments_private <- function(plot, tracks, cgaps) {

  # Mark gap segements
  tracks <- mark_gap_segments(tracks, cgaps)
  unique_segments <- unique(tracks[,"gap_segments"])

  # Plot segments
  for (segement in unique_segments) {

    track_segment <- tracks[ tracks[,"gap_segments"] == segement ,]

    if (NROW(track_segment) > 1) {
      plot <- psyosphere::plot_line(track_segment, plot = plot)
    }

  }

  return(plot)

}

plot_gaps_private <- function(plot, tracks, cgaps) {

  # Get previous coordinate
  tracks <- psyosphere::apply_shift(
    tracks, "-1", csubset = c("lon","lat"), t_id = ""
  )

  # Get gaps
  gaps <- select_gaps(tracks, cgaps)

  # Check if there are gaps present
  if (NROW(gaps) == 0) {
    return(plot)
  }

  # Plot each gap
  for (i in 1:NROW(gaps)) {

    # Create line for gap
    lon <- gaps[i, "lon_pre_1"]
    lat <- gaps[i, "lat_pre_1"]
    p1 <- data.frame(lon, lat)
    p2 <- gaps[i, c("lon","lat"), drop = FALSE]
    gap_line <- rbind(p1, p2)

    # Plot line
    line <- stats::na.omit(gap_line)
    if (NROW(line) > 1) {
      plot <- psyosphere::plot_line(line, colour = "red", plot = plot)
    }

    remove(gap_line)

  }

  # Return result
  return(plot)

}

wait_for_input_private <- function()
{

  cat("Press [enter] to continue or enter 'exit' and confirm with [enter]")
  line <- readline()

  if (line != "") {

    warning("Abborted by user", call. = FALSE, noBreaks. = TRUE)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()

  }

}

plot_labels_private <- function(tracks, t_id, zoom) {

  # Prepare variables
  points <- nrow(tracks)
  begin <- min(tracks[,c("time")])
  end <- max(tracks[,c("time")])
  id <- count_unique_labels_private(tracks,t_id)
  team <- count_unique_labels_private(tracks,"team")
  ppn <- count_unique_labels_private(tracks,"ppn")
  label <- ""

  # Append data to label
  label <- append_label_private(label, zoom)
  label <- append_label_private(label, points)
  label <- append_label_private(label, begin)
  label <- append_label_private(label, end)

  label <- append_label_private(label, id)
  label <- append_label_private(label, team)
  label <- append_label_private(label, ppn)

  label <- gsub("\n$", "", label)

  return(label)

}

count_unique_labels_private <- function(tracks, cname) {

  # Check variables
  if (!cname %in% colnames(tracks)) {
    return("")
  }

  # Prepare variables
  track_column <- tracks[,c(cname)]
  unique_val <- unique(track_column)
  count <- NROW(unique_val)

  # Decide what to return
  if (count == 1) {
    return(unique_val)
  }

  return(paste0("multiple (",count,")"))

}

append_label_private <- function(
  label, variable, name = deparse(substitute(variable))
) {

  # Check variables
  if (as.character(variable) == "") {
    return(label)
  }

  # Append new label
  label <- paste0(label,name,": ", variable, "\n")
  return(label)

}

plot_coordinates_private <- function(map, tracks) {

  # get color

  if("dot_color" %in% colnames(tracks)) {
    colour <- tracks[,"dot_color"][1]
  } else {
    colour <- "black"
  }

  plot <- map +
    ggplot2::geom_point(data = tracks,
                        size = 2,
                        pch = 20,
                        show.legend = TRUE,
                        colour = colour
    )

  return(plot)

}

get_plot_range_poly_private <- function(plot) {

  # Prepare variables
  lon_r <- ggplot2::ggplot_build(plot)$panel$ranges[[1]]$x.range
  lat_r <- ggplot2::ggplot_build(plot)$panel$ranges[[1]]$y.range
  lon <- c(lon_r[1],lon_r[1],lon_r[2],lon_r[2], lon_r[1])
  lat <- c(lat_r[1],lat_r[2],lat_r[2],lat_r[1], lat_r[1])
  poly_r <- data.frame(lon, lat)

  # Return results
  return(poly_r)

}
