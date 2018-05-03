
# We advice you to read the information about the demos first.

?psyosphere::about_demos

# Settings

demo_dir <- tempdir()
# demo_dir <- getwd() # Uncomment this to run demo in working directory

dir <- paste0(demo_dir,"/smuggler1/")

# prepare R --------------------------------------------------------------------

rm(list = setdiff(ls(), "dir"))
library(psyosphere)

# Check directories
dir.create(dir, showWarnings = FALSE)
dir.create(paste0(dir,"rdata"), showWarnings = FALSE)
dir.create(paste0(dir,"plots"), showWarnings = FALSE)
dir.create(paste0(dir,"plots/rounds"), showWarnings = FALSE)
dir.create(paste0(dir,"gpx"), showWarnings = FALSE)
dir.create(paste0(dir,"kml"), showWarnings = FALSE)

# 01 store GPX files in data frame ---------------------------------------------

rm(list = setdiff(ls(), "dir"))

# Download and unzip files
url <- "https://analyse-gps.com/downloads/smuggle1.zip"
download.file(url = url, destfile = paste0(dir, "gps_files.zip"))
unzip(paste0(dir, "gps_files.zip"), exdir = paste0(dir, "gpx"))
remove(url)

# Save GPS files with participant movement as dataframe
tracks <- dir_get_gpx(paste0(dir, "/gpx"), stringsAsFactors = FALSE)

# The experiment took place between 13:45 and 14:55. Therefore we select this
# data.

begin <- as.POSIXct("2014-09-04 13:45:00")
end <- as.POSIXct("2014-09-04 14:55:00")

tracks <- tracks[ tracks[,"time"] > begin & tracks[,"time"] < end ,]

remove(begin, end)

save.image(paste0(dir, "rdata/01.RData"))

# 02 add information from id file to data frame --------------------------------

# The CSV stores some data about each participants that will be added to the
# data frame.

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/01.RData"))

tracks <- dir_add_csv(
  tracks, paste0(dir, "/gpx/ids.csv"), stringsAsFactors = FALSE
)

# Get first descriptives
des_begin <- des_summary(tracks)

save.image(paste0(dir, "/rdata/02.RData"))

# 03 save all plots but do not display them ------------------------------------

# The plots are generated to give you an idea how the data look like. You can
# check the plots in the plots folder in the directory set in dir.

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/02.RData"))

plot_tracks(
  tracks, single = FALSE, save_dir = paste0(dir, "/plots")
)

save.image(paste0(dir, "/rdata/03.RData"))

# 04 set start and finish areas ------------------------------------------------

# The participants walked several rounds from start to finish. We are only
# interested in the data between start and finish and want to save them
# as seperate tracks / rounds. First we create the start and finish areas
# as closed polygons.

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/03.RData"))

lon <- c(6.849975, 6.849627, 6.850001, 6.850350, 6.849975)
lat <- c(52.241745, 52.241100, 52.241004, 52.241649, 52.241745)
poly_start <- data.frame(lon, lat)
remove(lon, lat)

lon <- c(6.851810, 6.851000, 6.851489, 6.852296, 6.851810)
lat <- c(52.241800, 52.240300, 52.240163, 52.241657, 52.241794)
poly_finish <- data.frame(lon, lat)
remove(lon, lat)

# Create a map that centres on the polygons

plot_map <- plot_map(rbind(poly_start, poly_finish))
plot <- plot_polygon(poly_start ,plot = plot_map)
plot_polygon(poly_finish ,plot = plot)

rm(plot)
save.image(paste0(dir, "/rdata/04.RData"))

# 05 map tracks and shapes -----------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/04.RData"))

# We map the gps data and the start and finish polygons with Google Maps. It
# will take some time before the plot images appears because of the high amount
# of data.

plot_all <- plot_tracks(tracks, single = FALSE, plot = plot_map)
plot_all <- plot_polygon(poly_start, plot = plot_all)
plot_all <- plot_polygon(poly_finish, plot = plot_all)

# The plot will focus on the polygons and therefore several coordinates will be
# outside of the plot. The coordinates outside the plot will create warning
# messages. The following code will show the plot without the warnings.

suppressWarnings(print(plot_all))

rm(plot_all)
save.image(paste0(dir, "/rdata/05.RData"))

# 06 select tracks between start and finish ------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/05.RData"))

# seperate the "smugglers" from the guards

guard_ids <- c("D1","D2","D3","D4","D5")
tracks_guards <- tracks[ tracks[,"team"] %in% guard_ids, ]
tracks <- tracks[ !tracks[,"team"] %in% guard_ids, ]

# We select the different rounds of each participant and drop the other data.

tracks <- select_between_polygons(
  tracks, poly_start, poly_finish
)

rm(guard_ids)
save.image(paste0(dir, "/rdata/06.RData"))

# 07 map tracks without guards ------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/06.RData"))

# We plot now the results without the border guards. You can see clearly that
# only the movement between start and finish remain.

plot_rou <- plot_tracks(tracks, plot = plot_map)
plot_rou <- plot_polygon(poly_start, plot = plot_rou)
plot_rou <- plot_polygon(poly_finish, plot = plot_rou)
plot_rou

rm(plot_rou)

save.image(paste0(dir, "/rdata/07.RData"))

# 08 clean-up data -------------------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/07.RData"))

# Remove time duplicates
tracks <- average_duplicates(tracks)

# Mark time interval gaps between coordinates
tracks <- t_time_difference(tracks)
tracks <- mark_time_gaps(tracks)

save.image(paste0(dir, "/rdata/08.RData"))

# 09 add speed -----------------------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/08.RData"))

# Add speed
tracks <- t_speed(tracks)

# Mark impossible speed as gaps
tracks <- mark_speed_gaps(tracks,40)

# Save the gap names for later use
cgaps <- c("time_gap","speed_gap")

save.image(paste0(dir, "/rdata/09.RData"))

# 10 add bearing ---------------------------------------------------------------

# This function will add the bearing between each point of a track.

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/09.RData"))

tracks <- t_bearing(tracks)

save.image(paste0(dir, "/rdata/10.RData"))

# 11 add distance --------------------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/10.RData"))

tracks <- t_distance(tracks)

save.image(paste0(dir, "/rdata/11.RData"))

# 12 set colors per track ------------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/11.RData"))

# download and unzip file with package data

url <- "https://analyse-gps.com/downloads/cards.zip"
download.file(url = url, destfile = paste0(dir, "cards.zip"))
unzip(paste0(dir, "cards.zip"), exdir = paste0(dir, "."))
remove(url)

cards <- data.frame(); load(paste0(dir, "/Cards.RData"))

illegal <- cards[ cards[,"cocaine"] == 1, "hist_track_id"]
legal <- cards[ cards[,"cocaine"] == 0, "hist_track_id"]

rm(cards)

# Set roles

tracks[,"hist_track_id"] <- paste0("S1_",tracks[,"id"])
tracks[ tracks[,"hist_track_id"] %in% illegal, "role"] <- "illegal"
tracks[ tracks[,"hist_track_id"] %in% legal, "role"] <- "legal"
tracks[,"hist_track_id"] <- NULL
rm(illegal, legal)

tracks_guards[,"role"] <- "guard"

# Set colors

tracks[,"track_color"] <- ifelse(
  tracks[,"role"] == "illegal", "#00000099", # black
  ifelse(tracks[,"role"] == "legal", "#ffffff99", NA) # white
)

tracks_guards[,"track_color"] <- "blue"

save.image(paste0(dir, "rdata/12.RData"))

# 13 plot tracks by participants -----------------------------------------------

# We want to check how each seperate round looks like. This will generate more
# than 200 plots. You can review the plots in the plots folder.

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/12.RData"))

# Coordinates are black dots, legal roles have a white tracks, illegal roles
# have a black tracks, guards have a blue track and gaps in the data have red
# tracks.

plot_tracks(
  tracks, single = FALSE, save_dir = paste0(dir, "plots/rounds"), cgaps = cgaps,
  plot = plot_map
)

save.image(paste0(dir, "/rdata/13.RData"))

# 14 mark if border guards are visible -----------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/13.RData"))

# Create a plygon to mark when the guards are visible

polygon_guard_lon <- c(
  6.851234, 6.851129, 6.850950, 6.850821, 6.850699, 6.850582, 6.850477,
  6.850410, 6.850362, 6.850362, 6.850378, 6.850424, 6.850478, 6.850540,
  6.850705, 6.850898, 6.851100, 6.851255, 6.851399, 6.851942, 6.851234
)

polygon_guard_lat <- c(
  52.240456, 52.240448, 52.240478, 52.240531, 52.240601, 52.240666, 52.240772,
  52.240894, 52.241018, 52.241146, 52.241266, 52.241381, 52.241490, 52.241585,
  52.241656, 52.241698, 52.241717, 52.241706, 52.241662, 52.241620, 52.240456
)

poly_guard <- data.frame(lon = polygon_guard_lon,lat = polygon_guard_lat)
remove(polygon_guard_lon, polygon_guard_lat)

# Illustrate polygons with the three tracks of a participant

plot <- plot_polygon(poly_start, plot = plot_map)
plot <- plot_polygon(poly_finish, plot = plot)
plot <- plot_polygon(poly_guard, plot = plot)
plot_tracks(tracks[ tracks[,"ppn"] == 6 ,], plot = plot)

# Mark coordinates of each track that are within the guard polygon

tracks <- mark_inside_polygon(
  tracks, poly_guard, cname = "guard_vis"
)

rm(plot)
save.image(paste0(dir, "/rdata/14.RData"))

# 15 add distance to finish ----------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/14.RData"))

finish <- data.frame(lon = c(6.851810,6.851000), lat = c(52.241800,52.240300))

tracks_sel <- tracks[ tracks[,"ppn"] == 6 ,]
plot <- plot_line(finish, plot = plot_map)
plot_tracks(tracks_sel, plot = plot)

tracks <- distance_line(tracks,finish, TRUE)

rm(tracks_sel, plot)
save.image(paste0(dir, "/rdata/15.RData"))

# 16 add distance to shortest route from begin ---------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/15.RData"))

tracks <- distance_to_direct_line(
  tracks,finish, cname = "deviation"
)

remove(finish)

save.image(paste0(dir, "/rdata/16.RData"))

# 17 add average distance to team mates ----------------------------------------

# Be aware this functions can take a lot of time because of the many positions.
# With the following code you can test the calculation first:
#
# tracks_test <- select_test_sample(tracks)
# tracks_test <- distance_peers(tracks_test, cpeer = "team")

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/16.RData"))

# calculate average distance to peers

tracks <- distance_peers(tracks, cpeer = "team")

save.image(paste0(dir, "/rdata/17.RData"))

# 18 add distance to border guards ---------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/17.RData"))

tracks <- distance_psyo(tracks, tracks_guards)

tracks <- plyr::rename(
  tracks, c(
    "dis_to_D1-01-42.gpx_in_m" = "dis_guard1",
    "dis_to_D2-27-27.gpx_in_m" = "dis_guard2",
    "dis_to_D3-03-63.gpx_in_m" = "dis_guard3",
    "dis_to_D4-04-31.gpx_in_m" = "dis_guard4",
    "dis_to_D5-05-55.gpx_in_m" = "dis_guard5"
  )
)

save.image(paste0(dir, "/rdata/18.RData"))

# 19 create summary data frame -------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/18.RData"))

cgaps <- c("time_gap","speed_gap")
cweight <- "time_difference"

# Descriptives of the whole tracks
des_summary <- des_summary(tracks, cgaps = cgaps)
des_summary <- des_first(tracks, "track", cname = "round", des_df = des_summary)

des_summary <- des_sd(
  tracks, ctarget = "speed", cweight = cweight, cgaps = cgaps,
  cname = "sd_kmh", des_df = des_summary
)

des_summary <- des_mean(
  tracks, ctarget = "average_dis", cweight = cweight,
  cgaps = cgaps, cname = "mean_team_distance", des_df = des_summary
)

des_summary <- des_mean(
  tracks, ctarget = "deviation", cweight = cweight,
  cgaps = cgaps, cname = "mean_deviation_route", des_df = des_summary
)

des_summary <- des_sd(
  tracks, ctarget = "deviation", cweight = cweight, cgaps = cgaps,
  cname = "sd_deviation_route", des_df = des_summary
)

# Before hill descriptives

tracks_bh <- tracks[tracks$guard_vis %in% c(0),]

des_summary <- des_mean(
  tracks_bh, ctarget = "speed", cweight = cweight,
  cgaps = cgaps, cname = "bh_mean_kmh", des_df = des_summary
)

des_summary <- des_sd(
  tracks_bh, ctarget = "speed", cweight = cweight, cgaps = cgaps,
  cname = "bh_sd_kmh", des_df = des_summary
)

des_summary <- des_mean(
  tracks_bh, ctarget = "average_dis", cweight = cweight,
  cgaps = cgaps, cname = "bh_mean_team_distance", des_df = des_summary
)

des_summary <- des_mean(
  tracks_bh, ctarget = "deviation", cweight = cweight,
  cgaps = cgaps, cname = "bh_mean_deviation_route", des_df = des_summary
)

des_summary <- des_sd(
  tracks_bh, ctarget = "deviation", cweight = cweight, cgaps = cgaps,
  cname = "bh_sd_deviation_route", des_df = des_summary
)

# After hill descriptives

tracks_ah <- tracks[tracks$guard_vis %in% c(1),]

des_summary <- des_mean(
  tracks_ah, ctarget = "speed", cweight = cweight,
  cgaps = cgaps, cname = "ah_mean_kmh", des_df = des_summary
)

des_summary <- des_sd(
  tracks_ah, ctarget = "speed", cweight = cweight, cgaps = cgaps,
  cname = "ah_sd_kmh", des_df = des_summary
)

des_summary <- des_mean(
  tracks_ah, ctarget = "average_dis", cweight = cweight,
  cgaps = cgaps, cname = "ah_mean_team_distance", des_df = des_summary
)

des_summary <- des_mean(
  tracks_ah, ctarget = "deviation", cweight = cweight,
  cgaps = cgaps, cname = "ah_mean_deviation_route", des_df = des_summary
)

des_summary <- des_sd(
  tracks_ah, ctarget = "deviation", cweight = cweight, cgaps = cgaps,
  cname = "ah_sd_deviation_route", des_df = des_summary
)

save.image(paste0(dir, "/rdata/19.RData"))

# 20 export to excel -----------------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/19.RData"))

library(xlsx)
xlsx::write.xlsx(des_summary, paste0(dir, "/smuggler1.xlsx"))

save.image(paste0(dir, "/rdata/20.RData"))

# 21 export tracks with colors -------------------------------------------------

rm(list = setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/20.RData"))

# set guard track colors to transparent

tracks_guards[,"track_color"] <- "#ff000000"

# set file name and track name

tracks_ah[,"track_name"] <- tracks_ah[,"team"]
tracks_ah[,"file_name"] <- paste0(dir,"kml/", tracks_ah[,"id"], ".kml")

tracks_guards[,"track_name"] <- "G"
tracks_guards[,"file_name"] <- paste0(
  dir,"kml/G_", tracks_guards[,"id"], ".kml"
)

# export tracks

export_kml(tracks_ah)
export_kml(tracks_guards)

save.image(paste0(dir, "/rdata/21.RData"))
