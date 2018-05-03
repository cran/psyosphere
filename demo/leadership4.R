
# We advice you to read the information about the demos first.

?psyosphere::about_demos

# 01 Prepare R -----------------------------------------------------------------

# Settings

demo_dir <- tempdir()
# demo_dir <- getwd() # Uncomment this to run demo in working directory

# Reset Environment

dir <- paste0(demo_dir,"/leadership4/")

rm(list=setdiff(ls(), "dir"))
library(psyosphere)

# Check directories
dir.create(dir, showWarnings = FALSE)
dir.create(paste0(dir,"rdata"), showWarnings = FALSE)
dir.create(paste0(dir,"plots"), showWarnings = FALSE)
dir.create(paste0(dir,"gps_files"), showWarnings = FALSE)

# 02 Download and unzip files --------------------------------------------------

url <- "https://analyse-gps.com/downloads/leadership4.zip"
download.file(url = url, destfile = paste0(dir, "/gps_files.zip"))
unzip(paste0(dir, "/gps_files.zip"), exdir = paste0(dir, "/gps_files"))
remove(url)

# 03 Store GPX files in data frame ---------------------------------------------

rm(list=setdiff(ls(), "dir"))

tracks <- dir_get_gpx(paste0(dir, "/gps_files"))

save.image(paste0(dir, "/rdata/03.RData"))

# 04 Save all plots ------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/03.RData"))

# The plots are generated to give you an idea how the data look like. You can
# check the plots in the plots folder in this directory.

plot_tracks(
  tracks, single = FALSE, save_dir = paste0(dir, "/plots/all")
)

# For each participant there are two tracks. After creating the plots it was
# possible to decide which tracks are included and which not. Tracks with more
# coordinates and less gaps were preferred. In the "ids.csv" in the gps_files
# directory is noted which files were included and which were excluded.

save.image(paste0(dir, "/rdata/04.RData"))

# 05 add id file info to data frame --------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/04.RData"))

# The CSV stores some data about each participants that will be added to the
# data frame.

tracks <- dir_add_csv(tracks, paste0(dir, "/gps_files/ids.csv"))

save.image(paste0(dir, "/rdata/05.RData"))

# 06 Clean-up data -------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/05.RData"))

# Remove partcipants that are excluded
tracks <- tracks[ tracks[,c("include")] == 1 ,]

# Get first descriptives
begin_des <- des_summary(tracks)

# Remove time duplicates
tracks <- average_duplicates(tracks)

# Mark time interval gaps between coordinates
tracks <- t_time_difference(tracks)
tracks <- mark_time_gaps(tracks)

save.image(paste0(dir, "/rdata/06.RData"))

# 07 Add speed -----------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/06.RData"))

# Add speed
tracks <- t_speed(tracks)

# Mark impossible speed (above 40km/h) as gaps
tracks <- mark_speed_gaps(tracks,40)

save.image(paste0(dir, "/rdata/07.RData"))

# 08 Save all plots but do not display them ------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/07.RData"))

# All plots are created that where included and cleaned up. All gaps are marked
# as red lines. These gaps will be excluded from the further analysis.

plot_tracks(
  tracks, single = FALSE, save_dir = paste0(dir, "/plots"),
  cgaps = c("time_gap", "speed_gap")
)

save.image(paste0(dir, "/rdata/08.RData"))

# 09 Add bearing ---------------------------------------------------------------

# This function will add the bearing between each point of a track.

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/08.RData"))

tracks <- t_bearing(tracks)

save.image(paste0(dir, "/rdata/09.RData"))

# 10 Add distance --------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/09.RData"))

tracks <- t_distance(tracks)

save.image(paste0(dir, "/rdata/10.RData"))

# 11 Add average distance to team mates ----------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/10.RData"))

tracks <- distance_peers(tracks, cpeer = "team")

save.image(paste0(dir, "/rdata/11.RData"))

# 12 Add average distance to formal followers in team --------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/11.RData"))

follower_tracks <- tracks[ tracks[,c("formal_leader")] != 1  ,]
follower_tracks <- distance_peers(
  follower_tracks, cpeer = "team", cname = "formal_follower_mean_dis"
)

leader_tracks <- tracks[ tracks[,c("formal_leader")] == 1  ,]
leader_tracks[c("formal_follower_mean_dis")] <- leader_tracks[c("average_dis")]

tracks <- rbind(follower_tracks, leader_tracks)
tracks <- tracks[order(tracks$id, tracks$p_id),]

remove(follower_tracks, leader_tracks)

save.image(paste0(dir, "/rdata/12.RData"))

# 13 Add average distance to perceived followers in team -----------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/12.RData"))

follower_tracks <- tracks[ tracks[,c("perceived_leader")] != 1  ,]
follower_tracks <- distance_peers(
  follower_tracks, cpeer = "team", cname = "perceived_follower_mean_dis"
)

leader_tracks <- tracks[ tracks[,c("perceived_leader")] == 1  ,]
leader_tracks[c("perceived_follower_mean_dis")] <-
  leader_tracks[c("average_dis")]

tracks <- rbind(follower_tracks, leader_tracks)
tracks <- tracks[order(tracks$id, tracks$p_id),]

remove(follower_tracks, leader_tracks)

save.image(paste0(dir, "/rdata/13.RData"))

# 14 Create summary data frame -------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/13.RData"))

cgaps <- c("time_gap","speed_gap")

summary <- des_summary(tracks, cgaps = cgaps)

summary <- des_first(
  tracks, "formal_leader", cname = "formal_leader", des_df = summary
)

summary <- des_first(
  tracks, "perceived_leader", cname = "perceived_leader", des_df = summary
)

summary <- des_sd(
  tracks, ctarget = "speed", cweight = "time_difference", cgaps = cgaps,
  cname = "sd_kmh", des_df = summary
)

summary <- des_mean(
  tracks, ctarget = "average_dis", cweight = "time_difference",
  cgaps = cgaps, cname = "mean_team_distance", des_df = summary
)

summary <- des_mean(
  tracks, ctarget = "formal_follower_mean_dis", cweight = "time_difference",
  cgaps = cgaps, cname = "mean_formal_follower_dis", des_df = summary
)

summary <- des_mean(
  tracks, ctarget = "perceived_follower_mean_dis", cweight = "time_difference",
  cgaps = cgaps, cname = "mean_perceived_follower_dis", des_df = summary
)

summary <- des_first(
  tracks, "comment", cname = "comment", des_df = summary
)

save.image(paste0(dir, "/rdata/14.RData"))

# 15 Export to excel -----------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "/rdata/14.RData"))

library(xlsx)
xlsx::write.xlsx(summary, paste0(dir, "/leadership4.xlsx"))
