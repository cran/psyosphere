
# We advice you to read the information about the demos first.

?psyosphere::about_demos

# 00 Prepare R -----------------------------------------------------------------

# Settings

demo_dir <- tempdir()
# demo_dir <- getwd() # Uncomment this to run demo in working directory

# Reset Environment

dir <- paste0(demo_dir,"/leadership2/")

rm(list=setdiff(ls(), "dir"))
library(psyosphere)

# Check directories
dir.create(dir, showWarnings = FALSE)
dir.create(paste0(dir,"rdata"), showWarnings = FALSE)
dir.create(paste0(dir,"plots"), showWarnings = FALSE)
dir.create(paste0(dir,"gps_files"), showWarnings = FALSE)

# 01 store GPX files in data frame ---------------------------------------------

# Download and unzip files
url <- "https://analyse-gps.com/downloads/leadership2.zip"
download.file(url = url, destfile = paste0(dir, "gps_files.zip"))
unzip(paste0(dir, "gps_files.zip"), exdir = paste0(dir, "gps_files"))
remove(url)

# Save GPS files with participant movement as dataframe
tracks <- dir_get_gpx(paste0(dir, "gps_files"))

save.image(paste0(dir, "rdata/01.RData"))

# 02 add information from id file to data frame --------------------------------

# The CSV stores some data about each participants that will be added to the
# data frame.

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/01.RData"))

tracks <- dir_add_csv(tracks, paste0(dir, "gps_files/ids.csv"))

# Remove partcipants that are excluded
tracks <- tracks[ tracks[,c("include")] == 1 ,]

# Get first descriptives
begin_des <- des_summary(tracks)

save.image(paste0(dir, "rdata/02.RData"))

# 03 clean-up data -------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/02.RData"))

# Remove time duplicates
tracks <- average_duplicates(tracks)

# Mark time interval gaps between coordinates
tracks <- t_time_difference(tracks)
tracks <- mark_time_gaps(tracks)

save.image(paste0(dir, "rdata/03.RData"))

# 04 add speed -----------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/03.RData"))

# Add speed
tracks <- t_speed(tracks)

# Mark impossible speed as gaps
tracks <- mark_speed_gaps(tracks,40)

save.image(paste0(dir, "rdata/04.RData"))

# 05 save all plots but do not display them ------------------------------------

# The plots are generated to give you an idea how the data look like. You can
# check the plots in the plots folder in this directory.

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/04.RData"))

plot_tracks(
  tracks, single = FALSE, save_dir = paste0(dir, "plots"),
  cgaps = c("time_gap", "speed_gap")
)

save.image(paste0(dir, "rdata/05.RData"))

# 06 add bearing ---------------------------------------------------------------

# This function will add the bearing between each point of a track.

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/05.RData"))

tracks <- t_bearing(tracks)

save.image(paste0(dir, "rdata/06.RData"))

# 07 add distance --------------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/06.RData"))

tracks <- t_distance(tracks)

save.image(paste0(dir, "rdata/07.RData"))

# 08 add average distance to team mates ----------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/07.RData"))

tracks <- distance_peers(tracks, cpeer = "team")

save.image(paste0(dir, "rdata/08.RData"))

# 09 add average distance to not leader team mates -----------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/08.RData"))

follower_tracks <- tracks[ tracks[,c("leader")] != 1  ,]
follower_tracks <- distance_peers(
  follower_tracks, cpeer = "team", cname = "non_leader_mean_dis"
)

leader_tracks <- tracks[ tracks[,c("leader")] == 1  ,]
leader_tracks[c("non_leader_mean_dis")] <- leader_tracks[c("average_dis")]

tracks <- rbind(follower_tracks, leader_tracks)
tracks <- tracks[order(tracks$id, tracks$p_id),]

remove(follower_tracks, leader_tracks)

save.image(paste0(dir, "rdata/09.RData"))

# 10 create summary data frame -------------------------------------------------
rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/09.RData"))

cgaps <- c("time_gap","speed_gap")

summary <- des_summary(tracks, cgaps = cgaps)
summary <- des_first(tracks, "leader", cname = "leader", des_df = summary)

summary <- des_sd(
  tracks, ctarget = "speed", cweight = "time_difference", cgaps = cgaps,
  cname = "sd_kmh", des_df = summary
)

summary <- des_mean(
  tracks, ctarget = "average_dis", cweight = "time_difference",
  cgaps = cgaps, cname = "mean_team_distance", des_df = summary
)

summary <- des_mean(
  tracks, ctarget = "non_leader_mean_dis", cweight = "time_difference",
  cgaps = cgaps, cname = "mean_non_leader_dis", des_df = summary
)

save.image(paste0(dir, "rdata/10.RData"))

# 11 export to excel -----------------------------------------------------------

rm(list=setdiff(ls(), "dir"))
load(paste0(dir, "rdata/10.RData"))

library(xlsx)
xlsx::write.xlsx(summary, paste0(dir, "leadership2.xlsx"))
