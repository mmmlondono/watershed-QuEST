# edited from MD Madhusudan, mdm@ncf-india.org, Nature Conservation Foundation, 2016-04-10
# Code to import waypoints from multiple GPX files in a folder,
# extract coordinates from each file into a single object, and 
# export to CSV or some other format


#packages
library(tmaptools)
library(dplyr)

# Initialize empty data frame
wpfull <- NULL

# - Select first file from the list and import data into R object
wplist <- read_GPX("data_geo/wp.gpx")
# extract latitude, longituDe, elevation, time, name and comments and apppend to R dataframe
wpdf<- wplist$waypoints
# append dataframe from last index to a full waypoint object
wpfull <- bind_rows(wpfull, wpdf)

####If you have to do it for multiple files at a time####

# - List all filenames in folder starting with "Waypoint"
# Works for waypoint files with names like "Waypoints_22-FEB-16.gpx"
files <- list.files(pattern = "\\bWaypoints_")

# Initialise empty data frame
wpfull <- NULL

for (i in 1:length(files)) {
  # - Select first file from the list and import data into R object
  wplist <- readGPX(files[i], way=T)
  # extract latitude, longituDe, elevation, time, name and comments and apppend to R dataframe
  wpdf<- wplist$waypoints
  # append dataframe from last index to a full waypoint object
  wpfull <- bind_rows(wpfull, wpdf)
}

# export object with all waypoints to csv file
write.csv(wpfull, "data_geo/finalwp.csv")
