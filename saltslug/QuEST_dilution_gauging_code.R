##==============================================================================
## Project: QuEST
## Script to estimate discharge using dilution gauging
## Code author: J.R. Blaszczak
##==============================================================================

## Explanation based off: Moore, R.D. 2005. Salt Injection Using Salt in Solution.
## "Streamline Watershed Management Bulletin". Volume 8 (2)

## Two assumptions:
## (1) All the injected mass is recovered downstream
## (2) Tracer is completely mixed in the channel

## The time required for the peak of the wave to move past an observation point 
## depends inversely on the mean velocity of streamflow
## The duration of the salt wave depends on the amount of longitudinal dispersion,
## which depends on how variable velocities are across the stream

## At any time (t) while tracer is passing in the salt wave,
## the discharge of the tracer solution is: q(t) = Q*RC(t)
## Where Q is stream discharge (L/s), and RC(t) is the relative
## concentration of the tracer solution (L/L) at t

## Integrate over the salt wave to get discharge (Q):
## Q = V/integral(RC(t))dt

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#############################
## Import & Visualize Data ##
#############################

# 1) Import and compile raw cond data

## If multiple sites and data from HOBO U24 loggers:
folder_path <- "/Users/manuelalondono/Documents/watershed-QuEST/saltslug" ## set the working directory to the folder with your data

# List all CSV files in the specified folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
# The script below pull in each individual file and creates a composite file
# Function to process each file
process_file <- function(filename) {
  d <- read.csv(filename, skip = 1, header = TRUE, row.names = NULL)  # Read the CSV file, skipping the first line, and ensure no row names
  d$file <- filename  # Add a column to the data frame with the filename
  d <- d[, c(2:5)]  # Select columns 2 to 5
  colnames(d) <- c("DateTime", "Cond", "TempC", "File")  # Rename the columns
  d$DateTime <- as.POSIXct(as.character(d$DateTime), format = "%m/%d/%y %H:%M:%S")  # Convert the DateTime column to POSIXct
  return(d)  # Return the modified data frame
}

# Apply the function to each file and combine the results into a single data frame
raw_dat <- ldply(file_list, process_file)

# Removing the specified part of the path
raw_dat$File <- sub("^/Users/manuelalondono/Documents/watershed-QuEST/saltslug/", "", raw_dat$File)

# Check
sapply(raw_dat, class) ## want date to be POSIXct, and Cond and TempC to be numeric
head(raw_dat)


##############################################################
## Manually subset time frame based on plot and notes
##############################################################
# Split composite file into list by the different file names
dat_l <- split(raw_dat, raw_dat$File)

# Visualize
ggplot(dat_l[[4]], aes(DateTime, Cond))+
  geom_point()+
  labs(title = dat_l[[4]]$File[1])

#Oak Creek Lolomai
dat_Lolomai <- subset(dat_l$`2018_09_28_Cond_Pink_LolomaiOakCreek.csv`,
                      DateTime >= as.POSIXct("2018-09-28 07:15:00") & DateTime <= as.POSIXct("2018-09-28 09:00:00")) # Lolomai
ggplot(dat_Lolomai, aes(DateTime, Cond))+geom_point()
#Oak Creek Willow
dat_Willow <- subset(dat_l$`2018_09_28_Cond_Yellow_WillowPtOakCreek.csv`,
                     DateTime >= as.POSIXct("2018-09-28 09:00:00") & DateTime <= as.POSIXct("2018-09-28 10:14:00")) # Willow
ggplot(dat_Willow, aes(DateTime, Cond))+geom_point()
#Blaine
dat_Blaine <- subset(dat_l$`2018_08_28_Cond_Yellow_Blaine.csv`,
                     DateTime >= as.POSIXct("2018-08-23 07:00:00") & DateTime <= as.POSIXct("2018-08-23 9:00:00"))[1:100,] # Blaine
ggplot(dat_Blaine, aes(DateTime, Cond))+geom_point()
#Beaver
dat_Beaver <- subset(dat_l$`2018_09_25_Cond_Pink_Beaver.csv`,
                     DateTime >= as.POSIXct("2018-08-31 07:00:00") & DateTime <= as.POSIXct("2018-08-31 11:00:00"))[1:200,] # Beaver
ggplot(dat_Beaver, aes(DateTime, Cond))+geom_point()


################
## Estimate Q ##
################
## Equation
Qint<-function(time,cond, bkg, condmass){
  condcorr<-cond-bkg
  
  ##below routine integrates
  ydiff<- condcorr[-1]+ condcorr[-length(condcorr)]
  condint<-sum(diff(time)*ydiff/2)
  
  Q<-condmass/condint
  Q
  
}

## (1) Determine the background conductivity
## Select area before or after the salt wave which is constant
## for at least 30 minutes and take the average

bg_cond_Lolomai <- mean(subset(dat_Lolomai, DateTime >= as.POSIXct("2018-09-28 08:30:00") & DateTime <= as.POSIXct("2018-09-28 09:00:00"))$Cond) #Lolomai
bg_cond_Willow <- mean(subset(dat_Willow, DateTime >= as.POSIXct("2018-09-28 09:00:00") & DateTime <= as.POSIXct("2018-09-28 09:30:00"))$Cond) #Willow
bg_cond_Blaine <- mean(subset(dat_Blaine, DateTime >= as.POSIXct("2018-08-23 07:00:00") & DateTime <= as.POSIXct("2018-08-23 07:30:00"))$Cond) #Blaine
bg_cond_Beaver <- mean(subset(dat_Beaver, DateTime >= as.POSIXct("2018-08-31 07:30:00") & DateTime <= as.POSIXct("2018-08-31 08:00:00"))$Cond) #Beaver

## (2) Estimate conductivity slug based on mass of Cl added
## 1 g salt in 1 L of water gives cond=2100 uS / cm
Oak_Lolomai_Cond_mass <- 2100*3000 # Oak @ Lolomai: 3.00 kg
Oak_Willow_Cond_mass <- 2100*7210 # Oak @ Willow: 7.21 kg
Blaine_Cond_mass <- 2100*1209 # Blaine: 1209 g
Beaver_Cond_mass <- 2100*2084 # Beaver: 2084 g

## Calculate Q!
## Units = L/sec
Q_Lolomai <- Qint(as.numeric(dat_Lolomai$DateTime), dat_Lolomai$Cond, bg_cond_Lolomai, Oak_Lolomai_Cond_mass)
Q_Willow <- Qint(as.numeric(dat_Willow$DateTime), dat_Willow$Cond, bg_cond_Willow, Oak_Willow_Cond_mass)
Q_Blaine <- Qint(as.numeric(dat_Blaine$DateTime), dat_Blaine$Cond, bg_cond_Blaine, Blaine_Cond_mass)
Q_Beaver <- Qint(as.numeric(dat_Beaver$DateTime), dat_Beaver$Cond, bg_cond_Beaver, Beaver_Cond_mass)

## Summary
## Oak @ Lolomai: Q = 491.2901 L/sec (0.49 cms)
## Oak @ Willow: Q = 724.7055 L/sec (0.72 cms)
## Blaine: Q = 33.9 L/sec (0.034 cms)
## Beaver: Q = 107.102 L/sec (0.107 cms)

#######################
## Estimate Velocity ##
#######################

inj_time_Lolomai <- as.POSIXct("2018-09-28 07:23:00") #Lolomai
inj_time_Willow <- as.POSIXct("2018-09-28 09:34:00") #Willow
inj_time_Blaine <- as.POSIXct("2018-08-23 07:23:00") #Blaine
inj_time_Beaver <- as.POSIXct("2018-08-31 07:23:00") # Beaver

## Velocity = distance in meters/time in seconds

## Distance upstream
Oak_Lolomai_dist_upstream <- 107.5 # Oak @ Lolomai: 107.5 m upstream
Oak_Willow_dist_upstream <- 150 # Oak @ Willow: 150 m upstream
Blaine_dist_upstream <- 50 # Blaine: 50 m upstream
Beaver_dist_upstream <- 217 # Beaver: 217 m upstream

v_Lolomai <- Oak_Lolomai_dist_upstream/(as.numeric(dat_Lolomai[which.max(dat_Lolomai$Cond),]$DateTime - inj_time_Lolomai)*60)
v_Willow <- Oak_Willow_dist_upstream/(as.numeric(dat_Willow[which.max(dat_Willow$Cond),]$DateTime - inj_time_Willow)*60)
v_Blaine <- Blaine_dist_upstream/(as.numeric(dat_Blaine[which.max(dat_Blaine$Cond),]$DateTime - inj_time_Blaine)*60)
v_Beaver <- Beaver_dist_upstream/(as.numeric(dat_Beaver[which.max(dat_Beaver$Cond),]$DateTime - inj_time_Beaver)*60)


## Summary
## Oak @ Lolomai: 0.346 m/s
## Oak @ Willow: 0.199 m/s
## Blaine: 0.069 m/s
## Beaver: 0.066 m/s

#############################
## Estimate mean depth (z) ##
#############################
## effective depth (z) can be estimated using the following equation
##
## z = Q/(w*v)
## where z is effective depth (m)
## Q is discharge (m^3/sec)
## w is average width (m)
## v is velocity (m/sec)

## Enter average width measurement in m
# Oak @ Lolomai: 8.6
# Oak @ Willow: 11.8
# Blaine: 2.05
# Beaver: 8.92  (from upstream of cattle crossing -- need better)

## Calculate effective depth
z_Lolomai <- (Q_Lolomai/1000)/(8.6*v_Lolomai)
z_Willow <- (Q_Willow/1000)/(11.8*v_Willow)
z_Blaine <- (Q_Blaine/1000)/(2.05*v_Blaine)
z_Beaver <- (Q_Beaver/1000)/(8.92*v_Beaver)

## Summary
## Oak @ Lolomai: 0.17 m
## Oak @ Willow: 0.31 m
## Blaine: 0.24 m
## Beaver: 0.18 m (based on estimated width)









