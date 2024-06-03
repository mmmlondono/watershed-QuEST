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
         "lubridate","tidyverse", "googledrive", "changepoint"), require, character.only=T)

#############################
## Import & Visualize Data ##
#############################

# 1) Import and compile raw cond data
#### load data from Google drive ####
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/folders/1CVd8EdzGom05rfwbMdOTDaRyVH6ALzFA")
# List all CSV files in the folder
Saltslugs_csvs <- googledrive::drive_ls(path = Saltslugs, type = "csv")

googledrive::drive_download(file = YSIProQuatro_csvs$id[YSIProQuatro_csvs$name=="ProQ_Logdata_20240524_USF5.csv"], 
                            path = "googledrive/ProQ_Logdata_20240524_USF5.csv",
                            overwrite = T)
log_20240524_USF5 = read.csv("googledrive/ProQ_Logdata_20240524_USF5.csv")
# Check the contents of the list
str(csv_list)

log_20240524_USF5 = read.csv("googledrive/ProQ_Logdata_20240524_USF5.csv")

# Combine date and time columns with a separator
log_20240524_USF5$DateTime <- paste(log_20240524_USF5$Date, log_20240524_USF5$Time, sep = " ")

log_20240524_USF5$DateTime <- as.POSIXct(log_20240524_USF5$DateTime, format = "%y/%m/%d %H:%M:%S")  # Convert the DateTime column to POSIXct

# Check
sapply(log_20240524_USF5, class) ## want date to be POSIXct, and Cond and TempC to be numeric
head(log_20240524_USF5)


##############################################################
## Plot curves
##############################################################

# Visualize
ggplot(log_20240524_USF5, aes(DateTime, SPC.uS.cm.))+
  geom_point()

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
USF5 <- mean(subset(log_20240524_USF5, DateTime >= as.POSIXct("2024-05-24 15:34:46") & DateTime <= as.POSIXct("2024-05-24 15:37:40"))$SPC.uS.cm.) 

cpt <- cpt.mean(log_20240524_USF5$SPC.uS.cm., penalty='SIC', method='PELT') 

# Get the location of the changepoint
changepoint <- cpts(cpt)[1]

# Calculate the average of the data before the changepoint
average_before_change <- mean(log_20240524_USF5$SPC.uS.cm.[1:(changepoint - 1)])

# Calculate the average of the data before the changepoint, ignoring the first n measurements
n = 15
average_before_change <- mean(log_20240524_USF5$SPC.uS.cm.[(n + 1):(changepoint - 1)])

# Get the DateTime corresponding to the changepoint
changepoint_datetime <- log_20240524_USF5$DateTime[changepoint]

# Plot to view where the cut is to start measuring
ggplot(log_20240524_USF5, aes(x = DateTime, y = SPC.uS.cm.)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = changepoint_datetime, color = "red", linetype = "dashed") +
  labs(title = "Data with Changepoint Highlighted",
       x = "Index",
       y = "Value") +
  theme_minimal()

## (2) Estimate conductivity slug based on mass of Cl added
## 1 g salt in 1 L of water gives cond=2100 uS / cm
USF5_Cond_mass <- 2100*1012.95 # Oak @ Lolomai: 3.00 kg


## Calculate Q!
## Units = L/sec
Q_USF5 <- Qint(as.numeric(log_20240524_USF5$DateTime), log_20240524_USF5$SPC.uS.cm., USF5, USF5_Cond_mass)


## Summary
## USF5 20204-08-24 Q = 812.2298 L/sec (0.81 cms)

#######################
## Estimate Velocity ##
#######################

inj_time_USF5 <- as.POSIXct("2024-05-24 14:30:00")

## Velocity = distance in meters/time in seconds

## Distance upstream
USF5_dist_upstream <- 40 # Oak @ Lolomai: 40 m upstream

v_USF5 <- USF5_dist_upstream /(as.numeric(log_20240524_USF5[which.max(log_20240524_USF5$SPC.uS.cm.),]$DateTime - inj_time_USF5)*60)
#0.5852231 m/s


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
z_USF5<- (Q_USF5/1000)/(2*v_USF5)
#0.4783656