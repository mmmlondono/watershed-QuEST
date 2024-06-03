##==============================================================================
## Project: QuEST
## Script to estimate discharge using dilution gauging for multiple files at a time
## 
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
         "lubridate","tidyverse", "googledrive"), require, character.only=T)

#############################
## Import & Visualize Data ##
#############################
library("googledrive")

# 1) Import and compile raw cond data
#### Load data from Google drive ####
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/folders/1CVd8EdzGom05rfwbMdOTDaRyVH6ALzFA")
# List all CSV files in the folder
Saltslugs_csvs <- googledrive::drive_ls(path = Saltslugs, type = "csv")

# Create empty list to store data frames
csv_list <- list()

# Loop over each file in the `Saltslugs_csvs` data frame
for (i in seq_along(Saltslugs_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", Saltslugs_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = Saltslugs_csvs$id[i],
    path = local_path,
    overwrite = T
    )
  # Read the CSV file and add it to the list
  csv_list[[Saltslugs_csvs$name[i]]] <- read.csv(local_path)
}

# Check the contents of the list
str(csv_list)

#### Combine Date and Time into one column #### 
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%y/%m/%d %H:%M:%S")
  # Update the data frame in the list
  csv_list[[i]] <- df
}

#### Check ####
#this does not work yet
for (i in seq_along(csv_list)) {
     print(class(csv_list[[i]][[i]]))
}

class(csv_list[["ProQ_Logdata_20240524_USF3.csv"]][["Temp.C."]])

## We want date to be POSIXct, and Cond and TempC to be numeric


##############################################################
## Plot curves
##############################################################

# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
  geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  ggsave(paste0("saltslug_figs/plot_", Saltslugs_csvs$name[i], ".png"))
}


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
## you want to calculate the average background conductivity from the measurement
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  
  # Get the changepoint from the SPC
  cpt <- cpt.mean(df$SPC.uS.cm., penalty='SIC', method='PELT') 
  # Get the location of the changepoint
  changepoint <- cpts(cpt)[1]
  # Calculate the average of the data before the changepoint, ignoring the first n measurements
  n = 15
  average_before_change[i] <- mean(df$SPC.uS.cm.[(n + 1):(changepoint - 1)])
  # Get the DateTime corresponding to the changepoint
  changepoint_datetime[i] <- df$DateTime[changepoint]

}

# Visualize where the cut is to start measuring
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(csv_list$name[i])) +
    geom_vline(xintercept = changepoint_datetime, color = "red", linetype = "dashed") +
  ggsave(paste0("saltslug_figs/changepoint_", csv_list$name[i], ".png"))
}


## (2) Estimate conductivity slug based on mass of Cl added
## 1 g salt in 1 L of water gives cond=2100 uS / cm
#multiply 2100 times the ammount of salt added to each site

# Create a new column by multiplying the 'salt' column by 2100
salt$Cond_mass <- salt$salt * 2100

# Add newly calculated values to data frames
# Function to combine info
combine_info <- function(df, info) {
  merged_df <- merge(df, info, by = "DataID", all.x = TRUE)
  return(merged_df)
}

csv_list_combined <- lapply(csv_list, combine_info, info = salt)

## Calculate Q!
## Units = L/sec
#example of how it looks for one saltslug
#Q_USF5 <- Qint(as.numeric(log_20240524_USF5$DateTime), log_20240524_USF5$COND.uS.cm., USF5, USF5_Cond_mass)

# Create empty df to store Q data 
Q_list <- data.frame(DataID = c(), 
                     Q = c())

# Loop through each data frame in the list
for (i in seq_along(csv_list_combined)) {
  # Access the current data frame
  df <- csv_list_combined[[i]]
  # Combine Date and Time columns into a new DateTime column
  df$Q[1] <- Qint(as.numeric(df$DateTime), df$SPC.uS.cm., df$spc_base[[1]], df$Cond_mass[[1]])

  csv_list_combined[[i]] <-  df
}

Q_Lolomai <- Qint(as.numeric(dat_Lolomai$DateTime), dat_Lolomai$Cond, bg_cond_Lolomai, Oak_Lolomai_Cond_mass)

# Update the data frame in the list
csv_list[[i]] <- df
Summary
## USF5 20204-08-24 Q = 812.2298 L/sec (0.81 cms)

#######################
## Estimate Velocity ##
#######################

inj_time_USF5 <- as.POSIXct("2024-05-24 14:30:00")

## Velocity = distance in meters/time in seconds

## Distance upstream
USF5_dist_upstream <- 40 # Oak @ Lolomai: 40 m upstream

v_USF5 <- USF5_dist_upstream /(as.numeric(log_20240524_USF5[which.max(log_20240524_USF5$COND.uS.cm.),]$DateTime - inj_time_USF5)*60)
#0.5853659 m/s


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
#0.6937796