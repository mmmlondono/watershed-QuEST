####PACKAGES####
library(tidyverse)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(dplyr)
library(rgdal) # for mapping

####READ IN DATA AND TIDY####
data <- read.csv("data/QuEST-all.csv")

# check data classes 
str(data)

# format date/time
data$date = as.POSIXct(data$Collection.Date, format="%d-%B-%y")

####extract each separate state's data####
NM_data <- data[data$Sub_Project == "New Mexico", ]
AK_data <- data[data$Sub_Project == "Arkansas", ]
NH_data <- data[data$Sub_Project == "QuEST", ]

#Combine data in NM 
#create a new column without the 'd' prefix in Sample.Name
NM_data$Sample.Name_No_D <- sub("^d_", "", NM_data$Sample.Name)

#group by Sample.Name_No_D (without 'd' prefix) and keep only non-NA values
NM_data <- NM_data %>%
  group_by(Sample.Name_No_D) %>%
  summarise_all(~ if (any(!is.na(.))) na.omit(.)[1] else NA)

#Fix columns to match all sites
# Replace "apple" with "mango"
NH_data$Sub_Project <- replace(NH_data$Sub_Project, NH_data$Sub_Project == "QuEST", "New Hampshire")
NH_data$Project <- replace(NH_data$Project, NH_data$Project == "Lamprey", "QuEST")

#using dplyr
NH_data <- NH_data %>% mutate(Sub_Project = ifelse(Sub_Project == "QuEST", "New Hampshire", Sub_Project))
NH_data <- NH_data %>% mutate(Project = ifelse(Project == "Lamprey", "QuEST", Project))

NH_data <- NH_data %>% mutate(Sub_ProjectA = ifelse(Sub_ProjectA == "", "Lamprey", Sub_ProjectA))

#Remove and identify rows to remove (date "03-Mar-24" has no data)
rows_to_remove <- AK_data$Collection.Date == "03-Mar-24"

#Subset the data frame excluding the identified rows
AK_data <- AK_data[!rows_to_remove, ]

#Merging sites again
#Remove column from NM data to match all data frames
NM_data <- NM_data[, !(colnames(NM_data) %in% "Sample.Name_No_D")] 
data <- rbind(AK_data, NH_data, NM_data)

#### describe dataset size and structure ####
head(data)
str(data)

with(data, table(Project, Sub_Project))
with(data, table(Sub_ProjectA, Sub_ProjectB))

### check timesteps by looking and time series of most frequently collected parameters
# add year and day of year for plotting
data$year = lubridate::year(data$date)
data$doy = lubridate::yday(data$date)

# plot DOC
ggplot(data=data, aes(x=doy, y=NPOC..mg.C.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot TDN
ggplot(data=data, aes(x=doy, y=TDN..mg.N.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw() 
# plot NH4
ggplot(data=data, aes(x=doy, y=NH4..ug.N.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw() 
# plot PO4
ggplot(data=data, aes(x=doy, y=PO4..ug.P.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw() 
# plot Cl
ggplot(data=data, aes(x=doy, y=Cl..mg.Cl.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot NO3
ggplot(data=data, aes(x=doy, y=NO3..mg.N.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot SO4
ggplot(data=data, aes(x=doy, y=SO4..mg.S.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot Na
ggplot(data=data, aes(x=doy, y=Na..mg.Na.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot K
ggplot(data=data, aes(x=doy, y=K..mg.K.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot Mg
ggplot(data=data, aes(x=doy, y=Mg..mg.Mg.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# plot Ca
ggplot(data=data, aes(x=doy, y=Ca..mg.Ca.L., color=Sub_Project))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# timesteps are all over the place from year to year and site to site, what we would call "irregular"
# timesteps are not sub-daily, at most frequent are approximately monthly

#### check distributions ####

# I'm only going to check the distributions of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data

#DOC
qqPlot(data$NPOC..mg.C.L.); shapiro.test(data$NPOC..mg.C.L.) # not normal
qqPlot(data$NPOC..mg.C.L.[data$Sub_Project=='New Mexico']); shapiro.test(data$NPOC..mg.C.L.[data$Sub_Project=='New Mexico']) #not normal 
qqPlot(data$NPOC..mg.C.L.[data$Sub_Project=='Arkansas']); shapiro.test(data$NPOC..mg.C.L.[data$Sub_Project=='Arkansas']) #not normal
qqPlot(data$NPOC..mg.C.L.[data$Sub_Project=='QuEST']); shapiro.test(data$NPOC..mg.C.L.[data$Sub_Project=='QuEST']) #normal?

#TDN
qqPlot(data$TDN..mg.N.L.); shapiro.test(data$TDN..mg.N.L.) # not normal
qqPlot(data$TDN..mg.N.L.[data$Sub_Project=='New Mexico']); shapiro.test(data$TDN..mg.N.L.[data$Sub_Project=='New Mexico']) #not normal
qqPlot(data$TDN..mg.N.L.[data$Sub_Project=='Arkansas']); shapiro.test(data$TDN..mg.N.L.[data$Sub_Project=='Arkansas']) #not normal
qqPlot(data$TDN..mg.N.L.[data$Sub_Project=='QuEST']); shapiro.test(data$TDN..mg.N.L.[data$Sub_Project=='QuEST']) #normal

#NH4
qqPlot(data$NH4..ug.N.L.); shapiro.test(data$NH4..ug.N.L.) #not normal
qqPlot(data$NH4..ug.N.L.[data$Sub_Project=='New Mexico']); shapiro.test(data$NH4..ug.N.L.[data$Sub_Project=='New Mexico']) #normal?
qqPlot(data$NH4..ug.N.L.[data$Sub_Project=='Arkansas']); shapiro.test(data$NH4..ug.N.L.[data$Sub_Project=='Arkansas']) #not normal 
qqPlot(data$NH4..ug.N.L.[data$Sub_Project=='QuEST']); shapiro.test(data$NH4..ug.N.L.[data$Sub_Project=='QuEST']) #not normal 

#PO4
qqPlot(data$PO4..ug.P.L.); shapiro.test(data$PO4..ug.P.L.) #not normal
qqPlot(data$PO4..ug.P.L.[data$Sub_Project=='New Mexico']); shapiro.test(data$PO4..ug.P.L.[data$Sub_Project=='New Mexico']) #not normal
qqPlot(data$PO4..ug.P.L.[data$Sub_Project=='Arkansas']); shapiro.test(data$PO4..ug.P.L.[data$Sub_Project=='Arkansas']) #not normal 
qqPlot(data$PO4..ug.P.L.[data$Sub_Project=='QuEST']); shapiro.test(data$PO4..ug.P.L.[data$Sub_Project=='QuEST']) #normal? 

### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal?
# can I justify removing outliers based on my knowledge of the data?
# if data is still non-normal, what distribution is it?
#DOC
#NM
summary(data$NPOC..mg.C.L.[data$Sub_Project=='New Mexico'])
hist(data$NPOC..mg.C.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$NPOC..mg.C.L.), ]
plot(density(temp_filtered$NPOC..mg.C.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$NPOC..mg.C.L.[data$Sub_Project=='Arkansas'])
hist(data$NPOC..mg.C.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$NPOC..mg.C.L.), ]
plot(density(temp_filtered$NPOC..mg.C.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$NPOC..mg.C.L.[data$Sub_Project=='New Hampshire'])
hist(data$NPOC..mg.C.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$NPOC..mg.C.L.), ]
plot(density(temp_filtered$NPOC..mg.C.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot TDN
#NM
summary(data$TDN..mg.N.L.[data$Sub_Project=='New Mexico'])
hist(data$TDN..mg.N.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$TDN..mg.N.L.), ]
plot(density(temp_filtered$TDN..mg.N.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$TDN..mg.N.L.[data$Sub_Project=='Arkansas'])
hist(data$TDN..mg.N.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$TDN..mg.N.L.), ]
plot(density(temp_filtered$TDN..mg.N.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$TDN..mg.N.L.[data$Sub_Project=='New Hampshire'])
hist(data$TDN..mg.N.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$TDN..mg.N.L.), ]
plot(density(temp_filtered$TDN..mg.N.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot NH4
#NM
summary(data$NH4..ug.N.L.[data$Sub_Project=='New Mexico'])
hist(data$NH4..ug.N.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$NH4..ug.N.L.), ]
plot(density(temp_filtered$NH4..ug.N.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$NH4..ug.N.L.[data$Sub_Project=='Arkansas'])
hist(data$NH4..ug.N.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$NH4..ug.N.L.), ]
plot(density(temp_filtered$NH4..ug.N.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$NH4..ug.N.L.[data$Sub_Project=='New Hampshire'])
hist(data$NH4..ug.N.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$NH4..ug.N.L.), ]
plot(density(temp_filtered$NH4..ug.N.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot PO4
#NM
summary(data$PO4..ug.P.L.[data$Sub_Project=='New Mexico'])
hist(data$PO4..ug.P.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$PO4..ug.P.L.), ]
plot(density(temp_filtered$PO4..ug.P.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$PO4..ug.P.L.[data$Sub_Project=='Arkansas'])
hist(data$PO4..ug.P.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$PO4..ug.P.L.), ]
plot(density(temp_filtered$PO4..ug.P.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$PO4..ug.P.L.[data$Sub_Project=='New Hampshire'])
hist(data$PO4..ug.P.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$PO4..ug.P.L.), ]
plot(density(temp_filtered$PO4..ug.P.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot Cl
#NM
summary(data$Cl..mg.Cl.L.[data$Sub_Project=='New Mexico'])
hist(data$Cl..mg.Cl.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$Cl..mg.Cl.L.), ]
plot(density(temp_filtered$Cl..mg.Cl.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$Cl..mg.Cl.L.[data$Sub_Project=='Arkansas'])
hist(data$Cl..mg.Cl.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$Cl..mg.Cl.L.), ]
plot(density(temp_filtered$Cl..mg.Cl.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$Cl..mg.Cl.L.[data$Sub_Project=='New Hampshire'])
hist(data$Cl..mg.Cl.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$Cl..mg.Cl.L.), ]
plot(density(temp_filtered$Cl..mg.Cl.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot NO3
#NM
summary(data$NO3..mg.N.L.[data$Sub_Project=='New Mexico'])
hist(data$NO3..mg.N.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$NO3..mg.N.L.), ]
plot(density(temp_filtered$NO3..mg.N.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$NO3..mg.N.L.[data$Sub_Project=='Arkansas'])
hist(data$NO3..mg.N.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$NO3..mg.N.L.), ]
plot(density(temp_filtered$NO3..mg.N.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$NO3..mg.N.L.[data$Sub_Project=='New Hampshire'])
hist(data$NO3..mg.N.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$NO3..mg.N.L.), ]
plot(density(temp_filtered$NO3..mg.N.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot SO4
#NM
summary(data$SO4..mg.S.L.[data$Sub_Project=='New Mexico'])
hist(data$SO4..mg.S.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$SO4..mg.S.L.), ]
plot(density(temp_filtered$SO4..mg.S.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$SO4..mg.S.L.[data$Sub_Project=='Arkansas'])
hist(data$SO4..mg.S.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$SO4..mg.S.L.), ]
plot(density(temp_filtered$SO4..mg.S.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$SO4..mg.S.L.[data$Sub_Project=='New Hampshire'])
hist(data$SO4..mg.S.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$SO4..mg.S.L.), ]
plot(density(temp_filtered$SO4..mg.S.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot Na
#NM
summary(data$Na..mg.Na.L.[data$Sub_Project=='New Mexico'])
hist(data$Na..mg.Na.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$Na..mg.Na.L.), ]
plot(density(temp_filtered$Na..mg.Na.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$Na..mg.Na.L.[data$Sub_Project=='Arkansas'])
hist(data$Na..mg.Na.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$Na..mg.Na.L.), ]
plot(density(temp_filtered$Na..mg.Na.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$Na..mg.Na.L.[data$Sub_Project=='New Hampshire'])
hist(data$Na..mg.Na.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$Na..mg.Na.L.), ]
plot(density(temp_filtered$Na..mg.Na.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot K
#NM
summary(data$K..mg.K.L.[data$Sub_Project=='New Mexico'])
hist(data$K..mg.K.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$K..mg.K.L.), ]
plot(density(temp_filtered$K..mg.K.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$K..mg.K.L.[data$Sub_Project=='Arkansas'])
hist(data$K..mg.K.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$K..mg.K.L.), ]
plot(density(temp_filtered$K..mg.K.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$K..mg.K.L.[data$Sub_Project=='New Hampshire'])
hist(data$K..mg.K.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$K..mg.K.L.), ]
plot(density(temp_filtered$K..mg.K.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot Mg
#NM
summary(data$Mg..mg.Mg.L.[data$Sub_Project=='New Mexico'])
hist(data$Mg..mg.Mg.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$Mg..mg.Mg.L.), ]
plot(density(temp_filtered$Mg..mg.Mg.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$Mg..mg.Mg.L.[data$Sub_Project=='Arkansas'])
hist(data$Mg..mg.Mg.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$Mg..mg.Mg.L.), ]
plot(density(temp_filtered$Mg..mg.Mg.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$Mg..mg.Mg.L.[data$Sub_Project=='New Hampshire'])
hist(data$Mg..mg.Mg.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$Mg..mg.Mg.L.), ]
plot(density(temp_filtered$Mg..mg.Mg.L.[temp_filtered$Sub_Project=='New Hampshire']))
# plot Ca
#NM
summary(data$Ca..mg.Ca.L.[data$Sub_Project=='New Mexico'])
hist(data$Ca..mg.Ca.L.[data$Sub_Project=='New Mexico'])
temp_filtered <- data[!is.na(data$Ca..mg.Ca.L.), ]
plot(density(temp_filtered$Ca..mg.Ca.L.[temp_filtered$Sub_Project=='New Mexico']))
#AK
summary(data$Ca..mg.Ca.L.[data$Sub_Project=='Arkansas'])
hist(data$Ca..mg.Ca.L.[data$Sub_Project=='Arkansas'])
temp_filtered <- data[!is.na(data$Ca..mg.Ca.L.), ]
plot(density(temp_filtered$Ca..mg.Ca.L.[temp_filtered$Sub_Project=='Arkansas']))
#NH
summary(data$Ca..mg.Ca.L.[data$Sub_Project=='New Hampshire'])
hist(data$Ca..mg.Ca.L.[data$Sub_Project=='New Hampshire'])
temp_filtered <- data[!is.na(data$Ca..mg.Ca.L.), ]
plot(density(temp_filtered$Ca..mg.Ca.L.[temp_filtered$Sub_Project=='New Hampshire']))

# Define project names
projects <- c("New Mexico", "Arkansas", "New Hampshire")

# Loop through projects
for (project in projects) {
  # Filter data for current project
  project_data <- data[data$Sub_Project == project, ]
  
  # Analyze all desired columns (replace with your column names)
  #cols <- c("NPOC..mg.C.L.", "TDN..mg.N.L.", "NH4..ug.N.L.")  
  cols <- c(7:17) 
  
  for (col in cols) {
    # Check for at least one missing value
    if (any(!complete.cases(project_data[, col]))) {
      # Remove rows with missing values (alternative: impute if necessary)
      project_data_filtered <- project_data[!is.na(project_data[, col]), ]
    } else {
      project_data_filtered <- project_data
    }
    
    # Perform summary, histogram, and density plot
    summary(project_data_filtered[, col])
    hist(project_data_filtered[, col])
    plot(density(project_data_filtered[, col]))
  }
}
