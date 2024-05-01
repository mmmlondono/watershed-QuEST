####PACKAGES####
install.packages("sf")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

#for mapping
library(maps)
library(mapdata)
library(sf)
library(sp)

#for installing isoWater
library(devtools)
library(rjags)
#devtools::install_github("SPATIAL-Lab/isoWater@*release")
library(isoWater)

#for PCA
library("FactoMineR")
library(factoextra)

#for color palette
library(microViz)
####READ IN DATA####
isodata <- read.csv("data/isodata.csv")
nh <- read.csv("data/nhdata.csv")
webster <- read.csv("data/webstermain.csv")

###PLOTTING REGRESSION####
#regression
h2o <- lm(dD ~ d18O, 
                  data = isodata)
summary(h2o)

#scatterplot
ggplot(isodata, aes(x=d18O, 
                        y = dD)) + geom_point() 
#scatterplot with sample ids
ggplot(isodata, 
       aes(x=d18O, y = dD, 
           label=samplename)) +  geom_text()

#regression plot
fitted <- tibble(isodata$d18O, 
                             fitted(h2o))
colnames(fitted) <- c("d18O", "hat")

ggplot(isodata, aes(x=d18O, y = dD))  + 
  geom_point(color="black")  +  
  geom_line(data = fitted, aes(x=d18O, y=hat)) 

#scatter plot with GMWL
#The bold black line represents the Global Meteoric Water Line (δ2H = δ18O x 8 + 10)
ggplot(isodata, aes(x = d18O, y = dD)) +
  geom_point() +  # Add points for the data
  geom_abline(intercept = 8, slope = 10, linetype = "dashed")  # Add GMWL line

####COMBINING DATASHEETS for PCA####

#Format date/time
#combine date and time into a single character column
webster$datetime <- paste(webster$Date, webster$Time)
webster$datetime = as.POSIXct(webster$datetime, format="%m/%d/%y %H:%M", tz="MST")

#change same column names
colnames(nh)[colnames(nh) == "Sub_ProjectB"] <- "Site"
colnames(nh)[colnames(nh) == "NPOC..mg.C.L."] <- "DOC_mgl"

#define columns to keep from each spreadsheet
names(nh)
#nh <- nh[, c("samplename", "DOC_mgl", "Site")]

names(webster)
#webster <- webster[, c("samplename", "DOC_mgl", "Project", "Site", "ID", "lat","lon")]

#remove rows with NA or empty values in the DOC_mgl column
nh <- nh[!is.na(nh$DOC_mgl) & nh$DOC_mgl != "", ]
webster <- webster[!is.na(webster$DOC_mg), ]

head(webster)
head(nh)
head (isodata)

#merge isodata into webster based on samplename
webster_with_iso <- merge(webster, isodata, by = "samplename", all.x = TRUE)

#merge data
merged_data <- merge(webster_with_iso, nh, by = c("samplename", "DOC_mgl", "Site"), all = TRUE)
head(merged_data)

#new column order
new_col_order <- c(1, 3:6, 2, 7)

#rearrange the columns
merged_data <- merged_data[, new_col_order]

#check the updated data
head(merged_data)

#removing NAs
merged_data <- merged_data[!is.na(merged_data$DOC_mgl) & merged_data$DOC_mgl != "", ]
merged_data <- merged_data[!is.na(merged_data$d18O) & merged_data$d18O != "", ]

####PLOTTING PCA####
#standardization? (optional)
#if not going to standardize
isodoc_pca <- merged_data[, 5:7]  #use data without scaling
res.pca <- PCA(isodoc_pca, scale.unit = TRUE, ncp = 5, graph = FALSE)  #perform PCA

nrow(isodoc_pca)

#eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

#plot eigenvalues
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

#variables
var <- get_pca_var(res.pca)

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#plot variables
fviz_pca_var(res.pca, col.var = "black")

fviz_pca_ind(res.pca)

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = merged_data$Project, # color by groups
             palette = c("#00AFBB","#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

####TIME-SERIES OF dD####
webster_date <- read.csv("data/webstermain.csv")
names(webster_date)
webster_date <- webster_date[, c("samplename", "Project", "Site", "ID", "Date")]
str(webster_date)

#change to date format
webster_date$Date <- as.Date(webster_date$Date, format = "%m/%d/%y")

#merge isodata into webster based on samplename
webster_date_iso <- merge(webster_date, isodata, by = "samplename", all.x = TRUE)
webster_date_iso <- webster_date_iso[!is.na(webster_date_iso$d18O), ]

#plot time-series
ggplot(webster_date_iso, aes(x = Date, y = dD, color = Project)) + 
  geom_point() +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(color = "Project") + # Add legend title
  scale_x_date(date_breaks = "1 month")  # Adjust frequency of date breaks

ggplot(data=webster_date_iso, aes(x=Date, y=dD, color=Site))+
  geom_point(aes(shape=Project, color=Site), size=2) +
  theme(legend.title = element_blank()) +
  theme_bw()
 
####TIME-SERIES OF 18O####
#plot time-series
ggplot(webster_date_iso, aes(x = Date, y = d18O, color = Project)) + 
  geom_point() +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(color = "Project") + # Add legend title
  scale_x_date(date_breaks = "1 month")  # Adjust frequency of date breaks

ggplot(data=webster_date_iso, aes(x=Date, y=d18O, color=Site))+
  geom_point(aes(shape=Project, color=Site), size=2) +
  theme(legend.title = element_blank()) +
  theme_bw()

####USF only TIME-SERIES OF dD####
#load data
webster_date <- read.csv("data/webstermain.csv")
names(webster_date)
#keep only columns that I am interested in
webster_date <- webster_date[, c("samplename", "Project", "Site", "ID", "Date")]
str(webster_date)
#remove BEGI sites
webster_date <- webster_date[!webster_date$Project == "BEGI", ]

#change to date format
webster_date$Date <- as.Date(webster_date$Date, format = "%m/%d/%y")

#merge isodata into webster based on samplename
webster_date_iso <- merge(webster_date, isodata, by = "samplename", all.x = TRUE)
webster_date_iso <- webster_date_iso[!is.na(webster_date_iso$dD), ]
webster_date_iso <- webster_date_iso[!duplicated(webster_date_iso$samplename),]

#Create new column for site number
webster_date_iso$Site_Number <- str_extract(webster_date_iso$Site, "\\d+$")
#change it from chr to factor
webster_date_iso$Site_Number <- as.factor(webster_date_iso$Site_Number)
#change it from chr to factor
webster_date_iso$Site <- as.factor(webster_date_iso$Site)

# Define shapes for 19 sites
site_shapes <- c(1:19)
#plot time series
ggplot(webster_date_iso, aes(x = Date, y = dD, color=Site, shape=Site)) + 
  geom_point(size=3) +
  labs(color = "Site") + 
  scale_shape_manual(values = site_shapes) +
  geom_line(aes(group=Site, colour=Site), alpha=.5, lwd=.9)
#by site
ggplot(webster_date_iso, aes(x = Date, y = dD, color=Site, shape=Site)) + 
  geom_point(size=3) +
  labs(color = "Site") + 
  scale_shape_manual(values = site_shapes)+
  geom_line(aes(group=Site, colour=Site), alpha=.5, lwd=.9) +
  facet_wrap(~ Site_Number)  # Wrap by Site variable

#change the color
bp <- distinct_palette()
#plot time series
ggplot(webster_date_iso, aes(x = Date, y = dD, color=Site_Number, shape=Site_Number)) + 
  geom_point(size=3) +
  labs(color = "Site_Number") + 
  scale_shape_manual(values = site_shapes) +
  scale_color_manual(values = bp)
#by site
ggplot(webster_date_iso, aes(x = Date, y = dD, color=Site_Number, shape=Site_Number)) + 
  geom_point(size=3) +
  labs(color = "Site_Number") + 
  scale_shape_manual(values = site_shapes) +
  scale_color_manual(values = bp) +
  facet_wrap(~ Site_Number)  # Wrap by Site variable

####USF only TIME-SERIES OF 18O####
#plot time series
ggplot(webster_date_iso, aes(x = Date, y = d18O, color=Site, shape=Site)) + 
  geom_point(size=3) +
  labs(color = "Site") + 
  geom_line(aes(group=Site, colour=Site), alpha=.5, lwd=.9) +
  scale_shape_manual(values = site_shapes)
#by site
ggplot(webster_date_iso, aes(x = Date, y = d18O, color=Site, shape=Site)) + 
  geom_point(size=3) +
  labs(color = "Site") + 
  scale_shape_manual(values = site_shapes) +
  geom_line(aes(group=Site, colour=Site), alpha=.5, lwd=.9) +
  facet_wrap(~ Site_Number)  # Wrap by Site variable

#change the color

#plot time series
ggplot(webster_date_iso, aes(x = Date, y = d18O, color=Site_Number, shape=Site_Number)) + 
  geom_point(size=3) +
  labs(color = "Site_Number") + 
  scale_shape_manual(values = site_shapes) +
  scale_color_manual(values = bp)
#by site
ggplot(webster_date_iso, aes(x = Date, y = d18O, color=Site_Number, shape=Site_Number)) + 
  geom_point(size=3) +
  labs(color = "Site_Number") + 
  scale_shape_manual(values = site_shapes) +
  scale_color_manual(values = bp) +
  facet_wrap(~ Site_Number)  # Wrap by Site variable


####GETTING REFERENCE VALUES using isoWater####
#Querie for river or stream data for the US
ls = wiDB_sites(countries = "US", types = "River_or_stream")

omar = par("mar")
par(mar = c(4, 4, 1, 1))
plot(ls[, 2:1], xlim = c(-125, -68), ylim = c(25, 50))

#refine our query and request isotope data using the function wiDB_data(). 
ld <- wiDB_data(minLat = 31, maxLat = 36, minLong = -109, maxLong = -103, 
                types = c("River_or_stream", "Lake_or_pond", "Ground", "Precipitation", "River/Stream", "Snow_pit"))
ld$data
#extract dataframe from list
isowater <- as.data.frame(ld[['data']])

#dataframe with lat long
points_df <- st_as_sf(isowater, coords = c('Longitude', 'Latitude'))
#convert to an sf object
points_sf <- st_as_sf(points_df, coords = c("Longitude", "Latitude"), crs = st_crs(us))
#check and set projection
st_crs(points_df)
points_sf <- st_set_crs(points_sf, 4326) # WGS84

#convert POINT geometries to separate columns for longitude and latitude
points_sf <- st_coordinates(points_sf)

#Get state boundaries
us_states <- map("state", fill = TRUE, plot = FALSE)
#filter to extract New Mexico
nm_boundary <- map("state", region = "New Mexico", fill = TRUE, plot = FALSE)
#convert to sf object
nm_sf <- st_as_sf(nm_boundary)

# Plot New Mexico
ggplot() +
  geom_sf(data = nm_sf) +
  geom_point(data = points_sf, aes(X, Y, color = isowater$Type), size = 3) +
  theme_minimal()

####COMBINING AND PLOTTING COMBINED DATASETS####
#format date+time
isowater$Collection_Date = as.POSIXct(isowater$Collection_Date, format="%Y-%m-%d %H:%M:%S", tz="MST")

#change to same column names
colnames(isowater)[colnames(isowater) == "Longitude"] <- "lon"
colnames(isowater)[colnames(isowater) == "Latitude"] <- "lat"
colnames(isowater)[colnames(isowater) == "Site_Name"] <- "Site"
colnames(isowater)[colnames(isowater) == "Sample_ID"] <- "samplename"
colnames(isowater)[colnames(isowater) == "Type"] <- "Project"
colnames(isowater)[colnames(isowater) == "d2H"] <- "dD"
colnames(isowater)[colnames(isowater) == "Collection_Date"] <- "datetime"

#Filter dataset for only high elevation data
isowater <- filter(isowater, Elevation >= 2100)
#Filter by date
isowater <- isowater %>%
  filter(datetime > as.POSIXct("2010-01-01"))
#Filter by Longitude and Latitude
isowater <- isowater %>%
  filter(lon >= -108 & lon <= -105  &
           lat >= 33 & lat <= 38)

#Merge isowater data and our data 
merged_data <- merge(isowater, webster_with_iso, by = c("samplename", "Site", "lon", "lat", "Project", "dD", "d18O", "datetime"), all = TRUE)

# add year and day of year for plotting
merged_data$year = lubridate::year(merged_data$datetime)
merged_data$doy = lubridate::yday(merged_data$datetime)

#scatterplot
ggplot(merged_data, aes(x=d18O, 
                    y = dD)) + geom_point() 
#scatterplot with sample ids
ggplot(merged_data, 
       aes(x=d18O, y = dD, 
           color=Project)) +  geom_point()

#remove outlier
merged_data <- filter(merged_data, dD > -150)

#scatter plot with GMWL
#The dotted line represents the Global Meteoric Water Line (δ2H = δ18O x 8 + 10)
#The bold black line represents the Local Meteoric Water Line (δ2H = δ18O x 6 - 17)
ggplot(merged_data, aes(x = d18O, y = dD, color=Project, shape=Project, group = Project)) +
  geom_point() +  # Add points for the data
  geom_abline(aes(intercept = 10, slope = 8, linetype = "GMWL"), show.legend = TRUE) + # Add GMWL line
  geom_abline(aes(intercept = -17, slope = 6, linetype = "LMWL"), show.legend = TRUE) + # Add LMWL line
  facet_wrap(~ Project) +
  labs(color = "Project", shape = "Project", linetype = "Line Type", linetype = "Legend")

#plot for only rivers and US sites
temp <- merged_data %>% 
  filter(Project == "River_or_stream" | Project == "USF")

ggplot(temp, aes(x = d18O, y = dD, color=Project, shape=Project, group = Project)) +
  geom_point() +  # Add points for the data
  geom_abline(aes(intercept = 10, slope = 8, linetype = "GMWL"), show.legend = TRUE) + # Add GMWL line
  geom_abline(aes(intercept = -17, slope = 6, linetype = "LMWL"), show.legend = TRUE) # Add LMWL line

#plot dD by project
ggplot(merged_data, aes(x = Project, y = dD, color=Project, shape=Project)) +
  geom_point()  # Add points for the data

#plot d18O by project
ggplot(merged_data, aes(x = Project, y = d18O, color=Project, shape=Project)) +
  geom_point()  # Add points for the data

#plot dD by year
ggplot(merged_data, aes(x = year, y = dD, color=Project, shape=Project)) +
  geom_point()  # Add points for the data
#plot d18O by year
ggplot(merged_data, aes(x = year, y = d18O, color=Project, shape=Project)) +
  geom_point()  # Add points for the data

# Plot dD and d18O
ggplot(merged_data, aes(x = datetime, y = dD, color = Project)) +
  geom_line() +
  labs(title = "dD Over Time by Project",
       x = "Datetime",
       y = "dD") +
  facet_wrap(~ Project, scales = "free_y") +
  theme_minimal()

# Plot d18O
ggplot(merged_data, aes(x = datetime, y = d18O, color = Project)) +
  geom_line() +
  labs(x = "Datetime",
       y = "d18O") +
  facet_wrap(~ Project, scales = "free_y") +
  theme_minimal()

#plot dD by month
ggplot(merged_data, aes(x = doy, y = dD, color=Project, shape=Project)) +
  geom_point()  + # Add points for the data
  geom_line()
#plot d18O by month
ggplot(merged_data, aes(x = doy, y = d18O, color=Project, shape=Project)) +
  geom_point() + # Add points for the data
  geom_line()

