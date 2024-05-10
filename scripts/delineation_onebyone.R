####PACKAGES####
library(tidyverse)
library(dplyr)
library(raster)
library(sf)
#if using spatial points,
library(sp)
library(elevatr)
#library(rgdal)
library(mapview)
library(stars)
#library(rayshader)
library("vroom")
#whitebox::install_whitebox()
library(whitebox)

####load data####

# Initialize empty data frame
wpfull <- NULL
# - Select first file from the list and import data into R object
wplist <- read_GPX("data_geo/wp.gpx")
# extract latitude, longituDe, elevation, time, name and comments and apppend to R dataframe
wpdf<- wplist$waypoints
# append dataframe from last index to a full waypoint object
wpfull <- bind_rows(wpfull, wpdf)
wpfull <- wpfull %>% 
  filter(name == "USF4") 
outlet <- wpfull 

#option 2 of getting the points
sites = read_csv("data_geo/sites.csv")
site <- sites %>% 
  filter(Site == "USF4") 
#convert it into barebones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
outlet <- st_as_sf(site, coords = c("lon", "lat"), 
                   crs = '+proj=longlat +datum=WGS84 +no_defs')

##skip to here
#reproject to utm 16
outlet <- st_transform(outlet, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


####PULL A DEM (digital elevation model)####
# DEM - by AJS 
pour = as_Spatial(outlet) # make pour points = spatial object
pour # check dataset
#convert SpatialPoints to sf (simple features)
pour_sf <- st_as_sf(pour) 
#use the sf object in get_elev_raster
elevation = get_elev_raster(pour_sf, z = 11, clip = "bbox", expand = 10000) #works at 8

#plot the elevation
plot(elevation)

#I don't know what this is for yet but...
#save the elevation raster in a folder called temp
writeRaster(elevation, paste0("temp/dem_newmex.tif"), overwrite = T)
# Load DEM
dem<-raster(paste0("temp/dem_newmex.tif"))
dem # matches pour projections 

# Plot to check
plot(dem) 

#plot with mapview to check
mapview(dem) + mapview(pour_sf)

####PREP DEM AND DELINEATE####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and outlet to temp
#1.2 fill cell pits
#1.3 breach depressions
#1.4 write flow direction raster
#1.5.2 write flow accumulation raster
#1.5.2 write stream layer
#1.6 snap pour point
#1.7 delineate
#1.8 read back into main
#1.9 convert to polygons

#whitebox functions do not work just using project directory, so you have to set the working directory
#we are using all the files that are stored in the temp directory, so:
getwd()
temp <- "/Users/manuelalondono/Documents/watershed-QuEST/temp"

#These next lines are preprocessing steps in digital elevation model (DEM) - they are creating intermediate files
#1.1 -----
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("temp/demNM.tif"), overwrite = T)
st_write(outlet, paste0("temp/outlet_newmex.shp"), delete_layer = T)

#1.2 -----
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "demNM.tif",
  output = "dem_newmex_fill.tif",
  wd = temp)

#1.3 -----
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "dem_newmex_fill.tif",
  output = "dem_newmex_breach.tif",
  wd = temp)


#1.4 -----
#Assigns flow direction
wbt_d8_pointer(
  dem = "dem_newmex_breach.tif",
  output = "flowdir_newmex.tif",
  wd = temp)


#1.5.1 -----
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "dem_newmex_breach.tif",
  #you have to use the DEM not the flow direction for some reason
  output = "flowaccum_newmex.tif",
  wd = temp
)

#1.6 -----
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "outlet_newmex.shp",
  flow_accum = "flowaccum_newmex.tif",
  snap_dist = 50,
  output = "snap_pour_newmex.shp",
  wd = temp
)

#1.7 -----
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "flowdir_newmex.tif",
  pour_pts = "snap_pour_newmex.shp",
  output = "shed_newmex.tif",
  wd = temp
)

#1.8 -----
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
newmex_ws <- raster(paste0("temp/shed_newmex.tif"))
mapview(newmex_ws)

#1.9 -----
#converts newmex_ws into a stars object, it is a multi-dimensional array that represents raster data.
newmex_ws <- st_as_stars(newmex_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(newmex_ws)
#writes shapefile to data folder
st_write(newmex_ws, paste0("data_geo/site4.shp"), delete_layer = T)

#make site into point for mapping
outlet
#plots dem raster with newmex shapefile
mapview(newmex_ws) + mapview(dem) + mapview(pour_sf)

#crop the DEM to run again#crop the DEM to rpourun again
#read the shapefile defining the extent to crop the DEM
crop_extent <- st_read("data_geo/site4.shp")
#crop the DEM to the specified extent
cropped_DEM <- raster::crop(dem, crop_extent)

#plotting
plot(cropped_DEM)
plot(newmex_ws, add = TRUE)

#this all worked, so run the entire WBT series again to try making streams off of the smaller DEM
writeRaster(cropped_DEM, paste0("temp/cropped_dem_newmex.tif"), overwrite=T)
#didn't change
wbt_fill_single_cell_pits(
  dem = "cropped_dem_newmex.tif",
  output = "cropped_dem_newmex_fill.tif",
  wd = temp)

wbt_breach_depressions(
  dem = "cropped_dem_newmex_fill.tif",
  output = "cropped_dem_newmex_breach.tif",
  wd = temp)

wbt_d8_pointer(
  dem = "cropped_dem_newmex_breach.tif",
  output = "cropped_flowdir_newmex.tif",
  wd = temp)

wbt_d8_flow_accumulation(
  input = "cropped_dem_newmex_breach.tif",
  output = "cropped_flowaccum_newmex.tif",
  wd = temp
)

### RUN FROM HERE ----
#read stream raster
streams <- raster(paste0("temp/cropped_flowaccum_newmex.tif")) #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "cropped_flowaccum_newmex.tif",
  output = "cropped_streams_newmex.tif",
  threshold = 50,
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "cropped_streams_newmex.tif",
  d8_pntr = "cropped_flowdir_newmex.tif",
  output = "cropped_streams_newmex.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("temp/cropped_streams_newmex.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(cropped_DEM)
#crop to the watershed
streams <- streams[newmex_ws,]
plot(streams)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + mapview(newmex_ws) + 
  mapview(streams) + mapview(pour_sf) 

#export all of these so we have them!
st_write(newmex_ws, paste0("data_geo/area4.shp"), delete_layer = T)
st_write(streams, paste0("data_geo/area4_stream_network.shp"), delete_layer = T)
writeRaster(cropped_DEM, paste0("data_geo/croppedDEM_area4.tif"), overwrite=T)


#TO GET THE AREA OF YOUR WATERSHED POLYGONS it has to be in sf format
sum(st_area(newmex_ws))

#Check area is ok with flowdir
flowdir = raster('temp/flowdir_newmex.tif')
plot(flowdir) + plot(streams)
mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(newmex_ws)

#add area values to new dataframe
# Create an empty dataframe with column names
aareas <- data.frame

  
sum(st_area(newmex_ws))

#site 1: 3091.338 [m^2]
#site 3: 28860197 [m^2]
#site 3:
#site 11: 5003890 [m^2]
#site 12: 35499892 [m^2]
#site 20: 19141915 [m^2]

