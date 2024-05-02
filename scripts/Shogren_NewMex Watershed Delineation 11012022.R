#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Watershed Delineations:
#   newmexield Creek, Alabama

#First attempt; 20221022
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#OUTLINE --

#Step 0: Download and import data, setup workspace
# - DEM (preferably 3m or 1m if possible)
# - STIC and LTM points
# - load necessary packages
# - DEFINE DIRECTORIES FOR WBT
#Step 1: prep DEM to delineate, and DELINEATE
# - also export spatial files for later
#Step 2: prep sensor data and add to map

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 0: Download and import data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#0.1 clear workspace and install packages
#0.2 define directories
#0.3 Load in DEM
#0.4 import sensor location file
#0.5 write (?) outlet point

#0.1 -----
remove(list=ls())

#if (!require("remotes")) install.packages('remotes')
#remotes::install_github("giswqs/whiteboxR", build = FALSE)

library(tidyverse)
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
whitebox::install_whitebox()
library(whitebox)


#does not like rayshader or rgl 
#reinstall whitebox
#install.packages("whitebox", repos="http://R-Forge.R-project.org")
#whitebox::wbt_init()
#library(whitebox)

#0.2 ----- Setting Working directories
data_dir<-"data_dir/" 
temp_dir<-"temp_dir/" 

#0.3 -----


#0.4 -----
#this is for mapping our AIMS sensors, you don't have to do this! BUT you will need to 
#tell it where your outlet it -- mine is included in tal_sensors, but you can import yours
#separately!
newmex_sensors <- read_csv(paste0(data_dir, "QUESTsiteCoords_tent_MAYF.csv")) 
newmex_smartrockE <- newmex_sensors %>% 
  filter(SiteID == "NM_SF") 

#0.5 -----
#convert it into barebones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
outlet <- st_as_sf(newmex_smartrockE, coords = c("Long", "Lat"), 
                   crs = '+proj=longlat +datum=WGS84 +no_defs')
#reproject to utm 16
outlet <- st_transform(outlet, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()

# DEM - by AJS 
pour = as_Spatial(outlet) # make pour points = spatial object
pour # check dataset
#convert into sf object
pour = st_as_sf(outlet)
elevation = get_elev_raster(pour, z = 11, clip = "bbox", expand = 10000) #works at 8

plot(elevation) # works
writeRaster(elevation, paste0(data_dir, "NMdem_10m_v3.tif"), overwrite = TRUE) # check that this is in the right directory

#writeRaster(dem, paste0(temp_dir, "dem_newmex.tif"), overwrite = T)
# Load DEM
#data_dir <-("/Users/arialshogren/Weyer")
dem<-raster(paste0(data_dir,"NMdem_10m_v3.tif"))
dem # matches pour projections 

# Plot to check
plot(dem) 
#dem <- raster(paste0(data_dir,"ALdem_10m.tif"))

#plot with mapview to check
mapview(dem) + mapview(outlet)


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

#A NOTE TO THOSE WHO MAY BE READING THIS (Stella <3);
# there are some redundancies here because the DEM I downloaded from USDA is
# STUPIDLY large. But the steps are overall the same regardless, the only part 
# that changes is the input rasters (the first round, I used the large one and 
# DID NOT export the stream layer, and the second round I used the WS extent 
# values to crop the raster, run it again, and then delineate the streams so it
# didn't take forever). If there's any confusion, I can walk you through, but I 
#left all these steps so you could see them!!!

#1.1 -----

writeRaster(dem, paste0(temp_dir, "dem_newmex.tif"), overwrite = T)
st_write(outlet, paste0(temp_dir, "outlet_newmex.shp"), delete_layer = T)

#1.2 -----

wbt_fill_single_cell_pits(
  dem = "dem_newmex.tif",
  output = "dem_newmex_fill.tif",
  wd = temp_dir)

#1.3 -----
wbt_breach_depressions(
  dem = "dem_newmex_fill.tif",
  output = "dem_newmex_breach.tif",
  wd = temp_dir)

#1.4 -----
wbt_d8_pointer(
  dem = "dem_newmex_breach.tif",
  output = "flowdir_newmex.tif",
  wd = temp_dir)

#1.5.1 -----
wbt_d8_flow_accumulation(
  input = "dem_newmex_breach.tif",
  #you have to use the DEM not the flow direction for some reason
  output = "flowaccum_newmex.tif",
  wd = temp_dir
)

#1.5.2 -----
#DO THIS AGAIN LATER ON THE SMALLER RASTER
#streams <- raster(paste0(temp_dir, "flowaccum_tal.tif"))
#streams[streams<10000] <- NA
#writeRaster(streams, paste0(temp_dir, "streams_tal.tif"), overwrite=T)

#1.6 -----
wbt_snap_pour_points(
  pour_pts = "outlet_newmex.shp",
  flow_accum = "flowaccum_newmex.tif",
  snap_dist = 1000,
  output = "snap_pour_newmex.shp",
  wd = temp_dir
)

#1.7 -----
wbt_watershed(
  d8_pntr = "flowdir_newmex.tif",
  pour_pts = "snap_pour_newmex.shp",
  output = "shed_newmex.tif",
  wd = temp_dir
)

#1.8 -----
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
newmex_ws <- raster(paste0(temp_dir, "shed_newmex.tif"))
mapview(newmex_ws)
#tal_streams <- raster(paste0(temp_dir, "streams_tal.tif"))
#will not work this way
newmex_sensor <- st_read(paste0(temp_dir, "snap_pour_newmex.shp")) %>% st_geometry()

#1.9 -----

newmex_ws <- st_as_stars(newmex_ws) %>% st_as_sf(merge=T) # SKIP STEP

#tal_streams <- st_as_stars(tal_streams) %>% st_as_sf(merge=T)
#also will not work
mapview(newmex_ws)
st_write(newmex_ws, paste0(data_dir, "WS_newmex.shp"), delete_layer = T)

mapview(newmex_ws) + mapview(newmex_sensor) + mapview(dem)


#crop the DEM to run again
crop_extent <- readOGR(paste0(data_dir, "WS_newmex.shp"))
cropped_DEM <- crop(dem, crop_extent)
cropped_DEM <- readOGR(paste0(data_dir, "cropped_DEM.shp"))
plot(cropped_DEM)
plot(newmex_ws, add = T)

#this all worked, so run the entire WBT series again to try making streams off of the smaller DEM
writeRaster(cropped_DEM, paste0(temp_dir, "cropped_dem_newmex.tif"), overwrite=T)
#st_write(outlet, paste0(temp_dir, "outlet_tal.shp"), delete_layer = T)
#didn't change

wbt_fill_single_cell_pits(
  dem = "cropped_dem_newmex.tif",
  output = "cropped_dem_newmex_fill.tif",
  wd = temp_dir)

wbt_breach_depressions(
  dem = "cropped_dem_newmex_fill.tif",
  output = "cropped_dem_newmex_breach.tif",
  wd = temp_dir)

wbt_d8_pointer(
  dem = "cropped_dem_newmex_breach.tif",
  output = "cropped_flowdir_newmex.tif",
  wd = temp_dir)

wbt_d8_flow_accumulation(
  input = "cropped_dem_newmex_breach.tif",
  output = "cropped_flowaccum_newmex.tif",
  wd = temp_dir
)

### RUN FROM HERE ----
streams <- raster(paste0(temp_dir, "cropped_flowaccum_newmex.tif"))
streams[streams<50] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
writeRaster(streams, paste0(temp_dir, "cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "cropped_flowaccum_newmex.tif",
  output = "cropped_streams_newmex.tif",
  threshold = 50,
  wd = temp_dir
)

wbt_raster_streams_to_vector(
  streams = "cropped_streams_newmex.tif",
  d8_pntr = "cropped_flowdir_newmex.tif",
  output = "cropped_streams_newmex.shp",
  wd = temp_dir
)

#input into R 
# NOTE BY AJS: this does not work, given RGDAL is depreciated
streams <- st_read(paste0(temp_dir, "cropped_streams_newmex.shp"))
st_crs(streams) <- st_crs(cropped_DEM)
#crop to the watershed
streams <- streams[newmex_ws,]
plot(streams)

## NOTE: AJS deleted DMP's older code, the above works 

#### END OLD CODE
mapview(dem) + mapview(newmex_ws) + 
  mapview(streams)# this worked (!)


#export all of these so we have them!
st_write(newmex_ws, paste0(data_dir, "newmex_watershed.shp"), delete_layer = T)
st_write(streams, paste0(data_dir, "newmex_stream_network.shp"), delete_layer = T)
#writeRaster(cropped_DEM, paste0(data_dir, "/20211115_spatial_files/Talladega/croppedDEM.tif"), overwrite=T)


#TO GET THE AREA OF YOUR WATERSHED POLYGONS it has to be in sf format

sum(st_area(newmex_ws)) #12493385 m2
#spits out 925712 m^2 which is 92 hectares WHICH IS RIGHT

#shout out to Michelle for saving our lives hehe

#--------

#EVERYTHING FROM HERE IS FOR MAPPING -- if you want it, it's yours, but this is definitely
#not the best way to do all of this; it's just what worked for me this first try

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: prep sensor data and add to map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 edit data for better analysis
#2.2 convert to spatial data
#0.3 map it

#2.1 -----
#separate if we want to put sensor sites- can keep this for now with just pour points
newmex_sensors <- newmex_sensors %>% 
  mutate(ID = NA,
         Type = NA) %>% 
  separate("SiteID", into = c("ID", "Type"), sep = "_")

newmex_smr <- newmex_sensors %>% 
  dplyr::filter(Type == "NM") %>% 
  dplyr::select(ID, Long, Lat)

newmex_smr <- st_as_sf(newmex_smr, coords = c("Long", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')
newmex_smr <- st_transform(newmex_smr, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()

mapview(dem) + mapview(streams, color = "blue") +
  mapview(newmex_ws)  
#mapview(newmex_stics, color = "lightblue", col.regions = "lightblue") +
#mapview(newmex_smr, color = "red", col.regions = "red") +
#mapview(newmex_ltms, color = "white", col.regions = "indianred") +
#mapview(newmex_sensor)

#plot(cropped_DEM)

#now map
#install.packages("tmap")
library(tmap)
#writeRaster(elevation, paste0(data_dir, "NMdem_10m_v3.tif"), overwrite = TRUE) # check that this is in the right directory

#dem_extent <- readOGR(paste0(data_dir,"cropped_dem_newmex.tif"))
dem_extent <- raster(paste0(temp_dir,"cropped_dem_newmex.tif"))
#cropped_DEM <- readOGR(paste0(data_dir, "cropped_DEM.shp"))
dem_extent
extent_ws <- extent(-1213269, -1204441, 4114924, 4127796) #from the cropped dem
crop_ws <- raster::crop(dem, extent_ws)

# Test plot to make sure it looks ok
bgc <- tm_shape(crop_ws) +
  tm_raster(col = "white") +
  tm_shape(newmex_ws) +
  tm_polygons(col = "gray90", border.col = "black", lwd = 3, alpha = 0.2) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 2.5) +
  tm_add_legend(title = "Sensor Type",
                type = "symbol", 
                shape = 23,
                col = c("red"),
                border.col = c("black"),
                labels = c("SmartRock")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.frame = TRUE,
            legend.position = c(0.63, 0.65),
            legend.bg.color = "white",
            legend.bg.alpha = 0.25,
            legend.text.size = 1.75,
            title = "newmex",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 1.5)
bgc

dem.plot <- tm_shape(crop_ws) +
  tm_raster(palette = "-Greys", n = 10, alpha = 0.75, title = "Elevation [m]", legend.reverse = TRUE) +
  tm_shape(newmex_ws) +
  tm_borders(col = "black", lwd = 2) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 1) +
  #tm_shape(outlet) +
  #tm_dots(size = 1.5, shape = 23, col = "#ffffbf", border.col = "black") +
  #tm_add_legend(title = "",
  #              type = "symbol", 
  #              shape = 23, 
  #              col = c("red"),
  #              border.col = c("black"),
  #              labels = c("Site")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 1,
            legend.frame = TRUE,
            # frame.lwd = c(0,2),
            legend.position = c(0, 0.62),
            #legend.positon = "left", "bottom",
            legend.bg.color = "white",
            legend.bg.alpha = 0.75,
            legend.text.size = 0.8,
            title = "",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  #tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 2) +
  #tm_grid(x = seq(32.456, 32.867), y= seq(-87.0, -87.2))
  quartz()
dem.plot

masked_dem <- mask(crop_ws, tal_ws)
dem_greymask <- tm_shape(masked_dem) +
  tm_raster(palette = "-Greys", n = 8, alpha = 0.75, title = "Elevation [m]", legend.reverse = TRUE) +
  tm_shape(tal_ws) +
  tm_borders(col = "black", lwd = 4) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 3) +
  tm_shape(tal_stics) +
  tm_dots(size = 1.5, shape = 21, col = "gold2", border.col = "black") +
  tm_shape(tal_ltms) +
  tm_dots(size = 1.5, shape = 21, col = "purple1", border.col = "black") +
  tm_shape(outlet) +
  tm_dots(size = 1.5, shape = 23, col = "springgreen3", border.col = "black") +
  tm_add_legend(title = "Sensor Type",
                type = "symbol", 
                col = c("gold2", "purple1", "springgreen3"),
                border.col = c("black", "black", "black"),
                labels = c("STIC", "LTM", "Supersensor")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.frame = TRUE,
            legend.position = c(0.77, 0.3),
            legend.bg.color = "white",
            legend.bg.alpha = 0.75,
            legend.text.size = 1.75,
            title = "Talladega Sensor Locations",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 1.5)