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
library(tmaptools)
library(googledrive)

####load data###
sites = read_csv("data_geo/Site_lat_lon.csv")

target <- c("USF7", "USF19")
site <- sites %>%
  filter(SiteSub_ProjectB %in% target)
#convert it into barebones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
outlet <- st_as_sf(site, coords = c("Lon", "Lat"), 
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
temp <- "/Users/awebster2/Library/CloudStorage/Dropbox/CQNetwork/QuEST/ProjectPhase/watershed-QuEST"

#These next lines are preprocessing steps in digital elevation model (DEM) - they are creating intermediate files
#1.1 -----
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("temp/demNM.tif"), overwrite = T)
st_write(outlet, paste0("temp/outlet_newmex.shp"), delete_layer = T)

#1.2 -----
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "temp/demNM.tif",
  output = "temp/dem_newmex_fill.tif",
  wd = temp)

#1.3 -----
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "temp/dem_newmex_fill.tif",
  output = "temp/dem_newmex_breach.tif",
  wd = temp)


#1.4 -----
#Assigns flow direction
wbt_d8_pointer(
  dem = "temp/dem_newmex_breach.tif",
  output = "temp/flowdir_newmex.tif",
  wd = temp)


#1.5.1 -----
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "temp/dem_newmex_breach.tif",
  #you have to use the DEM not the flow direction for some reason
  output = "temp/flowaccum_newmex.tif",
  wd = temp
)

#1.6 -----
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "temp/outlet_newmex.shp",
  flow_accum = "temp/flowaccum_newmex.tif",
  snap_dist = 50,
  output = "temp/snap_pour_newmex.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream by picking it in Gaia GPS, then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream (it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("temp/flowaccum_newmex.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "temp/flowaccum_newmex.tif",
  output = "temp/streams_newmex.tif",
  threshold = 50,
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "temp/streams_newmex.tif",
  d8_pntr = "temp/flowdir_newmex.tif",
  output = "temp/streams_newmex.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("temp/streams_newmex.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("temp/snap_pour_newmex.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###


#1.7 -----
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "temp/flowdir_newmex.tif",
  pour_pts = "temp/snap_pour_newmex.shp",
  output = "temp/shed_newmex.tif",
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
st_write(newmex_ws, paste0("data_geo/site_test.shp"), delete_layer = T)

#make site into point for mapping
outlet
#plots dem raster with newmex shapefile
mapview(newmex_ws) + mapview(dem) + mapview(pour_sf)

#crop the DEM to run again#crop the DEM to rpourun again
#read the shapefile defining the extent to crop the DEM
crop_extent <- st_read("data_geo/site_test.shp")
#crop the DEM to the specified extent
cropped_DEM <- raster::crop(dem, crop_extent)

#plotting
plot(cropped_DEM)
plot(newmex_ws, add = TRUE)

#this all worked, so run the entire WBT series again to try making streams off of the smaller DEM
writeRaster(cropped_DEM, paste0("temp/cropped_dem_newmex.tif"), overwrite=T)
#didn't change
wbt_fill_single_cell_pits(
  dem = "temp/cropped_dem_newmex.tif",
  output = "temp/cropped_dem_newmex_fill.tif",
  wd = temp)

wbt_breach_depressions(
  dem = "temp/cropped_dem_newmex_fill.tif",
  output = "temp/cropped_dem_newmex_breach.tif",
  wd = temp)

wbt_d8_pointer(
  dem = "temp/cropped_dem_newmex_breach.tif",
  output = "temp/cropped_flowdir_newmex.tif",
  wd = temp)

wbt_d8_flow_accumulation(
  input = "temp/cropped_dem_newmex_breach.tif",
  output = "temp/cropped_flowaccum_newmex.tif",
  wd = temp
)

### RUN FROM HERE ----
#read stream raster
streams <- raster(paste0("temp/cropped_flowaccum_newmex.tif")) #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<21] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "temp/cropped_flowaccum_newmex.tif",
  output = "temp/cropped_streams_newmex.tif",
  threshold = 50,
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "temp/cropped_streams_newmex.tif",
  d8_pntr = "temp/cropped_flowdir_newmex.tif",
  output = "temp/cropped_streams_newmex.shp",
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
st_write(newmex_ws, paste0("data_geo/area_test.shp"), delete_layer = T)
st_write(streams, paste0("data_geo/area_test_stream_network.shp"), delete_layer = T)
writeRaster(cropped_DEM, paste0("data_geo/croppedDEM_area_test.tif"), overwrite=T)


#TO GET THE AREA OF YOUR WATERSHED POLYGONS it has to be in sf format
sum(st_area(newmex_ws))

#Check area is ok with flowdir
flowdir = raster('temp/flowdir_newmex.tif')
plot(flowdir) + plot(streams)
mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(newmex_ws)

#add area values to new dataframe
#area <- ()

sum(st_area(newmex_ws))

####Areas in m^2####
##site 1: 37143453 
##site 2: 1657987 
##site 3: 28860197 
##site 4: 1857528 
##site 5: 23590504 
##site 6: 21454810 
##site 7: 2296412 
##site 8: 82419.44 
##site 9: 1938917 
##site 10: 6393688 
##site 11: 5003890 
##site 12: 35499892 
##site 13: 765470.5 
##site 14: 2027518 
##site 15: 848920.2 
##site 16: 388401.6 
##site 17: 219441.8 
##site 18: 114357 
##site 19: 104705.5 
#site 20: 19141915 

###Total: 37617340