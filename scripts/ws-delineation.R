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

#install.packages("whitebox")

####MAKE POUR POUNT LOCATION DATAFRAME####
#load data
webstermain = read_csv("data/webstermain.csv")

#remove BEGI sites
webstermain <- webstermain[!webstermain$Project == "BEGI", ]
#remove rows with NA or empty 
webstermain <- webstermain[!is.na(webstermain$Project) & webstermain$Project != "", ]

#keep only unique rows based on the "Site" column and reduce df to long lat per site only
sites <- webstermain %>%
  distinct(Site, .keep_all = TRUE) %>%
  dplyr::select(Site, lat, lon)

#convert it into barebones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
outlet <- st_as_sf(sites, coords = c("lon", "lat"), 
                   crs = '+proj=longlat +datum=WGS84 +no_defs')

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
plot(elevation) # works
writeRaster(elevation, paste0("data/NMdem.tif"), overwrite = TRUE) # check that this is in the right directory

#I don't know what this is for yet but...
#save the elevation raster in a folder called temp
writeRaster(elevation, paste0("temp/dem_newmex.tif"), overwrite = T)
# Load DEM
dem<-raster(paste0("temp/dem_newmex.tif"))
dem # matches pour projections 

# Plot to check
plot(dem) 

#plot with mapview to check
mapview(dem) + mapview(outlet)

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

#A NOTE TO THOSE WHO MAY BE READING THIS (Stella <3);
# there are some redundancies here because the DEM I downloaded from USDA is
# STUPIDLY large. But the steps are overall the same regardless, the only part 
# that changes is the input rasters (the first round, I used the large one and 
# DID NOT export the stream layer, and the second round I used the WS extent 
# values to crop the raster, run it again, and then delineate the streams so it
# didn't take forever). If there's any confusion, I can walk you through, but I 
#left all these steps so you could see them!!!

#whitebox functions are not working jusy using project directory, so have to set the working directory
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
  snap_dist = 1000,
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


#plot areas with collection points
#for that i need the newmex_ws as a polygon
newmex_ws_poly <- rasterToPolygons(newmex_ws, dissolve = TRUE)
#and then I can plot it
mapview(newmex_ws_poly) + 
  mapview(outlet, col.regions = "red")

mapview(outlet, col.regions = "red")

# Check CRS of outlet
st_crs(outlet)

# Check CRS of newmex_ws_poly
st_crs(newmex_ws_poly)

#1.9 -----
#converts newmex_ws into a stars object, it is a multi-dimensional array that represents raster data.
newmex_ws <- st_as_stars(newmex_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one
#plots watershed shapefile
mapview(newmex_ws)
#writes shapefile to data folder
st_write(newmex_ws, paste0("data/WS_newmex.shp"), delete_layer = T)
#plots dem raster with newmex shapefile
mapview(newmex_ws) + mapview(newmex_sensor) + mapview(dem)


#crop the DEM to run again
###this uses rgdal which was removed from the CRAN repository###
#crop_extent <- readOGR(paste0("data/WS_newmex.shp"))
#cropped_DEM <- crop(dem, crop_extent)
#cropped_DEM <- readOGR(paste0(data_dir, "cropped_DEM.shp"))
#plot(cropped_DEM)
#plot(newmex_ws, add = T)

###will try to use sf instead###
#read the shapefile defining the extent to crop the DEM
crop_extent <- st_read("data/WS_newmex.shp")
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
streams <- raster(paste0("temp/cropped_flowaccum_newmex.tif")) #Raster likely represents flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
streams[streams<50] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

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

#### END OLD CODE
mapview(dem) + mapview(newmex_ws) + 
  mapview(streams)# this worked (!)

#export all of these so we have them!
st_write(newmex_ws, paste0("data/newmex_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/newmex_stream_network.shp"), delete_layer = T)
writeRaster(cropped_DEM, paste0("data/croppedDEM.tif"), overwrite=T)


#TO GET THE AREA OF YOUR WATERSHED POLYGONS it has to be in sf format
sum(st_area(newmex_ws))
#spits out 43690325 m^2 which is 4369 hectares, 10796 acres. 

#to get the area of each polygon
#change the site column name to match column names
names(sites)[names(sites) == "Site"] <- "shed_newmex"
#remove the "USF" part from the "Site" column
sites$shed_newmex <- gsub("^USF", "", sites$shed_newmex)
#merge the site data with the watershed polygons
merged_data <- merge(sites, newmex_ws, by = "shed_newmex")

#convert data frame to sf object
merged_data_sf <- st_as_sf(merged_data)
#calculate the area
areas <- st_area(merged_data_sf)

# Create a data frame with site IDs and corresponding areas
site_areas <- data.frame(Site = merged_data$shed_newmex, Area = areas)
#save areas data frame
write.csv(site_areas, "data/site_areas")

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
newmex_smr <- sites %>% 
  mutate(ID = substr(Site, 1, 3),
         Type = substr(Site, 4, nchar(Site)))

newmex_smr <- newmex_pourpoints %>% 
  dplyr::filter(Type == "NM") %>% 
  dplyr::select(ID, Long, Lat)

newmex_smr <- st_as_sf(newmex_smr, coords = c("lon", "lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')
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
dem_extent <- raster(paste0("temp/cropped_dem_newmex.tif"))
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