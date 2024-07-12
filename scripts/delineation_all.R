##############
## PACKAGES ##
##############
library(sf)
library(sp)
library(raster)
library(elevatr)
library(mapview)
library(whitebox)
library(dplyr)
library(readr)

###############
## Load data ##
###############
outlet = read_csv("data/site_latlon.csv")
outlets_sf = st_as_sf(outlet, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

####################################
## Clear folders that we will use ##
####################################
# List and delete all files in the folder
files <- list.files(path = "data_geo", full.names = TRUE)
file.remove(files)

files <- list.files(path = "temp", full.names = TRUE)
file.remove(files)

####################################################
## Create a Function to Process Each Outlet Point ##
####################################################
#This is the function
process_outlet <- function(outlet_point, id) {
  temp_dir <- paste0("temp_", id)
  dir.create(temp_dir, showWarnings = FALSE)
  
  outlet_sf <- st_as_sf(outlet_point, crs = '+proj=longlat +datum=WGS84 +no_defs')
  outlet_sf <- st_transform(outlet_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% st_geometry()
  
  ############
  ##Pull DEM##
  ############
  # Convert to Spatial object
  pour = as_Spatial(outlet_sf) # make pour points = spatial object
  pour_sf = st_as_sf(pour) # check data set
  #get the dem
  dem = get_elev_raster(pour_sf, z = 11, clip = "bbox", expand = 10000)
  
  # Save DEM
  writeRaster(dem, paste0(temp_dir, "/dem.tif"), overwrite = TRUE)
  
  ##########################
  ##Prep DEM and delineate##
  ##########################
  #whitebox functions do not work just using project directory, so you have to set the working directory
  #we are using all the files that are stored in the temp directory, so:
  getwd()
  #copy and paste that directory if that's where you are working from and make it a temp
  temp <- "/Users/manuelalondono/Documents/watershed-QuEST"
  
  ##Pre-processing steps - they are creating intermediate files
  #1.Prepares the DEM and delineates the watershed through a series of steps:
  st_write(outlet_sf, paste0(temp_dir, "/outlet.shp"), delete_layer = TRUE)
  #2.Fills single-cell pits (small depressions or pits in the elevation data are filled in)
  wbt_fill_single_cell_pits(dem = paste0(temp_dir, "/dem.tif"), output = paste0(temp_dir, "/dem_fill.tif"), wd = temp_dir)
  #3.Breaches depressions (remove artificial depressions or flat areas in the elevation data)
  wbt_breach_depressions(dem = paste0(temp_dir, "/dem_fill.tif"), output = paste0(temp_dir, "/dem_breach.tif"), wd = temp_dir)
  #4.Assigns flow direction
  wbt_d8_pointer(dem = paste0(temp_dir, "/dem_breach.tif"), output = paste0(temp_dir, "/flowdir.tif"), wd = temp_dir)
  #5.Computes flow accumulation
  wbt_d8_flow_accumulation(input = paste0(temp_dir, "/dem_breach.tif"), output = paste0(temp_dir, "/flowaccum.tif"), wd = temp_dir)
  #6.Snaps pour points to the nearest flow path
  wbt_snap_pour_points(pour_pts = paste0(temp_dir, "/outlet.shp"), flow_accum = paste0(temp_dir, "/flowaccum.tif"), snap_dist = 50, output = paste0(temp_dir, "/snap_pour.shp"), wd = temp_dir)
  
  #now use the new WBT functions to extract the streams!
  wbt_extract_streams(flow_accum = paste0(temp_dir, "/flowaccum.tif"), output = paste0(temp_dir, "/streams.tif"), threshold = 50, wd = temp_dir)
  wbt_raster_streams_to_vector(streams = paste0(temp_dir, "/streams.tif"), d8_pntr = paste0(temp_dir, "/flowdir.tif"), output = paste0(temp_dir, "/streams.shp"), wd = temp_dir)
  
  #input into R 
  #read shapefile containing stream data
  streams <- st_read(paste0(temp_dir, "/streams.shp"))
  #assigns the coordinate reference system (CRS) of the stream data to match DEM system
  st_crs(streams) <- st_crs(dem)
  
  #input snapped pour pt
  pour_pt_snap <- st_read(paste0(temp_dir, "/snap_pour.shp"))
  #assigns the coordinate reference system (CRS) of the stream data to match DEM system
  st_crs(pour_pt_snap) <- st_crs(dem)
  
  ## Check if points are on stream
  mapview(dem, maxpixels = 742182) + mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color = "red")
  
  #7.Delineates the watershed 
  wbt_watershed(d8_pntr = paste0(temp_dir, "/flowdir.tif"), pour_pts = paste0(temp_dir, "/snap_pour.shp"), output = paste0(temp_dir, "/shed.tif"), wd = temp_dir)
  
  #8.Be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
  newmex_ws <- raster(paste0(temp_dir, "/shed.tif"))
  #9.Converts newmex_ws into a stars object, it is a multi-dimensional array that represents raster data.
  newmex_ws <- st_as_stars(newmex_ws) %>% st_as_sf(merge = TRUE)
  
  #Writes shapefile to data folder
  st_write(newmex_ws, paste0("data_geo/site_", id, ".shp"), delete_layer = TRUE)
  
  # Plots dem raster with newmex shapefile
  mapview(newmex_ws) + mapview(dem) + mapview(pour_sf)
  
  # Calculate area and return result
  area <- st_area(newmex_ws)
  return(area)
}

#### iterate over each outlet ####
areas <- data.frame(ID = integer(), Area_m2 = numeric())

for (i in 1:nrow(outlets_sf)) {
  outlet_point <- outlets_sf[i, ]
  area <- process_outlet(outlet_point, i)
  areas <- rbind(areas, data.frame(ID = i, Area_m2 = as.numeric(area)))
}

print(areas)

write.csv(areas, "data/areas.csv", row.names = FALSE)
