####packages####
library(mapview)
library(dplyr)
#remotes::install_github('r-tmap/tmap')
library(tmap)
library(sf)
#if using spatial points,
library(sp)
#for color palettes
library(viridis)


####load data####
#read shapefiles
areas <- st_read("data_geo/area.shp")
#filter areas to use
target <- c(3, 5, 6, 7, 13, 14, 15, 16)
area <- areas %>%
  dplyr::filter(shd_nwm %in% target)

#load areas individually
area1 <- st_read("data_geo/area1.shp")
area3 <- st_read("data_geo/area3.shp")
area5 <- st_read("data_geo/area5.shp")
area6 <- st_read("data_geo/area6.shp")
area12 <- st_read("data_geo/area12.shp")

#load streams
streams <- st_read("data_geo/area_stream_network.shp")

#load leverage data  
leverage <- read.csv("data/leverage.csv")
#filter out by what you will use
DOC <- leverage %>%
  dplyr::select(Sub_ProjectB, lat, lon, NPOC..mg.C.L.)
#transform lat lon to geometries
DOC <- st_as_sf(DOC, coords = c("lon", "lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- DOC %>%
  dplyr::select(Sub_ProjectB, geometry)


####plot on mapview####
#make color palette
pal <- magma(n = length(unique(lev$NPOC..mg.C.L.)), direction = -1)

#plot
mapview(area1, alpha.regions = 0., aplha = 1) + mapview(area, legend = FALSE, col.regions = pal) + 
  mapview(streams, legend = FALSE) + 
  mapview(DOC, cex = "NPOC..mg.C.L.", zcol = "NPOC..mg.C.L.")

####plotplot###
#still very ugly and not finished
sfws <- tm_shape(areas) +
  tm_borders(col = "black", lwd = 2) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 2.5) +
  #add pour points
  tm_shape(levpoints) +
  tm_dots(size = 1.5, shape = 5, fill = "#ffffbf", col = "black") +
  tm_add_legend(title = "",
                type = "symbols", 
                shape = 23, 
                fill = c("red"),
                col = c("black"),
                labels = c("Site")) + 
  tm_shape(levpoints) +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scalebar(text.size = 1) +
  #add legend
  tm_layout(legend.title.fontface = "bold",
          legend.title.size = 1,
          legend.frame = TRUE,
          # frame.lwd = c(0,2),
          #legend.position = c(0, 0.62),
          #legend.positon = "left", "bottom",
          legend.bg.color = "white",
          legend.bg.alpha = 0.75,
          legend.text.size = 0.8,
          tm_title = "",
          title.size = 3,
          title.fontface = "bold")
sfws

