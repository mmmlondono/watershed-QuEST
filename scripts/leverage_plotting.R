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
# for plotting with ggplot
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
#library(vapoRwave)
library(tidyverse)

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
area7 <- st_read("data_geo/area7.shp")
area13 <- st_read("data_geo/area13.shp")
area14 <- st_read("data_geo/area14.shp")
area15 <- st_read("data_geo/area15.shp")
area16 <- st_read("data_geo/area16.shp")

#load streams
streams <- st_read("data_geo/area_stream_network.shp")

#load leverage data  
leverage <- read.csv("data/leverage.csv")
#filter out by what you will use
#DOC
DOC <- leverage %>%
  dplyr::select(Sub_ProjectB, lat, lon, NPOC..mg.C.L.)
#transform lat lon to geometries
DOC <- st_as_sf(DOC, coords = c("lon", "lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- DOC %>%
  dplyr::select(Sub_ProjectB, geometry)

#TDN
TDN <- leverage %>%
  dplyr::select(Sub_ProjectB, lat, lon, TDN..mg.N.L.)
#transform lat lon to geometries
TDN <- st_as_sf(TDN, coords = c("lon", "lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- TDN %>%
  dplyr::select(Sub_ProjectB, geometry)


####plot on mapview####
#make color palette
pal <- magma(n = length(unique(lev$NPOC..mg.C.L.)), direction = -1)

#plot DOC
mapview(area1, alpha.regions = 0., alpha = 5, lwd = 3, color = "white", legend = FALSE) + 
  mapview(area, col.regions = pal) + 
  mapview(streams, legend = FALSE) + 
  mapview(DOC, cex = "NPOC..mg.C.L.", col.regions = "orange")


# Custom colors for each area
area_colors <- c("USF03" = "lightgreen", "USF05" = "tomato", "USF06" = "powderblue", "USF07" = "white",
                 "USF13" = "cornflowerblue", "USF14" = "gold", "USF15" = "lightslateblue", 
                 "USF16" = "orangered")
DOC$color <- area_colors[DOC$Sub_ProjectB]
# Plot with mapview
mapview(area1, alpha.regions = 0, alpha = 5, lwd = 3, color = "white", legend = FALSE) + 
  mapview(area3, col.regions = "lightgreen", legend = FALSE) + 
  mapview(area5, col.regions = "tomato", legend = FALSE) +
  mapview(area6, col.regions = "powderblue", legend = FALSE) +
  mapview(area7, col.regions = "white", legend = FALSE) +
  mapview(area13, col.regions = "cornflowerblue", legend = FALSE) +
  mapview(area14, col.regions = "gold", legend = FALSE) +
  mapview(area15, col.regions = "lightslateblue", legend = FALSE) +
  mapview(area16, col.regions = "orangered", legend = FALSE) +
  mapview(streams, legend = FALSE) + 
  mapview(DOC, zcol = "Sub_ProjectB", col.regions = area_colors, cex = "NPOC..mg.C.L.", legend = TRUE)

#plot TDN
#map
mapview(area1, alpha.regions = 0., alpha = 5, lwd = 3, color = "white", legend = FALSE) + 
  mapview(area, col.regions = pal) + 
  mapview(streams, legend = FALSE) + 
  mapview(TDN, cex = "TDN..mg.N.L.", col.regions = "mediumaquamarine")

# Custom colors for each area
area_colors <- c("USF03" = "lightgreen", "USF05" = "tomato", "USF06" = "powderblue", "USF07" = "white",
                 "USF13" = "cornflowerblue", "USF14" = "gold", "USF15" = "lightslateblue", 
                 "USF16" = "orangered")
TDN$color <- area_colors[TDN$Sub_ProjectB]
# Plot with mapview
mapview(area1, alpha.regions = 0, alpha = 5, lwd = 3, color = "white", legend = FALSE) + 
  mapview(area3, col.regions = "lightgreen", legend = FALSE) + 
  mapview(area5, col.regions = "tomato", legend = FALSE) +
  mapview(area6, col.regions = "powderblue", legend = FALSE) +
  mapview(area7, col.regions = "white", legend = FALSE) +
  mapview(area13, col.regions = "cornflowerblue", legend = FALSE) +
  mapview(area14, col.regions = "gold", legend = FALSE) +
  mapview(area15, col.regions = "lightslateblue", legend = FALSE) +
  mapview(area16, col.regions = "orangered", legend = FALSE) +
  mapview(streams, legend = FALSE) + 
  mapview(TDN, zcol = "Sub_ProjectB", col.regions = area_colors, cex = "TDN..mg.N.L.", legend = TRUE)


####plot with tmap###
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

####plot with ggplot####
###NPOC###
combined_plot <- ggplot() +
  geom_sf(data = area1, color = "black", fill = NA, cex = 5) +  # Outline area1
  geom_sf(data = area,  aes(fill = shd_nwm), show.legend = FALSE) +         # Add area
  geom_sf(data = DOC, aes(size = NPOC..mg.C.L.), alpha = 0.5) +  # Add DOC with fill
  scale_fill_viridis_c(name = "NPOC (mg C/L)") +                 # Color scale for DOC
  theme_minimal() +                                              # Minimal theme
  ggtitle("Combined Map") +                                      # Title
  theme(legend.position = "bottom")                              # Position the legend at the bottom
#display
print(combined_plot)

###TDN###
TDN <- ggplot() +
  geom_sf(data = area1, color = "black", fill = NA, cex = 5) +  # Outline area1
  geom_sf(data = area,  aes(fill = shd_nwm), show.legend = FALSE) +         # Add area
  geom_sf(data = TDN, aes(size = TDN..mg.N.L.), alpha = 0.5) +  # Add DOC with fill
  scale_fill_viridis_c(name = "TDN (mg C/L)") +                 # Color scale for DOC
  theme_minimal() +                                              # Minimal theme
  ggtitle("TDN") +                                      # Title
  theme(legend.position = "bottom")                              # Position the legend at the bottom
#display
print(TDN)
