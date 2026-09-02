
### Afghanistan elevation map with my stations location###


# Install and load necessary libraries
install.packages(c("elevatr", "sp", "raster", "ggplot2"))
library(elevatr)
library(sp)
library(raster)
library(ggplot2)
library(automap)
library(tmap)
library(RColorBrewer)
library(rgeoboundaries)
library(sf)
library(viridis)
library(rgdal)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(gstat) # Use gstat's idw routine
require(tmaptools)
library(dplyr)
library(stringr)
library(terra)


# 1: Get the shape file of Afghanistan
library(geodata)
# Load the outline for Afghanistan # Get Afghanistan shape file
sf_afghanistan <- geodata::gadm(country = "AF", level = 1, path = tempdir()) 
neighbors <- c("IR", "PK", "TJ", "TM", "UZ", "CN")
sf_neighbors <- lapply(neighbors, function(x) geodata::gadm(country = x, level = 0, path = tempdir()))
sf_neighbors <- do.call(rbind, lapply(sf_neighbors, sf::st_as_sf))
sf_neighbors <- sf_neighbors[!sf_neighbors$GID_0 %in% c("CHN", "Z03", "Z08"), ]

# Make geometry valid + force a consistent type
sf_afghanistan <- sf::st_make_valid(sf_afghanistan)
sf_afghanistan <- sf::st_cast(sf_afghanistan, "MULTIPOLYGON")

# (optional) dissolve to one boundary to speed things up
af_union <- sf::st_union(sf_afghanistan)

sf_afghanistan <- af_union
# Convert Afghanistan shape to sf
sf_afghanistan <- sf::st_as_sf(sf_afghanistan) 

# 2: Download elevation raster for whole Afghanistan
Afg_elevation <- get_elev_raster(sf_afghanistan, z = 7, src = "aws")

Afg_elevation <- mask(Afg_elevation, sf_afghanistan) # this is very big vector size of 2.5 Gb

#plot(Afg_elevation)


# 3: Plotting
orchards_coor <- data.frame(name = 'PHDC',
                            latitude = '36.70893',
                            longitude = '68.86145')
# covert to spatial object
orchards_coor <- st_as_sf(orchards_coor, coords = c('longitude', 'latitude'), crs = 4326)

# # Reproject orchards_coor point to match our map's CRS
orchards_coor <- st_transform(orchards_coor, st_crs(Afg_elevation))



color_comb <- c("#e65b43","#f18b5a","#fbc17c","#fde6a4","#f7f7f7", "#d1e5f0","#92c5de","#4393c3","#2166ac" )
#color_comb <- c("#b2182b",   "#fddbc7","#f7f7f7", "#e0f3f8",   "#2166ac")

#color_comb <- c("#e65b43",  # Sand/Desert (Light Brown)
#                #"#e6aa68",  # Dry Grassland (Orange-Brown)
#                "#a5c882",  # Light Green (Shrubland)
#                "#4d9221",  # Dense Green Forest
#                "#377eb8",  # Alpine Tundra (Blue-Green)
#                "#f7f7f7",  # Snow/Glacier (White))
#                "#d9d9d9") # Higher Snow Caps (Grayish White)


# Create the kunduz elevation map
afg_map <- tm_shape(Afg_elevation) +
  tm_raster(palette= color_comb,
            style = 'cont',
            stretch = TRUE,
            #breaks = c(250, 700, 1400, 2100, 2800, 3500, 4200, 5000, 7492),
            title="Elevation (m a.s.l.)") +

  tm_shape(sf_neighbors)+
  tm_borders(col='grey', lwd = 1.5, lty = "dashed", alpha = 0.7) + # borders lines
  
  #tm_text("COUNTRY", size = 0.8, col = "black", shadow = TRUE, auto.placement = TRUE, xmod = -0.2, ymod = -0.2, remove.overlap = TRUE) + # names 
  tm_shape(sf_neighbors[sf_neighbors$COUNTRY == "Iran",]) +
  tm_text("COUNTRY", size = 0.5, col = "black", shadow = TRUE, xmod = 8, ymod = -3) +
  
  tm_shape(sf_neighbors[sf_neighbors$COUNTRY == "Pakistan",]) +
  tm_text("COUNTRY", size = 0.5, col = "black", shadow = TRUE, xmod = -0.2, ymod = 0.4) +
  
  tm_shape(sf_neighbors[sf_neighbors$COUNTRY == "Tajikistan",]) +
  tm_text("COUNTRY", size = 0.5, col = "black", shadow = TRUE, xmod = 0.1, ymod = 0.2) +
  
  tm_shape(sf_neighbors[sf_neighbors$COUNTRY == "Turkmenistan",]) +
  tm_text("COUNTRY", size = 0.5, col = "black", shadow = TRUE, xmod = 4, ymod = -1.5) +
  
  tm_shape(sf_neighbors[sf_neighbors$COUNTRY == "Uzbekistan",]) +
  tm_text("COUNTRY", size = 0.5, col = "black", shadow = TRUE, xmod = 4.5, ymod = -4.8) +
  
  tm_shape(sf_neighbors[sf_neighbors$COUNTRY == "China",]) +
  tm_text("COUNTRY", size = 0.5, col = "black", shadow = TRUE, xmod = -1.3, ymod = 1.85) +  

  
  tm_shape(sf_afghanistan)+
  tm_borders(col='black', lwd = 0.4) +
  
  # tm_shape(sf_afghanistan[sf_afghanistan$NAME_1 == "Kunduz", ]) +
  # tm_borders(col='black', lwd = 1.5, lty = "solid", alpha = 0.9) +
  
  tm_shape(orchards_coor) + tm_dots(shape = 23, col= "green", size=0.15) + # # Add a shape layer with point symbols
  #tm_text('name', size=0.68, ymod=-0.44, xmod = 0, auto.placement = FALSE) + # # Add stations abbreviations from each column
  
  
  tm_legend(legend.outside=F) +
  tm_scale_bar(position = c(0.35, 0.082),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 0.8) +  # Add a scale bar on the left side
  tm_compass(position = c(0.12, 0.77), text.size = 0.8) +
  tm_graticules(lines = TRUE, labels.size = 0.8, labels.col = "black", labels.inside.frame = FALSE,
                alpha=0.3, n.y=5, n.x = 5, labels.margin.y=0.1,) +

  #tm_add_legend(type = "symbol", labels = "  Weather station", shape = 23, size = 0.9, col = "red") +
  tm_add_legend(type = "symbol", labels = "PHDC Kunduz", shape = 23, size = 0.60, col = "green") +
  
  
  tm_layout(#main.title = "Locations of the weather stations",
    #main.title.position = "center",
    #main.title.size = 1.3,
    #main.title.color = "black",
    legend.show = TRUE,
    legend.title.size = 1.2,
    legend.text.size = 0.9,
    legend.text.color = 'black',
    legend.position = c(1.06, 0.4), # old value c(1.04, 0.2)
    legend.width = 1.2,
    legend.height = 1.5,
    #attr.color = 'white',
    bg.color = "white",
    outer.bg.color = "white",
    frame = FALSE)

afg_map

#elevation_map
tmap_save(afg_map, filename = 'plot/elevation_map/250515_elevation_afg.png', height = 10, width = 22, units = 'cm')


#----------------------------------------------------------------------------#
