library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tidyverse)
library(tmap)   
library(leaflet) 
library(ggplot2)
library(rgdal)
library(GISTools)

#1. Using the same descriptions as the in-class frankenmap, 
#   create a map that follows ¡°good¡± cartographic principles. 
#   I have included the instructions below for reference.

# State Data Frame:

#Reading Nebraska county 
ne.counties <- sf::read_sf("./data/County_Boundaries-_Census.shp") %>%
  sf::st_make_valid()
    # glimpse(ne.counties)
    # Confirming: tm_shape(ne.counties) +tm_polygons()

    # Reading Nebraska boundry 
ne.boundary <- sf::read_sf("./data/Nebraska_State_Boundary.shp") %>%
  sf::st_make_valid()
    # Confirming
    # tm_shape(ne.bodry) +tm_polygons()

    # Checking class & coordinates 
    # class(ne.counties)
    # class(ne.bodry)
    # sf::st_crs(ne.counties) == sf::st_crs(ne.boundary)

Dataset.map1 <- ne.counties %>% dplyr::select(Median_Income=MedHousInc)

map1 <- tm_shape(Dataset.map1, unit = "miles", unit.size=1609) + 
  tm_polygons(col="Median_Income",
              palette=c("azure4", "grey77", "white", "khaki1", "gold2")) +
  tm_shape(Dataset.map1) + tm_borders("black", lwd=2) + 
  tm_scale_bar(breaks = c(0, 25, 50), text.size = 0.48,
               position=c("left", "bottom")) +
  tm_layout(legend.title.size=1.1, legend.text.size = 0.8) + 
  tm_shape(ne.boundary) + tm_polygons(lwd=4, alpha=0) + 
  tm_layout(legend.outside = TRUE, legend.position = c(-2.2, 1.01))
            

# County Data Frame:

Lancaster <- ne.counties[ne.counties$NAMELSAD10 == "Lancaster County",]

    # Data Reading / intersection
    # municipal  
municipal <- sf::read_sf("./data/Municipal_Boundaries.shp") %>%
  sf::st_make_valid()
Lancaster.Mun <- sf::st_intersection(municipal, Lancaster)
    # Park
parks <- sf::read_sf("./data/State_Park_Locations.shp") %>%
  sf::st_make_valid()
Lancaster.parks <- sf::st_intersection(parks, Lancaster)
    # Stream
streams <- sf::read_sf("./data/Streams_303_d_.shp") %>%
  sf::st_make_valid()
Lancaster.streams <- sf::st_intersection(streams, Lancaster)
    # Dem.
Dem <- raster("./data/Dem/lc_dem.tif")
raster::crs(Dem) <- crs(Lancaster)


# To symbolize differently by impairment
Lancaster.streams.diff <- Lancaster.streams %>% 
  mutate(Impaired=factor(ifelse(str_detect(Impairment,'Recreation_E. coli, Aquatic Life'), 
                         'Both',
                         ifelse(str_detect(Impairment, 'Recreation_E. coli'),
                                'Recreation_E. coli', 'Aquatic Life')), 
         levels = c('Aquatic Life', 'Recreation_E. coli', "Both"))) %>% 
  dplyr::select(Impairment = Impaired)

map2 <- tm_shape(Dem) + 
  tm_raster(alpha = 0.7, palette = colorRampPalette(c("darkolivegreen4","yellow", "brown"))(12),
            legend.show = F) +
  tm_shape(Lancaster.Mun) + tm_polygons("NAME", legend.show = F, alpha=0.5) +
  tm_text("NAME", size = 0.8, fontface = "bold", xmod = 0.0, ymod = 0.7) +
  tm_shape(Lancaster.parks) + tm_dots(size=0.5, col="darkgreen", alpha=0.7) + 
  tm_shape(Lancaster.streams.diff) + tm_lines(lwd = 2, col="Impairment",
                                              palette=c("red", "blue", "black"),
                                              alpha=0.5) +
  tm_layout(legend.position = c("right", "top"), legend.text.size = 0.8, 
            legend.bg.color = "beige", legend.title.fontface = "bold") 

  
# Putting it together
Lancaster.region = st_bbox(c(xmin = -96.91394, xmax = -96.46363,
                             ymin = 40.52302, ymax = 41.04612),
                           crs = st_crs(ne.counties)) %>% st_as_sfc()
Inset.map <- tm_shape(Lancaster.region) + tm_borders(lwd = 5, col="blue")
North.arrow <- tm_compass(type = "8star", position = c("left", "bottom"), size = 2)
Title <- tm_layout(title="  Map of Lancaster County", title.size = 1.1)


map2 + North.arrow + Title
library(grid)
print(map1 + Inset.map, vp = viewport(0.902, 0.131, width = 0.43, height = 0.47))


#2. Make a second static map of your choosing. 
#   You may choose any spatial extent, domain, or technique.
#   I¡¯m looking for creativity, good coding practices (including comments), 
#   and for you to demonstrate independent thinking. 
#   There are minor restrictions you must follow:

# County Data Frame:
ne.counties <- sf::read_sf("./data/County_Boundaries-_Census.shp") %>%
  sf::st_make_valid()
SB.county <- ne.counties[ne.counties$NAMELSAD10 == "Scotts Bluff County",]

# making crs identical. 
crs <- st_crs(SB.county)

# Reading Flood-Inundation data and Plotting Flooding map
# It is confusing if drawing all 10 stages, so I adjusted it to 3 stages.
Flood <- sf::read_sf('./data/ScbfNE.shp') %>% sf::st_make_valid() %>%
  st_transform(crs=crs) %>%
  mutate(STAGE_Ft. = factor(ifelse(STAGE < 12, '1_9-11 ft',
                           ifelse(STAGE < 15, '2_11-14 ft', '3_15-18 ft')))) %>%
  arrange(desc(STAGE))

Flooding.map <- tm_shape(Flood) + tm_polygons(col="STAGE_Ft.", alpha = 0.8, lwd = 0,
                              palette=c("black", "grey50", "grey80")) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.title.size=1.4, legend.text.size = 1.1, 
            legend.bg.color = "beige",
            title= "Flood-Inundation Maps for\nthe North Platte River (2018) ",
            title.position = c("center", "top"), title.size = 1.6,
            title.bg.color ="beige") 

#Reading City data 
City <- sf::read_sf("./data/City_Boundaries.shp") %>%
  sf::st_make_valid() %>% st_transform(crs=crs) 
SB.city <- sf::st_intersection(City, SB.county)

# Subset of cities around the mapping area 
City3 <-SB.city[SB.city$City_Name == "Scottsbluff"|
                  SB.city$City_Name == "Terrytown"|
                  SB.city$City_Name == "Gering",]

# Plotting City map 
City.map <- tm_shape(City3) + tm_polygons(alpha=0.0) + 
  tm_shape(City3) + tm_borders(col="black", lwd=2, alpha = 0.5) +
  tm_text("City_Name", size = 0.8, fontface = "bold",
          xmod = -1.0, ymod = -0.5)

#Reading Streams data and Plotting Streams map
streams <- sf::read_sf("./data/Streams_303_d_.shp") %>%
  sf::st_make_valid() %>% st_transform(crs=crs) 
SB.stream <- sf::st_intersection(streams, SB.county)

stream.map <- tm_shape(SB.stream) + tm_lines(col="blue", lwd = 5, alpha = 0.3) +
  tm_text("Waterbody_", size = 0.8, fontface = "bold", 
          col = "gray80", xmod = -14.4, ymod = 13)

#Reading Road data and Plotting Roads map
Roads <- sf::read_sf("./data/Highways.shp") %>%
  sf::st_make_valid() %>% st_transform(crs=crs) 
SB.Road <- sf::st_intersection(Roads, SB.county)

Roads.map <- tm_shape(SB.Road) + tm_lines(lwd = 3, col="red", alpha = 0.5) +
  tm_text("HwyLabel", size = 0.8, fontface = "bold",
          xmod = 3, ymod = 0.5)

# Reading Scotts_Bluff Dem and Plotting Dem map 
SB.Dem <- raster("./data/Dem/scottsDEM.tif") 
Mapping.Area <- c(604383.9, 616883.9, 4631214, 4637815)
Dem.Area <- crop(SB.Dem, Mapping.Area)

Dem.map <- tm_shape(Dem.Area) + 
  tm_raster(palette = colorRampPalette(c("darkolivegreen4", "yellow", 
                                         "orange", "brown", 
                                         "grey68"))(20),
            legend.show = F, n=50)

# Map elements
map.element <- tm_compass(type = "8star", position = c("right", "top"), size = 4) +
  tm_scale_bar(breaks = c(0, 0.5, 1.5), text.size = 1, position = c("right", "top")) 
  

# Inset map
SB.region = st_bbox(c(xmin = -104.053, xmax = -103.3615,
                             ymin = 41.69778, ymax = 42.00423),
                             crs = st_crs(ne.counties)) %>% st_as_sfc()

ne.map <- tm_shape(ne.counties) + tm_polygons(col="white")
Inset.map <- tm_shape(SB.region) + tm_borders(lwd = 5, col="red")

# Lap04.map

Lap04.map <- Dem.map + City.map + Roads.map + Flooding.map + stream.map + map.element

Lap04.map 
library(grid)
print(ne.map + Inset.map, vp = viewport(0.85, 0.55, width = 0.25, height = 0.25))

# Thank you very much !!


