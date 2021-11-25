library(raster)
library(tidyverse)
library(leaflet)
library(sf)
library(tmap)
library(RColorBrewer)
library(spdep)
library(cdlTools)
library(rgdal)

#-------------------------------------------------------------------------------
# Task 1. From lab 2, task 2.3:

# Make a map of the counties, shading each county by the total cost of BMPs 
# funded/implemented in that county. This will required you to join multiple 
# datasets together Leaflet/other extras to add:

# Mouse-over label the displays the total cost of BMPs funded in that county
# Use an equal-interval classification method with 5 classes. Determine the breaks programmatically.
# Do NOT use the default color scheme

#Reading data for counties and bmps 
counties <- sf::read_sf("./data/County_Boundaries.shp") %>%
  sf::st_make_valid()
bmps <- read_csv("./data/BMPreport2016_landbmps.csv")

# Making Dataset
bmps_1 <- bmps %>% mutate(., County_FIPS = stringr::str_sub(GeographyName, 1, 5)) %>%
  subset(select=c(County_FIPS, Cost)) %>% group_by(County_FIPS) %>%
  summarise(Total_Cost = sum(Cost, na.rm=T))
counties_1 <- counties %>% mutate(., County_FIPS = GEOID10,
                                  County_Name = NAME10) %>%
  subset(select=c(County_FIPS, County_Name))

Dataset_1 <- left_join(counties_1, bmps_1, by="County_FIPS")
Dataset_1[is.na(Dataset_1)] <- 0

# Breaks(bins) Setting
cost_min <- min(Dataset_1$Total_Cost)
cost_max <- max(Dataset_1$Total_Cost)
class <- 5
bins <- seq(cost_min, cost_max, (cost_max - cost_min) * (1/class)) 

# Setting Color
pal <- colorBin("BuPu", domain = Dataset_1$Total_Cost, bins = bins)

# Setting Label
labels <- sprintf("<strong>%s County</strong><br/>%.0f",
                  Dataset_1$County_Name, Dataset_1$Total_Cost) %>% 
  lapply(htmltools::HTML)

# Plotting Map
leaflet(Dataset_1) %>%
  setView(lng = -77.57297, lat = 40.0477, zoom = 6) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(Total_Cost),
              weight = 2, # borders width
              opacity = 0.5, # borders transparency 
              color = "black", # borders color 
              dashArray = "1", fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5, color = "blue",  # borders color highlight
                dashArray = "", fillOpacity = 0.0,  # polygon transparency in highlight
                bringToFront = TRUE), 
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal=pal, values = ~Total_Cost, opacity = 0.7, 
            title = "Cost of BMPs funded/implemented",
            position = "bottomright")

#-------------------------------------------------------------------------------
#Task 2. From lab 3, task Bonus #2:

#Original task to be recreated using Leaflet: plot a choropleth map of your 
#dataset with a categorical color scheme, where the shading corresponds to 
#the Moran plot (really, ¡°LISA¡±) quadrants. Thus, your map will have four shades
# of color.

#Leaflet/other extras to add:
# Add a pop-up window that displays the p-value (you¡¯ll have to look at the 
#  moran.test() documentation) when you click on that county with a mouse
# Add a control to change between 3 different basemaps

#file reading
# d.all <- sf::read_sf("./data2/County_2010Census_DP1/County_2010Census_DP1.shp")

# Making subset.    
# State.subset <- c('Wisconsin', 'Iowa', 'Ohio', 'Indiana', 'Illinois') 
# States.s <- d.all %>%
#  mutate(., STATE = fips(stringr::str_sub(GEOID10, 1, -4), to='NAME')) %>%
#  dplyr::filter(STATE %in% State.subset) %>% relocate(STATE) %>% sf::as_Spatial()

#Save and Export as shapefile.
#writeOGR(obj=States.s, dsn='./Data', 
#         layer='States.s', driver = 'ESRI Shapefile')

#New file reading
S.States <- sf::read_sf("./data/States.s.shp")
glimpse(S.States)  # 5 states, 453 counties

#Choosing variables and re-projection
Senior <- S.States %>% 
  mutate(POP.1000 = (DP0010015+DP0010016+DP0010017+DP0010018+DP0010019)
         /(DP0010001)*1000) %>% select(POP.1000, County=NAMELSAD10)

crs <- st_crs(Dataset_1)
Senior.projected <- Senior %>% sf::st_transform(crs=crs)
# tmap::tm_shape(Senior.projected) + tm_polygons()

#make the neighborhood & Assigning weight (Row-standardize)
nb <- spdep::poly2nb(Senior.projected, queen=TRUE)
nb
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# lw$weights[1:length(lw$weights)]

# Local Moran's I statistic 
Local.moran <- localmoran(Senior.projected$POP.1000, lw, zero.policy=TRUE)
Local.moran

# Making Dataset
Dataset_2 <- Senior.projected %>% 
  mutate(p_value = Local.moran[,5], 
         Quadrents = 
           factor(attr(Local.moran, "quadr")$mean, 
                  levels = c("Low-Low", "Low-High", "High-Low", "High-High"))) %>% 
  select(County, POP.1000, p_value, Quadrents)

col.2 <- c("dodgerblue", "lightskyblue1", "lightpink", "firebrick1")
factpal <- colorFactor(col.2, Dataset_2$Quadrents)

# Setting pop-up window
popups <- sprintf("<strong>%s</strong><br/>p-value=%.6f",
                  Dataset_2$County, Dataset_2$p_value) %>% 
  lapply(htmltools::HTML)

# Plotting Map
leaflet(Dataset_2) %>%
  setView(lng = -88.57897, lat = 42.14006, zoom = 6) %>%
  addTiles(group = "Street") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI") %>%
  addPolygons(fillColor = ~factpal(Quadrents), weight = 2,
              opacity = 0.5, color = "black", dashArray = "1", fillOpacity = 0.7,
              highlightOptions = 
                highlightOptions(weight = 5, color = "yellow",
                                 opacity = 1.0, fillOpacity = 0.1,
                                 bringToFront = TRUE), 
              popup = popups) %>% 
  addLayersControl(baseGroups = c("Street", "Toner", "ESRI"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal=factpal, values = ~Quadrents, opacity = 0.7, 
            title = "Quadrents", position = "topright")

#-------------------------------------------------------------------------------
# Task 3: From lab 4, task 2:
# Original task to be recreated using Leaflet: Make a second map of your choosing. 
# You may choose any spatial extent, domain, or technique. I¡¯m looking for 
# creativity, good coding practices (including comments), and for you to 
# demonstrate independent thinking. There are minor restrictions you must follow:
# 1. It must include vector AND raster data in some manner
# 2. It must include spatial data relating to a social process 
#   (e.g., political boundaries) AND spatial data relating to an environmental
# process (e.g., water resources)
# 3. The map should ¡°stand on its own¡± and communicate its purpose without 
#    additional text
# 4. That¡¯s it!

# Leaflet/other extras to add:
# Add a control that turns on/off each layer
# Since everyone¡¯s maps are different, I can¡¯t specify exactly what else you
# Should add. But, find one thing cool, interesting, or applicable to YOUR 
# map, and implement it.

# County Data Frame:
ne.counties <- sf::read_sf("./data/County_Boundaries-_Census.shp") %>%
  sf::st_make_valid()  %>% sf::st_transform(crs=crs)
SB.county <- ne.counties[ne.counties$NAMELSAD10 == "Scotts Bluff County",]

# making crs identical. 
# crs <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Reading Flood-Inundation data and subsetting by polygon
Flood <- sf::read_sf('./data/ScbfNE.shp') %>% sf::st_make_valid() %>%
  st_transform(crs=crs)

# Reading Raster and assigning colors 
Dem <- raster("./data/scottsDEM.tif")
pal.r <- colorNumeric(c("darkolivegreen4", "yellow",
                        "orange", "brown", "grey68"), values(Dem), 
                      na.color = "transparent")

# It may take up to 30 seconds, and show warning messages like text.
leaflet() %>%
  setView(lng = -103.6662, lat = 41.85435, zoom = 13) %>%
  addProviderTiles("Wikimedia", group = "Street + Raster") %>%
  addRasterImage(Dem, colors = pal.r, opacity = 1.0, group = "Raster") %>%
  addRasterImage(Dem, colors = pal.r, opacity = 0.5, group = "Street + Raster") %>%
  addProviderTiles("Wikimedia", group = "Street") %>%
  addPolygons(data = Flood, fillColor = "blue", fillOpacity = 0.3,
              weight = 1, opacity = 0.2, color = "blue", 
              group = c("STAGE_ 9(ft.)", "STAGE_10(ft.)", "STAGE_11(ft.)",
                        "STAGE_12(ft.)", "STAGE_13(ft.)", "STAGE_14(ft.)",
                        "STAGE_15(ft.)", "STAGE_16(ft.)", "STAGE_17(ft.)",
                        "STAGE_18(ft.)")) %>%
  addLegend(pal = pal.r, values = values(Dem), title = "Height (m)", 
            position = "bottomleft", opacity = 0.9, group = "Legend (Height)") %>%
  addLayersControl(baseGroups = c("Street", "Street + Raster", "Raster"),
                   overlayGroups = c("Legend (Height)", "STAGE_ 9(ft.)", "STAGE_10(ft.)", "STAGE_11(ft.)",
                                     "STAGE_12(ft.)", "STAGE_13(ft.)", "STAGE_14(ft.)",
                                     "STAGE_15(ft.)", "STAGE_16(ft.)", "STAGE_17(ft.)",
                                     "STAGE_18(ft.)"),
                   options = layersControlOptions(collapsed = FALSE)) 


# Thank you very much !!



