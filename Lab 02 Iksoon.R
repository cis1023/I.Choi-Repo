rm(list=ls())

library(tidyverse)
library(GISTools)
library(sf)
library(tmap)
library(dplyr)
library(cdlTools)
  
counties <- sf::read_sf("./data/CBW/County_Boundaries.shp") %>%
  sf::st_make_valid()
dams <- sf::read_sf("./data/CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% 
  sf::st_make_valid()
streams <- sf::read_sf("./data/CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>%
  sf::st_make_valid()
bmps <- read_csv("./data/CBW/BMPreport2016_landbmps.csv")


# -----------------------------------------------------------------------------------

# 1. Aspatial operations

#  1-1 Calculate summary statistics for the Cost of 
                       #  BMPs for each State (including DC)
options(scipen=999)
with(bmps, aggregate(x=Cost, by=list(StateAbbreviation), 
                     summary, na.rm=TRUE))
# or
with(bmps, tapply(Cost, StateAbbreviation, summary, na.rm=TRUE))

# -----------------------------------------------------------------------------------
#  1-2 Make a scatterplot of Cost vs. TotalAmountCredited, ONLY FOR Units of type ¡°Acres¡±.
#  You may need to apply a data transformation to one or more axes 
#  if the data are heavily skewed.

bmps %>% dplyr::filter(Unit == "Acres") %>% 
  ggplot(., aes(x=TotalAmountCredited, y=Cost)) +
  geom_point()    # Very skewed, need to subset the data

# check it w/ another graph type
bmps %>% dplyr::filter(Unit == "Acres") %>% 
  ggplot(., aes(x=TotalAmountCredited, y=Cost)) +
  geom_boxplot(aes(fill=StateAbbreviation))  

# Very skewed, need to subset the data

bmps %>% dplyr::filter(Unit == "Acres", 
                       TotalAmountCredited > 1 & TotalAmountCredited < 100,
                       Cost > 0.1 & Cost < 2000) %>% 
  ggplot(., aes(x=TotalAmountCredited, y=Cost)) +
  geom_point() +
  labs(title="Cost vs.TotalAmountCredited only for Unit of type_Acres")

# by colored STATE
bmps %>% dplyr::filter(Unit == "Acres", 
                       TotalAmountCredited > 1 & TotalAmountCredited < 100,
                       Cost > 0.1 & Cost < 2000) %>% 
  ggplot(., aes(x=TotalAmountCredited, y=Cost)) +
  geom_point(aes(color=StateAbbreviation)) +
  labs(title="Cost vs.TotalAmountCredited only for Unit of type_Acres")

#confirm w/ boxplot
bmps %>% dplyr::filter(Unit == "Acres", 
                       TotalAmountCredited > 1 & TotalAmountCredited < 100,
                       Cost > 1 & Cost < 6500) %>% 
  ggplot(., aes(x=TotalAmountCredited, y=Cost)) +
  geom_boxplot(aes(fill=StateAbbreviation)) +
  labs(title="Cost vs.TotalAmountCredited only for Unit of type_Acres")

#--------------------------------------------------------------------------------
#  1.3 Make a boxplot with ¡°StateAbbreviation¡± on the x-axis and
#     ¡°TotalAmountCredited¡± on the y-axis. HOWEVER, the only data I want plotted are
#      for cover crop BMPs. Note, there are many types of cover
#       crops in this dataset, and I want you to include them ALL. 
#         There are handy functions within the stringr package 
#         that can help you here.

bmps %>% 
  dplyr::filter("covercrop"==stringr::str_sub(BMPShortName, 1, 9)) %>%
  ggplot(aes(x=StateAbbreviation, y=TotalAmountCredited)) +
  geom_boxplot(aes(fill=StateAbbreviation)) +
  labs(x = "STATE", fill="STATE")

# Very skewed, need to subset the data

bmps %>% 
  dplyr::filter("covercrop"==stringr::str_sub(BMPShortName, 1, 9) & 
                                                (TotalAmountCredited > 1 &
                                                TotalAmountCredited < 100)) %>%
  ggplot(aes(x=StateAbbreviation, y=TotalAmountCredited)) +
  geom_boxplot(aes(fill=StateAbbreviation)) + 
  labs(x = "STATE", fill="STATE", 
       title="TotalAmountCredited only for cover crop BMPs by STATE")

#--------------------------------------------------------------------------------
#  1.4 make a scatterplot of the dam dataset, this time 
#       with ¡°YEAR¡± on the x-axis and ¡°STATE¡± on y-axis
#       (think of it like a timeline). 
#       Assume no dams were built in year 0, 
#       so you¡¯ll need to remove those data points.

dams %>% dplyr::filter(YEAR != 0) %>%
  ggplot(., aes(x=YEAR, y=STATE)) + geom_point(size=3) +
  labs(title="Dams built by YEAR")


#  1.5 make one last (aspatial) visualization. But this time, 
#    it¡¯s your choice what data and plots to use. 
#    The only requirement is that you link two of the datasets together
#    in some manner. Be creative. Make it look nice (e.g., use proper labels,
#    interesting colors/shading/size).

# Assuming that there will be no dams removed in 2018, 2019

# Number of dams Removed by year
Removed_dams <-  dams %>% as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  group_by(DamRemoval) %>%
  summarise(removed_dams = n()) %>% 
  rbind(c(2018, 0), c(2019,0))  # Assuming that there will be no dams removed in 2018, 2019

# Length of streams opened by year
Opened.stream.length <- streams %>% as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  group_by(DamRemoval) %>%
  summarise(Opened.stream.length = sum(LengthKM)) 

# Join two datasets.
DRSOBY <- left_join(
  Removed_dams, Opened.stream.length, by="DamRemoval") %>% 
  rename(Year = DamRemoval)

# Graph.
library(grid)
max_ratio <- max(DRSOBY$Opened.stream.length)/max(DRSOBY$removed_dams); max_ratio
ggplot(DRSOBY, aes(x=Year)) +
  geom_bar(aes(y=Opened.stream.length), fill="skyblue", stat="identity") +
  geom_point(aes(y=removed_dams*max_ratio), 
             shape=22, size=5, color="black", bg="grey", stroke=3) +
  theme_minimal() +
  scale_x_continuous(breaks=c(2012:2019)) +
  scale_y_continuous(sec.axis=sec_axis(~./max_ratio, 
                                       name="Number of Dams removed", 
                                       breaks=c(1:10))) +
  labs(title="Streams opened following Dams removed by Year",
       x= "Year(2012-2019)", y="Streams opened(Km)") +
  theme(title = element_text(size= 15, face="bold", color="blue")) +
  theme(axis.title.x = element_text(size= 14, face="bold", color="black")) +
  theme(axis.title.y = element_text(size= 14, face="bold", color="black")) +
  theme(axis.text.x = element_text(size= 12, color="black")) +
  theme(axis.text.y = element_text(size= 12, color="black")) +
  annotate("text", x = 2013, y = 460, colour="blue", label="Streams opened") + 
  annotate("text", x = 2017.5, y = 420, colour="black", label="Dam") +
  annotate("segment", x=2013, y= 390, xend = 2013, yend = 440, color="blue", 
           arrow=arrow()) +
  annotate("segment", x=2017.15, y= 360, xend = 2017.3, yend = 400, 
           color="black", arrow=arrow()) 

# -------------------------------------------------------------------------
# Task 2: Spatial operations

   #2-1. 5 longest streams

streams <- sf::read_sf("./data/CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>%
  sf::st_make_valid()

Long_stream <- streams %>% as_tibble() %>% 
  subset(select=c(OBJECTID_1, GNIS_Name)) %>% mutate(Length=st_length(streams)) %>% 
  arrange(desc(Length))  
Long_stream[c(1:5),]

# -------------------------------------------------------------------------
   #2-2. Three counties with the greatest TOTAL length of streams 

# checKing the CRS
counties %>% sf::st_crs() == dams %>% sf::st_crs()
counties %>% sf::st_crs() == streams %>% sf::st_crs()
dams %>% sf::st_crs() == streams %>% sf::st_crs()

# To run below code may take up to a minute.

streams_by_county <- sf::st_intersection(streams, counties)

GREAT_Total.length.stream.county <- streams_by_county %>% as_tibble() %>%
  subset(select=c(GEOID10, NAME10)) %>% 
  mutate(Length.in.county=st_length(streams_by_county)) %>%
  group_by(GEOID10, NAME10) %>% summarise(Total_length=sum(Length.in.county)) %>% 
  arrange(desc(Total_length)) %>% rename(county=NAME10) 
GREAT_Total.length.stream.county[c(1:3),c("county", "Total_length")]

#  -------------------------------------------------------------------------------------
#2-3. Map of the counties, shading each county by the total cost of 
#      BMPs funded/implemented in that county. 

bmps_1 <- bmps %>% mutate(., Unique.county = stringr::str_sub(GeographyName, 1, 5)) %>%
  subset(select=c(Unique.county, Cost)) %>% group_by(Unique.county) %>%
  summarise(Total_Cost = sum(Cost, na.rm=T))

counties_1 <- counties %>% mutate(., Unique.county = GEOID10) %>%
  subset(select=Unique.county)

counties.bmps.join <- left_join(counties_1, bmps_1, by="Unique.county")
counties.bmps.join[is.na(counties.bmps.join)] <- 0

tm_shape(counties.bmps.join) + tm_borders() +
  tm_shape(counties.bmps.join) + tm_polygons(col="Total_Cost", n=8)

#  -------------------------------------------------------------------------------------
#2-4. For each removed dam (No 26th dam, so, Total 34 dams), find the closest stream segment

dams.stream.matrix <- sf::st_distance(dams, streams)
rownames(dams.stream.matrix) <- dams$DAM_NAME
colnames(dams.stream.matrix) <- streams$OBJECTID_1
closest.streams.from.dams <-
  t(sapply(seq(nrow(dams.stream.matrix)),
           function(i) {j <- which.min(dams.stream.matrix[i,])
           c(paste(rownames(dams.stream.matrix)[i], colnames(dams.stream.matrix)[j], sep=' - '),
             dams.stream.matrix[i,j])}))
colnames(closest.streams.from.dams) <- c("DAM - CLOSEST STREAM OBJECTID_1", "DIST(m)")
rownames(closest.streams.from.dams) <- dams$OBJECTID_1
print(closest.streams.from.dams, quote = F)

#  -------------------------------------------------------------------------------------
#2-5. Number of dams removed in Each State

States.and.dam <- st_join(counties, dams, join = st_intersects) %>%
  mutate(STATE=fips(STATEFP10, to='abbreviation'))
State.having.dams.incl.zero <- 
  with(States.and.dam, aggregate(OBJECTID_1, by=list(STATE),
                                 n_distinct, na.rm=TRUE))
colnames(State.having.dams.incl.zero) <- c("STATE", "Number_of_dams")
State.having.dams.incl.zero

#or 
# (Aspatial)
State.counties <- counties %>% as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  subset(select=c(OBJECTID, STATEFP10)) %>% 
  mutate(STATE=fips(STATEFP10, to='abbreviation'))
State.having.dams <- left_join(State.counties, dams, by="STATE")
State.having.dams.incl.zero <- 
  with(State.having.dams, aggregate(OBJECTID_1, by=list(STATE),
                                    n_distinct, na.rm=TRUE))
colnames(State.having.dams.incl.zero) <- c("STATE", "Number_of_dams")
State.having.dams.incl.zero


# Thanks a lot !!





    











                                       
                                                
                                            
                                                

                   




                        