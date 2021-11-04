library(spdep)
library(sp)
library(spData)
library(spDataLarge)
library(spdep)
library(sf)
library(tidyverse)
library(tmap)
library(cdlTools)
library(rgdal)

#file reading
d.all <- sf::read_sf("./data/County_2010Census_DP1/County_2010Census_DP1.shp")

# 1. Create a spatial subset of the US, with at AT MINIMUM 4 states, 
#    MAXIMUM 7 states. States must be contiguous. Save this subset as a 
#    shapefile such that it¡¯s sufficiently small in size that GitHub will
#    accept the git-push 
#    Subset: 'Wisconsin', 'Michigan', 'Ohio', 'Indiana', 'Illinois' 

State.subset <- c('Wisconsin', 'Iowa', 'Ohio', 'Indiana', 'Illinois') 
States.s <- d.all %>%
  mutate(., STATE = fips(stringr::str_sub(GEOID10, 1, -4), to='NAME')) %>%
  dplyr::filter(STATE %in% State.subset) %>% relocate(STATE) %>% sf::as_Spatial()

#Save and Export as shapefile.
writeOGR(obj=States.s, dsn='./Data2', 
         layer='States.s', driver = 'ESRI Shapefile')

#New file reading
S.States <- sf::read_sf("./Data2/States.s.shp")
glimpse(S.States)  # 5 states, 453 counties
#View(S.States)
#tm_shape(S.States) + tm_polygons()

# 2. Choose a variable. If it¡¯s a raw count, you should normalize the variable
#    in an appropriate manner (e.g., by total population, percent, by area)

# Variable: Population equal and more than 65 - normalized by POP per 1000
Senior <- S.States %>% mutate(POP.1000 = (DP0010015+DP0010016+DP0010017+DP0010018+DP0010019)
            /(DP0010001)*1000) %>% select(POP.1000)

# 3. Make a histogram of your chosen variable

#Histgram 
hist(Senior$POP.1000)
# with ggplot
Senior %>% ggplot(aes(POP.1000)) + geom_histogram(bins=10) 

#4. Make a choropleth map of your chosen variable. 
#   Choose an appropriate data classification scheme.

Senior.projected <- Senior %>% sf::st_transform(., "ESRI:102010")

tm_shape(Senior.projected) + tm_borders() + 
  tm_polygons(col= "POP.1000", title = "POP (>=65),\n per 1,000 ") + 
  tm_layout(legend.position = c("right", "top"),
            main.title="Map of Senior POP") 

#5. Develop a contiguity-based spatial weights matrix of 
#   your choosing (i.e., rook or queen)

#5-1. Row-standardize the W
#make the neighborhood
nb <- spdep::poly2nb(Senior.projected, queen=TRUE)
nb
# Average number of links: 5.699779 

# Identifying neighboring Counties and weighting (Row-standardize)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1:length(lw$weights)]
# or lw$weights[1:453] 
# or View(lw$weights[1:length(lw$weights)])


#5-2. Plot a histogram of the number of neighbors. 
# To get the count of neighbors 
neighbors <- attr(lw$weights,"comp")$d 

hist(neighbors, axis(side=1,at=seq(1,9,1)))


#5-3. Calculate the average number of neighbors

nb %>% card() %>% mean() %>% round(digits=2)
# or mean(neighbors)

#5-4 Make a Moran Plot
# Getting Moran's I value 
moran.test(Senior.projected$POP.1000, lw)
# Testing and plotting MC simulation
MC <- moran.mc(Senior.projected$POP.1000, lw, nsim=999)
plot(MC, main="Senior POP per 1000 Monte-Carlo simulation of Moran I", las=1)
# Moran's Plots
moran.plot(Senior.projected$POP.1000, lw, zero.policy=TRUE, plot=T)

#6. Repeat #5 (and 5.1 - 5.4) above with a W developed using the IDW method. 
# You will need to investigate the spdep documentation 
# to find the correct method/function.

# 6-1 Row-standardize the W
# Change to spatial
coords <- Senior.projected %>% as_Spatial() %>% coordinates()
# search radius to include neighboring States within 50Km
nb.dist <- dnearneigh(coords, 0, 50000)
nb.dist   # Average number of links: 5.037528


# Identifying neighboring Counties and weighting (Row-standardize) 
lw.d <- nb2listwdist(nb.dist, Senior.projected, 
                     type="idw", style="W", zero.policy=TRUE)
View(lw.d)

lw.d$weights[1:length(lw.d$weights)]
# or lw.d$weights[1:453]
# or View(lw.d$weights[1:length(lw.d$weights)])


#6-2. Plot a histogram of the number of neighbors.
# To get the count of neighbors

neighbors.d <- nb.dist %>% card()
hist(neighbors.d)

#6-3. Calculate the average number of neighbors

nb.dist %>% card() %>% mean() 
# or mean(neighbors.d)

#6-4 Make a Moran Plot
# Getting Moran's I value 
moran.test(Senior.projected$POP.1000, lw.d, zero.policy=TRUE)
# Testing and ploting MC simulation 
MI <- moran.mc(Senior.projected$POP.1000, lw.d, nsim=999, zero.policy=TRUE)
plot(MI, main="Senior POP per 1000 Monte-Carlo simulation of Moran I", las=1)
#Moran's Plotting
moran.plot(Senior.projected$POP.1000, lw.d, zero.policy=TRUE, plot=T)


# Bonus(+50)  -----------------------------------------------------
# B.1
#make another Moran plot, this time do so manually (use geom_point from ggplot).
#You must label each quadrant with HH, HL, LL, and LH, respectively. 
#You should also use color and/or shape to denote whether an observation is 
#statistically significant. Tip, you can find the data you want using the moran.
#plot function, but you¡¯ll have to alter the function call and read some documentation.

Local.moran <- localmoran(Senior.projected$POP.1000, lw, zero.policy=TRUE)
Local.moran

#lagged values for Senior (>=65) 
lag.Senior <- lag.listw(lw, Senior.projected$POP.1000)
Equation <- lm(lag.Senior ~ Senior.projected$POP.1000)

# Making Dataset to plotting with ggplot

Dataset <- Senior.projected %>% 
  mutate(lag.Senior=lag.Senior, 
         p_value = ifelse(Local.moran[,5] < 0.01, '< 0.01', 
                ifelse(Local.moran[,5] < 0.05, '< 0.05',
                ifelse(Local.moran[,5] < 0.10, '< 0.10',
                ifelse(Local.moran[,5] < 0.50, '< 0.50', '>=0.50')))), 
         Quadrent = attr(Local.moran, "quadr")$mean) %>%  
  select(POP.1000, lag.Senior, p_value, Quadrent)

# Plotting
ggplot(Dataset) +
  geom_point(aes(x=POP.1000, y=lag.Senior, color=p_value)) +
  geom_abline(intercept=Equation$coefficients[1], 
              slope=Equation$coefficients[2], color='black', size = 1) +
  geom_vline(xintercept=mean(Dataset$POP.1000), 
             linetype = 'dashed', color='black', size = 1) +  
  geom_hline(yintercept=mean(Dataset$lag.Senior), 
             linetype = 'dashed', color='black', size = 1) +
  annotate("text", x = 85, y = 100, colour="blue", label="LL", size=10) +
  annotate("text", x = 85, y = 215, colour="blue", label="LH", size=10) +
  annotate("text", x = 235, y = 100, colour="blue", label="HL", size=10) +
  annotate("text", x = 235, y = 215, colour="blue", label="HH", size=10) +
  annotate("text", x = 245, y = 140, colour="red",
           label="p-value less than 0.05", size=4) +
  annotate("text", x = 242, y = 133, colour="red",
           label="(typically <= 0.5) is", size=4) +
  annotate("text", x = 244.5, y = 126, colour="red",
           label="statistically significant", size=4) +
  labs(title="Moran's Plot for Senior (>=65) POP per 1,000 by ggplot (contiguity-based (Queen))",
       x= "Population per 1,000", y="Lagged Values")


##################################################################################

# B.1
# plot a choropleth map of your dataset with a categorical color scheme, 
# where the shading corresponds to the Moran plot (really, ¡°LISA¡±) quadrants.
# Thus, your map will have four shades of color.

# Use the same Dataset above.

Dataset.map <- Senior.projected %>%
  mutate(lag.Senior=lag.Senior, 
         Quadrents = factor(attr(Local.moran, "quadr")$mean, 
                            levels = c("Low-Low", "Low-High", "High-Low", "High-High"))) %>%
  select(POP.1000, lag.Senior, Quadrents)

tm_shape(Dataset.map) + 
  tm_polygons("Quadrents", 
              palette=c("dodgerblue", "lightskyblue1", "lightpink", "firebrick1")) +
    tm_layout(legend.position = c("right", "top"), 
              main.title="POP(>=65) per 1,000 with Quadrants", 
              title.position = c("right", "top"),
              title.size = 1.1,
              legend.text.size = 1.1, legend.frame = TRUE,
              legend.title.size = 1.4) 

## Thanks you very much !!



























