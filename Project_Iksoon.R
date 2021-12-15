# The size of original Images is around 80 Mbyte. So, I included aggregated raster used for project.
# I used drawExtent function provided by raster package to make extent of objects. 
# So, to run this script, need to make extent with drawExtent function. Depending on the extent,
# the number could be different from the numbers recorded in this script and paper (Not a big deal)
# I included another two tiff files where extent were selected. Pls refer to them when setting extent.

library(sp)
library(rgdal)
library(raster)
library(GISTools)
library(tidyverse)
library(dplyr)
library(reshape)
library(RStoolbox)

# Reading raster files 
#P2011 <- brick("./data1/Saemankeum_WGS84/2011_35603022s_WGS84.tif")
#P2012 <- brick("./data1/Saemankeum_WGS84/2012_35603022s_WGS84.tif")

# Dividing brick into each layer(band)
#P2011_1 <- raster(P2011, layer=1)
#P2011_2 <- raster(P2011, layer=2)
#P2011_3 <- raster(P2011, layer=3)
#P2012_1 <- raster(P2012, layer=1)
#P2012_2 <- raster(P2012, layer=2)
#P2012_3 <- raster(P2012, layer=3)

# Data for Table 1 (Page 3)
# nrow(P2011_1)        # 5,798
# ncol(P2011_1)        # 4,771
# ncell(P2011_1)       # 27,662,258
# nrow(P2012_1)        # 5,804
# ncol(P2012_1)        # 4,776
# ncell(P2012_1)       # 27,661,864  

# Coordinates for cropping to the same extent
#e <- c(126.524243236555, 126.550636938111, 35.9244149473111, 35.9505419867111)

#Cropping for making the same extent and aggregation for fast processing  
# crop.aggre.P2011_1 <- raster::crop(P2011_1, e) %>% aggregate(8, fun=mean)
# crop.aggre.P2011_2 <- raster::crop(P2011_2, e) %>% aggregate(8, fun=mean)
# crop.aggre.P2011_3 <- raster::crop(P2011_3, e) %>% aggregate(8, fun=mean)
# crop.aggre.P2012_1 <- raster::crop(P2012_1, e) %>% aggregate(8, fun=mean)
# crop.aggre.P2012_2 <- raster::crop(P2012_2, e) %>% aggregate(8, fun=mean)
# crop.aggre.P2012_3 <- raster::crop(P2012_3, e) %>% aggregate(8, fun=mean)

# Data for Table 1 (Page 3)
# nrow(crop.aggre.P2011_1)        # 725
# ncol(crop.aggre.P2011_1)        # 596
# ncell(crop.aggre.P2011_1)       # 432,100
# nrow(crop.aggre.P2012_1)        # 725
# ncol(crop.aggre.P2012_1)        # 596
# ncell(crop.aggre.P2012_1)       # 432,100

# renaming
# P2011.b1 <- crop.aggre.P2011_1    #2011, band 1 (Red)
# P2011.b2 <- crop.aggre.P2011_2    #2011, band 2 (Green)
# P2011.b3 <- crop.aggre.P2011_3    #2011, band 3 (Blue)
# P2012.b1 <- crop.aggre.P2012_1    #2012, band 1 (Red)
# P2012.b2 <- crop.aggre.P2012_2    #2012, band 2 (Green)
# P2012.b3 <- crop.aggre.P2012_3    #2012, band 3 (Red)

# P2011.brick <- brick(P2011.b1, P2011.b2, P2011.b3)
# P2012.brick <- brick(P2012.b1, P2012.b2, P2012.b3)

#Saving Rasterbrick for posting on Github
# (The size of the original image is about 80 Mbyte.)
# writeRaster(P2011.brick, 'P2011.tif', overwrite=TRUE)
# writeRaster(P2012.brick, 'P2012.tif', overwrite=TRUE)


# ----------------------------Method (Page 2)--------------------------------------------------------------------

# Data Preparation (Page 2) -------------------------------------------------------------------------------------

# Reading Rasterbrick
P2011 <- brick("./data/P2011.tif")
P2012 <- brick("./data/P2012.tif")

# Dividing brick into each layer(band)
P2011.b1 <- raster(P2011, layer=1)
P2011.b2 <- raster(P2011, layer=2)
P2011.b3 <- raster(P2011, layer=3)
P2012.b1 <- raster(P2012, layer=1)
P2012.b2 <- raster(P2012, layer=2)
P2012.b3 <- raster(P2012, layer=3)

#Plotting: For Figure 1 (Page 2). * Another colors were applied in paper
par(mfrow=c(2,3))
plot(P2011.b1, axes=F, legend=F, main="P2011.Band 1")
plot(P2011.b2, axes=F, legend=F, main="P2011.Band 2")
plot(P2011.b3, axes=F, legend=F, main="P2011.Band 3")
plot(P2012.b1, axes=F, legend=F, main="P2011.Band 4")
plot(P2012.b2, axes=F, legend=F, main="P2011.Band 5")
plot(P2012.b3, axes=F, legend=F, main="P2011.Band 6")

# Data for Table 1 (Page 3)
nrow(P2011.b1)   # 725       
ncol(P2011.b1)   # 596       
ncell(P2011.b1)  # 432,100    
nrow(P2012.b1)   # 725  
ncol(P2012.b1)   # 596
ncell(P2012.b1)  # 432,100

# Dataframes (Page 3) -------------------------------------------------------------------------------------------

# convert to dataframe 
P2011.b1.df <- P2011.b1 %>% as.data.frame(xy=T) %>%
  reshape::rename(c(P2011.1 = "values")) #2011, band 1 (Red)
P2011.b2.df <- P2011.b2 %>% as.data.frame(xy=T) %>% 
  reshape::rename(c(P2011.2 = "values")) #2011, band 2 (Green)
P2011.b3.df <- P2011.b3 %>% as.data.frame(xy=T) %>% 
  reshape::rename(c(P2011.3 = "values")) #2011, band 3 (Blue)
P2012.b1.df <- P2012.b1 %>% as.data.frame(xy=T) %>%
  reshape::rename(c(P2012.1 = "values")) #2012, band 1 (Red) 
P2012.b2.df <- P2012.b2 %>% as.data.frame(xy=T) %>% 
  reshape::rename(c(P2012.2 = "values")) #2012, band 2 (Green)  
P2012.b3.df <- P2012.b3 %>% as.data.frame(xy=T) %>% 
  reshape::rename(c(P2012.3 = "values")) #2012, band 3 (Blue)

#Value Table (Page 3)
Value.Table <- data.frame(P2011.b1.df$x, P2011.b1.df$y,
                          P2011.b1.df$values, P2012.b1.df$values,
                          P2011.b1.df$values - P2012.b1.df$values,
                          P2012.b1.df$values - P2011.b1.df$values,
                          abs(P2011.b1.df$values - P2012.b1.df$values),
                          P2011.b2.df$values, P2012.b2.df$values,
                          P2011.b2.df$values - P2012.b2.df$values,
                          P2012.b2.df$values - P2011.b2.df$values,
                          abs(P2011.b2.df$values - P2012.b2.df$values),
                          P2011.b3.df$values, P2012.b3.df$values, 
                          P2011.b3.df$values - P2012.b3.df$values,
                          P2012.b3.df$values - P2011.b3.df$values,
                          abs(P2011.b3.df$values - P2012.b3.df$values),
                          stringsAsFactors = FALSE) %>%
  reshape::rename(c(P2011.b1.df.x = "x", P2011.b1.df.y = "y",
                    P2011.b1.df.values = "P2011.b1.values", 
                    P2012.b1.df.values = "P2012.b1.values",
                    P2011.b1.df.values...P2012.b1.df.values = "P2011_P2012.b1",
                    P2012.b1.df.values...P2011.b1.df.values = "P2012_P2011.b1", 
                    abs.P2011.b1.df.values...P2012.b1.df.values. = "b1.abs.diff",
                    P2011.b2.df.values = "P2011.b2.values", 
                    P2012.b2.df.values = "P2012.b2.values",
                    P2011.b2.df.values...P2012.b2.df.values = "P2011_P2012.b2",
                    P2012.b2.df.values...P2011.b2.df.values = "P2012_P2011.b2",
                    abs.P2011.b2.df.values...P2012.b2.df.values. = "b2.abs.diff",
                    P2011.b3.df.values = "P2011.b3.values", 
                    P2012.b3.df.values = "P2012.b3.values",
                    P2011.b3.df.values...P2012.b3.df.values = "P2011_P2012.b3",
                    P2012.b3.df.values...P2011.b3.df.values = "P2012_P2011.b3",
                    abs.P2011.b3.df.values...P2012.b3.df.values. = "b3.abs.diff")) %>%
  as_tibble()

# Radiometry of two images (Page 3)------------------------------------------------------------------------------

# Histgram for Value by each band (Page 3)
par(mfrow=c(2,3))
hist(P2011.b1.df$values, main="P2011 Band_2 Values", xlab="Values", axes = T)
hist(P2011.b2.df$values, main="P2011 Band_2 Values", xlab="Values", axes = T)
hist(P2011.b3.df$values, main="P2011 Band_3 Values", xlab="Values", axes = T)
hist(P2012.b1.df$values, main="P2012 Band_1 Values", xlab="Values", axes = T)
hist(P2012.b2.df$values, main="P2012 Band_2 Values", xlab="Values", axes = T)
hist(P2012.b3.df$values, main="P2012 Band_3 Values", xlab="Values", axes = T)
par(mfrow=c(1, 1))
# ----------------------------Result (Page 4)--------------------------------------------------------------------

# Identification of objects based on the pixel value (Page 4)----------------------------------------------------

# Classify cells by value (25)
b1.higher.0 <- Value.Table %>% dplyr::filter(P2011.b1.values > 0) #Ncell:    427877
b1.higher.25 <- Value.Table %>% dplyr::filter(P2011.b1.values > 25) #Ncell:  202862
b1.higher.50 <- Value.Table %>% dplyr::filter(P2011.b1.values > 50) #Ncell:  155469
b1.higher.75 <- Value.Table %>% dplyr::filter(P2011.b1.values > 75) #Ncell:  118749
b1.higher.100 <- Value.Table %>% dplyr::filter(P2011.b1.values > 100) #Ncell: 96985
b1.higher.125 <- Value.Table %>% dplyr::filter(P2011.b1.values > 125) #Ncell: 75805
b1.higher.150 <- Value.Table %>% dplyr::filter(P2011.b1.values > 150) #Ncell: 52748
b1.higher.175 <- Value.Table %>% dplyr::filter(P2011.b1.values > 175) #Ncell: 27476
b1.higher.200 <- Value.Table %>% dplyr::filter(P2011.b1.values > 200) #Ncell:  6947
b1.higher.225 <- Value.Table %>% dplyr::filter(P2011.b1.values > 225) #Ncell:  1303
b2.higher.0 <- Value.Table %>% dplyr::filter(P2011.b2.values > 0) #Ncell:    427924
b2.higher.25 <- Value.Table %>% dplyr::filter(P2011.b2.values > 25) #Ncell:  426753
b2.higher.50 <- Value.Table %>% dplyr::filter(P2011.b2.values > 50) #Ncell:  402974
b2.higher.75 <- Value.Table %>% dplyr::filter(P2011.b2.values > 75) #Ncell:  141170
b2.higher.100 <- Value.Table %>% dplyr::filter(P2011.b2.values > 100) #Ncell:106838
b2.higher.125 <- Value.Table %>% dplyr::filter(P2011.b2.values > 125) #Ncell: 80388
b2.higher.150 <- Value.Table %>% dplyr::filter(P2011.b2.values > 150) #Ncell: 49540
b2.higher.175 <- Value.Table %>% dplyr::filter(P2011.b2.values > 175) #Ncell: 21399
b2.higher.200 <- Value.Table %>% dplyr::filter(P2011.b2.values > 200) #Ncell:  3786
b2.higher.225 <- Value.Table %>% dplyr::filter(P2011.b2.values > 225) #Ncell:   431
b3.higher.0 <- Value.Table %>% dplyr::filter(P2011.b3.values > 0)   #Ncell:  427927
b3.higher.25 <- Value.Table %>% dplyr::filter(P2011.b3.values > 25) #Ncell:  426536
b3.higher.50 <- Value.Table %>% dplyr::filter(P2011.b3.values > 50) #Ncell:  396535
b3.higher.75 <- Value.Table %>% dplyr::filter(P2011.b3.values > 75) #Ncell:  329241
b3.higher.100 <- Value.Table %>% dplyr::filter(P2011.b3.values > 100) #Ncell: 91306
b3.higher.125 <- Value.Table %>% dplyr::filter(P2011.b3.values > 125) #Ncell: 52056
b3.higher.150 <- Value.Table %>% dplyr::filter(P2011.b3.values > 150) #Ncell: 20016
b3.higher.175 <- Value.Table %>% dplyr::filter(P2011.b3.values > 175) #Ncell:  4948
b3.higher.200 <- Value.Table %>% dplyr::filter(P2011.b3.values > 200) #Ncell:  1314
b3.higher.225 <- Value.Table %>% dplyr::filter(P2011.b3.values > 225) #Ncell:   250


# Dataframe to find Ncell based on values (Graph 1, Page 4)
nrow.more <- seq(0, 225, 25) %>% as.factor()
Band_1 <- c(nrow(b1.higher.0), nrow(b1.higher.25), nrow(b1.higher.50), nrow(b1.higher.75),
            nrow(b1.higher.100), nrow(b1.higher.125), nrow(b1.higher.150),
            nrow(b1.higher.175), nrow(b1.higher.200), nrow(b1.higher.225))
Band_2 <- c(nrow(b2.higher.0), nrow(b2.higher.25), nrow(b2.higher.50), nrow(b2.higher.75),
            nrow(b2.higher.100), nrow(b2.higher.125), nrow(b2.higher.150),
            nrow(b2.higher.175), nrow(b2.higher.200), nrow(b2.higher.225))
Band_3 <- c(nrow(b3.higher.0), nrow(b3.higher.25), nrow(b3.higher.50), nrow(b3.higher.75),
            nrow(b3.higher.100), nrow(b3.higher.125), nrow(b3.higher.150),
            nrow(b3.higher.175), nrow(b3.higher.200), nrow(b3.higher.225))
nrow_byband <- data.frame(nrow.more, Band_1, Band_2, Band_3)
#nrow_byband
options(scipen = 100)

# Plotting Ncell based on values (Graph 1, Page 4)
ggplot(data=nrow_byband) + 
  geom_line(aes(x=nrow.more, y= Band_1, group = 1), color="red", size = 1.5) +
  geom_point(aes(x=nrow.more, y= Band_1, group = 1), color="red", size=3) +
  annotate("text", x=2.3, y=330000, label="Band 1 (R)", size = 6, color = "red") +
  geom_line(aes(x=nrow.more, y= Band_2, group = 2), color="green3", size = 1.5) +
  geom_point(aes(x=nrow.more, y= Band_2, group = 2), color="green3", size=3) +
  annotate("text", x=2.8, y=240000, label="Band 2 (G)", size = 6, color = "green3") +
  geom_line(aes(x=nrow.more, y= Band_3, group = 3), color="blue", size = 1.5) +
  geom_point(aes(x=nrow.more, y= Band_3, group = 3), color="blue", size=3) +
  annotate("text", x=5.4, y=220000, label="Band 3 (B)", size = 6, color = "blue") +
  theme_minimal() + labs(x= "Values (more than)", y= "Number of cells") +
  theme(axis.title = element_text(face = "bold", size = 20, color = "darkblue")) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))


# plotting images (entire area) by Band based on the pixel value (Table 2, Page 4) 
# Row 243 to 523: Plotting for object identification according to Ncell
# Inverval is 25, but some to decrease to 1 to check sensibility of that band.
# If there are many Ncells, plotting takes a long time. If not popping, pls rerun it

# Band 1 
# b1.higher.0 means Ncells with a value greater than 0 among cells in band 1.
b1.higher.0 <- Value.Table %>% dplyr::filter(P2011.b1.values > 0)  #Ncell: 427877
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.0, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.5 <- Value.Table %>% dplyr::filter(P2011.b1.values > 5) #Ncell: 426685
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "grey40", guide = "none") +
  geom_tile(data=b1.higher.5, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.6 <- Value.Table %>% dplyr::filter(P2011.b1.values > 6) #Ncell: 426481
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.6, aes(x=x, y=y), color = "tomato")

b1.higher.7 <- Value.Table %>% dplyr::filter(P2011.b1.values > 7) #Ncell: 400883
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none")+
  geom_tile(data=b1.higher.7, aes(x=x, y=y), color = "tomato")

b1.higher.8 <- Value.Table %>% dplyr::filter(P2011.b1.values > 8) #Ncell: 291081  
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none")+
  geom_tile(data=b1.higher.8, aes(x=x, y=y), color = "tomato")

b1.higher.9 <- Value.Table %>% dplyr::filter(P2011.b1.values > 9) #Ncell: 274229
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none")+
  geom_tile(data=b1.higher.9, aes(x=x, y=y), color = "tomato")

b1.higher.10 <- Value.Table %>% dplyr::filter(P2011.b1.values > 10) #Ncell: 264258
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none")+
  geom_tile(data=b1.higher.10, aes(x=x, y=y), color = "tomato")

b1.higher.25 <- Value.Table %>% dplyr::filter(P2011.b1.values > 25) #Ncell: 202862
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.25, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.50 <- Value.Table %>% dplyr::filter(P2011.b1.values > 50) #Ncell: 155469
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.50, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.75 <- Value.Table %>% dplyr::filter(P2011.b1.values > 75) #Ncell: 118749
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.75, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.100 <- Value.Table %>% dplyr::filter(P2011.b1.values > 100) #Ncell: 96985
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.100, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.125 <- Value.Table %>% dplyr::filter(P2011.b1.values > 125) #Ncell: 75805
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.125, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.150 <- Value.Table %>% dplyr::filter(P2011.b1.values > 150) #Ncell: 52748
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.150, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.175 <- Value.Table %>% dplyr::filter(P2011.b1.values > 175) #Ncell: 27476
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.175, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.200 <- Value.Table %>% dplyr::filter(P2011.b1.values > 200) #Ncell: 6947
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.200, aes(x=x, y=y), color = "tomato") +
  theme_void()

b1.higher.225 <- Value.Table %>% dplyr::filter(P2011.b1.values > 225) #Ncell: 1303
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.higher.225, aes(x=x, y=y), color = "tomato") +
  theme_void()

# Band 2

b2.higher.0 <- Value.Table %>% dplyr::filter(P2011.b2.values > 0) #Ncell: 427924
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.0, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.25 <- Value.Table %>% dplyr::filter(P2011.b2.values > 25) #Ncell: 426753
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.25, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.50 <- Value.Table %>% dplyr::filter(P2011.b2.values > 50) #Ncell: 402974
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.50, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.52 <- Value.Table %>% dplyr::filter(P2011.b2.values > 52) #Ncell: 386358
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.52, aes(x=x, y=y), color = "green4")

b2.higher.53 <- Value.Table %>% dplyr::filter(P2011.b2.values > 53) #Ncell: 374301
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.53, aes(x=x, y=y), color = "green4")

b2.higher.54 <- Value.Table %>% dplyr::filter(P2011.b2.values > 54) #Ncell: 359571
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.54, aes(x=x, y=y), color = "green4")

b2.higher.55 <- Value.Table %>% dplyr::filter(P2011.b2.values > 55) #Ncell: 334804
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.55, aes(x=x, y=y), color = "green4")

b2.higher.56 <- Value.Table %>% dplyr::filter(P2011.b2.values > 56) #Ncell: 252780 
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.56, aes(x=x, y=y), color = "green4")

b2.higher.57 <- Value.Table %>% dplyr::filter(P2011.b2.values > 57) #Ncell: 194632
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.57, aes(x=x, y=y), color = "green4")

b2.higher.60 <- Value.Table %>% dplyr::filter(P2011.b2.values > 60) #Ncell: 182829
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.60, aes(x=x, y=y), color = "green4")

b2.higher.75 <- Value.Table %>% dplyr::filter(P2011.b2.values > 75) #Ncell: 141170
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.75, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.100 <- Value.Table %>% dplyr::filter(P2011.b2.values > 100) #Ncell: 106838
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.100, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.125 <- Value.Table %>% dplyr::filter(P2011.b2.values > 125) #Ncell: 80388
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.125, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.150 <- Value.Table %>% dplyr::filter(P2011.b2.values > 150) #Ncell: 49540
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.150, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.175 <- Value.Table %>% dplyr::filter(P2011.b2.values > 175) #Ncell: 21399
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.175, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.200 <- Value.Table %>% dplyr::filter(P2011.b2.values > 200) #Ncell: 3786
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.200, aes(x=x, y=y), color = "green4") +
  theme_void()

b2.higher.225 <- Value.Table %>% dplyr::filter(P2011.b2.values > 225) #Ncell: 431
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.higher.225, aes(x=x, y=y), color = "green4") +
  theme_void()

# Band 3 

b3.higher.0 <- Value.Table %>% dplyr::filter(P2011.b3.values > 0)   #Ncell: 427927
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.0, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.25 <- Value.Table %>% dplyr::filter(P2011.b3.values > 25) #Ncell: 426536
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.25, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.50 <- Value.Table %>% dplyr::filter(P2011.b3.values > 50) #Ncell: 396535
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.50, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.75 <- Value.Table %>% dplyr::filter(P2011.b3.values > 75) #Ncell: 329241
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.75, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.80 <- Value.Table %>% dplyr::filter(P2011.b3.values > 80) #Ncell: 304956
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.80, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.85 <- Value.Table %>% dplyr::filter(P2011.b3.values > 85) #Ncell: 263538
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.85, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.86 <- Value.Table %>% dplyr::filter(P2011.b3.values > 86) #Ncell: 234115
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.86, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.87 <- Value.Table %>% dplyr::filter(P2011.b3.values > 87) #Ncell: 152148 
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.87, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.88 <- Value.Table %>% dplyr::filter(P2011.b3.values > 88) #Ncell: 110687
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.88, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.89 <- Value.Table %>% dplyr::filter(P2011.b3.values > 89) #Ncell: 106508
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.89, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.90 <- Value.Table %>% dplyr::filter(P2011.b3.values > 90) #Ncell: 104721
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.90, aes(x=x, y=y), color = "dodgerblue3")

b3.higher.100 <- Value.Table %>% dplyr::filter(P2011.b3.values > 100) #Ncell: 91306
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.100, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.125 <- Value.Table %>% dplyr::filter(P2011.b3.values > 125) #Ncell: 52056
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.125, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.150 <- Value.Table %>% dplyr::filter(P2011.b3.values > 150) #Ncell: 20016
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.150, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.175 <- Value.Table %>% dplyr::filter(P2011.b3.values > 175) #Ncell: 4948
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.175, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.200 <- Value.Table %>% dplyr::filter(P2011.b3.values > 200) #Ncell: 1314
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.200, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

b3.higher.225 <- Value.Table %>% dplyr::filter(P2011.b3.values > 225) #Ncell: 250
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.higher.225, aes(x=x, y=y), color = "dodgerblue3") +
  theme_void()

# Average value of object (Water, Soil, Shrub, Blueroof Building) 
# by Band (For Graph 2, Page 5) 
# Pls click 2 times on the map to make extent.
# objects randomly selected by drawExtent function

# Selected Area (The Extent1 is selected by me, roughly)
Extent1 <- brick("./data/Extent1.tif")
par(mfrow=c(1, 1))
plotRGB(Extent1)        # pls watch where I selected

#Water
par(mfrow=c(1, 2))
plotRGB(Extent1)
plotRGB(P2012)
W <- drawExtent()  # Pls click 2 times on the right map to make "Water" extent like left map  
# mean value could be variable depending on extent 
mean(values(crop(P2011.b1, W)))   # 7.16
mean(values(crop(P2011.b2, W)))   # 55.87
mean(values(crop(P2011.b3, W)))   # 86.69

# Soil
par(mfrow=c(1, 2))
plotRGB(Extent1)
plotRGB(P2012)
S <- drawExtent()  # Pls click 2 times on the right map to make "Soil" extent like left map 
# mean value could be variable depending on extent
mean(values(crop(P2012.b1, S)))  # 133.42
mean(values(crop(P2012.b2, S)))  # 113.01
mean(values(crop(P2012.b3, S)))  # 76.50  

#Shrub
par(mfrow=c(1, 2))
plotRGB(Extent1)
plotRGB(P2012)
SH <- drawExtent()  # Pls click 2 times on the right map to make "Shrug" extent like left map  
# mean value could be variable depending on extent
mean(values(crop(P2011.b1, SH)))  # 52.95 
mean(values(crop(P2011.b2, SH)))  # 67.06
mean(values(crop(P2011.b3, SH)))  # 51.99

#Blueroof Building         
par(mfrow=c(1, 2))
plotRGB(Extent1)
plotRGB(P2012)
BR <- drawExtent() # Pls click 2 times on the right map to make "Blueroof Building" extent like left map 
# mean value could be variable depending on extent
mean(values(crop(P2012.b1, BR)))  # 86.47
mean(values(crop(P2012.b2, BR)))  # 99.37
mean(values(crop(P2012.b3, BR)))  # 110.21 

#Dataframe for Objects (surface) mean value by Band (Graph 2, Page 5)
# I input my numbers to make dataframe
Bands <- c("Band 1 (R)", "Band 2 (G)", "Band 3 (B)") %>% as.factor()
Water <- c(7.16, 55.87, 86.69) %>% as.numeric()
Soil <- c(133.42, 113.01, 76.50) %>% as.numeric()
Shrub <- c(52.95, 67.06, 51.99) %>% as.numeric()
Blueroof_Building <- c(86.47, 99.37, 110.21) %>% as.numeric()
Mean_Value <- data.frame(Bands, Water, Soil, Shrub, Blueroof_Building)

# Average values of objects by band(Graph 2, Page 5).
ggplot(data=Mean_Value) + geom_line(aes(x=Bands, y= Water, group = 1), color="blue", size = 1.5) +
  geom_point(aes(x=Bands, y= Water, group = 1), color="blue", size=4) + 
  annotate("text", x="Band 3 (B)", y=93, label="Water", size = 5, color = "blue") +
  geom_line(aes(x=Bands, y= Soil, group = 2), color="tan3", size = 1.5) +
  geom_point(aes(x=Bands, y= Soil, group = 2), color="tan3", size=4) + 
  annotate("text", x="Band 3 (B)", y=72, label="Soil", size = 5, color = "tan3") +
  geom_line(aes(x=Bands, y= Shrub, group = 3), color="springgreen4", size = 1.5) +
  geom_point(aes(x=Bands, y= Shrub, group = 3), color="springgreen4", size=4) + 
  annotate("text", x="Band 3 (B)", y=47, label="Shrub", size = 5, color = "springgreen4") +
  geom_line(aes(x=Bands, y= Blueroof_Building, group = 4), color="black", size = 1.5) +
  geom_point(aes(x=Bands, y= Blueroof_Building, group = 4), color="black", size=4) + 
  annotate("text", x="Band 3 (B)", y=120, label="Blueroof\nBuilding", size = 5, color = "black") +
  labs(x= "", y= "Values") + scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  theme_minimal() + theme(axis.text.y= element_text(size=15)) + 
  theme(axis.title = element_text(face = "bold", size = 15)) +
  theme(axis.text.x = element_text(size = 15, face = "bold")) 

# Change detection by value difference between two images (Page 5)------------------------------------------------

# Dataframe for number of cells with values difference

# Classify cells by value difference between images 

b1.more.abs.0 <- Value.Table %>% dplyr::filter(b1.abs.diff > 0)       #425800 
b1.more.abs.25 <- Value.Table %>% dplyr::filter(b1.abs.diff > 25)     #75606 
b1.more.abs.50 <- Value.Table %>% dplyr::filter(b1.abs.diff > 50)     #46023 
b1.more.abs.75 <- Value.Table %>% dplyr::filter(b1.abs.diff > 75)     #26728
b1.more.abs.100 <- Value.Table %>% dplyr::filter(b1.abs.diff > 100)   #11277
b1.more.abs.125 <- Value.Table %>% dplyr::filter(b1.abs.diff > 125)   #2383
b1.more.abs.150 <- Value.Table %>% dplyr::filter(b1.abs.diff > 150)   #391
b1.more.abs.175 <- Value.Table %>% dplyr::filter(b1.abs.diff > 175)   #89
b2.more.abs.0 <- Value.Table %>% dplyr::filter(b2.abs.diff > 0)       #425520
b2.more.abs.25 <- Value.Table %>% dplyr::filter(b2.abs.diff > 25)     #63279
b2.more.abs.50 <- Value.Table %>% dplyr::filter(b2.abs.diff > 50)     #32545
b2.more.abs.75 <- Value.Table %>% dplyr::filter(b2.abs.diff > 75)     #15558
b2.more.abs.100 <- Value.Table %>% dplyr::filter(b2.abs.diff > 100)   #5784
b2.more.abs.125 <- Value.Table %>% dplyr::filter(b2.abs.diff > 125)   #1241
b2.more.abs.150 <- Value.Table %>% dplyr::filter(b2.abs.diff > 150)   #205
b2.more.abs.175 <- Value.Table %>% dplyr::filter(b2.abs.diff > 175)   #45
b3.more.abs.0 <- Value.Table %>% dplyr::filter(b3.abs.diff > 0)       #425711
b3.more.abs.25 <- Value.Table %>% dplyr::filter(b3.abs.diff > 25)     #54002
b3.more.abs.50 <- Value.Table %>% dplyr::filter(b3.abs.diff > 50)     #26976
b3.more.abs.75 <- Value.Table %>% dplyr::filter(b3.abs.diff > 75)     #12156
b3.more.abs.100 <- Value.Table %>% dplyr::filter(b3.abs.diff > 100)   #4022
b3.more.abs.125 <- Value.Table %>% dplyr::filter(b3.abs.diff > 125)   #662
b3.more.abs.150 <- Value.Table %>% dplyr::filter(b3.abs.diff > 150)   #99
b3.more.abs.175 <- Value.Table %>% dplyr::filter(b3.abs.diff > 175)   #17

# Making Dataframe for Number of cells with values difference (Graph 3, Page 5)  
nrow.more175 <- seq(0, 175, 25) %>% as.factor()
Band.1 <- c(nrow(b1.more.abs.0), nrow(b1.more.abs.25), nrow(b1.more.abs.50), 
            nrow(b1.more.abs.75), nrow(b1.more.abs.100), nrow(b1.more.abs.125),
            nrow(b1.more.abs.150), nrow(b1.more.abs.175))
Band.2 <- c(nrow(b2.more.abs.0), nrow(b2.more.abs.25), nrow(b2.more.abs.50), 
            nrow(b2.more.abs.75), nrow(b2.more.abs.100), nrow(b2.more.abs.125),
            nrow(b2.more.abs.150), nrow(b2.more.abs.175))
Band.3 <- c(nrow(b3.more.abs.0), nrow(b3.more.abs.25), nrow(b3.more.abs.50), 
            nrow(b3.more.abs.75), nrow(b3.more.abs.100), nrow(b3.more.abs.125),
            nrow(b3.more.abs.150), nrow(b3.more.abs.175))
nrow_bydiff <- data.frame(nrow.more175, Band.1, Band.2, Band.3)
#nrow_bydiff

# plotting Number of cells with values difference (Graph 3, Page 5)
options(scipen = 100)
ggplot(data=nrow_bydiff) + 
  geom_line(aes(x=nrow.more175, y= Band.1, group = 1), color="red", size = 1.5) +
  geom_point(aes(x=nrow.more175, y= Band.1, group = 1), color="red", size=3) +
  annotate("text", x=2.6, y=90000, label="Band 1 (R)", size = 6, color = "red") +
  geom_line(aes(x=nrow.more175, y= Band.2, group = 2), color="green3", size = 1.5) +
  geom_point(aes(x=nrow.more175, y= Band.2, group = 2), color="green3", size=3) +
  annotate("text", x=3.1, y=73000, label="Band 2 (G)", size = 6, color = "green3") +
  geom_line(aes(x=nrow.more175, y= Band.3, group = 3), color="blue", size = 1.5) +
  geom_point(aes(x=nrow.more175, y= Band.3, group = 3), color="blue", size=3) +
  annotate("text", x=3.7, y=56000, label="Band 3 (B)", size = 6, color = "blue") +
  theme_minimal() + labs(x= "Values (more than)", y= "Number of cells") +
  theme(axis.title = element_text(face = "bold", size = 20, color = "darkblue")) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))

# Dataframe for Small Graph (value range: 25 to 100)     (Graph 3, Page 5)
nrow.more25_100 <- seq(25, 100, 25) %>% as.factor()
BAND.1 <- c(nrow(b1.more.abs.25), nrow(b1.more.abs.50), 
            nrow(b1.more.abs.75), nrow(b1.more.abs.100))
BAND.2 <- c(nrow(b2.more.abs.25), nrow(b2.more.abs.50),
            nrow(b2.more.abs.75), nrow(b2.more.abs.100))
BAND.3 <- c(nrow(b3.more.abs.25), nrow(b3.more.abs.50), 
            nrow(b3.more.abs.75), nrow(b3.more.abs.100))
nrow_bydiff25_100 <- data.frame(nrow.more25_100, BAND.1, BAND.2, BAND.3)

# Number of cells with value range: 25 to 100 (Small Graph (Graph 3, Page 5))
options(scipen = 100)
ggplot(data=nrow_bydiff25_100) + 
  geom_line(aes(x=nrow.more25_100, y= BAND.1, group = 1), color="red", size = 1.5) +
  geom_point(aes(x=nrow.more25_100, y= BAND.1, group = 1), color="red", size=3) +
  annotate("text", x=2.3, y=48000, label="Band 1 (R)", size = 5, color = "red") +
  geom_line(aes(x=nrow.more25_100, y= BAND.2, group = 2), color="green3", size = 1.5) +
  geom_point(aes(x=nrow.more25_100, y= BAND.2, group = 2), color="green3", size=3) +
  annotate("text", x=2.3, y=34000, label="Band 2 (G)", size = 5, color = "green3") +
  geom_line(aes(x=nrow.more25_100, y= BAND.3, group = 3), color="blue", size = 1.5) +
  geom_point(aes(x=nrow.more25_100, y= BAND.3, group = 3), color="blue", size=3) +
  annotate("text", x=2.3, y=17000, label="Band 3 (B)", size = 5, color = "blue") +
  theme_minimal() + labs(x= "Values (more than)", y= "Number of cells") +
  theme(axis.title = element_text(face = "bold", size = 20, color = "darkblue")) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))


# Value difference (Min/Max/Mean) by band between the two images (Table 3, Page 6)
# Objects randomly selected by drawExtent function
# Pls click 2 times on the right map to make extent for that object

# Area selected by me. (This area will be also applied row 968 to 1402 of this script)
Extent2 <- brick("./data/Extent2.tif")  
plotRGB(Extent2)        # pls watch the plotting where I selected    

#Sea Area (Table. 3)

par(mfrow=c(1, 2))
plotRGB(Extent2)
plotRGB(P2012)
Sea <- drawExtent()  # Pls click 2 times on the right map to make "Sea" extent like left map

# Values could be variable depending on extent
# par(mfrow=c(2,3))  
# hist(values(crop(P2011.b1, Sea)), main="Sea Area 2011 Band_1 Values", xlab="Values")
# hist(values(crop(P2011.b2, Sea)), main="Sea Area 2011 Band_2 Values", xlab="Values")
# hist(values(crop(P2011.b3, Sea)), main="Sea Area 2011 Band_3 Values", xlab="Values")
# hist(values(crop(P2012.b1, Sea)), main="Sea Area 2012 Band_1 Values", xlab="Values")
# hist(values(crop(P2012.b2, Sea)), main="Sea Area 2012 Band_2 Values", xlab="Values")
# hist(values(crop(P2012.b3, Sea)), main="Sea Area 2012 Band_3 Values", xlab="Values")

# Value difference (Min/Max/Mean) by band between the two images (Table 3, Page 6)
min(abs(values(crop(P2011.b1, Sea))-values(crop(P2012.b1, Sea))))  #0
min(abs(values(crop(P2011.b2, Sea))-values(crop(P2012.b2, Sea))))  # 0
min(abs(values(crop(P2011.b3, Sea))-values(crop(P2012.b3, Sea))))  # 0
max(abs(values(crop(P2011.b1, Sea))-values(crop(P2012.b1, Sea))))  #6.03125
max(abs(values(crop(P2011.b2, Sea))-values(crop(P2012.b2, Sea))))  # 6.078125
max(abs(values(crop(P2011.b3, Sea))-values(crop(P2012.b3, Sea))))  # 5.640625
median(abs(values(crop(P2011.b1, Sea))-values(crop(P2012.b1, Sea))))  #0.28125
median(abs(values(crop(P2011.b2, Sea))-values(crop(P2012.b2, Sea))))  # 0.34375
median(abs(values(crop(P2011.b3, Sea))-values(crop(P2012.b3, Sea))))  # 0.375
abs(mean(values(crop(P2011.b1, Sea))-values(crop(P2012.b1, Sea))))  #0.2899608
abs(mean(values(crop(P2011.b2, Sea))-values(crop(P2012.b2, Sea))))  # 0.3014228
abs(mean(values(crop(P2011.b3, Sea))-values(crop(P2012.b3, Sea))))  # 0.3311926 

#Landfill Area (Table. 3)

par(mfrow=c(1, 2))
plotRGB(Extent2)
plotRGB(P2012)
Landfill <- drawExtent()  # Pls click 2 times on the right map to make "Sea" extent like left map
# Values could be variable depending on extent

# par(mfrow=c(2,3))  
# hist(values(crop(P2011.b1, Landfill)), main="Landfill Area 2011 Band_1 Values", xlab="Values")
# hist(values(crop(P2011.b2, Landfill)), main="Landfill Area 2011 Band_2 Values", xlab="Values")
# hist(values(crop(P2011.b3, Landfill)), main="Landfill Area 2011 Band_3 Values", xlab="Values")
# hist(values(crop(P2012.b1, Landfill)), main="Landfill Area 2012 Band_1 Values", xlab="Values")
# hist(values(crop(P2012.b2, Landfill)), main="Landfill Area 2012 Band_2 Values", xlab="Values")
# hist(values(crop(P2012.b3, Landfill)), main="Landfill Area 2012 Band_3 Values", xlab="Values")

# Value difference (Min/Max/Mean) by band between the two images (Table 3, Page 6)
min(abs(values(crop(P2011.b1, Landfill))-values(crop(P2012.b1, Landfill))))  # 0.16
min(abs(values(crop(P2011.b2, Landfill))-values(crop(P2012.b2, Landfill))))  # 0.03
min(abs(values(crop(P2011.b3, Landfill))-values(crop(P2012.b3, Landfill))))  # 0
max(abs(values(crop(P2011.b1, Landfill))-values(crop(P2012.b1, Landfill))))  #127.4688
max(abs(values(crop(P2011.b2, Landfill))-values(crop(P2012.b2, Landfill))))  # 88.20312
max(abs(values(crop(P2011.b3, Landfill))-values(crop(P2012.b3, Landfill))))  # 64.4375
median(abs(values(crop(P2011.b1, Landfill))-values(crop(P2012.b1, Landfill))))  #80.29688
median(abs(values(crop(P2011.b2, Landfill))-values(crop(P2012.b2, Landfill))))  # 28.21875
median(abs(values(crop(P2011.b3, Landfill))-values(crop(P2012.b3, Landfill))))  # 9.023438
abs(mean(values(crop(P2011.b1, Landfill))-values(crop(P2012.b1, Landfill)))) #81.63537
abs(mean(values(crop(P2011.b2, Landfill))-values(crop(P2012.b2, Landfill)))) #32.1323
abs(mean(values(crop(P2011.b3, Landfill))-values(crop(P2012.b3, Landfill)))) # 7.048565

#Vegetation Area
par(mfrow=c(1, 2))
plotRGB(Extent2)
plotRGB(P2012)
Vegetation <- drawExtent()  # Pls click 2 times on the right map to make "Sea" extent like left map
# Values could be variable depending on extent

# par(mfrow=c(2,3))  
# hist(values(crop(P2011.b1, Vegetation)), 
#      main="Non vegetation Area 2011 Band_1 Values", xlab="Values")
# hist(values(crop(P2011.b2, Vegetation)),
#      main="Non Vegetation Area 2011 Band_2 Values", xlab="Values")
# hist(values(crop(P2011.b3, Vegetation)),
#      main="Non Vegetation Area 2011 Band_3 Values", xlab="Values")
# hist(values(crop(P2012.b1, Vegetation)),
#      main="Vegetation Area 2012 Band_1 Values", xlab="Values")
# hist(values(crop(P2012.b2, Vegetation)),
#      main="Vegetation Area 2012 Band_2 Values", xlab="Values")
# hist(values(crop(P2012.b3, Vegetation)),
#      main="Vegetation Area 2012 Band_3 Values", xlab="Values")

# Value difference (Min/Max/Mean) by band between the two images (Table 3, Page 6)
min(abs(values(crop(P2011.b1, Vegetation))-values(crop(P2012.b1, Vegetation))))  # 1.203125
min(abs(values(crop(P2011.b2, Vegetation))-values(crop(P2012.b2, Vegetation))))  # 0.875
min(abs(values(crop(P2011.b3, Vegetation))-values(crop(P2012.b3, Vegetation))))  # 1.296875
max(abs(values(crop(P2011.b1, Vegetation))-values(crop(P2012.b1, Vegetation))))  #132.3906
max(abs(values(crop(P2011.b2, Vegetation))-values(crop(P2012.b2, Vegetation))))  # 136.9219
max(abs(values(crop(P2011.b3, Vegetation))-values(crop(P2012.b3, Vegetation))))  # 123.8906
median(abs(values(crop(P2011.b1, Vegetation))-values(crop(P2012.b1, Vegetation))))  #70.35938
median(abs(values(crop(P2011.b2, Vegetation))-values(crop(P2012.b2, Vegetation))))  # 84.39062
median(abs(values(crop(P2011.b3, Vegetation))-values(crop(P2012.b3, Vegetation))))  # 75.67188
abs(mean(values(crop(P2011.b1, Vegetation))-values(crop(P2012.b1, Vegetation)))) # 65.93399  
abs(mean(values(crop(P2011.b2, Vegetation))-values(crop(P2012.b2, Vegetation)))) # 81.12465
abs(mean(values(crop(P2011.b3, Vegetation))-values(crop(P2012.b3, Vegetation)))) # 73.39929

#Building Area  
par(mfrow=c(1, 2))
plotRGB(Extent2)
plotRGB(P2012)
Building <- drawExtent()  # Pls click 2 times on the right map to make "Sea" extent like left map
# Values could be variable depending on extent

# par(mfrow=c(2,3))  
# hist(values(crop(P2011.b1, Building)),
#      main="Non Building Area 2011 Band_1 Values", xlab="Values")
# hist(values(crop(P2011.b2, Building)), 
#      main="Non Building Area 2011 Band_2 Values", xlab="Values")
# hist(values(crop(P2011.b3, Building)), 
#      main="Non Building Area 2011 Band_3 Values", xlab="Values")
# hist(values(crop(P2012.b1, Building)), 
#      main="Building Area 2012 Band_1 Values", xlab="Values")
# hist(values(crop(P2012.b2, Building)), 
#      main="Building Area 2012 Band_2 Values", xlab="Values")
# hist(values(crop(P2012.b3, Building)), 
#      main="Building Area 2012 Band_3 Values", xlab="Values")

# Value difference (Min/Max/Mean) by band between the two images (Table 3, Page 6)
min(abs(values(crop(P2011.b1, Building))-values(crop(P2012.b1, Building))))  # 0
min(abs(values(crop(P2011.b2, Building))-values(crop(P2012.b2, Building))))  # 0.03125
min(abs(values(crop(P2011.b3, Building))-values(crop(P2012.b3, Building))))  # 0
max(abs(values(crop(P2011.b1, Building))-values(crop(P2012.b1, Building))))  #227.2344
max(abs(values(crop(P2011.b2, Building))-values(crop(P2012.b2, Building))))  # 221.9062
max(abs(values(crop(P2011.b3, Building))-values(crop(P2012.b3, Building))))  # 212.1875
median(abs(values(crop(P2011.b1, Building))-values(crop(P2012.b1, Building))))  #28.92188
median(abs(values(crop(P2011.b2, Building))-values(crop(P2012.b2, Building))))  # 27.59375
median(abs(values(crop(P2011.b3, Building))-values(crop(P2012.b3, Building))))  #29.60156
abs(mean(values(crop(P2011.b1, Building))-values(crop(P2012.b1, Building)))) # 5.274562  
abs(mean(values(crop(P2011.b2, Building))-values(crop(P2012.b2, Building)))) # 6.231876
abs(mean(values(crop(P2011.b3, Building))-values(crop(P2012.b3, Building)))) # 14.14143

# Change detection of entire area according to pixel value difference (Table 4, Page 6)
# Upto Row 956, plotting of value difference pixel in all area 

# Band 1 
# b1.more.abs.0: The Ncell with band 1 value difference of two images greater than 0
b1.more.abs.0 <- Value.Table %>% dplyr::filter(b1.abs.diff > 0)                 #425800,  > 0
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.0, aes(x=x, y=y), color = "tomato")

b1.more.abs.25 <- Value.Table %>% dplyr::filter(b1.abs.diff > 25)               #75606,  > 0
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.25, aes(x=x, y=y), color = "tomato") 

b1.more.abs.50 <- Value.Table %>% dplyr::filter(b1.abs.diff > 50)               #46023,  >50
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.50, aes(x=x, y=y), color = "tomato") 

b1.more.abs.75 <- Value.Table %>% dplyr::filter(b1.abs.diff > 75)               #26728,  >75
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.75, aes(x=x, y=y), color = "tomato")

b1.more.abs.100 <- Value.Table %>% dplyr::filter(b1.abs.diff > 100)             #11277, >100
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.100, aes(x=x, y=y), color = "tomato")

b1.more.abs.125 <- Value.Table %>% dplyr::filter(b1.abs.diff > 125)             #2383, > 125
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.125, aes(x=x, y=y), color = "tomato")

b1.more.abs.150 <- Value.Table %>% dplyr::filter(b1.abs.diff > 150)             #391, >150
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.150, aes(x=x, y=y), color = "tomato")

b1.more.abs.175 <- Value.Table %>% dplyr::filter(b1.abs.diff > 175)             #89, >175
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b1.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b1.more.abs.175, aes(x=x, y=y), color = "tomato")

# Band 2 

b2.more.abs.0 <- Value.Table %>% dplyr::filter(b2.abs.diff > 0)                 #425520,  > 0
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.0, aes(x=x, y=y), color = "green4")

b2.more.abs.25 <- Value.Table %>% dplyr::filter(b2.abs.diff > 25)               #63279,  > 25
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.25, aes(x=x, y=y), color = "green4")

b2.more.abs.50 <- Value.Table %>% dplyr::filter(b2.abs.diff > 50)               #32545,  > 50
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.50, aes(x=x, y=y), color = "green4")

b2.more.abs.75 <- Value.Table %>% dplyr::filter(b2.abs.diff > 75)               #15558,  > 75
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.75, aes(x=x, y=y), color = "green4")

b2.more.abs.100 <- Value.Table %>% dplyr::filter(b2.abs.diff > 100)             #5784,  > 100
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.100, aes(x=x, y=y), color = "green4")

b2.more.abs.125 <- Value.Table %>% dplyr::filter(b2.abs.diff > 125)             #1241,  > 125
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.125, aes(x=x, y=y), color = "green4")

b2.more.abs.150 <- Value.Table %>% dplyr::filter(b2.abs.diff > 150)             #205,  > 150
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.150, aes(x=x, y=y), color = "green4")

b2.more.abs.175 <- Value.Table %>% dplyr::filter(b2.abs.diff > 175)             #45,  > 175
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b2.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b2.more.abs.175, aes(x=x, y=y), color = "green4")

# Band 3

b3.more.abs.0 <- Value.Table %>% dplyr::filter(b3.abs.diff > 0)                 #425711,  > 0
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.0, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.25 <- Value.Table %>% dplyr::filter(b3.abs.diff > 25)               #54002,  > 25
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.25, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.50 <- Value.Table %>% dplyr::filter(b3.abs.diff > 50)               #26976,  > 50
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.50, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.75 <- Value.Table %>% dplyr::filter(b3.abs.diff > 75)               #12156,  > 75
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.75, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.100 <- Value.Table %>% dplyr::filter(b3.abs.diff > 100)             #4022,  > 100
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.100, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.125 <- Value.Table %>% dplyr::filter(b3.abs.diff > 125)             #662,  > 125
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.125, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.150 <- Value.Table %>% dplyr::filter(b3.abs.diff > 150)             #99,  > 150
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.150, aes(x=x, y=y), color = "dodgerblue3")

b3.more.abs.175 <- Value.Table %>% dplyr::filter(b3.abs.diff > 175)             #17,  > 175
ggplot() + geom_tile(data=Value.Table, aes(x=x, y=y, fill=P2011.b3.values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=b3.more.abs.175, aes(x=x, y=y), color = "dodgerblue3")

# Change detection by the biggest value difference between two images (Page 7)---------------------------------

# Dataframe for plotting biggest value difference pixels by object (Table 5, Page 7)
# The results of drawExtent will be applied. (Pls refer to Row 701, 733, 762, 797)
# So, number of cells may be different depending on the extent selected

# Making Dataframe to calculate the number of the biggest value difference pixel by object 
# (Table 5, Page 7)   It continues to row 1402.

# Sea

Sea.b1.diff <- abs(values(crop(P2011.b1, Sea))-values(crop(P2012.b1, Sea))) #19656
Sea.b2.diff <- abs(values(crop(P2011.b2, Sea))-values(crop(P2012.b2, Sea))) #19656
Sea.b3.diff <- abs(values(crop(P2011.b3, Sea))-values(crop(P2012.b3, Sea))) #19656
Sea.df <- data.frame(Sea.b1.diff, Sea.b2.diff, Sea.b3.diff, stringsAsFactors = FALSE) #19656 
Sea.df <- Sea.df %>% mutate(max.Sea.diff.value = apply(Sea.df[,c(1, 2, 3)], 1, max)) %>%
  dplyr::filter(max.Sea.diff.value != 0) %>%           # all are more than 0, so still #19656 
  mutate(big.Sea.diff.band =
           ifelse(Sea.b1.diff > Sea.b2.diff & Sea.b1.diff > Sea.b3.diff, 'Sea.b1.diff',
                  ifelse(Sea.b2.diff > Sea.b1.diff & Sea.b2.diff > Sea.b3.diff, 'Sea.b2.diff',
                         ifelse(Sea.b3.diff > Sea.b1.diff & Sea.b3.diff > Sea.b2.diff,
                                'Sea.b3.diff', 'NA')))) %>%
    dplyr::filter(big.Sea.diff.band != "NA")    
#18916 (780 cells' biggest values shared by more than 1 band) 

# Values could be variable depending on extent
range(Sea.df$max.Sea.diff.value)   # Range: 0.015625 to 6.078125

# Value > 0  # 18916 
ncell.Sea.0 <- Sea.df %>% dplyr::filter(max.Sea.diff.value > 0) 
count(ncell.Sea.0) 
ncell.Sea.b1.0 <- Sea.df %>% dplyr::filter(max.Sea.diff.value > 0 & big.Sea.diff.band == "Sea.b1.diff")
ncell.Sea.b2.0 <- Sea.df %>% dplyr::filter(max.Sea.diff.value > 0 & big.Sea.diff.band == "Sea.b2.diff")
ncell.Sea.b3.0 <- Sea.df %>% dplyr::filter(max.Sea.diff.value > 0 & big.Sea.diff.band == "Sea.b3.diff")
count(ncell.Sea.b1.0) # 3986
count(ncell.Sea.b2.0) # 6119
count(ncell.Sea.b3.0) # 8811

# Landfill

Landfill.b1.diff <- abs(values(crop(P2011.b1, Landfill))-values(crop(P2012.b1, Landfill))) #4816
Landfill.b2.diff <- abs(values(crop(P2011.b2, Landfill))-values(crop(P2012.b2, Landfill))) #4816
Landfill.b3.diff <- abs(values(crop(P2011.b3, Landfill))-values(crop(P2012.b3, Landfill))) #4816
Landfill.df <- data.frame(Landfill.b1.diff, Landfill.b2.diff, Landfill.b3.diff,
                          stringsAsFactors = FALSE) #4816
Landfill.df <- Landfill.df %>% 
  mutate(max.Landfill.diff.value = apply(Landfill.df[,c(1, 2, 3)], 1, max)) %>%
  dplyr::filter(max.Landfill.diff.value != 0) %>%           # all are more than 0, so still #4816 
  mutate(big.Landfill.diff.band =
           ifelse(Landfill.b1.diff > Landfill.b2.diff &
                    Landfill.b1.diff > Landfill.b3.diff, 'Landfill.b1.diff',
                  ifelse(Landfill.b2.diff > Landfill.b1.diff &
                           Landfill.b2.diff > Landfill.b3.diff, 'Landfill.b2.diff',
                         ifelse(Landfill.b3.diff > Landfill.b1.diff &
                                  Landfill.b3.diff > Landfill.b2.diff,
                                'Landfill.b3.diff', 'NA')))) %>%
  dplyr::filter(big.Landfill.diff.band != "NA")    
#4816 (0 cells' biggest values shared by more than 1 band)     

range(Landfill.df$max.Landfill.diff.value) # Range: 0.8750 to 127.4688
# Values could be variable depending on extent
# Value > 0  # 4816 
ncell.Landfill.0 <- Landfill.df %>% dplyr::filter(max.Landfill.diff.value > 0) 
count(ncell.Landfill.0) 
ncell.Landfill.b1.0 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 0 & big.Landfill.diff.band == "Landfill.b1.diff")
ncell.Landfill.b2.0 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 0 & big.Landfill.diff.band == "Landfill.b2.diff")
ncell.Landfill.b3.0 <- Landfill.df %>%
  dplyr::filter(max.Landfill.diff.value > 0 & big.Landfill.diff.band == "Landfill.b3.diff")
count(ncell.Landfill.b1.0) # 4812
count(ncell.Landfill.b2.0) # 1
count(ncell.Landfill.b3.0) # 3

# Value > 25
ncell.Landfill.25 <- Landfill.df %>% dplyr::filter(max.Landfill.diff.value > 25)
count(ncell.Landfill.25) # 4,805 
ncell.Landfill.b1.25 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 25 & big.Landfill.diff.band == "Landfill.b1.diff")
ncell.Landfill.b2.25 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 25 & big.Landfill.diff.band == "Landfill.b2.diff")
ncell.Landfill.b3.25 <- Landfill.df %>%
  dplyr::filter(max.Landfill.diff.value > 25 & big.Landfill.diff.band == "Landfill.b3.diff")
count(ncell.Landfill.b1.25) # 4804
count(ncell.Landfill.b2.25) # 0
count(ncell.Landfill.b3.25) # 1

# Value > 50
ncell.Landfill.50 <- Landfill.df %>% dplyr::filter(max.Landfill.diff.value > 50)
count(ncell.Landfill.50) # 4,173 
ncell.Landfill.b1.50 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 50 & big.Landfill.diff.band == "Landfill.b1.diff")
ncell.Landfill.b2.50 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 50 & big.Landfill.diff.band == "Landfill.b2.diff")
ncell.Landfill.b3.50 <- Landfill.df %>%
  dplyr::filter(max.Landfill.diff.value > 50 & big.Landfill.diff.band == "Landfill.b3.diff")
count(ncell.Landfill.b1.50) # 4,173
count(ncell.Landfill.b2.50) # 0
count(ncell.Landfill.b3.50) # 0

# Value > 75
ncell.Landfill.75 <- Landfill.df %>% dplyr::filter(max.Landfill.diff.value > 75)
count(ncell.Landfill.75) # 2,954 
ncell.Landfill.b1.75 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 75 & big.Landfill.diff.band == "Landfill.b1.diff")
ncell.Landfill.b2.75 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 75 & big.Landfill.diff.band == "Landfill.b2.diff")
ncell.Landfill.b3.75 <- Landfill.df %>%
  dplyr::filter(max.Landfill.diff.value > 75 & big.Landfill.diff.band == "Landfill.b3.diff")
count(ncell.Landfill.b1.75) # 2,954
count(ncell.Landfill.b2.75) # 0
count(ncell.Landfill.b3.75) # 0

# Value > 100
ncell.Landfill.100 <- Landfill.df %>% dplyr::filter(max.Landfill.diff.value > 100)
count(ncell.Landfill.100) # 1,352
ncell.Landfill.b1.100 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 100 & big.Landfill.diff.band == "Landfill.b1.diff")
ncell.Landfill.b2.100 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 100 & big.Landfill.diff.band == "Landfill.b2.diff")
ncell.Landfill.b3.100 <- Landfill.df %>%
  dplyr::filter(max.Landfill.diff.value > 100 & big.Landfill.diff.band == "Landfill.b3.diff")
count(ncell.Landfill.b1.100) # 1,352
count(ncell.Landfill.b2.100) # 0
count(ncell.Landfill.b3.100) # 0

# Value > 125
ncell.Landfill.125 <- Landfill.df %>% dplyr::filter(max.Landfill.diff.value > 125)
count(ncell.Landfill.125) # 3
ncell.Landfill.b1.125 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 125 & big.Landfill.diff.band == "Landfill.b1.diff")
ncell.Landfill.b2.125 <- Landfill.df %>% 
  dplyr::filter(max.Landfill.diff.value > 125 & big.Landfill.diff.band == "Landfill.b2.diff")
ncell.Landfill.b3.125 <- Landfill.df %>%
  dplyr::filter(max.Landfill.diff.value > 125 & big.Landfill.diff.band == "Landfill.b3.diff")
count(ncell.Landfill.b1.125) # 3
count(ncell.Landfill.b2.125) # 0
count(ncell.Landfill.b3.125) # 0



# Vegetation dataframe

Vegetation.b1.diff <- abs(values(crop(P2011.b1, Vegetation))-values(crop(P2012.b1, Vegetation))) #806
Vegetation.b2.diff <- abs(values(crop(P2011.b2, Vegetation))-values(crop(P2012.b2, Vegetation))) #806
Vegetation.b3.diff <- abs(values(crop(P2011.b3, Vegetation))-values(crop(P2012.b3, Vegetation))) #806
Vegetation.df <- data.frame(Vegetation.b1.diff, Vegetation.b2.diff, Vegetation.b3.diff, 
                            stringsAsFactors = FALSE)      #806  
Vegetation.df <- Vegetation.df %>% mutate(max.Vegetation.diff.value = 
                                            apply(Vegetation.df[,c(1, 2, 3)], 1, max)) %>%
  dplyr::filter(max.Vegetation.diff.value != 0) %>%        # 805 
  mutate(big.Vegetation.diff.band =    
           ifelse(Vegetation.b1.diff > Vegetation.b2.diff & 
                    Vegetation.b1.diff > Vegetation.b3.diff, 'Vegetation.b1.diff',
                  ifelse(Vegetation.b2.diff > Vegetation.b1.diff & 
                           Vegetation.b2.diff > Vegetation.b3.diff, 'Vegetation.b2.diff',
                         ifelse(Vegetation.b3.diff > Vegetation.b1.diff &
                                  Vegetation.b3.diff > Vegetation.b2.diff, 
                                'Vegetation.b3.diff', 'NA')))) %>%
  dplyr::filter(big.Vegetation.diff.band != "NA")    
#805 (0 cells' biggest values shared by more than 1 band)     

range(Vegetation.df$max.Vegetation.diff.value) # Range: 14.90625 to 136.92188
# Values could be variable depending on extent
# Value > 0  # 805
ncell.Vegetation.0 <- Vegetation.df %>% dplyr::filter(max.Vegetation.diff.value > 0) 
count(ncell.Vegetation.0)   
ncell.Vegetation.b1.0 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 0 &
                  big.Vegetation.diff.band == "Vegetation.b1.diff")
ncell.Vegetation.b2.0 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 0 & 
                  big.Vegetation.diff.band == "Vegetation.b2.diff")
ncell.Vegetation.b3.0 <- Vegetation.df %>%
  dplyr::filter(max.Vegetation.diff.value > 0 &
                  big.Vegetation.diff.band == "Vegetation.b3.diff")
count(ncell.Vegetation.b1.0) # 32
count(ncell.Vegetation.b2.0) # 648
count(ncell.Vegetation.b3.0) # 125

# Value > 25
ncell.Vegetation.25 <- Vegetation.df %>% dplyr::filter(max.Vegetation.diff.value > 25)
count(ncell.Vegetation.25) # 783 
ncell.Vegetation.b1.25 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 25 & 
                  big.Vegetation.diff.band == "Vegetation.b1.diff")
ncell.Vegetation.b2.25 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 25 &
                  big.Vegetation.diff.band == "Vegetation.b2.diff")
ncell.Vegetation.b3.25 <- Vegetation.df %>%
  dplyr::filter(max.Vegetation.diff.value > 25 &
                  big.Vegetation.diff.band == "Vegetation.b3.diff")
count(ncell.Vegetation.b1.25) # 25
count(ncell.Vegetation.b2.25) # 641
count(ncell.Vegetation.b3.25) # 117

# Value > 50
ncell.Vegetation.50 <- Vegetation.df %>% dplyr::filter(max.Vegetation.diff.value > 50)
count(ncell.Vegetation.50) # 667 
ncell.Vegetation.b1.50 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 50 & 
                  big.Vegetation.diff.band == "Vegetation.b1.diff")
ncell.Vegetation.b2.50 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 50 & 
                  big.Vegetation.diff.band == "Vegetation.b2.diff")
ncell.Vegetation.b3.50 <- Vegetation.df %>%
  dplyr::filter(max.Vegetation.diff.value > 50 &
                  big.Vegetation.diff.band == "Vegetation.b3.diff")
count(ncell.Vegetation.b1.50) # 17
count(ncell.Vegetation.b2.50) # 600
count(ncell.Vegetation.b3.50) # 50

# Value > 75
ncell.Vegetation.75 <- Vegetation.df %>% dplyr::filter(max.Vegetation.diff.value > 75)
count(ncell.Vegetation.75) # 472 

ncell.Vegetation.b1.75 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 75 & 
                  big.Vegetation.diff.band == "Vegetation.b1.diff")
ncell.Vegetation.b2.75 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 75 & 
                  big.Vegetation.diff.band == "Vegetation.b2.diff")
ncell.Vegetation.b3.75 <- Vegetation.df %>%
  dplyr::filter(max.Vegetation.diff.value > 75 &
                  big.Vegetation.diff.band == "Vegetation.b3.diff")
count(ncell.Vegetation.b1.75) # 16
count(ncell.Vegetation.b2.75) # 453
count(ncell.Vegetation.b3.75) # 3

# Value > 100
ncell.Vegetation.100 <- Vegetation.df %>% dplyr::filter(max.Vegetation.diff.value > 100)
count(ncell.Vegetation.100) # 260

ncell.Vegetation.b1.100 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 100 & 
                  big.Vegetation.diff.band == "Vegetation.b1.diff")
ncell.Vegetation.b2.100 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 100 & 
                  big.Vegetation.diff.band == "Vegetation.b2.diff")
ncell.Vegetation.b3.100 <- Vegetation.df %>%
  dplyr::filter(max.Vegetation.diff.value > 100 &
                  big.Vegetation.diff.band == "Vegetation.b3.diff")
count(ncell.Vegetation.b1.100) # 11
count(ncell.Vegetation.b2.100) # 249
count(ncell.Vegetation.b3.100) # 0

# Value > 125
ncell.Vegetation.125 <- Vegetation.df %>% dplyr::filter(max.Vegetation.diff.value > 125)
count(ncell.Vegetation.125) # 56
ncell.Vegetation.b1.125 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 125 & 
                  big.Vegetation.diff.band == "Vegetation.b1.diff")
ncell.Vegetation.b2.125 <- Vegetation.df %>% 
  dplyr::filter(max.Vegetation.diff.value > 125 & 
                  big.Vegetation.diff.band == "Vegetation.b2.diff")
ncell.Vegetation.b3.125 <- Vegetation.df %>%
  dplyr::filter(max.Vegetation.diff.value > 125 &
                  big.Vegetation.diff.band == "Vegetation.b3.diff")
count(ncell.Vegetation.b1.125) # 2
count(ncell.Vegetation.b2.125) # 54
count(ncell.Vegetation.b3.125) # 0

# Building dataframe  

Building.b1.diff <- abs(values(crop(P2011.b1, Building))-values(crop(P2012.b1, Building))) #5740
Building.b2.diff <- abs(values(crop(P2011.b2, Building))-values(crop(P2012.b2, Building))) #5740
Building.b3.diff <- abs(values(crop(P2011.b3, Building))-values(crop(P2012.b3, Building))) #5740 
Building.df <- data.frame(Building.b1.diff, Building.b2.diff, Building.b3.diff, 
                            stringsAsFactors = FALSE)      #5740  
Building.df <- Building.df %>% mutate(max.Building.diff.value = 
                                            apply(Building.df[,c(1, 2, 3)], 1, max)) %>%
  dplyr::filter(max.Building.diff.value != 0) %>%  # 5740 all are more than 0, so still #5740
  mutate(big.Building.diff.band =    
           ifelse(Building.b1.diff > Building.b2.diff & 
                    Building.b1.diff > Building.b3.diff, 'Building.b1.diff',
                  ifelse(Building.b2.diff > Building.b1.diff & 
                           Building.b2.diff > Building.b3.diff, 'Building.b2.diff',
                         ifelse(Building.b3.diff > Building.b1.diff &
                                  Building.b3.diff > Building.b2.diff, 
                                'Building.b3.diff', 'NA')))) %>%
  dplyr::filter(big.Building.diff.band != "NA")    
#5,738 (2 cells' biggest values shared by more than 1 band)     

range(Building.df$max.Building.diff.value)      # Range: 1.59375 to 227.23438

# Values could be variable depending on extent
# Value > 0  # 5,738
ncell.Building.0 <- Building.df %>% dplyr::filter(max.Building.diff.value > 0) 
count(ncell.Building.0)   
ncell.Building.b1.0 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 0 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.0 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 0 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.0 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 0 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.0) # 1985
count(ncell.Building.b2.0) # 1671
count(ncell.Building.b3.0) # 2082

# Value > 25  # 4,072
ncell.Building.25 <- Building.df %>% dplyr::filter(max.Building.diff.value > 25) 
count(ncell.Building.25)   

ncell.Building.b1.25 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 25 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.25 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 25 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.25 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 25 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.25) # 1510
count(ncell.Building.b2.25) # 934
count(ncell.Building.b3.25) # 1628

# Value > 50  # 2,442
ncell.Building.50 <- Building.df %>% dplyr::filter(max.Building.diff.value > 50) 
count(ncell.Building.50)   
ncell.Building.b1.50 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 50 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.50 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 50 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.50 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 50 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.50) # 988
count(ncell.Building.b2.50) # 443
count(ncell.Building.b3.50) # 1011

# Value > 75  # 1,331
ncell.Building.75 <- Building.df %>% dplyr::filter(max.Building.diff.value > 75) 
count(ncell.Building.75)   
ncell.Building.b1.75 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 75 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.75 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 75 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.75 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 75 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.75) # 629
count(ncell.Building.b2.75) # 186
count(ncell.Building.b3.75) # 516

# Value > 100  # 650
ncell.Building.100 <- Building.df %>% dplyr::filter(max.Building.diff.value > 100) 
count(ncell.Building.100)   
ncell.Building.b1.100 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 100 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.100 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 100 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.100 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 100 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.100) # 391
count(ncell.Building.b2.100) # 59
count(ncell.Building.b3.100) # 200

# Value > 125  # 338
ncell.Building.125 <- Building.df %>% dplyr::filter(max.Building.diff.value > 125) 
count(ncell.Building.125)   
ncell.Building.b1.125 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 125 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.125 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 125 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.125 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 125 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.125) # 237
count(ncell.Building.b2.125) # 21
count(ncell.Building.b3.125) # 80

# Value > 150  # 125
ncell.Building.150 <- Building.df %>% dplyr::filter(max.Building.diff.value > 150) 
count(ncell.Building.150)   
ncell.Building.b1.150 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 150 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.150 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 150 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.150 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 150 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.150) # 112
count(ncell.Building.b2.150) # 13
count(ncell.Building.b3.150) # 0

# Value > 175  # 63
ncell.Building.175 <- Building.df %>% dplyr::filter(max.Building.diff.value > 175) 
count(ncell.Building.175)   
ncell.Building.b1.175 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 175 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.175 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 175 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.175 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 175 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.175) # 62
count(ncell.Building.b2.175) # 1
count(ncell.Building.b3.175) # 0

# Value > 200  # 13
ncell.Building.200 <- Building.df %>% dplyr::filter(max.Building.diff.value > 200) 
count(ncell.Building.200)   
ncell.Building.b1.200 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 200 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.200 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 200 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.200 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 200 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.200) # 13
count(ncell.Building.b2.200) # 0
count(ncell.Building.b3.200) # 0

# Value > 225  # 1
ncell.Building.225 <- Building.df %>% dplyr::filter(max.Building.diff.value > 225) 
count(ncell.Building.225)   
ncell.Building.b1.225 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 225 &
                  big.Building.diff.band == "Building.b1.diff")
ncell.Building.b2.225 <- Building.df %>% 
  dplyr::filter(max.Building.diff.value > 225 & 
                  big.Building.diff.band == "Building.b2.diff")
ncell.Building.b3.225 <- Building.df %>%
  dplyr::filter(max.Building.diff.value > 225 &
                  big.Building.diff.band == "Building.b3.diff")
count(ncell.Building.b1.225) # 1
count(ncell.Building.b2.225) # 0
count(ncell.Building.b3.225) # 0


#Modifying Dataframe (Value.Table) to add maximum value difference by band
#This is related to whole area of the images

# Including the biggest difference values out of 3 bands in to the table
Total.values <- Value.Table %>% 
  mutate(max.abs.value = apply(Value.Table[,c(7, 12, 17)], 1, max)) %>%
  dplyr::filter(max.abs.value != 0) %>%    # 428104 (432100-3996) * 3996: max.value == 0 
  mutate(big.abs.diff.band =
           ifelse(b1.abs.diff > b2.abs.diff & b1.abs.diff > b3.abs.diff, 'b1.abs.diff',
                  ifelse(b2.abs.diff > b1.abs.diff & b2.abs.diff > b3.abs.diff, 'b2.abs.diff',
                         ifelse(b3.abs.diff > b1.abs.diff & b3.abs.diff > b2.abs.diff, 'b3.abs.diff', 'NA')))) %>%
  dplyr::filter(big.abs.diff.band != "NA")  
#  418699 (428104-9405): Total number of cells(points) 
#   9405: max.values shared by more than one band

# Plotting biggest value differences band (whole area)
# for Change detection of the objects based on the biggest value difference
# (Table 6, Page 8)
   
# Plotting biggest value differences band (> 0) 
# For reference, out of a total of 432,100 cells, 
# 3,996 cells have the biggest value difference of 0 (zero), 
# and 9,405 cells have the biggest value difference in two or three bands, They were excluded
# Plotting may take much time depending on Number of cell

# Number of cell: 418699 (Band 1(R)-176,155, Band 2(G)-106,746, Band 3(B)-135,798)
Abs.diff.more.0 <- Total.values %>% dplyr::filter(max.abs.value > 0) 
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.0, aes(x=x, y=y,
                                      color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide = "none") +
  theme_void()

# Plotting biggest value differences band (> 25)  
#Number of cell: 84053 (Band 1(R)-56,139, Band 2(G)-14,881, Band 3(B)-13,033)
Abs.diff.more.25 <- Total.values %>% dplyr::filter(max.abs.value > 25) 
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.25, aes(x=x, y=y,
                                       color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide="none") +
  theme_void()

# Plotting biggest value differences band (> 50)  
#Number of cell: 50800 (Band 1(R)-36,521, Band 2(G)-7,588, Band 3(B)-6,691)
Abs.diff.more.50 <- Total.values %>% dplyr::filter(max.abs.value > 50) 
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.50, aes(x=x, y=y,
                                       color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide = "none") +
  theme_void()

# Plotting biggest value differences band (> 75)  
#Number of cell: 29362 (Band 1(R)-22,227, Band 2(G)-3,713, Band 3(B)-3,422)
Abs.diff.more.75 <- Total.values %>% dplyr::filter(max.abs.value > 75) 
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.75, aes(x=x, y=y,
                                       color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide = "none") +
  theme_void()

# Plotting biggest value differences band (> 100)  
#Number of cell: 12386 (Band 1(R)-9,580, Band 2(G)-1,593, Band 3(B)-1,213)
Abs.diff.more.100 <- Total.values %>% dplyr::filter(max.abs.value > 100) 
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.100, aes(x=x, y=y,
                                        color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide = "none") +
  theme_void()

# Plotting biggest value differences band (> 125)  
#Number of cell: 2680 (Band 1(R)-2,104, Band 2(G)-347, Band 3(B)-229)

#colored by band
Abs.diff.more.125 <- Total.values %>% dplyr::filter(max.abs.value > 125) 
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.125, aes(x=x, y=y,
                                        color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide="none") +
  theme_void()

# Plotting biggest value differences band (> 150)  
#Number of cell: 424 (Band 1(R)-346, Band 2(G)-47, Band 3(B)-31)
Abs.diff.more.150 <- Total.values %>% dplyr::filter(max.abs.value > 150)
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.150, aes(x=x, y=y,
                                        color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide="none") +
  theme_void()

# Plotting biggest value differences band (> 175)  
#Number of cell: 91 (Band 1(R)-77, Band 2(G)-2, Band 3(B)-12)
Abs.diff.more.175 <- Total.values %>% dplyr::filter(max.abs.value > 175)
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.175, aes(x=x, y=y,
                                        color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide="none") + theme_void()

# Plotting biggest value differences band (> 200)  
#Number of cell: 19 (Band 1(R)-14, Band 2(G)-0, Band 3(B)-5)
Abs.diff.more.200 <- Total.values %>% dplyr::filter(max.abs.value > 200)
label = c("Band 1", "Band 2", "Band 3")
ggplot() + geom_tile(data=P2011.b1.df, aes(x=x, y=y, fill=values)) +
  scale_fill_gradient(low = "white", high = "gray", guide = "none") +
  geom_tile(data=Abs.diff.more.200, aes(x=x, y=y,
                                        color = big.abs.diff.band)) +
  scale_color_discrete(name="Band w/\n biggest\n difference",
                       labels=label, guide="none") + theme_void()
















  










