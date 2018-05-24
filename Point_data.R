setwd("C:/Users/stuwi/Dropbox/Sierra Project/")
library(rgdal)
library(raster)
library(maptools)
library(reshape)
library(rgeos)
library(cdlTools)
library(dplyr)

install.packages(c("maps", "mapdata"))
# there is a lot of garbage in the way ryans files were set up

data_1 = read.csv("R Master Data Set_for stu1_CSV_cahnged values for_row 21.csv", skip = 1, header = T)


# there are problems with the data
# there are doubles for multiple points for the same tree, whiich I'm not sure how that happens?
# There was an obvious outlier, so I changed the value. There is another one, which I am less comfortable with, and so I'm manually changing and will likely throw it out  it out (117). 


new<-distinct(data_2)


unique(data_2)

dim(data_1)

head(data_1)

names(data_1)

#lets process the data 
# all we want are coordiantes and depth 

data_2<-select(data_1, Latitude, Longitude, Depth)

with(data_2, identify(Latitude, Longitude))

which(is.na(data_2$Depth))
  data.2<-slice(data_2, c(-117,-20 ))
  
sum(is.na(data.2))

data.3<-distinct(data.2)

is.na(data.3)
identify(points)
dim(data_2)

which.max(data_2$Longitude)

data_2$Longitude[[103]]
head(data_2$Longitude)

data_2$Longitude<--(data_2$Longitude)
head(data_2$Longitude)

is.na(data_2$Longitude)

# Coerce out of tibble (maybe it didn't matter but I didn;t like that it was stored as a dbl and I couldn;t see the decimal for longitude ')

<-data.frame(data.3)


library(sp)



# ok, so the easiest data.3way is to JUST pull the coordiantes, and call them coords

coords<-select(data.3, Latitude, Longitude)

head(coords)

# It seemed to like it better when the coordiantes were assigned a projection first. 
crdref <- CRS('+proj=longlat +datum=WGS84')
coords<-SpatialPoints(coords,  proj4string=crdref)

crs<-CRS("+init=epsg:3310")

#points<-spTransform(coords, crs)# still doesn;t want to project so fuck it 


#proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

depth<-select(data.3, Depth)

data_3<-SpatialPointsDataFrame(coords, depth)

# oks so we are good now

#Ok now create a shape file
?shapefile

shapefile()

shapefile(data_3, filename='depth points', overwrite=TRUE)

points <- shapefile("depth points.shp")

crs<-CRS("+init=epsg:3310")




plot(s)

s

#lets use california albers for our projections
#EPSG:3310: NAD83 / California Albers
#newData <- spTransform(x, CRS("+init=epsg:4238"))

install.packages("sf")
library("sf")
crs.new<-CRS("+init=epsg:3310")

crs.new<-CRS("+init=EPSG:4326")

crs.new<- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0
+y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

plot(points)




crs(points)
points<-spTransform(points, crs.new)# ok so this worked fine



proj4string(data_3) <- CRS("+proj=longlat +datum=WGS84")
pts <- SpatialPoints(lonlat, proj4string=crdref)

plot(data_3)

#lets try with sf
library("sf")
tdwg4.laea = sf::read_sf("depth points.shp")  # assumes in project root
tdwg4.laea = sf::st_transform(tdwg4.laea, 3310)

plot(tdwg4.laea)

which.max(depth)

# Ok my conclusion is that this projection doesn;t work witht these data, 
