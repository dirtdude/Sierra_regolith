setwd("C:/Users/stuwi/Dropbox/Sierra Project/")
library(rgdal)
library(raster)
library(maptools)
library(reshape)
library(rgeos)
library(cdlTools)
library(dplyr)
library(sp)

install.packages(c("maps", "mapdata"))
# there is a lot of garbage in the way ryans files were set up

data_1 = read.csv("R Master Data Set_for stu1_CSV_cahnged values for_row 21.csv", skip = 1, header = T)


# there are problems with the data
# there are doubles for multiple points for the same tree, whiich I'm not sure how that happens?
# There was an obvious outlier, so I changed the value. There is another one, which I am less comfortable with, and so I'm manually changing and will likely throw it out  it out (117). 




#lets process the data 
# all we want are coordiantes and depth 
# 
data_2<-select(data_1, Latitude, Longitude, Depth)
# 
# with(data_2, plot(Latitude, Longitude))
# 
# with(data_2, identify(Latitude, Longitude))

which(is.na(data_2$Depth))
  data.2<-slice(data_2, c(-117,-20 ))
  which.max(data.2$Latitude)
  
  plot(data.2)
  


plot(data.3)

data.3<-distinct(data.2)

sum(is.na(data.3))
class(data.3)

with(data.3, plot(Latitude, Longitude))

with(data.3, identify(Latitude, Longitude))

data.3<-slice(data.3, -65)

is.na(data.3)
identify(points)
dim(data_2)


is.na(data_2$Longitude)

# Coerce out of tibble (maybe it didn't matter but I didn;t like that it was stored as a dbl and I couldn;t see the decimal for longitude ')

data.3<-data.frame(data.3)

data.3$Longitude<--(data.3$Longitude)



# ok, so the easiest data.3way is to JUST pull the coordiantes, and call them coords

coords<-select(data.3,  Longitude, Latitude)

head(coords)

# It seemed to like it better when the coordiantes were assigned a projection first. 
crdref <- CRS('+proj=longlat +datum=WGS84')
coords<-SpatialPoints(coords,  proj4string=crdref)



coordinates(coords) <- c("X", "Y")
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")  ## for example

res <- spTransform(coords, CRS("+proj=utm +zone=11 ellps=WGS84"))

crs<-CRS("+init=epsg:3310")

points<-spTransform(coords, crs)# still doesn;t want to project so fuck it 


#proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

depth<-select(data.3, Depth)#grab jus teh column we are interesetd in . 


data_3<-SpatialPointsDataFrame(res, depth)



# oks so we are good now

#Ok now create a shape file


shapefile(data_3, filename='depth points', overwrite=TRUE)

points <- shapefile("depth points.shp")

#points<-spTransform(points, crs)#

#check dta cleanlyness
plot(points, add=T)
length(points)# looks good 

sum(is.na(points$Depth)) # ok cool

#############Ok the data Look good

#lets use california albers for our projections
#EPSG:3310: NAD83 / California Albers
#newData <- spTransform(x, CRS("+init=epsg:4238"))

#############################################################################
              #Notes: Ok my conclusion is that this projection doesn't workwitht             these data, 
              # 

# Now we will attempt to import the rasters

setwd("C:/Users/stuwi/Dropbox/Sierra Project/GIS data/")

list.dirs(path = ".", full.names = TRUE, recursive = TRUE)# this was so I could find out waht each directory was and then write a raster with it. 
# for this step I just did a bunch of copy and pasting and put the raster and file name in, eg changed directory, then used directory name to name the raster. 
setwd("C:/Users/stuwi/Dropbox/Sierra Project/GIS data/tpic8_8411/")
tpic8_8411<-raster("w001001.adf")
#r2<-raster("aspect_8411.adf")
# ok so we are getting somewhere

(aspect, cticlp2), dist2oc, dist2r8411, dsdf8411, dist2str8411, tpic8_8411 )

ext<-extent(dsdf8411)
covariables<-stack (aspect,dist2oc, dist2r8411, dsdf8411, dist2str8411, tpic8_8411 ) #cticlp2

f3 <- function(raster) {
  #raster<-projectRaster(raster, crs=crs)
  raster<-resample(raster, dsdf8411, method="bilinear")
  raster<-crop(raster, extent(dsdf8411))
  return(raster)
  
}

# lets crop extent and get different 

aspect.2<-f3(aspect)
dist2oc.2<-f3(dist2oc)
dist2r8411.2<-f3(dist2r8411)
dsdf8411.2<-f3(dsdf8411)
dist2str8411.2<-f3(dist2str8411)
tpic8_8411.2<-f3(tpic8_8411)
cticlp2.2<-f3(cticlp2)

covariables<-stack(aspect.2, dist2oc.2, dist2r8411.2, dsdf8411.2, dist2str8411.2, tpic8_8411.2, cticlp2.2 )

covariable_stack<-extract(covariables, points)

data_stack<-data.frame(cbind(points$Depth, covariable_stack))

names(data_stack)[names(data_stack) == 'V1'] <- 'depth'


############THe Modeling Section########
library(randomForest)
  m2<-randomForest(depth~., data=data_stack, importance=TRUE)
# 
# twi SAME AS cti, TPI is the same as TPI, 
  
  planform  curvature topographic
 # Transvers cant get
 # cant get  profile curvature

m2
head(data_stack)

m1<-lm(depth~., data=data_stack)
summary(m1)


model<-predict(covariables, m1, ext=ext)

plot(model)
  crs<-crs(aspect.2)


test_stack<-stack(r1, r2)
crs<-crs(r1)

plot(r1)


