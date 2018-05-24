setwd("C:/Users/stuwi/Dropbox/Sierra Project/")
library(rgdal)
library(raster)
library(maptools)
library(reshape)
library(rgeos)
library(cdlTools)

# there is a lot of garbage in the way ryans files were set up

data_1 = read.csv("Ferrell_R Master Data Set_for stu1_CSV.csv", skip = 1, header = T, nrows = 1, as.is = T)


head(data_1)

names(data_1)

#lets process the data 
# all we want are coordiantes and depth 

df = read.csv(file, skip = 3, header = F)
colnames(df)= headers
read