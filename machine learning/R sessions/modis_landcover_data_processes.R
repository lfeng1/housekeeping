# This code downloads modis landcover data using MODISTools.
library(MODISTools)
# format lat-long location data to decimal degrees 
latlongloc <- ConvertExample
modis.subset <- ConvertToDD(XY = latlongloc,LatColName = "lat",LongColName = "long")
modis.subset <- data.frame(lat = modis.subset[,1],long = modis.subset[,2])
modis.subset$start.date <- rep(2012, nrow(modis.subset)) # retrieve data b/t 2012-2013
modis.subset$end.date <- rep(2013, nrow(modis.subset))

# download data
GetProducts() # display all modis products
GetBands(Product = "MCD12Q1") # Land cover product
GetDates(Product = "MCD12Q1", Lat = modis.subset$lat[1], Long = modis.subset$long[1])
dir.create('./LandCover')
setwd("/Users/lianfeng/Documents/machine learning/R sessions/")
MODISSubsets(LoadDat = modis.subset, Product = "MCD12Q1", Bands = "Land_Cover_Type_1", Size = c(1,1))
LandCover(Band = "Land_Cover_Type_1")
data.folder <- "./LandCover"
files <- list.files(path=data.folder, pattern = ".asc")[1] 
land.summary <- read.csv(list.files(pattern = "MODIS_Land_Cover_Summary"))
head(land.summary,10)
