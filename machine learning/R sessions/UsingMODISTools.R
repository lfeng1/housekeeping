# This code shows a complete work flow for how to use MODISTools.
library(MODISTools)

# format location data for extracting modis images 
data("ConvertExample")
# convert to Decimal degrees
modis.subset <- ConvertToDD(XY = ConvertExample,LatColName = "lat",LongColName = "long")
modis.subset <- data.frame(lat = modis.subset[,1],long = modis.subset[,2])
modis.subset$start.date <- rep(2003, nrow(modis.subset)) # retrieve data b/t 2003-2006
modis.subset$end.date <- rep(2006, nrow(modis.subset))

# download data
GetProducts() # display all modis products
GetBands(Product = "MOD13Q1") # EVI product
GetDates(Product = "MOD13Q1", Lat = modis.subset$lat[1], Long = modis.subset$long[1])

# MODISSubsets should be organized in WGS-1984 decimal degree lat long
# collect quality control data for these pixels as well
data.folder <- "./downloaded_modis" # folders to store downloaded data
MODISSubsets(LoadDat = modis.subset,Products = "MOD13Q1",Bands = c("250m_16_days_EVI","250m_16_days_pixel_reliability"),Size = c(1,1),SaveDir = data.folder)
# the download writes the modis data to ASCII files for each location subset specified
files <- list.files(path=data.folder, pattern = ".asc")[1] 
subset.string <- read.csv(paste(data.folder,files,sep = "/"), header=F, as.is=T)
# each row in each asc file is a different time-stepp in the time-series
# if multiple data bands have been downloaded for this subset, they will be contained in the same ascii file for that subset.
subset.string[1,]
# find average each pixel over time, to produce one tile of mean EVI pixels at each subset location
MODISSummaries(LoadDat = modis.subset, Dir = data.folder, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability",
               QualityThreshold = 0)