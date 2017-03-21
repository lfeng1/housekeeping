### using rts package: ModisDownload

library(rts)
library(raster)
library(RCurl)
#setNASAauth
# setting the working directory

modisProducts( )
x=3 # or x="MOD14A1"
# download 4 tiles (h14v04, h14v05, h15v04, h15v05) in single date (2011.05.01)
# Following command only downloads the source HDF images, no mosaic and no projection
ModisDownload(x=x,h=c(17,18),v=c(4,5),dates='2011.05.01',mosaic=F,proj=F)
# alternatively, you can use modisHDF to download only HDF images:
modisHDF(x=x,h=c(17,18),v=c(4,5),dates='2011.05.01')
# same as the above command, but downloads all available images in 2011:
ModisDownload(x=x,h=c(17,18),v=c(4,5),dates=c('2011.01.01','2011.12.31'))
#------ if you need different version of the product, let's say 006, you can simply put version="006" in the funcion
# Downloads selected tiles, and mosaic them, but no projections:

ModisDownload(x=x,h=c(17,18),v=c(4,5),dates=c('2011.05.01','2011.05.31'),MRTpath='d:/MRT/bin',mosaic=T,proj=F)
#--- alternatively, you can first download the HDF images using getMODIS, and then mosaic them using mosaicHDF!
# Downloads selected tiles, and mosaic, reproject them in UTM_WGS84, zone 30 projection and convert all bands into Geotif format (the original HDF will be deleted!):
ModisDownload(x=x,h=c(17,18),v=c(4,5),dates=c('2011.05.01','2011.05.31'),MRTpath='d:/MRT/bin', mosaic=T,proj=T,proj_type="UTM",utm_zone=30,datum="WGS84",pixel_size=1000)
# Same as above command, but only second band out of 6 bands will be kept. (You do not need to specify proj_params when "UTM" is selected as proj_type and the zone also is specified, but for other types of projections you do).
ModisDownload(x=x,h=c(17,18),v=c(4,5),dates=c('2011.05.01','2011.05.31'),MRTpath='d:/MRT/bin',mosaic=T,proj=T, bands_subset="0 1 0 0 0 0", proj_type="UTM",proj_params="-3 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=30,datum="WGS84",pixel_size=1000)
# Same as above command, but it spatially subsets the images into the specified box (UL and LR):
ModisDownload(x=x,h=c(17,18),v=c(4,5),dates=c('2011.05.01','2011.05.31'),MRTpath='d:/MRT/bin',mosaic=T,proj=T,UL=c(-42841.0,4871530.0),LR=c(1026104,3983860), bands_subset="0 1 0 0 0 0", proj_type="UTM",proj_params="-3 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=30,datum="WGS84",pixel_size=1000)

# setting the working directory:
setwd('./modis')
library(raster)
# A function to list the files, ended with '.tif', in the working directory
listfiles <- list.files(pattern='.tif$')
listfiles
# function to read an image or images
lai <- raster("MOD15A2_2011_05_25.Lai_1km.tif")
lai
# Multiplying to the scale factor of LAI
lai <- lai * 0.1
plot(lai)