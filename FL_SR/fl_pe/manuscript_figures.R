x <- c("raster","raster","dplyr","rgdal")
lapply(x,library,character.only =T)

# retrieve the ecoregions in the US
eco_eus <- readOGR("S:/Tech/lfeng/research/FL_SR/2017/gis_data","eco_eus")
proj4string(eco_eus) # PROJ: Lambert Azimuthal Equal Area
epsg <- make_EPSG() # create data frame of available EPSG codes
epsg[grepl("WGS 84$",epsg$note),] # search for WGS 84 code
eco_eus_nad83 <- spTransform(eco_eus, CRS("+init=epsg:4269")) # reproject/converts lnd into WGS84 CRS
eco_eus_f <- fortify(eco_eus_nad83)
eco_eus_nad83$id <- row.names(eco_eus_nad83)
eco_eus_f <- dplyr::left_join(eco_eus_f,eco_eus_nad83@data)
g <- ggplot() + geom_polygon(data = eco_eus_f, aes(x = long, y = lat, group= group, fill = PROVINCE)) +coord_equal() + theme(plot.title = element_text(size=12)) + guides(fill=FALSE)
g 
# + guides(fill=FALSE)

tbl <- read.csv("output/dat_lat_lon_band_all.csv")
fl_sp_list <- read.csv("output/florida_sp_list.csv")
fl_sp_list <- fl_sp_list[order(fl_sp_list$lat_round,decreasing = F),]

pdf(paste("output/fl_sp_range_map.pdf"))
for (i in 1:nrow(fl_sp_list)){
    spcd <- fl_sp_list$spcd[i]
    common_name <- fl_sp_list$common_name[i]
    dat <- tbl[which(tbl$spcd == spcd),]
    print(g + geom_point(data = dat,aes(x = lon,y = lat),size = 0.5) + ggtitle(paste(spcd,common_name)) +  labs(x = "Longitude", y = "Latitude"))  
}
dev.off()

pdf(paste("hof_result_I.pdf",sep = ""))
for (i in 1:nrow(hof_result_I)){
  spcd <- hof_result_I$spcd[i]
  common_name <- hof_result_I$common_name[i]
  dat <- tbl[which(tbl$spcd == spcd),]
  print(g + geom_point(data = dat,aes(x = lon,y = lat),size = 0.5) + ggtitle(paste(spcd,common_name)) +  labs(x = "Longitude", y = "Latitude"))  
}
dev.off()


pdf(paste("peninsula_effect_species_range_map.pdf",sep = ""))
for (i in 1:nrow(peninEffect)){
  spcd <- peninEffect$spcd[i]
  common_name <- peninEffect$common_name[i]
  dat <- tbl[which(tbl$spcd == spcd),]
  print(g + geom_point(data = dat,aes(x = lon,y = lat),size = 0.5) + ggtitle(paste(spcd,common_name)) +  labs(x = "Longitude", y = "Latitude"))  
}
dev.off()

