# calculate the effect of forested area
dat <- read.csv("S:/Tech/lfeng/research/FL_SR/plot_level_count_area_iv_hof_models/perplotperspcd_count_area_iv_details.csv")

# select FL species & excludes coastal parts of LA, MS, AL, and GA 
fl_grp_descr <- "FLORIDA 2014: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
la_grp_descr <- "LOUISIANA 2014: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
ms_grp_descr <- "MISSISSIPPI 2015: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
data.fl <- dat.sel[dat.sel$eval_grp_descr == fl_grp_descr,]
dat1 <- dat[dat$spcd %in% data.fl$spcd,] # occurence data for Florida tree species in east of Mississippi 
max_lat_Fl <- max(data.fl$lat)

lower_max_lat_fl <- dat[dat$lat < max_lat_Fl,]
lower_max_lat_fl_outside_fl <- lower_max_lat_fl[lower_max_lat_fl$eval_grp_descr == la_grp_descr| lower_max_lat_fl$eval_grp_descr == ms_grp_descr,] # coastal data but not in FL
dat2 <- setdiff(dat1,lower_max_lat_fl_outside_fl) # exclude occurence records in coastal neighbors of the same latitude with Florida 
dat2$latband <- round(dat2$lat)

unique_plot <- subset(dat2,!duplicated(plotsnap_cn))
unique_plot$latband <- round(unique_plot$lat)
unique_plot.byplot <- unique_plot %>% group_by(latband) %>% dplyr::summarise(count = n())
unique_plot.byplot.FL <- unique_plot.byplot[unique_plot.byplot$latband <=31,]
names(unique_plot.byplot.FL) <- c("latband","plot_num")

# calculate species richness
dat2.sr <- dat2 %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
unique.sp <- dat2.sr %>% group_by(latband) %>% dplyr::summarise(count = n())
unique.sp.FL <- unique.sp[unique.sp$latband <=31,]
names(unique.sp.FL) <- c("latband","species_richness")
area_sr <- merge(unique_plot.byplot.FL,unique.sp.FL)
area_sr$ratio <- area_sr$species_richness/area_sr$plot_num
area_sr
plot(area_sr$latband,area_sr$ratio)
