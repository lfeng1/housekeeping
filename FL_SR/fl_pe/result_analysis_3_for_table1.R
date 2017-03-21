# calculate the relative weight of species for each HOF model type and find the top 5 most common species for each type 
# retrieve their scientific name and common name
hof_result <- read.csv("./data/hof_result.csv")
fl_ls <- read.csv("./data/florida_list.csv")
florida <- fl_ls[,c("spcd","common_name","Genus","Species")]

# For HOF model type II, III & V, group species based on the relative location of maxima to range center
hof_result[,"types"] <- "" 
for (i in 1:nrow(hof_result)){
  if (hof_result$best_model[i] == "II" | hof_result$best_model[i] == "III"){
    peak_val <- as.numeric(hof_result$peak_val[i])
    min_val <- as.numeric(hof_result$min_val[i])
    max_val <- as.numeric(hof_result$max_val[i])
    if (round(peak_val - max_val) == 0){
      hof_result$types[i] <- "N"
    }
    if (round(peak_val - min_val) == 0){
      hof_result$types[i] <- "S"
    }
  }
  if (hof_result$best_model[i] == "V"){
    peak_lat <- as.numeric(hof_result$peak_lat[i])
    min_lat <- as.numeric(hof_result$min_lat[i])
    max_lat <- as.numeric(hof_result$max_lat[i])
    if ((peak_lat - min_lat) < 0.5 * (max_lat - min_lat)){
      hof_result$types[i] <- "S"
    } 
    else{
      hof_result$types[i] <- "N"
    } 
  }
}

florida[,"hof_model"] <- ""
florida[,"direction"] <- ""
for (i in 1:nrow(florida)){
  spcd <- florida$spcd[[i]]
  if (spcd %in% hof_result$spcd){
    florida$hof_model[[i]] <- as.character(hof_result[hof_result$spcd ==spcd,]$best_model)
    florida$direction[[i]] <- as.character(hof_result[hof_result$spcd ==spcd,]$types)
  }
  else{
    florida$hof_model[[i]] <- "endemic species"
  }
}
write.csv(florida,"./florida_13_species_hof_results.csv")

# retrieve cleaned data for Florida which went through HOF model simulation for 92 speciese 
dat <- read.csv("./data/all_species_observations_for_hof.csv")
dat <- dat[,c("spcd","common_name","count","count_all","lat")]
dat.Florida <- dat[dat$lat <= 31.08,]
dat[,"latband"] <- NA
for (i in 1:length(dat$spcd)){
  dat$latband[[i]] <- round(dat$lat[i],digits = 0)
}

# for model type II,III, V
hof_result_V_N <- hof_result[which(hof_result$best_model == 'V' & hof_result$types == 'N'),"spcd"]
hof_result_V_S <- hof_result[which(hof_result$best_model == 'V' & hof_result$types == 'S'),"spcd"]
hof_result_II_N <- hof_result[which(hof_result$best_model == 'II' & hof_result$types == 'N'),"spcd"]
hof_result_II_S <- hof_result[which(hof_result$best_model == 'II' & hof_result$types == 'S'),"spcd"]
hof_result_I <- hof_result[which(hof_result$best_model == 'I'),"spcd"]
hof_result_IV <- hof_result[which(hof_result$best_model == 'IV'),"spcd"]
hof_types <- list(hof_result_V_N,hof_result_V_S,hof_result_II_N,hof_result_II_S,hof_result_I,hof_result_IV)

# loop through each subtype of hof model types, find the top 5 most abundant species based on occurrence counts
for (i in 1:length(hof_types)){
  spcd.ls <- hof_types[i]
  dat.spcd.ls <- dat[dat$spcd %in% spcd.ls[[1]],]
  dat.hof.subtype <- dat.spcd.ls[,c("spcd","common_name","count","count_all")]
  data.mean <- ddply(dat.hof.subtype, c("spcd","common_name"), summarize, mean = mean(count),mean_all = mean(count_all))
  data.mean$rel_mean <- data.mean$mean/data.mean$mean_all
  data.mean <- data.mean[order(data.mean$rel_mean,decreasing = T),]
  top5 <- head(data.mean,5)
  spcd <- top5$spcd[i]
  top5[,"genus"] <- NA
  top5[,"species"] <- NA
  for (j in 1:length(top5$spcd)){
    spcd <- top5$spcd[[j]]
    top5$genus[[j]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Genus")])
    top5$species[[j]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Species")])
  }
  out <- paste(top5$common_name, top5$genus,top5$species, sep = " ")
  assign(paste('type', i, sep=''), out)
}

# hof model type III for each subtype, there is only 2 records 
hof_result_III <- hof_result[which(hof_result$best_model == 'III'),]
for (j in 1:length(hof_result_III$spcd)){
  spcd <- hof_result_III$spcd[[j]]
  hof_result_III$genus[[j]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Genus")])
  hof_result_III$species[[j]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Species")])
}
paste(hof_result_III$types,hof_result_III$common_name, hof_result_III$genus,hof_result_III$species, sep = " ")

# endemic species
dat <- read.csv("data/pergridperspcd_count_area_iv_details.csv")
FloridaEndemic <- setdiff(fl_ls$spcd,hof_result$spcd)
dat1 <- dat[dat$spcd %in% FloridaEndemic,] # occurence data for Florida tree species in east of Mississippi 
fl_grp_descr <- "FLORIDA 2014: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
dat.endemics <- dat1[dat1$eval_grp_descr == fl_grp_descr,]
dat.endemics[,"latband"] <- NA
for (i in 1:length(dat.endemics$spcd)){
  dat.endemics$latband[[i]] <- round(dat.endemics$lat[i],digits = 0)
}
dat.endemics <- dat.endemics[,c("spcd","common_name","count","count_all")]
data.mean.endemics <- ddply(dat.endemics, c("spcd","common_name"), summarize, mean = mean(count),mean_all = mean(count_all))
data.mean.endemics$rel_mean <- data.mean.endemics$mean/data.mean.endemics$mean_all
data.mean.endemics <- data.mean.endemics[order(data.mean.endemics$rel_mean,decreasing = T),]
sp.can <- head(data.mean,5)
top5[,"genus"] <- NA
top5[,"species"] <- NA
for (j in 1:length(top5$spcd)){
  spcd <- top5$spcd[[j]]
  top5$genus[[j]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Genus")])
  top5$species[[j]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Species")])
}
endemics.top5 <- paste(top5$common_name, top5$genus,top5$species, sep = " ")

########## list of species that extend their range to CAN##############
sp.list.CAN <- c(951,531, 972, 402, 762, 602, 313, 462,316, 975, 541, 621)
row.names <- c("spcd","common_name","genus","species")
sp.can<-array(dim=c(length(sp.list.CAN),length(row.names)))
sp.can <- as.data.frame(sp.can)
names(sp.can) <- row.names
sp.can$spcd <- sp.list.CAN
for (i in 1:nrow(sp.can)){
  spcd <- sp.can$spcd[[i]]
  sp.can$common_name[[i]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("common_name")])
  sp.can$genus[[i]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Genus")])
  sp.can$species[[i]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Species")])
}
write.csv(sp.can,"species_extend_CAN.csv")

# Criterion 2 - South-skewed species have peaks within Florida Peninsula
hof_result_V_S <- hof_result[which(hof_result$best_model == 'V' & hof_result$types == 'S'),c("spcd","common_name","peak_lat")]
hof_result_V_S$peak_location <- ""
hof_result_V_S$genus <- ""
hof_result_V_S$species <- ""
for (i in 1:nrow(hof_result_V_S)){
  spcd <- hof_result_V_S$spcd[[i]]
  hof_result_V_S$genus[[i]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Genus")])
  hof_result_V_S$species[[i]] <- as.character(fl_ls[fl_ls$spcd == spcd,c("Species")])
  if (hof_result_V_S$peak_lat[i] <= 32){
    hof_result_V_S$peak_location[i] <- "Florida"
  }
  else{
    hof_result_V_S$peak_location[i] <- "Mainland"
  }
}
write.csv(hof_result_V_S,"species_met_criteria_list.csv")






