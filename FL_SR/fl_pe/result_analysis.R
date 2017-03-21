#raw_count_hof_result <- hof_result[,c("spcd","common_name","best_model","peak_lat","min_lat","min_val","max_lat","max_val")]
hof_result <- read.csv("./data/hof_result.csv")
hof_result_I <- hof_result[hof_result$best_model == "I",]
general_I <- hof_result_I[,c("spcd","common_name")]

hof_result_II_III <- hof_result[hof_result$best_model == "II" | hof_result$best_model == "III",]
hof_result_II_III[,"types"] <- "" 
for (i in 1:nrow(hof_result_II_III)){
  peak_val <- as.numeric(hof_result_II_III$peak_val[i])
  min_val <- as.numeric(hof_result_II_III$min_val[i])
  max_val <- as.numeric(hof_result_II_III$max_val[i])
  if (round(peak_val - max_val) == 0){
    hof_result_II_III$types[i] <- "N"
  }
  if (round(peak_val - min_val) == 0){
    hof_result_II_III$types[i] <- "S"
  }
}

# spruce pine (spcd 115) from the small_ranged group, HOF model type IV, SE coastal plain dominant, see their ranges for details
general_II_III <- hof_result_II_III[,c("spcd","common_name")]
se_us_II_III <- hof_result_II_III[hof_result_II_III$types == "S",c("spcd","common_name","types")]

general_IV <- hof_result[hof_result$best_model == "IV",c("spcd","common_name")]

hof_result_V <- hof_result[hof_result$best_model == "V",]
hof_result_V[,"types"] <- NA
hof_result_V<-data.frame(hof_result_V)
for (i in 1:length(hof_result_V$spcd)){
    peak_lat <- as.numeric(hof_result_V$peak_lat[i])
    min_lat <- as.numeric(hof_result_V$min_lat[i])
    max_lat <- as.numeric(hof_result_V$max_lat[i])
  if ((peak_lat - min_lat) < 0.5 * (max_lat - min_lat)){
    hof_result_V$types[i] <- "S"
  } 
  else{
    hof_result_V$types[i] <- "N"
  } 
}
hof_V_N <- hof_result_V[hof_result_V$types == "N",c("spcd","common_name")]
hof_V_S <- hof_result_V[hof_result_V$types == "S",c("spcd","common_name")]
write.csv(hof_V_N,"hof_V_N.csv")
# American sycamore (731) and Black oak (837) are type III, increasing and plaetau with wide range, general species   
peninEffect <- hof_result_V[hof_result_V$types == "S",c("spcd","common_name")]
general <- do.call("rbind",list(general_I,general_II_III,general_IV,general_V))
lcalSEUS <- se_us_oII_III
FloridaEndemic <- setdiff(spcd_list,spcd_list_count_sel)
FloridaEndemic[,"types"] <- "Florida Endemic"
FloridaEndemic <- FloridaEndemic[,c("spcd","common_name","types")]
