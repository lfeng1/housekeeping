library(ggplot2)
library(dplyr)
library(plyr)
library(scales)

## This script will plot SR distribution along latitudes for the eastern US and for East of Mississippi river ## 
setwd("S:/Tech/lfeng/research/FL_SR/2017")

################calculate species richness for data in the eastern US###################
dat <- read.csv("data/pergridperspcd_count_area_iv_details.csv")
eastern_us.data <- dat[,c("spcd","common_name","lat")]
eastern_us.data[,"latband"] <- NA
for (i in 1:length(eastern_us.data$spcd)){
  eastern_us.data$latband[[i]] <- round(eastern_us.data$lat[i],digits = 0)
}
eastern_us.groupby <- group_by(eastern_us.data,latband,spcd,common_name)
eastern_us.groupby.s <- dplyr::summarise(eastern_us.groupby,count = n())

eastern_us.groupby.s[,"types"] <- NA
matches <- c(".spp","Unknown","Other")
for (i in 1:length(eastern_us.groupby.s$spcd)){
  if (grepl(paste(matches, collapse = "|"),eastern_us.groupby.s$common_name[[i]], ignore.case = TRUE) == TRUE){
    eastern_us.groupby.s$types[[i]] <- "Non tree spp."
  }
}

fl_sp_list <- read.csv("S:/Tech/lfeng/research/FL_SR/2017/FL_SR_PE/output/florida_list.csv")
temp <- fl_sp_list[fl_sp_list$affinity == "Temp",]
tropical <- fl_sp_list[fl_sp_list$affinity == "Tropical",]

# plot all
eastern_us_temp.groupby.s2 <- eastern_us.unique %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
eastern_us_temp.groupby.s <- eastern_us_temp.groupby.s2[order(eastern_us_temp.groupby.s2$latband,decreasing = T),]
eastern_us_temp.groupby.s.30 <- eastern_us_temp.groupby.s[eastern_us_temp.groupby.s$latband <= 31,]
barplot <- ggplot(eastern_us_temp.groupby.s.30,aes(x = -latband)) +geom_bar(width = .5,fill="darkblue",color = "black") +labs(x = "Latitude",y = "Species Richness") +theme_bw() +theme(text = element_text(size=10)) + scale_y_continuous(breaks= pretty_breaks())
print(barplot)

# plot temperate species
eastern_us_temp.unique <- eastern_us.unique[eastern_us.unique$spcd %in% temp$spcd,]
eastern_us_temp.groupby.s2 <- eastern_us_temp.unique %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
eastern_us_temp.groupby.s <- eastern_us_temp.groupby.s2[order(eastern_us_temp.groupby.s2$latband,decreasing = T),]
eastern_us_temp.groupby.s.30 <- eastern_us_temp.groupby.s[eastern_us_temp.groupby.s$latband <= 31,]
barplot <- ggplot(eastern_us_temp.groupby.s.30,aes(x = -latband)) +geom_bar(width = .5,fill="darkblue",color = "black") +labs(x = "Latitude",y = "Species Richness") +theme_bw() +theme(text = element_text(size=10)) + scale_y_continuous(breaks= pretty_breaks())
print(barplot)

# plot tropical species
eastern_us_tropical.unique <- eastern_us.unique[eastern_us.unique$spcd %in% tropical$spcd,]
eastern_us_tropical.groupby.s2 <- eastern_us_tropical.unique %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
eastern_us_tropical.groupby.s <- eastern_us_tropical.groupby.s2[order(eastern_us_tropical.groupby.s2$latband,decreasing = T),]
eastern_us_tropical.groupby.s.30 <- eastern_us_tropical.groupby.s[eastern_us_tropical.groupby.s$latband <= 31,]
barplot <- ggplot(eastern_us_tropical.groupby.s.30,aes(x = -latband)) +geom_bar(width = .5,fill="darkblue",color = "black") +labs(x = "Latitude",y = "Species Richness") +theme_bw() +theme(text = element_text(size=10)) + scale_y_continuous(breaks= pretty_breaks())
print(barplot)

# plot Southeastern costal plain
# eastern_us_tropical.unique <- eastern_us.unique[eastern_us.unique$spcd %in% localSEUS$spcd,]
# eastern_us_tropical.groupby.s2 <- eastern_us_tropical.unique %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
# eastern_us_tropical.groupby.s <- eastern_us_tropical.groupby.s2[order(eastern_us_tropical.groupby.s2$latband,decreasing = T),]
# eastern_us_tropical.groupby.s.30 <- eastern_us_tropical.groupby.s[eastern_us_tropical.groupby.s$latband <= 31,]
# barplot <- ggplot(eastern_us_tropical.groupby.s.30,aes(x = -latband)) +geom_bar(width = .5) +labs(x = "Latitude",y = "Species Richness") +theme_bw() +theme(text = element_text(size=10)) + scale_y_continuous(breaks= pretty_breaks())
# print(barplot)

# plot Florida endemic species
eastern_us_tropical.unique <- eastern_us.unique[eastern_us.unique$spcd %in% FloridaEndemic$spcd,]
eastern_us_tropical.groupby.s2 <- eastern_us_tropical.unique %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
eastern_us_tropical.groupby.s <- eastern_us_tropical.groupby.s2[order(eastern_us_tropical.groupby.s2$latband,decreasing = T),]
eastern_us_tropical.groupby.s.30 <- eastern_us_tropical.groupby.s[eastern_us_tropical.groupby.s$latband <= 31,]
barplot <- ggplot(eastern_us_tropical.groupby.s.30,aes(x = -latband)) +geom_bar(width = .5,fill="darkblue",color = "black") +labs(x = "Latitude",y = "Species Richness") +theme_bw() +theme(text = element_text(size=10)) + scale_y_continuous(breaks= pretty_breaks())
print(barplot)

Palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# plot H0 species supported by peninsular effect
eastern_us_tropical.unique <- eastern_us.unique[eastern_us.unique$spcd %in% peninEffect$spcd,]
eastern_us_tropical.groupby.s2 <- eastern_us_tropical.unique %>% group_by(latband,spcd,common_name) %>% dplyr::summarise(count = n())
eastern_us_tropical.groupby.s <- eastern_us_tropical.groupby.s2[order(eastern_us_tropical.groupby.s2$latband,decreasing = T),]
eastern_us_tropical.groupby.s.30 <- eastern_us_tropical.groupby.s[eastern_us_tropical.groupby.s$latband <= 31,]
barplot <- ggplot(eastern_us_tropical.groupby.s.30,aes(x = -latband)) +geom_bar(width = .5,fill="darkblue",color = "black") + labs(x = "Latitude",y = "Species Richness") +theme_bw() + 
  theme(text = element_text(size=10)) + scale_y_continuous(breaks= pretty_breaks())
print(barplot)


############## Plot ########
plot.data.FL <- eastern_us.groupby.s[eastern_us.groupby.s$latband <= 31,]
plot.data.FL.count <-  plot.data.FL %>% group_by(latband) %>% dplyr::summarise(count = n())
f <- lm(plot.data.FL.count$count~plot.data.FL.count$latband)
# predicted values for given variable values
X <- seq(25,31,1)
Y <- predict(f,newdata = data.frame(x=X))

# regression coefficients
cf <- round(coef(f), 2)
eq <- paste0("y = ", cf[1],ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " x ")
r_squared <- round(summary(f)$r.squared,2)
r_squared_text <- bquote(paste('R'^2*' = ', .(r_squared)))
p <- plot(plot.data.FL.count$count~plot.data.FL.count$latband,pch = 19,xlab = " Latitude", ylab = "Number of species", main = "Species Richness")
lines(X,Y, lty=2,lwd=2)
mtext(eq, 3, line=-2)
mtext(r_squared_text, 3, line=-3.5)

################calculate species richness for data eastern of Mississippi river###################
missi_east <- read.csv("pergridperspcd_count_area_iv_details_missi_east.csv")
missi_east.data <- missi_east[,c("spcd","common_name","lat")]
missi_east.data[,"latband"] <- NA
for (i in 1:length(missi_east.data$spcd)){
    missi_east.data$latband[[i]] <- round(missi_east.data$lat[i],digits = 0)
}
missi_east.groupby <- group_by(missi_east.data,latband,spcd,common_name)
missi_east.groupby.s <- dplyr::summarise(missi_east.groupby,Count = n())

missi_east.groupby.s[,"types"] <- NA
matches <- c(".spp","Unknown","Other")
for (i in 1:length(missi_east.groupby.s$spcd)){
    if (grepl(paste(matches, collapse = "|"),missi_east.groupby.s$common_name[[i]], ignore.case = TRUE) == TRUE){
        missi_east.groupby.s$types[[i]] <- "Non tree spp."
    }
}
missi_east.unique <- missi_east.groupby.s[is.na(missi_east.groupby.s$types),]
missi_east.groupby <- group_by(missi_east.unique,latband)
missi_east.groupby.s <- dplyr::summarise(missi_east.groupby,Count = n())
missi_east.groupby.s[,"coverage"] <- NA
for (i in 1:length(missi_east.groupby.s$latband)){
    missi_east.groupby.s$coverage[i] <- "Mississippi East"
}

dev.off()
