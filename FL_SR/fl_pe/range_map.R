library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

# load data that bounded by FL long as well as occur at least 10 0.5 lat bands 
dat <- read.csv("./data/tree_count_iv.csv")

# select only trees with known genus and species
dat[,"types"] <- NA
matches <- c(".spp","Unknown","Other")
for (i in 1:length(dat$spcd)){
    if (grepl(paste(matches, collapse = "|"),dat$common_name[[i]], ignore.case = TRUE) == TRUE){
        dat$types[[i]] <- "Non tree spp."
    }
}
dat.sel <- dat[is.na(dat$types),]

# select FL species
fl_grp_descr <- "FLORIDA 2014: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
data.fl <- dat.sel[dat.sel$eval_grp_descr == fl_grp_descr,]
tbl2 <- dat.sel[dat.sel$spcd %in% data.fl$spcd,] # occurence data for Florida tree species in eastern US 
tbl <- tbl2[,c("spcd","common_name","lat","lon","count")]

# lat-long bands
tbl[,"latband"] <- NA
tbl[,"lonband"] <- NA
for (i in 1:length(tbl$spcd)){
    tbl$latband[[i]] <- round(tbl$lat[i],digits = 0)
    tbl$lonband[[i]] <- round(tbl$lon[i],digits = 0)
}

# load hof model results and only select those went through hof-model fits
hof_result <- read.csv("raw_count_hof_result.csv")
tbl.s <- tbl[tbl$spcd %in% hof_result$spcd,]

tbl.s.spcd <- ddply(tbl.s,.(common_name), function(x) head(x,1))
data.mean <- ddply(tbl.s, c("common_name","spcd","latband","lonband"), summarize, mean = mean(count))
data.mean[, c(1)] <- lapply(data.mean[, c(1), drop=FALSE], as.factor)

data.mean[,"hof_model"] <- NA
for (i in 1:length(data.mean$spcd)){
    spcd <- data.mean$spcd[i]
    if (spcd %in% hof_result$spcd){
        data.mean$hof_model[[i]] <- as.character(hof_result[hof_result$spcd ==spcd,]$best_model)
        }
}

# data points delineating an approximate US-CANADA boundary cross the longitudinal extent of FL
us_n_bdry <- read.csv("./data/us_can_bdry.txt")

### plot each species' range map ###
pdf("raw_count_hof_type_range_map.pdf",onefile = TRUE)

# plot species with model type I
hof_result_I <- hof_result[hof_result$best_model == "I",]
data_mean_hof_I <- data.mean[data.mean$spcd %in% hof_result_I$spcd,]
plot <- ddply(data_mean_hof_I,.(common_name), function(x) head(x,1))

plot.data <- data_mean_hof_I
lolim <- min(plot.data$mean)
hilim <- max(plot.data$mean) # change based on the maximumn values 
bwdth <- 5 # 
plot.data$mean <- cut(plot.data$mean, breaks = seq(lolim, hilim, bwdth),include.lowest = TRUE)
brk <- levels(plot.data$mean)
ncol <- length(brk)
colors <- c("#808080FF", rainbow(ncol-1))
p <- ggplot() +
    geom_point(data = plot.data,aes(x = lonband,y = latband, group = paste(common_name),color = mean),size = 3) +
    geom_smooth(data = us_n_bdry,aes(x = long,y = lat)) +
    scale_fill_manual(breaks = brk, values = colors) + 
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    theme_bw() +
    ggtitle("HOF model type I") +
    facet_wrap(~ common_name+spcd, ncol = 3, scales = "free") 
print(p) 

# plot species with model type II & III
hof_result_II_III <- hof_result[hof_result$best_model == "II" | hof_result$best_model == "III",]
data_mean_hof_II_III <- data.mean[data.mean$spcd %in% hof_result_II_III$spcd,]
plot <- ddply(data_mean_hof_II_III,.(common_name), function(x) head(x,1))
seq <- seq(from = 1, to = length(plot$spcd), by = 9) # 3*3 plots per page 
for (i in seq) {
    plot.ls <- c()
    j <- 1
    while (j<= 9) {
        plot.ls[[j]] <- plot$spcd[i+j-1]
        j <- j +1
    }
    plot.data <- data_mean_hof_II_III[data_mean_hof_II_III$spcd %in% plot.ls,] 
    lolim <- min(plot.data$mean)
    hilim <- max(plot.data$mean) # change based on the maximumn values 
    bwdth <- 5 # 
    plot.data$mean <- cut(plot.data$mean, breaks = seq(lolim, hilim, bwdth),include.lowest = TRUE)
    brk <- levels(plot.data$mean)
    ncol <- length(brk)
    colors <- c("#808080FF", rainbow(ncol-1))
    p <- ggplot() +
        geom_point(data = plot.data,aes(x = lonband,y = latband, group = paste(common_name),color = mean),size = 3) +
        geom_smooth(data = us_n_bdry,aes(x = long,y = lat)) +
        scale_fill_manual(breaks = brk, values = colors) + 
        scale_x_continuous(breaks = pretty_breaks()) +
        scale_y_continuous(breaks = pretty_breaks()) +
        theme_bw() +
        ggtitle("HOF model type II & III") +
        facet_wrap(~ common_name+spcd, ncol = 3, scales = "free") 
    print(p) 
}

# plot species with model type IV & V
hof_result_IV_V <- hof_result[hof_result$best_model == "IV" | hof_result$best_model == "V",]
data_mean_hof_IV_V <- data.mean[data.mean$spcd %in% hof_result_IV_V$spcd,]
plot <- ddply(data_mean_hof_IV_V,.(common_name), function(x) head(x,1))
seq <- seq(from = 1, to = length(plot$spcd), by = 9) # 3*3 plots per page 
for (i in seq) {
    plot.ls <- c()
    j <- 1
    while (j<= 9) {
        plot.ls[[j]] <- plot$spcd[i+j-1]
        j <- j +1
    }
    plot.data <- data_mean_hof_IV_V[data_mean_hof_IV_V$spcd %in% plot.ls,] 
    lolim <- min(plot.data$mean)
    hilim <- max(plot.data$mean) # change based on the maximumn values 
    bwdth <- 5 # 
    plot.data$mean <- cut(plot.data$mean, breaks = seq(lolim, hilim, bwdth),include.lowest = TRUE)
    brk <- levels(plot.data$mean)
    ncol <- length(brk)
    colors <- c("#808080FF", rainbow(ncol-1))
    p <- ggplot() +
        geom_point(data = plot.data,aes(x = lonband,y = latband, group = paste(common_name),color = mean),size = 3) +
        geom_smooth(data = us_n_bdry,aes(x = long,y = lat)) +
        scale_fill_manual(breaks = brk, values = colors) + 
        scale_x_continuous(breaks = pretty_breaks()) +
        scale_y_continuous(breaks = pretty_breaks()) +
        theme_bw() +
        ggtitle("HOF model type IV & V") +
        facet_wrap(~ common_name+spcd, ncol = 3, scales = "free") 
    print(p) 
}
dev.off()
