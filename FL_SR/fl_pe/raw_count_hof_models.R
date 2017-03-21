x <- c("eHOF","plyr","dplyr")
lapply(x,library,character.only =T)

# dataset for the entire eastern US
dat <- read.csv("data/pergridperspcd_count_area_iv_details.csv")

# select only trees with known genus and species
dat[,"types"] <- NA
matches <- c(".spp","Unknown","Other")
for (i in 1:length(dat$spcd)){
    if (grepl(paste(matches, collapse = "|"),dat$common_name[[i]], ignore.case = TRUE) == TRUE){
        dat$types[[i]] <- "Non tree spp."
    }
}
dat.sel <- dat[is.na(dat$types),]

# select FL species & excludes coastal parts of LA, MS, AL, and GA 
fl_grp_descr <- "FLORIDA 2014: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
la_grp_descr <- "LOUISIANA 2014: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
ms_grp_descr <- "MISSISSIPPI 2015: ALL AREA, CURRENT AREA, CURRENT VOLUME, AREA CHANGE, GROWTH, REMOVALS, MORTALITY"
data.fl <- dat.sel[dat.sel$eval_grp_descr == fl_grp_descr,]
tbl2 <- dat.sel[dat.sel$spcd %in% data.fl$spcd,] # occurence data for Florida tree species in east of Mississippi 
max_lat_Fl <- max(data.fl$lat)
lower_max_lat_fl <- dat.sel[dat.sel$lat < max_lat_Fl,]
lower_max_lat_fl_outside_fl <- lower_max_lat_fl[lower_max_lat_fl$eval_grp_descr == la_grp_descr| lower_max_lat_fl$eval_grp_descr == ms_grp_descr,] # coastal data but not in FL
tbl3 <- setdiff(tbl2,lower_max_lat_fl_outside_fl) # exclude occurence records in coastal neighbors of the same latitude with Florida 

# Models for "count" values
tbl <- tbl3[,c("grid_id","spcd","common_name","lat","count")]

#Model info
#Number of bootstrapping runs
bootstrapping<-1000
#Choose test ("AIC","AICc","BIC")
test_sel<-"BIC"
#Models to use (Models I-V represent the HOF models, VI and VII the bimodal ones)
models<-c("I","II","III","IV","V")
#Set threshold value for model II slopes, anything < slp_thresh will be assigned 
#as model type I
#slp_thresh<-.5
#Use Murphy 2010 species (y for yes, n for no)
use_murphy<-"n"
###############################################################
#Main part of code
###############################################################

#Creating a list of SPCD's to cycle over
#merged<-merge(tbl,murphy,by="SPCD")
#merged_reordered<-merged[,colnames(tbl)]
spcd_list<-aggregate(lat~spcd+common_name,data=tbl,FUN=min)

#Generating a sequence of latitudinal bands (gradient data vector)
lat_seq<-seq(from=25, to=49,by=.5)

#Creating a blank array to place gradient data vector and occurence vectors 
bands<-array(dim=c(length(lat_seq),nrow(spcd_list)+1))
bands[,1]<-lat_seq #1st col (gradient data vector)

#Finding occurence vectors for each moving latitudinal band
for(i in 1:nrow(spcd_list)){
    for(j in 1:length(lat_seq)){
        lat_min<-lat_seq[j]-.5
        lat_max<-lat_seq[j]+.5
        
        #Defining the band width
        band<-subset(tbl,tbl$lat>=lat_min & tbl$lat<=lat_max)
        
        #Selecting the species of interest and calculating average abundace measures
        species<-subset(band,band$spcd==spcd_list[i,1])
        
        # mean iv occurence vectors 
        bands[j,i+1]<-mean(species$count) 
    }
}

# select non-NA occurence vector 
for(k in 2:ncol(bands)){
    spcd_list[k-1,4]<-sum( !is.na( bands[,k] ) ) 
}
#Assigning my array and renaming the columns
my_array<-bands[,]
colnames(my_array)[1:ncol(my_array)]<-c("LAT",spcd_list[,1])

#selection only the species with x number of bands (in this case x=10)
spcd_list_count_sel<-subset(spcd_list,spcd_list[,4]>=10) #92 out of 113 general species are selected 

bands_select<-my_array[,c("LAT",paste(spcd_list_count_sel[,1]))]

#Setting up the graphical parameters for generating a pdf (comment out if not making a pdf output)
outpdf<-"grid_raw_count_hof_plot.pdf"
pdf(outpdf,onefile=TRUE)
par(mfrow=c(3,2))

#Setting up blank matrices to accept the data
#Optima and model types
blank<-array(dim=c(nrow(spcd_list_count_sel),8))
colnames(blank)<-c("spcd","best_model","peak_lat","peak_val","min_lat","min_val","max_lat","max_val")
#Band values for abundance measures
blank2<-array(dim=c(length(lat_seq),nrow(spcd_list_count_sel)+1))
blank2[,1]<-as.matrix(lat_seq)
#scaled values for each spcies at every 0.1 latitude band
scaled_bound_blank2 <- array(dim=c(length(seq(from=25, to=49,by=.1)),nrow(spcd_list_count_sel)+1))
scaled_bound_blank2[,1] <- seq(from=25, to=49,by=.1)

#Fitting, graphing, and extracting latitudinal optima for latitudinal bands
#system.time(for(m in 2:3){
for(l in 1:nrow(spcd_list_count_sel)){
    #binding together the various abundance measure for the selected species
    species_bands2<-cbind(bands_select[,1],bands_select[,paste(spcd_list_count_sel[l,1])])
    species_bands<-as.matrix(na.omit(species_bands2))
    #Running the HOF models
    hof_m<-HOF(species_bands[,2],test=test_sel,species_bands[,1],lim=100,modeltypes=models,family=gaussian,bootstrap=bootstrapping)
    #Selecting the best model and assigning fitted values  
    best_model<-pick.model(hof_m,modeltypes=models,test=test_sel)
    
    #Predicting the modeled response and finding the peak latitude   
    predicting<-predict(hof_m,model=best_model,newdata=seq(from=min(species_bands[,1]),to=max(species_bands[,1]),by=.1))
    scaled<-predicting*max(species_bands[,2])
    scaled_bound<-cbind(seq(from=min(species_bands[,1]),to=max(species_bands[,1]),by=.1),scaled)  
    ifelse(best_model=="III",peak_lat<-((Para(hof_m,model=best_model)$opt[[1]]+Para(hof_m,model=best_model)$opt[[2]])/2),ifelse(best_model=="I",peak_lat<-NA,peak_lat<-scaled_bound[which.max(scaled_bound[,2]),1]))
    
    #recording scaled observation for each species for later to retrieve observations in range margins
    scaled_bound_all <- array(dim=c(length(seq(from=25, to=49,by=.1)),2))
    scaled_bound_all[,1] <- seq(from=25, to=49,by=.1)
    for (p in 1:(length(scaled_bound)/2)){
        for (j in 1:nrow(scaled_bound_all)){
            if (scaled_bound_all[j,1] == scaled_bound[p,1]){
                scaled_bound_all[j,2] <- scaled_bound[p,2]                    
            }
        }
    }
    scaled_bound_blank2[,l+1] <- scaled_bound_all[,2]
    
    #Graphing
    plot(species_bands[,1],species_bands[,2],pch=16,col="grey60", xlab="Latitude",ylab="Raw Count",main=paste(spcd_list_count_sel[l,1],spcd_list_count_sel[l,2],"; HOF type:",best_model), ps = 8, cex=1, cex.main = 1)
    lines(scaled_bound[,1],scaled_bound[,2],lwd=2,col="black")
    abline(v=peak_lat,col="red",lwd=2)

    #Assigning model info to output for finding H0 models
    blank[l,1]<-spcd_list_count_sel[l,1]
    blank[l,2]<-best_model
    blank[l,3]<-peak_lat
    if (is.na(peak_lat)) {
        blank[l,4] <- NA
    }
    else{
        blank[l,4] <- scaled_bound[which(abs(scaled_bound[,1] - round(peak_lat,digits = 1)) <= 0.01),2]
        # blank[l,4] <- scaled_bound[which(scaled_bound[,1] == round(peak_lat,digits = 1)),2]
    }
    min_lat <- scaled_bound[which.min(scaled_bound[,1]),1]
    blank[l,5]<-min_lat
    blank[l,6] <- scaled_bound[which(scaled_bound[,1] == min_lat),2]
    max_lat <- scaled_bound[which.max(scaled_bound[,1]),1]
    blank[l,7]<-max_lat
    blank[l,8] <- scaled_bound[which(scaled_bound[,1] == max_lat),2]
    #Storing the values from the latitudinal bands
    blank2[,l+1]<-species_bands2[,2]
}

dev.off()

######################################
##assign EIDH types to each species
hof_result<-data.frame(blank[,])
hof_result[,"common_name"] <- NA
tree_ref <- read.csv("REF_SPECIES.txt")
for (i in 1:length(hof_result$spcd)){
    spcd <- hof_result$spcd[i]
    if (spcd %in% tree_ref$SPCD){
        hof_result$common_name[[i]] <- as.character(tree_ref[tree_ref$SPCD == spcd,c("COMMON_NAME")])
    }
}
write.csv(hof_result,"hof_result.csv")