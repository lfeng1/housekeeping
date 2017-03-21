# This script constructs SDM for bats using 2013 occurence data and land classification data 
x <- c("raster","dismo")
lapply(x,library,character.only= T)

### data import and cleaning ### start
dat2013 <- read.csv('./data/occurence2013.csv')
dat2014 <- read.csv('./data/occurence2014.csv')
#remove duplicates
dat2013 <- dat2013[!duplicated(dat2013),]
dat2014 <- dat2014[!duplicated(dat2014),]
female_roost <- read.csv("./data/female_roost_from_arcmap_layer.csv")
male_roost <- read.csv("./data/male_roost_from_arcmap_layer.csv")
female_roost$name_only <- ""
female_roost$roost <- ""
female_roost$roost_site <- ""

for (i in nrow(female_roost)){
  name <- as.character(female_roost$name[i])
  female_roost$name_only[i] <- strsplit(name, " ")[[1]][1]
}
#from roost files to 
dat2013$sex <- ""
for (i in nrow(dat2013)){
  name <- as.character(dat2013$name[i])
  if (name %in% male_roost$name)
    dat2013$sex[i] <- as.character(male_roost[male_roost$name==name,"sex"])
  else if (name %in% female_roost$name)
    dat2013$sex[i] <- as.character(female_roost[female_roost$name==name,"sex"])
}

####### modeling
occ <- dat2013.unique[,c("x","y")]
names(occ) <- c("lon","lat")

# land classification as predictors
fnames <- list.files(path="./data", pattern='lulc.tif$', full.names=TRUE )
predictors <- stack(fnames)
predictors


# plot sample data
plot(predictors)
points(occ)

# 20% sample for testing, the other 80% for training  
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]

# fit model
me <- maxent(predictors, occtrain, factors="lulc") #
me

# predict to entire dataset
r <- predict(me, predictors) 
plot(r)

#testing
bg <- randomPoints(predictors, 1000) # change num of background data for testing

#simplest way to use 'evaluate'
e1 <- evaluate(me, p=occtest, a=bg, x=predictors)
plot(e1, 'ROC')
