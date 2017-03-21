# This script constructs SDM for bats using 2013 occurence data and land classification data 
x <- c("raster","dismo")
lapply(x,library,character.only= T)

# land classification as predictors
fnames <- list.files(path="./data", pattern='lulc.tif$', full.names=TRUE )
predictors <- stack(fnames)
predictors

# occurence data
dat2013 <- read.csv('./data/occurence2013.csv')
occ <- dat2013[,c("x","y")]
names(occ) <- c("lon","lat")

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
