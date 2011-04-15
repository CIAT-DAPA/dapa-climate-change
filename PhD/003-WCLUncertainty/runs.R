source("anuTesting.R")

rd <- "F:/climate-data-assessment/mask-srtm"
std <- "F:/climate-data-assessment/input-data"
od <- "F:/climate-data-assessment/outputs/cleansing"

cs <- cleansing(anuDir="F:/climate-data-assessment/anu/Anuspl43/bin", rDir=rd, stDir=std, oDir=od, vn="tean", ntiles=5, round=1, unix=F)

########Linux folders
source("tileCreation.R")

rd <- "/home/jramirez/climate-data-assessment/mask-srtm"
std <- "/home/jramirez/climate-data-assessment/input-data"
od <- "/home/jramirez/climate-data-assessment/mask-srtm"

createTiles(rd, std, od, vn="rain", ntiles=5, overlap=1000)

################################################
source("densityMap.R")

std <- "/home/jramirez/climate-data-assessment/input-data"
rd <- "/home/jramirez/climate-data-assessment/mask-srtm"
od <- "/home/jramirez/climate-data-assessment/outputs/density-map"

dm <- densityMap(std, rd, od, vn="rain", nclosest=10)


###########################################################
# TO FIT SPLINES ##########################################
###########################################################

1. Copy files from 172.22.33.85\climate-data-assessment to a local folder
2. Uncompress tiles.zip
3. Update the repository if not already updated (dapa-climate-change)
4a. In windows 

go to the dapa-climate-change/trunk/PhD/003-WCLUncertainty
type "R" and enter

source("fitSplines.R")
drive <- "E"

#set the folders
sd <- paste(drive, ":/climate-data-assessment/input-data", sep="")
rd <- paste(drive, ":/climate-data-assessment/mask-srtm", sep="")
od <- paste(drive, ":/climate-data-assessment/outputs/cross-validation", sep="")
ad <- paste(drive, ":/climate-data-assessment/anu/Anuspl43/bin", sep="")

spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="rain", ntiles=5, unix=F)
spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="tean", ntiles=5, unix=F)
spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="tmin", ntiles=5, unix=F)
spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="tmax", ntiles=5, unix=F)

The purpose is to achieve 100 folds, so you need 10 processors with 10 folds each, for each variable


4b. In Linux it is 

go to the dapa-climate-change/trunk/PhD/003-WCLUncertainty
type "R" and enter

source("fitSplines.R")
drive <- "/data"

#set the folders
sd <- paste(drive, "/climate-data-assessment/input-data", sep="")
rd <- paste(drive, "/climate-data-assessment/mask-srtm", sep="")
od <- paste(drive, "/climate-data-assessment/outputs/cross-validation/part-", sep="")
ad <- paste(drive, "/climate-data-assessment/anu/Anuspl43/bin", sep="")


spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="rain", ntiles=5, unix=T)
spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="tean", ntiles=5, unix=T)
spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="tmin", ntiles=5, unix=T)
spf <- splineFitting(anuDir=ad, stDir=sd, rDir=rd, oDir=od, nfolds=10, train.per=0.85, vn="tmax", ntiles=5, unix=T)

The purpose is to achieve 100 folds, so you need 10 processors with 10 folds each, for each variable

Copy periodically processed folds