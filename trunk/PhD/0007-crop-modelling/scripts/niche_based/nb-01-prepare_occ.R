#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

#load libraries
library(maptools)

#i/o directories
#r_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
occDir <- paste(bDir,"/occurrences",sep="")

#two types of occurrences and I need to make a single
#file that has single unique locations
occ_p1 <- readShapePoints(paste(occDir,"/gnut-india_genesys.shp",sep=""))
occ_p2 <- readShapePoints(paste(occDir,"/gnut-india_other.shp",sep=""))

#merge both sets of coordinates
all_occ <- rbind(cbind(SOURCE="GENESYS",as.data.frame(coordinates(occ_p1))),
                       cbind(SOURCE="OTHER",as.data.frame(coordinates(occ_p2))))
names(all_occ)[2:3] <- c("LON","LAT")

#get unique data
all_occ <- unique(all_occ)
#plot(all_occ$LON,all_occ$LAT)

#write output
write.csv(all_occ,paste(occDir,"/gnut-india_final.csv",sep=""),row.names=F)





