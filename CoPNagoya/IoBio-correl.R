require(sp)
require(rgdal)
require(raster)
require(dismo)

bseDir <- "C:/CIAT_work/COP_CONDESAN"

#Output folder
outDir <- paste(bseDir, "/correlations", sep="")

#Load the mask

mskDir <- paste(bseDir, "/maskData/AAIGrids", sep="")
msk <- paste(mskDir, "/andes_msk_25m.asc", sep="")
msk <- raster(msk)

#Extract a set of random points (10,000)

npts <- 0
tf <- 2
iter <- 1

while (npts != 10000) {
	cat("Iteration", iter, "\n")
	rpts <- randomPoints(msk, 10000, ext=NULL, tryf=tf, warn=0)
	npts <- nrow(rpts)
	tf <- tf+10
	iter <- iter+1
}

#Now extract the data for each grid

grdDir <- paste(bseDir, "/climateData/andes/baseline/20C3M/WorldClim-2_5min-bioclim/1950_2000", sep="")
grdList <- list.files(grdDir, pattern=".asc")
gCount <- 1
oMtx <- rpts

for (grd in grdList) {
	cat("Extracting for", grd, "\n")
	rs <- paste(grdDir, "/", grd, sep="")
	rs <- raster(rs)
	
	lsVals <- xyValues(rs, rpts)
	oMtx <- cbind(oMtx,lsVals)
	
	gCount <- gCount+1
}

cat("Calculating correlations \n")

oMtx <- as.data.frame(oMtx)
names(oMtx) <- c("X","Y",grdList)
write.csv(oMtx,paste(outDir, "/data-correl-bio2.5min.csv", sep=""), row.names=F, quote=F)

correls <- cor(oMtx)
write.csv(correls,paste(outDir, "/resl-correl-bio2.5min.csv", sep=""), row.names=F, quote=F)
