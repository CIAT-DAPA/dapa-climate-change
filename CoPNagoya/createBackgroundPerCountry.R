require(sp)
require(rgdal)
require(raster)
require(dismo)

bDir <- "C:/CIAT_work/COP_CONDESAN"
mDir <- paste(bDir, "/maskData/AAIGrids", sep="")
oDir <- paste(bDir, "/backgrounds", sep="")

if (!file.exists(oDir)) {
	dir.create(oDir)
}

zList <- c("ven","col","ecu","per","bol")
zCntr <- 1

for (zn in zList) {
	fName <- paste("z", zCntr, "_", zn, "_25m.asc", sep="")
	rs <- paste(mDir, "/", fName, sep="")
	rs <- raster(rs)
	
	#Iterating to select 10k points
	
	cat("Selecting the 10,000 random points for", zn, "\n")
	
	npts <- 0
	tf <- 10
	iter <- 1
	while (npts != 10000) {
		cat("Iteration", iter, "\n")
		rpts <- randomPoints(rs, 10000, ext=NULL, tryf=tf, warn=0)
		npts <- nrow(rpts)
		tf <- tf+5
		iter <- iter+1
	}
	
	txName <- rep("background", times=nrow(rpts))
	rpts <- cbind(txName, rpts)
	rpts <- as.data.frame(rpts)
	names(rpts) <- c("ID","X","Y")
	
	cName <- paste("z", zCntr, "_", zn, "_25m.csv", sep="")
	oName <- paste(oDir, "/", cName, sep="")
	write.csv(rpts, oName, row.names=F, quote=F)
	
	zCntr <- zCntr + 1
}

