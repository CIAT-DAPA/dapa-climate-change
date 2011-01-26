require(rgdal)
require(raster)
require(maptools)

source("./src/EcoCrop.R")
source("./src/suitMerge.R")

#This script is to be improved (make it more generic)
#add a part to create a .projection file containing the info on the environmental data, etc
#an overwrite parameter should be added

futruns <- function(gcm="", bDir="F:/EcoCrop-development", gs=3, parlist="") {
	#GCM data location and naming
	iDir <- paste(bDir, "/climate", sep="")
	fDir <- paste(iDir, "/afasia_2_5min_future", sep="")
	period <- "2020_2049"
	#gcm <- "cccma_cgcm3_1_t47"
	
	#Runs data location and naming
	rDir <- paste(bDir, "/analyses/runs-future", sep="")
	if (!file.exists(rDir)) {dir.create(rDir)}
	oDir <- paste(rDir, "/", gcm, sep="")
	
	#Uncompressing the ascii files
	zDir <- paste(fDir, "/", gcm, "/", period, "/_asciis", sep="")
	aDir <- paste(fDir, "/", gcm, "/", period, sep="")
	vList <- c("prec", "tmin", "tmax", "tmean")
	for (v in vList) {
		f <- paste(zDir, "/", v, "_asc.zip", sep="")
		fd <- paste(aDir, "/", v, "_asc.zip", sep="")
		cat("Copy..."); file.copy(f, fd)
		cat("Unzip..."); unzip(fd, files=NULL, exdir=aDir)
		cat("Remove dup... \n"); file.remove(fd)
	}
	
	#Run the model (tmin & tmax)
	p <- read.csv(parlist)
	p <- p[which(p$GS==gs),]
	for (rw in c(3,4)) {
		if (!file.exists(paste(oDir, "/", gs, "-sorghum-", p$VARIABLE[rw], "_suitability.asc",sep=""))) {
			eco <- suitCalc(climPath=aDir, Gmin=180,Gmax=180,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder=oDir, cropname=paste(gs,'-sorghum-',p$VARIABLE[rw],sep=""))
			jpeg(paste(oDir, "/", gs, "-sorghum-",p$VARIABLE[rw],"-suitability.jpg",sep=""), quality=100)
			plot(eco)
			dev.off()
		}
	}
	#Merge suitability predictions
	data(wrld_simpl)
	n <- raster(paste(oDir, "/", gs, "-sorghum-tmin_suitability.asc", sep=""))
	x <- raster(paste(oDir, "/", gs, "-sorghum-tmax_suitability.asc", sep=""))
	ps <- raster(paste(bDir, "/analyses/runs/", gs, "-sorghum-mergedwhich_suitability.asc", sep=""))
	r <- suitMerge(n,x, ps, future=T)
	rs <- r; rs <- writeRaster(rs, paste(oDir, "/", gs, "-sorghum-merged_suitability.asc", sep=""), overwrite=T, format='ascii')
	pd <- ps; pd <- writeRaster(pd, paste(oDir, "/", gs, "-sorghum-mergedwhich_suitability.asc", sep=""), overwrite=T, format='ascii')
	jpeg(paste(oDir, "/", gs, "-sorghum-merged_suitability.jpg", sep=""), quality=100, height=600, width=1800)
	par(mfrow=c(1,2))
	plot(rs, col=colorRampPalette(c("yellow","red"))(100)); plot(wrld_simpl, add=T)
	plot(pd); plot(wrld_simpl, add=T)
	dev.off()
	
	#Delete ascii files
	vList <- c("prec", "tmin", "tmax", "tmean")
	for (v in vList) {
		fList <- list.files(aDir, pattern=v)
		for (f in fList) {
			if (file.exists(paste(aDir, "/", f, sep=""))) {file.remove(paste(aDir, "/", f, sep=""))}
		}
	}
}