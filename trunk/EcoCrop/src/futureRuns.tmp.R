require(rgdal)
require(raster)
require(maptools)

source("./src/EcoCrop.R")
source("./src/suitMerge.R")

#This script is to be improved (make it more generic)
#an overwrite parameter should be added

futruns <- function(climdir="", oDir="", cDir="", gs=3, gsl=180, parlist="", cropname="", ow.runs=T, ow.merge=T) {
	#Run the model (tmin & tmax)
	p <- read.csv(parlist)
	p <- p[which(p$GS==gs),]
	for (rw in c(3,4)) {
		if (!file.exists(paste(oDir, "/", gs, "-", cropname, "-", p$VARIABLE[rw], "_suitability.asc",sep="")) | ow.runs) {
			eco <- suitCalc(climPath=climdir, Gmin=gsl,Gmax=gsl,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder=oDir, cropname=paste(gs,'-', cropname, '-',p$VARIABLE[rw],sep=""))
			jpeg(paste(oDir, "/", gs, "-", cropname, "-",p$VARIABLE[rw],"-suitability.jpg",sep=""), quality=100)
			plot(eco)
			dev.off()
		}
	}
	#Merge suitability predictions
	if (!file.exists(paste(oDir, "/", gs, "-", cropname, "-merged_suitability.asc", sep="")) | ow.merge) {
		data(wrld_simpl)
		n <- raster(paste(oDir, "/", gs, "-", cropname, "-tmin_suitability.asc", sep=""))
		x <- raster(paste(oDir, "/", gs, "-", cropname, "-tmax_suitability.asc", sep=""))
		ps <- raster(paste(cDir, "/", gs, "-", cropname, "-mergedwhich_suitability.asc", sep=""))
		r <- suitMerge(n,x, ps, future=T)
		rs <- r; rs <- writeRaster(rs, paste(oDir, "/", gs, "-", cropname, "-merged_suitability.asc", sep=""), overwrite=T, format='ascii')
		pd <- ps; pd <- writeRaster(pd, paste(oDir, "/", gs, "-", cropname, "-mergedwhich_suitability.asc", sep=""), overwrite=T, format='ascii')
		jpeg(paste(oDir, "/", gs, "-", cropname, "-merged_suitability.jpg", sep=""), quality=100, height=600, width=1800)
		par(mfrow=c(1,2))
		plot(rs, col=colorRampPalette(c("yellow","red"))(100)); plot(wrld_simpl, add=T)
		plot(pd); plot(wrld_simpl, add=T)
		dev.off()
	}
	#Finalising
	if (!ow.runs | !ow.merge) {return("NA")} else {return(stack(n,x,pd,rs))}
}