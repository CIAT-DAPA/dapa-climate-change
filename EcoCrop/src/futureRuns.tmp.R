#Julian Ramirez-Villegas
#First version April 2011
#Changes made on January 2012
#CIAT / CCAFS / UoL

require(rgdal)
require(raster)
require(maptools)

#This script is to be improved (make it more generic) !done!
#an overwrite parameter should be added -done!
futruns <- function(climdir="", oDir="", cDir="", gs=3, gsl=180, parlist="", 
                    cropname="", run.type="tmean",ow.runs=T, ow.merge=T) {
	
  #check if run type is correct
  if (!run.type %in% c("tmean","tmin","tmax","merge")) {
    stop("EcoCrop: The type of run is not properly set, please check! Use tmean, tmin, tmax, merge")
  }
  
  #select the parameters for the given runs depending on the selection
  p <- read.csv(parlist)
  p <- p[which(p$GS==gs),]
  rainrow <- p[which(p$VARIABLE=="prec"),]
  tminrow <- p[which(p$VARIABLE=="tmin"),]
  tmeanrow <- p[which(p$VARIABLE=="tmean"),]
  tmaxrow <- p[which(p$VARIABLE=="tmax"),]
  
  #If merged then run run the model (tmin & tmax) else run single model
	if (run.type=="merge") {
    #min suit
		if (!file.exists(paste(oDir, "/", gs, "-", cropname, "-tmin_suitability.asc",sep="")) | ow.runs) {
			eco <- suitCalc(climPath=climdir, Gmin=gsl,Gmax=gsl,
                      Tkmp=tminrow$KILL,Tmin=tminrow$MIN,Topmin=tminrow$OPMIN,Topmax=tminrow$OPMAX,
                      Tmax=tminrow$MAX,Rmin=rainrow$MIN,Ropmin=rainrow$OPMIN,Ropmax=rainrow$OPMAX,
                      Rmax=rainrow$MAX, outfolder=oDir, 
                      cropname=paste(gs,'-', cropname, '-tmin',sep=""))
      #Plotting section deactivated
			#jpeg(paste(oDir, "/", gs, "-", cropname, "-tmax-suitability.jpg",sep=""), quality=100)
			#plot(eco)
			#dev.off()
		}
  	
    #max suit
  	if (!file.exists(paste(oDir, "/", gs, "-", cropname, "-tmax_suitability.asc",sep="")) | ow.runs) {
			eco <- suitCalc(climPath=climdir, Gmin=gsl,Gmax=gsl,
                      Tkmp=tmaxrow$KILL,Tmin=tmaxrow$MIN,Topmin=tmaxrow$OPMIN,Topmax=tmaxrow$OPMAX,
                      Tmax=tmaxrow$MAX,Rmin=rainrow$MIN,Ropmin=rainrow$OPMIN,Ropmax=rainrow$OPMAX,
                      Rmax=rainrow$MAX, outfolder=oDir, 
                      cropname=paste(gs,'-', cropname, '-tmax',sep=""))
      #Plotting section deactivated
			#jpeg(paste(oDir, "/", gs, "-", cropname, "-tmax-suitability.jpg",sep=""), quality=100)
			#plot(eco)
			#dev.off()
		}
    
  	#Merge suitability predictions
  	if (!file.exists(paste(oDir, "/", gs, "-", cropname, "-merged_suitability.asc", sep="")) | ow.merge) {
  		#data(wrld_simpl)
  		n <- raster(paste(oDir, "/", gs, "-", cropname, "-tmin_suitability.asc", sep=""))
  		x <- raster(paste(oDir, "/", gs, "-", cropname, "-tmax_suitability.asc", sep=""))
  		ps <- raster(paste(cDir, "/", gs, "-", cropname, "-mergedwhich_suitability.asc", sep=""))
  		r <- suitMerge(n,x, ps, future=T)
  		rs <- r; pd <- ps
      rs <- writeRaster(rs, paste(oDir, "/", gs, "-", cropname, "-merged_suitability.asc", sep=""), 
                        overwrite=T, format='ascii')
      pd <- writeRaster(pd, paste(oDir, "/", gs, "-", cropname, "-mergedwhich_suitability.asc", sep=""),
                        overwrite=T, format='ascii')
  		#jpeg(paste(oDir, "/", gs, "-", cropname, "-merged_suitability.jpg", sep=""), quality=100, height=600, width=1800)
  		#par(mfrow=c(1,2))
  		#plot(rs, col=colorRampPalette(c("yellow","red"))(100)); plot(wrld_simpl, add=T)
  		#plot(pd); plot(wrld_simpl, add=T)
  		#dev.off()
  	}
  } else {
    theRow <- get(paste(run.type,"row",sep=""))
    if (!file.exists(paste(oDir, "/", gs, "-", cropname, "-",theRow$VARIABLE,"_suitability.asc",sep="")) | ow.runs) {
      eco <- suitCalc(climPath=climdir, Gmin=gsl,Gmax=gsl,
                      Tkmp=theRow$KILL,Tmin=theRow$MIN,Topmin=theRow$OPMIN,Topmax=theRow$OPMAX,
                      Tmax=theRow$MAX,Rmin=rainrow$MIN,Ropmin=rainrow$OPMIN,Ropmax=rainrow$OPMAX,
                      Rmax=rainrow$MAX, outfolder=oDir, 
                      cropname=paste(gs,'-', cropname, "-",theRow$VARIABLE,sep=""))
    }
  }
	#Finalising
	if (!ow.runs | !ow.merge) {return("NA")} else {return(stack(n,x,pd,rs))}
}
