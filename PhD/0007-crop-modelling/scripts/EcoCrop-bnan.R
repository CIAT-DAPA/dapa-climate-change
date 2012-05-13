#### LIBRARIES: raster, maptools, rgdal, sp
####
stop("Error: do not source this whole thing!")

rDir <- "F:/PhD-work/crop-modelling"
bDir <- paste(rDir,"/EcoCrop",sep="")
crop <- "bnan"
cDir <- paste(bDir,"/models/EcoCrop-",toupper(crop),sep="")

library(raster)

################################################################################
################################################################################

#BNAN and BNME are special cases because they are genepools of the same crop (bean)
#and given the lack of data I decided to do a global calibration.

#so what i need is to get rid of everything outside the global 2.5 mask
#read mask
msk <- raster(paste(bDir,"/analysis-mask/global-mask/mask.tif",sep=""))
inData <- read.csv(paste(cDir,"/analyses/data/original/bean_and_v2.csv",sep=""))
inData$WORLD <- extract(msk,cbind(X=as.numeric(inData$LONGITUDED),Y=as.numeric(inData$LATITUDED)))
worldData <- inData[which(inData$WORLD==1),]
write.csv(worldData,paste(cDir,"/analyses/data/",crop,"-world.csv",sep=""),quote=T,row.names=F)

library(maptools); data(wrld_simpl)
tiff(paste(cDir,"/analyses/data/",crop,".tif",sep=""),res=150,height=775,width=1500,compression="lzw",pointsize=10)
plot(msk,legend=F); plot(wrld_simpl,add=T)
points(inData$LONGITUDED,inData$LATITUDED,pch=20,col='black',cex=0.7)
points(worldData$LONGITUDED,worldData$LATITUDED,pch=20,col='red',cex=0.7)
grid()
dev.off()


################################################################################
################################################################################
##### ANALYSES
#Getting unique coordinates
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
source(paste(src.dir,"/getUniqueCoord.R",sep="")) #loading the function

rs <- read.csv(paste(cDir,"/analyses/data/",crop,"-world.csv",sep="")) #load the data
latLonFields <- c("LONGITUDED","LATITUDED")
flds <- c(which(names(rs)==latLonFields[1]),which(names(rs)==latLonFields[2]))
rs <- getUniqueCoord(rs, fields=flds, resol=2.5/60) #running the function #fields is c(lon,lat)

#Dummy part of unloading and re-loading the data
write.csv(rs, paste(cDir,"/analyses/data/dataset2_5m.csv",sep=""), row.names=F, quote=T) #write outcome into new file
rs <- rs[which(rs$IS_UNIQUE == TRUE),] #selecting unique records
write.csv(rs, paste(cDir,"/analyses/data/unique2_5m.csv",sep=""), row.names=F, quote=T) #write new dataset only containing unique records
rs <- read.csv(paste(cDir,"/analyses/data/unique2_5m.csv",sep="")) #re-loading the data
nrow(rs)

#Splitting in test/train datasets
source(paste(src.dir,"/randomSplit.R",sep=""))
rs <- randomSplit(rs, 20) #20 is percentage of data to be taken out

#Plot & write test and train datasets separately
tiff(paste(cDir,"/analyses/img/test_train.tiff",sep=""), compression="lzw",res=150, height=675, width=1200,pointsize=10)
plot(cbind(rs$LONGITUDED[which(rs$TEST_TRAIN == "TEST")], rs$LATITUDED[which(rs$TEST_TRAIN == "TEST")]), pch="+", col="red", xlab="", ylab="")
library(maptools); data(wrld_simpl); plot(wrld_simpl, add=T)
points(cbind(rs$LONGITUDED[which(rs$TEST_TRAIN == "TRAIN")], rs$LATITUDED[which(rs$TEST_TRAIN == "TRAIN")]), pch=20, col="blue", cex=0.5)
grid()
dev.off()

write.csv(rs, paste(cDir,"/analyses/data/unique.csv",sep=""), row.names=F, quote=T) #write unique records with new field TRAIN/TEST
write.csv(rs[which(rs$TEST_TRAIN == "TEST"),], paste(cDir,"/analyses/data/test.csv",sep=""), row.names=F, quote=T) #reselect and store test data
write.csv(rs[which(rs$TEST_TRAIN == "TRAIN"),], paste(cDir,"/analyses/data/train.csv",sep=""), row.names=F, quote=T) #reselect and store train data

#Extracting climate data
source(paste(src.dir,"/extractClimateData.R",sep=""))
rs <- read.csv(paste(cDir,"/analyses/data/unique.csv",sep="")) #load unique records
for (v in c("prec", "tmin", "tmean", "tmax")) {
	rs <- extractMonthlyData(wd="F:/PhD-work/EcoCrop-development/climate/global_2_5min",
                                    variable=v, ext="", rs, fields=flds, verbose=T)
}
write.csv(rs, paste(cDir,"/analyses/data/climates.csv",sep=""), row.names=F, quote=T)

#Calculating gs parameters for calibration
source(paste(src.dir,"/calibrationParameters.R",sep=""))
rs <- read.csv(paste(cDir,"/analyses/data/climates.csv",sep=""))
rs <- calibrationParameters(rs, gs=3, verbose=T)
write.csv(rs, paste(cDir,"/analyses/data/calibration.csv",sep=""), row.names=F, quote=T)

#Get calibration parameters
source(paste(src.dir,"/getParameters.R",sep=""))
dataset <- read.csv(paste(cDir,"/analyses/data/calibration.csv",sep=""))
for (gs in 1:12) {
	plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1	
  for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, 
                            plotdir=paste(cDir,"/analyses/img",sep=""), gs=gs, varname=varn)
		if (v == 1 & gs == 1) {finalTable <- calPar} else {finalTable <- rbind(finalTable, calPar)}
		v <- v+1
	}
}
for (gs in c("MEAN", "MODE", "MAX", "MIN")) {
	plotdata <- dataset[,grep(paste(gs, "_", sep=""), names(dataset))]
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1
	for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, 
                            plotdir=paste(cDir,"/analyses/img",sep=""), gs=gs, varname=varn)
		finalTable <- rbind(finalTable, calPar)
		v <- v+1
	}
}
write.csv(finalTable, paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""), row.names=F)

#replace the existing stuff by the Beebe et al. (2011) parameterisation
# finalTable2 <- data.frame(matrix(ncol=ncol(finalTable),nrow=2))
# names(finalTable2) <- names(finalTable)
# finalTable2[1,] <- c(1,"prec",0,200,363,450,710)
# finalTable2[2,] <- c(1,"tmean",0,136,175,231,256)
# write.csv(finalTable2, paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""), row.names=F)


#Running the model
source(paste(src.dir,"/EcoCrop.R",sep=""))
for (gs in c(1:12,"MEAN","MODE","MAX","MIN")) {
	cat("GS", gs, "\n")
	p <- read.csv(paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""))
	p <- p[which(p$GS==gs),]
	vl <- c("tmean","tmin","tmax")
	for (rw in 2:4) {
    jpegFile <- paste(cDir,"/analyses/runs/global/", gs, "-",crop,"-",vl[rw-1],"-suitability.jpg",sep="")
		if (!file.exists(jpegFile)) {
			eco <- suitCalc(climPath="F:/PhD-work/EcoCrop-development/climate/global_10min", 
                      Gmin=90,Gmax=90,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],
                      Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                      Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                      outfolder=paste(cDir,'/analyses/runs/global', sep=""),
                      cropname=paste(gs,'-',crop,'-',vl[rw-1],sep=""))
			jpeg(jpegFile, quality=100, height=900,width=1200,units="px")
			plot(eco)
			dev.off()
		}
	}
}

# #!Not necessary
# #Merge tmax & tmin runs
# source(paste(src.dir,"/suitMerge.R",sep=""))
# library(maptools)
# data(wrld_simpl)
# d <- read.csv(paste(cDir,"/analyses/data/unique.csv",sep=""))
# parList <- read.csv(paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""))
# gsList <- unique(parList$GS)
# for (gs in gsList) {
# 	cat("GS:", gs, "\n")
# 	n <- raster(paste(cDir,"/analyses/runs/", gs, "-",crop,"-tmin_suitability.asc", sep=""))
# 	x <- raster(paste(cDir,"/analyses/runs/", gs, "-",crop,"-tmax_suitability.asc", sep=""))
# 	r <- suitMerge(n,x)
# 	
#   #suitability prediction
# 	rs <- raster(r,1)
#   rs <- writeRaster(rs, paste(cDir,"/analyses/runs/", gs, "-",crop,"-merged_suitability.asc", sep=""), overwrite=T, format='ascii')
# 	
#   #which parameter set
#   pd <- raster(r,2)
#   pd <- writeRaster(pd, paste(cDir,"/analyses/runs/", gs, "-",crop,"-mergedwhich_suitability.asc", sep=""), overwrite=T, format='ascii')
# 	
# 	jpeg(paste(cDir,"/analyses/runs/", gs, "-",crop,"-merged_suitability.jpg", sep=""), quality=100, height=600, width=1800)
# 	par(mfrow=c(1,2))
# 	plot(rs, col=colorRampPalette(c("yellow","red"))(100)); plot(wrld_simpl, add=T)
# 	points(d[,latLonFields[1]], d[,latLonFields[2]], pch=20, cex=0.7, col="blue")
# 	plot(pd); plot(wrld_simpl, add=T)
# 	points(d[,latLonFields[1]], d[,latLonFields[2]], pch=20, cex=0.7, col="blue")
# 	dev.off()
# }


#Assess accuracy of each growing season and each parameter tuning
source(paste(src.dir,"/accuracy.R",sep=""))
test <- read.csv(paste(cDir,"/analyses/data/test.csv",sep=""))
test <- cbind(test[,flds[1]], test[,flds[2]])
train <- read.csv(paste(cDir,"/analyses/data/train.csv",sep=""))
train <- cbind(train[,flds[1]], train[,flds[2]])
parList <- read.csv(paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""))
gsList <- unique(parList$GS)
for (gs in gsList) {
	pList <- parList[which(parList$GS==gs),][2:4,]
  #pList <- parList[which(parList$GS==gs),][2,]
	for (vr in pList$VARIABLE) {
		for (suf in c("_p","_t","_")) {
			cat("GS:", gs, "- VAR:", vr, "- SUF:", suf, "\n")
			rs <- raster(paste(cDir,"/analyses/runs/global/", gs, "-",crop,"-", vr, suf, "suitability.asc", sep=""))
			tem <- accMetrics(rs, test) #doing with test data
			trm <- accMetrics(rs, train) #doing with training data
			resrow <- data.frame(GS=gs, VARIABLE=vr, TYPE=paste(suf, "suitability", sep=""), TEST.AV.SUIT=tem$METRICS$SUIT, TEST.SD.SUIT=tem$METRICS$SUITSD, TEST.MAX.SUIT=tem$METRICS$SUITX, TEST.MIN.SUIT=tem$METRICS$SUITN, TEST.OMISSION.RATE=tem$METRICS$OMISSION_RATE, TEST.ERROR=tem$METRICS$RMSQE, TEST.ERR.DIST=tem$METRICS$ERR_DIST, TEST.MXE=tem$METRICS$MAX_ENT, TEST.SLOPE=tem$METRICS$SLOPE, TRAIN.AV.SUIT=trm$METRICS$SUIT, TRAIN.SD.SUIT=trm$METRICS$SUITSD, TRAIN.MAX.SUIT=trm$METRICS$SUITX, TRAIN.MIN.SUIT=trm$METRICS$SUITN, TRAIN.OMISSION.RATE=trm$METRICS$OMISSION_RATE, TRAIN.ERROR=trm$METRICS$RMSQE, TRAIN.ERR.DIST=trm$METRICS$ERR_DIST, TRAIN.MXE=trm$METRICS$MAX_ENT, TRAIN.SLOPE=trm$METRICS$SLOPE)
			rescol <- data.frame(tem$MXE_CURVE, trm$MXE_CURVE)
			names(rescol) <- c(paste("TEST.GS.",gs,sep=""), paste("TRAIN.GS.",gs,sep=""))
			if (gs == gsList[1] & vr == pList$VARIABLE[1] & suf == "_p") {
				rres <- resrow
				cres <- cbind(SUIT=c(1:100), rescol)
			} else {
				rres <- rbind(rres, resrow)
				cres <- cbind(cres, rescol)
			}
		}
	}
}
write.csv(rres, paste(cDir,"/analyses/data/accuracy-metrics.csv",sep=""), row.names=F)
write.csv(cres, paste(cDir,"/analyses/data/entropy-curves.csv",sep=""), row.names=F)

#plot accuracy metrics here
rres <- read.csv(paste(cDir,"/analyses/data/accuracy-metrics.csv",sep=""))
plot.acc <- rres[which(rres$TYPE=="_suitability"),]
tiff(paste(cDir,"/analyses/img/accuracy.tiff",sep=""),
     res=300,pointsize=10,width=1500,height=1300,units="px",compression="lzw")
par(mar=c(4.5,4,1,1),cex=1)
plot(plot.acc$TEST.OMISSION.RATE[which(plot.acc$VARIABLE=="tmean")],
     plot.acc$TEST.ERROR[which(plot.acc$VARIABLE=="tmean")],
     xlim=c(0,1),ylim=c(0,1),pch=21,
     xlab="OR",ylab="RMSE")
points(plot.acc$TEST.OMISSION.RATE[which(plot.acc$VARIABLE=="tmin")],
       plot.acc$TEST.ERROR[which(plot.acc$VARIABLE=="tmin")],
       pch=22)
points(plot.acc$TEST.OMISSION.RATE[which(plot.acc$VARIABLE=="tmax")],
       plot.acc$TEST.ERROR[which(plot.acc$VARIABLE=="tmax")],
       pch=24)
abline(h=0.5,lwd=1.2,lty=2)
abline(v=0.1,lwd=1.2,lty=2)
grid()
legend(0.6,1,cex=0.8,pch=c(21,22,24),legend=c("Tmean-based","Tmin-based","Tmax-based"))
dev.off()


#for each growing season and variable plot the crop dist. surfaces & the prediction
#in a n-panel (n=number of cropdist surfaces) figure. Overlay with points
library(maptools); data(wrld_simpl)
cDistDir <- paste(cDir,"/cropdist",sep="")
npan <- length(list.files(cDistDir,pattern=".asc"))+1

#load landrace data
rs <- read.csv(paste(cDir,"/analyses/data/unique.csv",sep="")) #load unique records

for (gs in c(1:12,"MEAN","MODE","MAX","MIN")) {
  cat("\n")
  cat("GS", gs, "\n")
	p <- read.csv(paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""))
	p <- p[which(p$GS==gs),]
	pList <- p[which(p$GS==gs),][2:4,]
	for (vr in pList$VARIABLE) {
    cat("Parameter set", vr, "\n")
    cat("Load crop distribution surface \n")
    stk <- stack(paste(cDistDir,"/",list.files(cDistDir,pattern=".asc"),sep=""))
    sui <- raster(paste(cDir,"/analyses/runs/global/", gs, "-",crop,"-", vr, "_suitability.asc", sep=""))
		
    #plotting all the tiffs
    tiffName <-paste(cDir,"/analyses/img/vc-",gs,"-",crop,"-",vr,".tif",sep="")
    tiff(tiffName,res=150,height=1900,width=3500,compression="lzw")
    par(mar=c(6.5,3.5,1,1),mfrow=c(2,2),cex=1.2)
    
    #plot the suitability surface first
    cat("Plotting suitability prediction \n")
    qs <- seq(0,100,by=5)
    sui[which(sui[]==0)] <- NA
    plot(sui,breaks=qs,lab.breaks=qs,
           col=colorRampPalette(c("red","yellow","green","dark green"))(length(qs)-1),
           legend=T,horizontal=T,legend.shrink=0.9) #,useRaster=F)
    plot(wrld_simpl,add=T)
    points(cbind(rs$LONGITUDED[which(rs$TEST_TRAIN == "TEST")], rs$LATITUDED[which(rs$TEST_TRAIN == "TEST")]), pch=20, col="black",cex=0.9)
    points(cbind(rs$LONGITUDED[which(rs$TEST_TRAIN == "TRAIN")], rs$LATITUDED[which(rs$TEST_TRAIN == "TRAIN")]), pch=20, col="blue", cex=0.9)
    grid()
    
    #now plot the cropDist layers
    laycount <- 1
    cat("Plotting crop distributions \n")
    for (i in 1:nlayers(stk)) {
      sp <- crop(stk[[i]],sui)
      qs <- unique(quantile(sp[],na.rm=T,probs=seq(0,1,by=0.05)))
      plot(sp,breaks=qs,lab.breaks=round(qs,2),
           col=colorRampPalette(c("light blue","blue"))(length(qs)-1),
           legend=T,horizontal=T,legend.shrink=0.9) #,useRaster=F)
      plot(wrld_simpl,add=T)
      grid()
      laycount<-laycount+1
    }
    
    if (nlayers(stk)!=3) {
      cat("Plotting dummy as crop. dist. were incomplete \n")
      for (i in 1:(3-nlayers(stk))) {
        rs_dumm <- raster(sui)
        rs_dumm[] <- 0
        plot(rs_dumm,col="grey 90",legend=F)
        plot(wrld_simpl,add=T)
        grid()
      }
    }
    dev.off()
	}
}


# #!Not necessary
# #Accuracy metrics for the merged grids
# source(paste(src.dir,"/accuracy.R",sep=""))
# test <- read.csv(paste(cDir,"/analyses/data/test.csv",sep=""))
# test <- cbind(test[,flds[1]], test[,flds[2]])
# train <- read.csv(paste(cDir,"/analyses/data/train.csv",sep=""))
# train <- cbind(train[,flds[1]], train[,flds[2]])
# parList <- read.csv(paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""))
# gsList <- unique(parList$GS)
# for (gs in gsList) {
# 	cat("GS:", gs, "\n")
# 	rs <- raster(paste(cDir,"/analyses/runs/", gs, "-",crop,"-merged_suitability.asc", sep=""))
# 	tem <- accMetrics(rs, test) #doing with test data
# 	trm <- accMetrics(rs, train) #doing with training data
# 	resrow <- data.frame(GS=gs, TEST.AV.SUIT=tem$METRICS$SUIT, TEST.SD.SUIT=tem$METRICS$SUITSD, TEST.MAX.SUIT=tem$METRICS$SUITX, TEST.MIN.SUIT=tem$METRICS$SUITN, TEST.OMISSION.RATE=tem$METRICS$OMISSION_RATE, TEST.ERROR=tem$METRICS$RMSQE, TEST.ERR.DIST=tem$METRICS$ERR_DIST, TEST.MXE=tem$METRICS$MAX_ENT, TEST.SLOPE=tem$METRICS$SLOPE, TRAIN.AV.SUIT=trm$METRICS$SUIT, TRAIN.SD.SUIT=trm$METRICS$SUITSD, TRAIN.MAX.SUIT=trm$METRICS$SUITX, TRAIN.MIN.SUIT=trm$METRICS$SUITN, TRAIN.OMISSION.RATE=trm$METRICS$OMISSION_RATE, TRAIN.ERROR=trm$METRICS$RMSQE, TRAIN.ERR.DIST=trm$METRICS$ERR_DIST, TRAIN.MXE=trm$METRICS$MAX_ENT, TRAIN.SLOPE=trm$METRICS$SLOPE)
# 	rescol <- data.frame(tem$MXE_CURVE, trm$MXE_CURVE)
# 	names(rescol) <- c(paste("TEST.GS.",gs,sep=""), paste("TRAIN.GS.",gs,sep=""))
# 	if (gs == gsList[1]) {
# 		rres <- resrow
# 		cres <- cbind(SUIT=c(1:100), rescol)
# 	} else {
# 		rres <- rbind(rres, resrow)
# 		cres <- cbind(cres, rescol)
# 	}
# }
# write.csv(rres, paste(cDir,"/analyses/data/accuracy-metrics-merged.csv",sep=""), row.names=F)
# write.csv(cres, paste(cDir,"/analyses/data/entropy-curves-merged.csv",sep=""), row.names=F)


########################################################################
##### HERE RUN THE MODEL USING THE afasia CLIMATE DATA #################
########################################################################
#Running the model
source(paste(src.dir,"/EcoCrop.R",sep=""))
for (gs in c(1:12,"MEAN","MODE","MAX","MIN")) {
  cat("GS", gs, "\n")
  p <- read.csv(paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""))
  p <- p[which(p$GS==gs),]
  vl <- c("tmean","tmin","tmax")
  for (rw in 2:4) {
    jpegFile <- paste(cDir,"/analyses/runs/", gs, "-",crop,"-",vl[rw-1],"-suitability.jpg",sep="")
    if (!file.exists(jpegFile)) {
      eco <- suitCalc(climPath=paste(rDir,"/climate-data/worldclim/afasia_10min",sep=""), 
                      Gmin=90,Gmax=90,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],
                      Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                      Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                      outfolder=paste(cDir,'/analyses/runs', sep=""),
                      cropname=paste(gs,'-',crop,'-',vl[rw-1],sep=""))
      jpeg(jpegFile, quality=100, height=900,width=1200,units="px")
      plot(eco)
      dev.off()
    }
  }
}



##########################################################
### HERE YOU NEED TO ORGANISE HARVESTED AREA DATA FIRST !!
##########################################################
#Evaluation stuff...
setwd(src.dir)
source("validation.R")
source("createMask.R")

#loop through different growing seasons
for (gs in c(1:12,"MEAN","MODE","MAX","MIN")) {
  evDir <- paste(cDir,"/analyses/evaluation/gs-",gs,sep="")
  if (!file.exists(evDir)) {dir.create(evDir)}
  
  #looping through different parameter sets
  for (vl in c("tmean","tmin","tmax")) {
    #process only if rates-%vl%.csv does not exist
    if (!file.exists(paste(evDir,"/rates-",vl,".csv",sep=""))) {
      #load raster
      rsl <- raster(paste(cDir,"/analyses/runs/",gs,"-",crop,"-",vl,"_suitability.asc",sep=""))
      
      #reference the reported crop distribution shapefiles
      shp.faostat <- c(paste(cDir,"/agricultural-data/FAOSTAT/faostat-adm0-joined.shp",sep=""),"HSBPRES")
      shp.agromaps1 <- c(paste(cDir,"/agricultural-data/agroMAPS/agromaps-adm1-joined.shp",sep=""),"ispresent")
      shp.agromaps2 <- c(paste(cDir,"/agricultural-data/agroMAPS/agromaps-adm2-joined.shp",sep=""),"ispresent")
      
      #list the referred shapefiles
      oblist <- ls(pattern="shp")
      
      #loop through each shapefile
      for (ob in oblist) {
      	cat("Processing", ob, "\n")
        
        #process only if shp.*-%vl%.csv does not exist else only load
        if (!file.exists(paste(evDir,"/", ob, "-",vl,".csv", sep=""))) {
        	shp <- readShapePoly(get(ob)[1]) #load shapefile
        	field <- get(ob)[2] #get field name
        	result <- extractFromShape(shp, field, naValue=-9999, rsl) #get 
        	write.csv(result, paste(evDir,"/", ob, "-",vl,".csv", sep=""),row.names=F)
          rm(shp); g=gc(); rm(g)
        } else {
          result <- read.csv(paste(evDir,"/", ob, "-",vl,".csv", sep=""))
        }
      	#calculate model evaluation metrics
      	mets <- valMetrics(result, pres.field=field)
        
        #cat metrics onto matrix
      	if (ob == oblist[1]) {
      		rr <- cbind(SHP=ob, mets)
      	} else {
      		rr <- rbind(rr, cbind(SHP=ob, mets))
      	}
      	rm(result); rm(mets); g=gc(); rm(g)
      }
      write.csv(rr, paste(evDir,"/rates-",vl,".csv",sep=""), quote=F, row.names=F)
      rm(rr); g=gc(); rm(g)
    }
  }
}


# #Validation for all adm1 and all adm2 (sorghum specific)
# source(paste(src.dir,"/validation.R",sep=""))
# dataf1 <- rbind(read.csv("./data/validation/shp.agromaps1.csv")[,1:12],read.csv("./data/validation/shp.countrystat1.csv")[,1:12])
# mets <- cbind(TYPE="adm1", valMetrics(dataf1, pres.field="Is_present"))
# dataf2 <- rbind(read.csv("./data/validation/shp.agromaps2.csv")[,1:12],read.csv("./data/validation/shp.countrystat2.csv")[,1:12],read.csv("./data/validation/shp.icrisat.csv")[,1:12])
# mets <- rbind(mets, cbind(TYPE="adm2", valMetrics(dataf2, pres.field="Is_present")))
# write.csv(mets, "./data/validation/rates-merged.csv", quote=F, row.names=F)


#################

#Projection onto future
source(paste(src.dir,"/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/EcoCrop.R",sep=""))
source(paste(src.dir,"/suitMerge.R",sep=""))

gls <- c("bccr_bcm2_0","cccma_cgcm3_1_t47","cccma_cgcm3_1_t63","cnrm_cm3","csiro_mk3_0",
"csiro_mk3_5","gfdl_cm2_0","gfdl_cm2_1","giss_aom","giss_model_eh","giss_model_er","iap_fgoals1_0_g",
"ingv_echam4","inm_cm3_0","ipsl_cm4","miroc3_2_hires","miroc3_2_medres","miub_echo_g",
"mpi_echam5","mri_cgcm2_3_2a","ncar_ccsm3_0","ncar_pcm1","ukmo_hadcm3","ukmo_hadgem1")

for (gcm in gls) {
	cat(gcm, "\n")
	fDir <- paste(rDir, "/climate-data/worldclim/afasia_10min_future", sep="") #future climate data dir
	frDir <- paste(cDir, "/analyses/runs-future/", gcm, sep="") #future runs dir
  if (!file.exists(frDir)) {dir.create(frDir)} #create future runs dir
	prDir <- paste(cDir, "/analyses/runs", sep="") #present-day runs dir
	aDir <- paste(fDir, "/", gcm, sep="") #gcm specific directory
  if (!file.exists(aDir)) {dir.create(aDir)} #create gcm specific folder
  
	#Run the function
	fut <- futruns(climdir=aDir, oDir=frDir, cDir=prDir, gs="MAX", gsl=90, 
                 parlist=paste(cDir,"/analyses/data/calibration-parameters.csv",sep=""), 
                 cropname=crop, run.type="tmin", ow.runs=F, ow.merge=NA)
}


##############################################################
#Calculate impact metrics per countries and for the study area
library(maptools)
source(paste(src.dir,"/impacts.R",sep=""))
source(paste(src.dir,"/createMask.R",sep=""))

#folders
cd <- paste(cDir, "/analyses/runs", sep="")
fd <- paste(cDir, "/analyses/runs-future/", sep="")
imd <- paste(cDir,"/analyses/impacts",sep=""); if (!file.exists(imd)) {dir.create(imd)}

#define and read the shapefile
shname <- "starea-countries.shp" #starea-countries selcountries
sh <- readShapePoly(paste(bDir,"/analysis-mask/", shname, sep=""))

#define other stuff
gs <- 1
run.type <- "tmean"

#loop the GCMs
gls <- list.files(fd)
for (gcm in gls) {
	cat("Model", gcm, "\n")
  
  #GCM specific folders (i/o)
	od <- paste(cDir, "/analyses/impacts/", gcm, sep="")
	if (!file.exists(od)) {dir.create(od)}
	id <- paste(fd, "/", gcm, sep="")
	r1 <- raster(paste(cd, "/",gs,"-",crop,"-",run.type,"_suitability.asc", sep="")) #current
	r2 <- raster(paste(id, "/",gs,"-",crop,"-",run.type,"_suitability.asc", sep="")) #future
  
  #Calculate the metrics
  if (shname == "selcountries.shp") {
	  pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
  } else {
    pp <- iMetrix(r1,r2,sh,od, chggrid=F, impact=T, classes=T)
  }
	
	im <- cbind(GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
	cl <- cbind(GCM=rep(gcm,times=nrow(pp$CLASSES)), pp$CLASSES)
	
	if (gcm == gls[1]) {
		res.im <- im
		res.cl <- cl
	} else {
		res.im <- rbind(res.im, im)
		res.cl <- rbind(res.cl, cl)
	}
	rm(im); rm(cl)
}
write.csv(res.im, paste(imd, "/impacts-", shname, ".csv", sep=""), quote=T, row.names=F)
write.csv(res.cl, paste(imd, "/classes-", shname, ".csv", sep=""), quote=T, row.names=F)


###################################################
#Average and uncertainties
#Creating the stack
source(paste(src.dir,"/uncertainty-v2.R",sep=""))

#Analysis folders
gcmDir <- paste(cDir,"/analyses/impacts",sep="")
gcmList <- list.files(gcmDir); gcmList <- gcmList[-grep(".shp", gcmList)]
unDir <- paste(cDir,"/analyses/uncertainties",sep="")
if (!file.exists(unDir)) {dir.create(unDir)}

#Precalculated suitability change raster (must have been calculated in previous step)
rsn <- "1-Study area-suitability-change"

#loop through GCMs
for (gcm in gcmList) {
	cat(gcm, "\n")
	rsDir <- paste(gcmDir, "/", gcm, sep="")
	rs <- raster(paste(rsDir, "/", rsn, ".asc", sep=""))
	#v <- extract(rs, xy)
	assign(gcm, rs); rm(rs)

	if (gcm == gcmList[1]) {
		gcmstack <- c(get(gcm))
	} else {
		gcmstack <- c(gcmstack, get(gcm))
	}
}
gcmstack <- stack(gcmstack)
uc <- uncertainties(gcmstack, outFolder=unDir)


########################################################
#Calculate currently suitable and future suitable area
source(paste(src.dir,"/impacts.R",sep=""))

#directories
bd <- paste(cDir,"/analyses",sep="")
cd <- paste(bd, "/runs", sep="")
fd <- paste(bd, "/runs-future", sep="")

#shape to work with
shname <- "starea-countries.shp" #starea-countries selcountries
sh <- readShapePoly(paste(bDir,"/analysis-mask/", shname, sep=""))

#define other stuff
gs <- 1
run.type <- "tmean"

#load and run stuff for current (comment if already done)
r1 <- raster(paste(cd, "/",gs,"-",crop,"-",run.type,"_suitability.asc", sep="")) #current
cp <- suitArea(r1, sh)
write.csv(cp, paste(bd, "/impacts/current-area-", shname,".csv", sep=""), row.names=F, quote=T)

#load and run stuff for each of the future predictions
gls <- list.files(fd)
for (gcm in gls) {
	cat("Model", gcm, "\n")
	id <- paste(fd, "/", gcm, sep="")
	r2 <- raster(paste(id, "/",gs,"-",crop,"-",run.type,"_suitability.asc", sep="")) #future
	pp <- suitArea(r2,sh)
	im <- cbind(GCM=rep(gcm,times=nrow(pp)), pp)
	
	if (gcm == gls[1]) {
		res.im <- im
	} else {
		res.im <- rbind(res.im, im)
	}
	rm(im)
}
write.csv(res.im, paste(bd, "/impacts/future-area-", shname, ".csv", sep=""), quote=T, row.names=F)


##########################################################################
#create images and figures of modelling results
library(maptools); data(wrld_simpl)

#1. plot current
gs <- 1
run.type <- "tmean"
rs <- raster(paste(cDir,"/analyses/runs/",gs,"-",crop,"-",run.type,"_suitability.asc",sep=""))
rs[which(rs[]==0)] <- NA

z <- extent(rs)
aspect <- (z@ymax-z@ymin)*1.15/(z@xmax-z@xmin)
zlims <- c(0,100)

brks <- seq(0,100,by=5)
brks.lab <- brks
nb <- length(brks)-1

imgdir <- paste(cDir,"/analyses/img/figures",sep="")
if (!file.exists(imgdir)) {dir.create(imgdir)}

tiff(paste(imgdir,"/current_suitability.tif",sep=""),
         res=300,pointsize=7,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(rs,
     col=colorRampPalette(c("red","orange","yellow","green","dark green"))(nb),
     #col=colorRampPalette(c("grey10","grey90"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     axes=T,
     #xlim=c(z@xmin,z@xmax),ylim=c(z@ymin,z@ymax),
     zlim=zlims,
     horizontal=T,
     legend.width=1,
     legend.shrink=0.99,
     #nlevel=nb*100,
     useRaster=T)
grid(col="black",lwd=0.8)
plot(wrld_simpl,lwd=0.8,add=T)
dev.off()

rm(rs); g=gc(); rm(g)

#2. plot mean, 25p, 75p, sd and agreement
a <- raster(paste(cDir,"/analyses/uncertainties/mean.asc",sep=""))
b <- raster(paste(cDir,"/analyses/uncertainties/mean-bottom25p.asc",sep=""))
c <- raster(paste(cDir,"/analyses/uncertainties/mean-top25p.asc",sep=""))
d <- raster(paste(cDir,"/analyses/uncertainties/agreement-percent.asc",sep=""))

#get limits of z values
impactvals <- c(a[],b[],c[])
mx <- max(impactvals,na.rm=T)
mn <- min(impactvals,na.rm=T)

brks <- c(-100,-80,-60,-50,-40,-20,-10,-5,0,10,20,30,40,50,60,100)
brks.lab <- c("-Inf", brks[2:(length(brks)-1)],"Inf")
nb <- length(brks)-1

#plot ensemble mean
tiff(paste(imgdir,"/ensemble-mean.tif",sep=""),
         res=300,pointsize=7,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(a,
     col=colorRampPalette(c("red","orange","yellow","green","dark green"))(nb),
     #col=colorRampPalette(c("grey10","grey90"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     axes=T,
     xlim=c(z@xmin,z@xmax),ylim=c(z@ymin,z@ymax),
     zlim=c(mn,mx),
     horizontal=T,
     legend.width=1,
     legend.shrink=0.99,
     #nlevel=nb*100,
     useRaster=T)
grid(col="black",lwd=0.8)
plot(wrld_simpl,lwd=0.8,add=T)
dev.off()

#bottom 25p
tiff(paste(imgdir,"/bottom-25p.tif",sep=""),
         res=300,pointsize=7,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(b,
     col=colorRampPalette(c("red","orange","yellow","green","dark green"))(nb),
     #col=colorRampPalette(c("grey10","grey90"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     axes=T,
     #xlim=c(z@xmin,z@xmax),ylim=c(z@ymin,z@ymax),
     zlim=c(mn,mx),
     horizontal=T,
     legend.width=1,
     legend.shrink=0.99,
     #nlevel=nb*100,
     useRaster=T)
grid(col="black",lwd=0.8)
plot(wrld_simpl,lwd=0.8,add=T)
dev.off()


#top 25p
tiff(paste(imgdir,"/top-25p.tif",sep=""),
         res=300,pointsize=7,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(c,
     col=colorRampPalette(c("red","orange","yellow","green","dark green"))(nb),
     #col=colorRampPalette(c("grey10","grey90"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     axes=T,
     #xlim=c(z@xmin,z@xmax),ylim=c(z@ymin,z@ymax),
     zlim=c(mn,mx),
     horizontal=T,
     legend.width=1,
     legend.shrink=0.99,
     #nlevel=nb*100,
     useRaster=T)
grid(col="black",lwd=0.8)
plot(wrld_simpl,lwd=0.8,add=T)
dev.off()

#agreement
brks <- c(0,.1,.2,.3,.4,.6,.8,.9,.95,1)
brks.lab <- brks
nb <- length(brks)-1

tiff(paste(imgdir,"/stippling.tif",sep=""),
         res=300,pointsize=7,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(d,
     col=colorRampPalette(c("grey90","grey10"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     axes=T,
     #xlim=c(z@xmin,z@xmax),ylim=c(z@ymin,z@ymax),
     zlim=c(0,1),
     horizontal=T,
     legend.width=1,
     legend.shrink=0.99,
     #nlevel=nb*100,
     useRaster=T)
grid(col="black",lwd=0.8)
plot(wrld_simpl,lwd=0.8,add=T)
dev.off()

#3. make boxplots for regions of interest
#   open the file "impacts-starea-countries.shp.csv"
imets <- read.csv(paste(cDir,"/analyses/impacts/impacts-starea-countries.shp.csv",sep=""))

#   sub-select SCEN=no.mig and THRESHOLD=0
imets <- imets[imets$SCEN=="no.mig",]
imets <- imets[imets$THRESHOLD==0,]

#   open the file "Country-regions-LO2008.csv"
regs <- read.csv("D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/data/Country-regions-LO2008.csv")

#   merge the region names with the other data
x <- merge(imets,regs,by="CID")

#   open "current-area-starea-countries.shp.csv" and merge it with the first one
ca <- read.csv(paste(cDir,"/analyses/impacts/current-area-starea-countries.shp.csv",sep=""))
ca <- ca[which(ca$THRESH==0),]
x <- merge(x,ca,by="CID")

#   open "future-area-starea-countries.shp.csv" and merge it with the first one
fa <- read.csv(paste(cDir,"/analyses/impacts/future-area-starea-countries.shp.csv",sep=""))
fa <- fa[which(fa$THRESH==0),]
x <- merge(x,fa,by=c("CID","GCM"))

#   remove extra fields
x$COUNTRY.y <- NULL
x$COUNTRY <- NULL
x$THRESH.x <- NULL; x$THRESH.y <- NULL
x$Country <- NULL

#   calculate percent change in area (future-current)/current*100
x$CHG.AREA <- (x$AREA.SUIT-x$AREA.SUIT.y)/x$AREA.SUIT.y * 100

#   make the boxplots for three variables: 
#           overall suitability change
tiff(paste(imgdir,"/osc-boxplot.tif",sep=""),res=300,pointsize=10,width=800,
     height=1000,units="px",compression="lzw")
par(mar=c(6,4,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(x$AV.SUIT.CHG~x$Region,col="grey",boxwex=0.5,
        pch=20,outwex=0.2,horizontal=T,ylim=c(-100,100),
        xlab="Overall suitability change (%)")
abline(v=0,lwd=0.7,lty=2)
abline(v=seq(-100,100,by=25),lwd=0.6,lty=2,col="grey50")
dev.off()

#           PIA/NIA ratio
x$PIA_NIA_RATIO <- x$AREA.SUIT.INC/x$AREA.SUIT.DEC
tiff(paste(imgdir,"/pia_nia_ratio-boxplot.tif",sep=""),res=300,pointsize=10,width=800,
     height=1000,units="px",compression="lzw")
par(mar=c(6,4,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(x$PIA_NIA_RATIO~x$Region,col="grey",boxwex=0.5,
        pch=20,outwex=0.2,horizontal=T,ylim=c(0,10),
        xlab="Positively to negatively impacted area ratio")
abline(v=0,lwd=0.7,lty=2)
abline(v=seq(0,10,by=2),lwd=0.6,lty=2,col="grey50")
dev.off()


#           percent change in suitable area
tiff(paste(imgdir,"/chg_area-boxplot.tif",sep=""),res=300,pointsize=10,width=800,
     height=1000,units="px",compression="lzw")
par(mar=c(6,4,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(x$CHG.AREA~x$Region,col="grey",boxwex=0.5,
        pch=20,outwex=0.2,horizontal=T,ylim=c(-100,100),
        xlab="Change in area suitable (%)")
abline(v=0,lwd=0.7,lty=2)
abline(v=seq(-100,100,by=25),lwd=0.6,lty=2,col="grey50")
dev.off()



