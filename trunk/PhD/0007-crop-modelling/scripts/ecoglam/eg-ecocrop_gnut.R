#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#### LIBRARIES: raster, maptools, rgdal, sp
library(raster); library(rgdal); library(maptools)
data(wrld_simpl)
stop("not to run yet")

#sources dir
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop" #local
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/EcoCrop" #eljefe
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir3 <- "~/Phd-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

source(paste(src.dir3,"/scripts/ecoglam/eg-ecocrop_gnut-functions.R",sep=""))
source(paste(src.dir,"/src/getUniqueCoord.R",sep=""))
source(paste(src.dir,"./src/randomSplit.R",sep=""))
source(paste(src.dir,"/src/extractClimateData.R",sep=""))
source(paste(src.dir,"/src/calibrationParameters.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/src/getParameters.R",sep=""))
source(paste(src.dir,"/src/EcoCrop.R",sep=""))
source(paste(src.dir,"/src/validation.R",sep=""))
source(paste(src.dir,"/src/createMask.R",sep=""))
source(paste(src.dir,"/src/accuracy.R",sep=""))


#basic information
crop_name <- "gnut"
r_dir <- "W:/eejarv/PhD-work/crop-modelling"
b_dir <- paste(r_dir,"/GLAM",sep="")
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")
ec_dir <- paste(crop_dir,"/ecg_analyses/ecocrop-",tolower(crop_name),sep="")

######################################################################
#Getting unique coordinates
rs <- read.csv(paste(ec_dir,"/analyses/data/gnut-india.csv",sep="")) #load the data
rs <- getUniqueCoord(rs, fields=c(24,23), resol=2.5/60) #running the function


######################################################################
#Dummy part of unloading and re-loading the data
write.csv(rs, paste(ec_dir,"/analyses/data/dataset2_5m.csv",sep=""), row.names=F, quote=T) #write outcome into new file
rs <- rs[which(rs$IS_UNIQUE == TRUE),] #selecting unique records
write.csv(rs, paste(ec_dir,"/analyses/data/unique2_5m.csv",sep=""), row.names=F, quote=T) #write new dataset only containing unique records
rs <- read.csv(paste(ec_dir,"/analyses/data/unique2_5m.csv",sep="")) #re-loading the data


######################################################################
#Splitting in test/train datasets
rs <- randomSplit(rs, 20) #20 is percentage of data to be taken out

#Plot & write test and train datasets separately
jpeg(paste(ec_dir,"/analyses/img/test_train.jpg",sep=""), quality=100, height=800, width=600)
plot(cbind(rs$longitude[which(rs$TEST_TRAIN == "TEST")], rs$latitude[which(rs$TEST_TRAIN == "TEST")]), 
     pch="+", col="red", xlim=c(min(rs$longitude),max(rs$longitude)), cex=1.5,
     ylim=c(min(rs$latitude),max(rs$latitude)), xlab="", ylab="")
plot(wrld_simpl, add=T)
points(cbind(rs$longitude[which(rs$TEST_TRAIN == "TRAIN")], 
             rs$latitude[which(rs$TEST_TRAIN == "TRAIN")]), pch=20, col="blue", cex=1.5)
dev.off()

#writing the test and training data
write.csv(rs, paste(ec_dir,"/analyses/data/unique.csv",sep=""), row.names=F, quote=T) #write unique records with new field TRAIN/TEST
write.csv(rs[which(rs$TEST_TRAIN == "TEST"),], paste(ec_dir,"/analyses/data/test.csv",sep=""), row.names=F, quote=T) #reselect and store test data
write.csv(rs[which(rs$TEST_TRAIN == "TRAIN"),], paste(ec_dir,"/analyses/data/train.csv",sep=""), row.names=F, quote=T) #reselect and store train data


######################################################################
#Extracting climate data
rs <- read.csv(paste(ec_dir,"/analyses/data/unique.csv",sep="")) #load unique records
for (v in c("prec", "tmin", "tmean", "tmax")) {
  rs <- extractMonthlyData(wd=paste(ec_dir,"/climate/ind_2_5min",sep=""), 
                           variable=v, ext=".tif", rs, fields=c(24,23), verbose=T)
}
write.csv(rs, paste(ec_dir,"/analyses/data/climates.csv",sep=""), row.names=F, quote=T)


######################################################################
#Calculating gs parameters for calibration
rs <- read.csv(paste(ec_dir,"/analyses/data/climates.csv",sep=""))

#here use Sacks et al. (2010) planting dates to get an approximation of when each
#of these points is sown. Use the average sowing and harvest date
pdate_dir <- paste(b_dir,"/climate-signals-yield/",toupper(crop_name),"/calendar/",tolower(crop_name),sep="")
pdate <- raster(paste(pdate_dir,"/plant.tif",sep=""))
xt <- extent(raster(paste(pdate_dir,"/plant_lr.tif",sep="")))
pdate <- crop(pdate,xt)

hdate <- raster(paste(pdate_dir,"/harvest.tif",sep=""))
hdate <- crop(hdate,xt)

#using these data select the growing season for each of the points
rs$SOW_DATE <- round(extract(pdate,cbind(x=rs$longitude,y=rs$latitude)),0)
rs$HAR_DATE <- round(extract(hdate,cbind(x=rs$longitude,y=rs$latitude)),0)

dg <- createDateGrid(2005)
dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
dg$DAY <- as.numeric(substr(dg$MTH.DAY,5,6))
dg$MTH.DAY <- NULL

rs$SOW_MTH <- round(sapply(rs$SOW_DATE,FUN=find_month,dg),0)
rs$HAR_MTH <- round(sapply(rs$HAR_DATE,FUN=find_month,dg),0)
rs$DUR <- NA
rs$DUR[which(rs$HAR_MTH < rs$SOW_MTH)] <- (365-rs$HAR_MTH[which(rs$HAR_MTH < rs$SOW_MTH)]) + rs$SOW_MTH[which(rs$HAR_MTH < rs$SOW_MTH)]
rs$DUR[which(rs$HAR_MTH > rs$SOW_MTH)] <- rs$HAR_MTH[which(rs$HAR_MTH > rs$SOW_MTH)] - rs$SOW_MTH[which(rs$HAR_MTH > rs$SOW_MTH)]

#produce growing season parameters based on sowing date and duration
rs_out <- lapply(1:nrow(rs),FUN=get_gs_data,rs)
rs_out <- do.call("rbind",rs_out)
write.csv(rs_out, paste(ec_dir,"/analyses/data/calibration.csv",sep=""), row.names=F, quote=T)


######################################################################
#Get calibration parameters
dataset <- read.csv(paste(ec_dir,"/analyses/data/calibration.csv",sep=""))
gs <- 1
plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
varList <- c("prec", "tmean", "tmin", "tmax")
v <- 1
for (varn in varList) {
  calPar <- getParameters(plotdata[,v], nb=200, plotit=T, 
                          plotdir=paste(ec_dir,"/analyses/img",sep=""), 
                          gs=gs, varname=varn)
  if (v == 1) {finalTable <- calPar} else {finalTable <- rbind(finalTable, calPar)}
  v <- v+1
}
write.csv(finalTable, paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""), row.names=F)


######################################################################
#Running the model
gs <- 1
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
vl <- c("tmean","tmin","tmax")
for (rw in 2:4) {
  if (!file.exists(paste(ec_dir,"/analyses/runs/", gs, "-",crop_name,"-",vl[rw-1],"-suitability.jpg",sep=""))) {
    eco <- suitCalc(climPath=paste(ec_dir,"/climate/ind_2_5min",sep=""), 
                    Gmin=120,Gmax=120,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],
                    Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                    Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                    outfolder=paste(ec_dir,"/analyses/runs",sep=""), 
                    cropname=paste(gs,"-",crop_name,"-",vl[rw-1],sep=""),ext=".tif")
    jpeg(paste(ec_dir,"/analyses/runs/", gs, "-",crop_name,"-",vl[rw-1],"-suitability.jpg",sep=""), quality=100)
    plot(eco)
    dev.off()
  }
}


######################################################################
#Assess accuracy of each growing season and each parameter tuning
test <- read.csv(paste(ec_dir,"/analyses/data/test.csv",sep=""))
test <- cbind(test[,"longitude"], test[,"latitude"])

train <- read.csv(paste(ec_dir,"/analyses/data/train.csv",sep=""))
train <- cbind(train[,"longitude"], train[,"latitude"])

parList <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
gsList <- unique(parList$GS)
for (gs in gsList) {
  pList <- parList[which(parList$GS==gs),][2:4,]
  for (vr in pList$VARIABLE) {
    for (suf in c("_p","_t","_")) {
      cat("GS:", gs, "- VAR:", vr, "- SUF:", suf, "\n")
      rs <- raster(paste(ec_dir,"/analyses/runs/", gs, "-",crop_name,"-", vr, suf, "suitability.asc", sep=""))
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
write.csv(rres, paste(ec_dir,"/analyses/data/accuracy-metrics.csv",sep=""), row.names=F)
write.csv(cres, paste(ec_dir,"/analyses/data/entropy-curves.csv",sep=""), row.names=F)

#plot accuracy metrics here
plot.acc <- rres[which(rres$TYPE=="_suitability"),]
tiff(paste(ec_dir,"/analyses/img/accuracy.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
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


######################################################################
#Validation stuff... got to validate 1(!),2(!),3(!),4(!),5(!),6(!),7(!),ME(!),MO,MX
rsl <- raster(paste(ec_dir,"/analyses/runs/1-",crop_name,"-tmean_suitability.asc",sep=""))
shp.icrisat <- paste(ec_dir,"/analyses/evaluation/IND2-gnut.shp",sep="")
oblist <- ls(pattern="shp")
for (ob in oblist) {
  cat("Processing", ob, "\n")
  shp <- readShapePoly(get(ob))
  field <- "H_ISPRES"
  res <- extractFromShape(shp, field, naValue=-9999, rsl)
  write.csv(res, paste(ec_dir,"/analyses/evaluation/", ob, ".csv", sep=""),row.names=F)
  res <- read.csv(paste(ec_dir,"/analyses/evaluation/", ob, ".csv", sep=""))
  mets <- valMetrics(res, pres.field=field,thresh=0)
  if (ob == oblist[1]) {
    rr <- cbind(SHP=ob, mets)
  } else {
    rr <- rbind(rr, cbind(SHP=ob, mets))
  }
  rm(res); rm(shp); rm(mets); gc()
}
write.csv(rr, paste(ec_dir,"/analyses/evaluation/rates.csv", sep=""),quote=F, row.names=F)


######################################################################
#aggregate the original evaluation data to desired resolution
dres <- 1 #desired resolution, in degree
fct <- round(dres/2.5*60,0) #aggregating factor

rsl <- raster(paste(ec_dir,"/analyses/runs/1-",crop_name,"-tmean_suitability.asc",sep=""))
shp <- readShapePoly(paste(ec_dir,"/analyses/evaluation/IND2-gnut.shp",sep=""))
naValue <- -9999

#Extract data from shapefile
shpData <- shp@data
pafield <- "H_ISPRES"

#create raster
rs <- raster(rsl)
pars <- raster:::.polygonsToRaster(shp,rs,field=pafield)
pars[which(pars[]==naValue)] <- NA

#aggregate raster
pars.agg <- aggregate(pars,fact=fct,fun=mean) #aggregate
pars.agg[which(pars.agg[]<0.75)] <- 0 #set anything below 25%
pars.agg[which(pars.agg[]>=0.75)] <- 1 #set anything below 25%

#plot the presence-absence surface
aspect <- (pars@extent@xmax-pars@extent@xmin)/(pars@extent@ymax-pars@extent@ymin)+0.15
tiff(paste(ec_dir,"/analyses/img/crop-presence-aggr.tiff",sep=""),
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(pars.agg,useRaster=F,
     col=c("red","green"),
     breaks=c(0,0.5,1),
     lab.breaks=c("Absent",NA,"Present"),
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=100)
#plot(wrld_simpl,add=T,lwd=0.6)
plot(shp,add=T,border="black")
grid()
dev.off()

#store the evaluation datasets
pars.agg <- writeRaster(pars.agg,paste(ec_dir,"/analyses/evaluation/pa_coarse.asc",sep=""),format="ascii")
pars <- writeRaster(pars,paste(ec_dir,"/analyses/evaluation/pa_fine.asc",sep=""),format="ascii")


######################################################################
#1. run & assess EcoCrop with IITM/CRU data, climatological means

#a. copy the data to a folder for further runs
iitm_dir <- paste(r_dir,"/climate-data/IND-TropMet_clm",sep="")
cru_dir <- paste(r_dir,"/climate-data/CRU_TS_v3-1_data",sep="")
clm_dir <- paste(ec_dir,"/climate/climatology",sep="")

clm_type <- "1966_1993"
oclm_dir <- copy_clim_data(clm_type=clm_type,
                           iitm_dir=paste(iitm_dir,"/climatology_",clm_type,sep=""),
                           cru_dir=paste(cru_dir,"/climatology_",clm_type,sep=""),
                           oclm_dir=clm_dir,cru_prefix=NA)

clm_type <- "1960_2000"
oclm_dir <- copy_clim_data(clm_type=clm_type,
                           iitm_dir=paste(iitm_dir,"/climatology_",clm_type,sep=""),
                           cru_dir=paste(cru_dir,"/climatology_",clm_type,sep=""),
                           oclm_dir=clm_dir,cru_prefix=NA)

#b. run EcoCrop with these two datasets
#run details (1966-1993 climatology)
gs <- 1
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
gs_type <- "tmean"
gs_loc <- which(p$VARIABLE == gs_type)

#model run
period <- "1966_1993"
eco <- suitCalc(climPath=paste(clm_dir,"/",period,sep=""), 
                Gmin=120,Gmax=120,Tkmp=p$KILL[gs_loc],Tmin=p$MIN[gs_loc],Topmin=p$OPMIN[gs_loc],
                Topmax=p$OPMAX[gs_loc],Tmax=p$MAX[gs_loc],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                outfolder=paste(ec_dir,"/analyses/runs_eg/clm_",period,sep=""), 
                cropname=paste(gs,"-",crop_name,"-",gs_type,sep=""),ext=".tif")

#model run, second period
period <- "1960_2000"
eco <- suitCalc(climPath=paste(clm_dir,"/",period,sep=""), 
                Gmin=120,Gmax=120,Tkmp=p$KILL[gs_loc],Tmin=p$MIN[gs_loc],Topmin=p$OPMIN[gs_loc],
                Topmax=p$OPMAX[gs_loc],Tmax=p$MAX[gs_loc],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                outfolder=paste(ec_dir,"/analyses/runs_eg/clm_",period,sep=""), 
                cropname=paste(gs,"-",crop_name,"-",gs_type,sep=""),ext=".tif")

#c. assess EcoCrop using the presence-absence surface (low resolution)
pa_rs <- raster(paste(ec_dir,"/analyses/evaluation/pa_coarse.asc",sep=""))
pred_p1 <- raster(paste(ec_dir,"/analyses/runs_eg/clm_1966_1993/",gs,"-",crop_name,"-",gs_type,"_suitability.asc",sep=""))
pred_p2 <- raster(paste(ec_dir,"/analyses/runs_eg/clm_1960_2000/",gs,"-",crop_name,"-",gs_type,"_suitability.asc",sep=""))

met_p1 <- eval_ecocrop(rsl=pred_p1,eval_rs=pa_rs)
met_p1 <- cbind(PERIOD="1966_1993",met_p1)

met_p2 <- eval_ecocrop(rsl=pred_p2,eval_rs=pa_rs)
met_p2 <- cbind(PERIOD="1960_2000",met_p2)

met <- rbind(met_p1,met_p2)

#write evaluation metrics
omet_dir <- paste(ec_dir,"/analyses/runs_eg/evaluation",sep="")
if (!file.exists(omet_dir)) {dir.create(omet_dir,recursive=T)}
write.csv(met,paste(omet_dir,"/clm_metrics.csv",sep=""),quote=T,row.names=F)


######################################################################
#2. run historical simulations of EcoCrop (1966-1993)
yr <- 1966
iitm_dir <- paste(r_dir,"/climate-data/IND-TropMet_mon",sep="")
cru_dir <- paste(r_dir,"/climate-data/CRU_TS_v3-1_data/monthly_grids",sep="")
clm_dir <- paste(ec_dir,"/climate/yearly",sep="")

oclm_dir <- copy_clim_data("1966_1993",iitm_dir,cru_dir,oclm_dir=clm_dir)
oclm_dir <- copy_clim_data("1960_2000",iitm_dir,cru_dir,oclm_dir=clm_dir)

for (i in 1:12) {
  
  
  
}

#use yearly harv. area data to assess EcoCrop, huh?



######################################################################
#3. start some exploratory data analysis with GLAM results





