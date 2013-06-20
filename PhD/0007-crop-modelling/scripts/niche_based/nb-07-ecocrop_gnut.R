#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("not to run yet")

#### LIBRARIES: raster, maptools, rgdal, sp
library(raster); library(rgdal); library(maptools); library(MASS)
library(sfsmisc); library(dismo)
data(wrld_simpl)

#sources dir
src.dir1 <- "~/Repositories/dapa-climate-change/trunk/EcoCrop" #local
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir3 <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/EcoCrop" #eljefe
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir3 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#source the model and other functions
source(paste(src.dir3,"/scripts/niche_based/EcoCrop-model.R",sep=""))
source(paste(src.dir3,"/scripts/niche_based/nb-07-ecocrop_gnut-fun.R",sep=""))
source(paste(src.dir1,"/src/randomSplit.R",sep=""))
source(paste(src.dir1,"/src/extractClimateData.R",sep=""))
source(paste(src.dir1,"/src/accuracy.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#basic information
crop_name <- "gnut"

#i/o directories
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
occDir <- paste(bDir,"/occurrences",sep="")
calDir <- paste(bDir,"/calendar",sep="")
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
modDir <- paste(bDir,"/models",sep="")
ecoDir <- paste(modDir,"/EcoCrop",sep="")

dataDir <- paste(ecoDir,"/data",sep="")
imgDir <- paste(ecoDir,"/img",sep="")
runDir <- paste(ecoDir,"/runs",sep="")
pdfDir <- paste(imgDir,"/pdf_runs",sep="")
mskDir <- paste(envDir,"/mask",sep="")
if (!file.exists(dataDir)) {dir.create(dataDir)}
if (!file.exists(imgDir)) {dir.create(imgDir)}
if (!file.exists(runDir)) {dir.create(runDir)}
if (!file.exists(pdfDir)) {dir.create(pdfDir)}
if (!file.exists(mskDir)) {dir.create(mskDir)}


#here load Sacks et al. (2010) planting dates to get an approximation of when each
#of these points is sown. Use the average sowing and harvest date
#JRV changed to Jones and Thornton methodology
pdate <- raster(paste(calDir,"/plant_doy_ind_jt.tif",sep=""))
hdate <- raster(paste(calDir,"/harvest_doy_ind_jt.tif",sep=""))


######################################################################
#Getting unique coordinates
loc_data <- read.csv(paste(occDir,"/gnut-india_final.csv",sep="")) #load the data

#load mask
msk <- raster(paste(clmDir,"/wcl_ind_30s/prec_1.tif",sep=""))
msk[which(!is.na(msk[]))] <- which(!is.na(msk[]))

#labelling unique or not
loc_data$CELL <- extract(msk,loc_data[,c("LON","LAT")])
loc_data$IS_UNIQUE <- !duplicated(loc_data$CELL)

#selecting unique records
loc_unique <- loc_data[which(loc_data$IS_UNIQUE),] 
rownames(loc_unique) <- 1:nrow(loc_unique)

#write data with labels of whether unique or not
write.csv(loc_data, paste(dataDir,"/dataset_30s.csv",sep=""), row.names=F, quote=T) #write outcome into new file
write.csv(loc_unique, paste(dataDir,"/unique_30s.csv",sep=""), row.names=F, quote=T) #write new dataset only containing unique records


######################################################################
#Splitting in test/train datasets
loc_split <- randomSplit(loc_unique, 20, seed=3819) #20 is percentage of data to be taken out

#Plot & write test and train datasets separately
jpeg(paste(imgDir,"/test_train.jpg",sep=""), quality=100, height=1024, width=900)
plot(cbind(loc_split$LON[which(loc_split$TEST_TRAIN == "TEST")], loc_split$LAT[which(loc_split$TEST_TRAIN == "TEST")]), 
     pch="+", col="red", xlim=c(min(loc_split$LON),max(loc_split$LON)), cex=2.5,
     ylim=c(min(loc_split$LAT),max(loc_split$LAT)), xlab="", ylab="")
plot(wrld_simpl, add=T)
points(cbind(loc_split$LON[which(loc_split$TEST_TRAIN == "TRAIN")], 
             loc_split$LAT[which(loc_split$TEST_TRAIN == "TRAIN")]), 
       pch=20, col="blue", cex=2.5)
points(cbind(loc_split$LON[which(loc_split$TEST_TRAIN == "TEST")], 
             loc_split$LAT[which(loc_split$TEST_TRAIN == "TEST")]), 
       pch="+", col="red", cex=2.5)
grid()
dev.off()

#writing the test and training data
write.csv(loc_split, paste(dataDir,"/splitted.csv",sep=""), row.names=F, quote=T) #write unique records with new field TRAIN/TEST
write.csv(loc_split[which(loc_split$TEST_TRAIN == "TEST"),], paste(dataDir,"/test.csv",sep=""), row.names=F, quote=T) #reselect and store test data
write.csv(loc_split[which(loc_split$TEST_TRAIN == "TRAIN"),], paste(dataDir,"/train.csv",sep=""), row.names=F, quote=T) #reselect and store train data


######################################################################
#Extracting climate data
loc_split <- read.csv(paste(dataDir,"/splitted.csv",sep="")) #load unique records
loc_clim <- loc_split
for (v in c("prec", "tmin", "tmean", "tmax")) {
  loc_clim <- extractMonthlyData(wd=paste(clmDir,"/wcl_ind_30s",sep=""), 
                           variable=v, ext=".tif", loc_clim, fields=c(2,3), verbose=T)
}
write.csv(loc_clim, paste(dataDir,"/climates.csv",sep=""), row.names=F, quote=T)


######################################################################
#Calculating gs parameters for calibration
loc_clim <- read.csv(paste(dataDir,"/climates.csv",sep=""))

#using these data select the growing season for each of the points
loc_clim$SOW_DATE <- round(extract(pdate,cbind(x=loc_clim$LON,y=loc_clim$LAT)),0)
loc_clim$HAR_DATE <- round(extract(hdate,cbind(x=loc_clim$LON,y=loc_clim$LAT)),0)

#remove those points that do not have sow date or harvest date
loc_clim <- loc_clim[which(!is.na(loc_clim$SOW_DATE)),]
loc_clim <- loc_clim[which(!is.na(loc_clim$HAR_DATE)),]
rownames(loc_clim) <- 1:nrow(loc_clim)

#alternatively, set the default JJAS period
#loc_clim$SOW_DATE[which(is.na(loc_clim$SOW_DATE) & is.na(loc_clim$HAR_DATE))] <- 152
#loc_clim$HAR_DATE[which(is.na(loc_clim$HAR_DATE))] <- loc_clim$SOW_DATE[which(is.na(loc_clim$HAR_DATE))]+122
#loc_clim$HAR_DATE[which(loc_clim$HAR_DATE > 365)] <- loc_clim$HAR_DATE[which(loc_clim$HAR_DATE > 365)] - 365

dg <- createDateGrid(2005)
dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
dg$DAY <- as.numeric(substr(dg$MTH.DAY,5,6))
dg$MTH.DAY <- NULL

loc_clim$SOW_MTH <- round(sapply(loc_clim$SOW_DATE,FUN=find_month,dg),0)
loc_clim$HAR_MTH <- round(sapply(loc_clim$HAR_DATE,FUN=find_month,dg),0)
loc_clim$HAR_MTH[which(loc_clim$HAR_MTH > 12)] <- loc_clim$HAR_MTH[which(loc_clim$HAR_MTH > 12)] - 12

loc_clim$DUR <- NA
loc_clim$DUR[which(loc_clim$HAR_MTH < loc_clim$SOW_MTH)] <- (365-loc_clim$HAR_MTH[which(loc_clim$HAR_MTH < loc_clim$SOW_MTH)]) + loc_clim$SOW_MTH[which(loc_clim$HAR_MTH < loc_clim$SOW_MTH)] + 1
loc_clim$DUR[which(loc_clim$HAR_MTH > loc_clim$SOW_MTH)] <- loc_clim$HAR_MTH[which(loc_clim$HAR_MTH > loc_clim$SOW_MTH)] - loc_clim$SOW_MTH[which(loc_clim$HAR_MTH > loc_clim$SOW_MTH)]

#produce growing season parameters based on sowing date and duration
loc_calib <- lapply(1:nrow(loc_clim),FUN=get_gs_data,loc_clim)
loc_calib <- do.call("rbind",loc_calib)
write.csv(loc_calib, paste(dataDir,"/calib_input_clim.csv",sep=""), row.names=F, quote=T)


######################################################################
#Load dataset for model calibration
dataset <- read.csv(paste(dataDir,"/calib_input_clim.csv",sep=""))
dataset <- dataset[which(dataset$TEST_TRAIN == "TRAIN"),]
dataset <- dataset[,grep("GS_",names(dataset))]


######################################################################
#Plot pdfs of all temperatures together for comparison / reference
col.sym <- expand.grid(COL=c("black","blue","red"),SYM=c(1,2,3))
for (vid in 2:length(names(dataset))) {
  #vid <- 2
  vn <- names(dataset)[vid]
  tpdf <- density(dataset[,vn])
  if (vid == 2) {
    jpeg(paste(imgDir, "/pdf_temperature.jpg", sep=""), 
         quality=100, height=1024, width=1024,pointsize=7,res=300)
    par(mar=c(4,4,1,1))
    plot(tpdf,main=NA,xlab="Seasonal temperatures (Celsius)",col=paste(col.sym$COL)[vid-1],
         lty=col.sym$SYM[vid-1],xlim=c(100,400),ylim=c(0,0.03))
    grid()
  } else {
    lines(tpdf,col=paste(col.sym$COL)[vid-1],lty=col.sym$SYM[vid-1])
  }
}
dev.off()

#plot of all temperatures
all_temp <- as.numeric(as.matrix(dataset[,c("GS_TN","GS_TX","GS_XN","GS_XX","GS_NN","GS_NX")]))
tpdf <- density(all_temp)
jpeg(paste(imgDir, "/pdf_temperature_all.jpg", sep=""), 
     quality=100, height=1024, width=1024,pointsize=7,res=300)
par(mar=c(4,4,1,1))
plot(tpdf,main=NA,xlab="Seasonal temperatures (Celsius)",col="black",
     lty=1)
grid()
dev.off()


######################################################################
#creating a perturbed parameter ensemble that will be assessed
#parameters (that can be perturbed)
#carry out 25 perturbations
min_list <- max_list <- c(0.9,0.925,0.95,0.975,0.99,1)
opmin_list <- opmax_list <- c(0.4,0.45,0.5,0.55,0.6)
runs <- expand.grid(ABS=min_list,OPT=opmin_list)

#precipitation and temperature vectors (only one temperature vector is allowed)
all_temp <- as.numeric(as.matrix(dataset[,c("GS_TN","GS_TX","GS_XN","GS_XX","GS_NN","GS_NX")]))
all_prec <- dataset$GS_P

#create parameter sets
all_pset <- data.frame()
for (ri in 1:nrow(runs)) {
  #ri <- 1
  cat("making parameter set for run:",ri,"\n")
  tpar <- list(kill=1,min=runs$ABS[ri],opmin=runs$OPT[ri],opmax=runs$OPT[ri],max=runs$ABS[ri])
  pr_par <- getParameters(all_prec, tpar, stat="mode", plotit=T, plotdir=pdfDir, 
                          xlabel="Seasonal precipitation",
                          filename=paste("calib_pdf_prec_run-",ri,".jpg",sep=""))
  pr_par <- cbind(RUN=ri,VARIABLE="PREC",pr_par)
  
  tm_par <- getParameters(all_temp, tpar, stat="mean", plotit=T, plotdir=pdfDir, 
                          xlabel="Seasonal mean temperature",
                          filename=paste("calib_pdf_tmean_run-",ri,".jpg",sep=""))
  tm_par <- cbind(RUN=ri,VARIABLE="TMEAN",tm_par)
  
  all_pset <- rbind(all_pset,pr_par,tm_par)
}
write.csv(all_pset, paste(dataDir,"/parameter_sets.csv",sep=""), row.names=F)


######################################################################
#Running the model

#loading parameter sets
p <- read.csv(paste(dataDir,"/parameter_sets.csv",sep=""))

#looping through parameter sets
for (rw in 1:(nrow(p)/2)) {
  #rw <- 1
  cat("\nrunning model (run: ",rw,")\n",sep="")
  tpset <- p[which(p$RUN == rw),]
  
  #output directory
  outf <- paste(runDir,"/run_",rw,sep="")
  
  #parameter values
  tkill <- tpset$KILL[2] - 40
  tmin <- tpset$MIN[2]; tmax <- tpset$MAX[2]
  topmin <- tpset$OPMIN[2]; topmax <- tpset$OPMAX[2]
  rmin <- tpset$MIN[1]; rmax <- tpset$MAX[1]
  ropmin <- tpset$OPMIN[1]; ropmax <- tpset$OPMAX[1]
  
  if (!file.exists(paste(outf,"/out_suit.png",sep=""))) {
    #run the model
    eco <- suitCalc(climPath=paste(clmDir,"/wcl_ind_2_5min",sep=""), 
                    sowDat=pdate@file@name,
                    harDat=hdate@file@name,
                    Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                    Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                    Ropmax=ropmax,Rmax=rmax, 
                    outfolder=outf,
                    cropname=crop_name,ext=".tif",cropClimate=F)
    
    #plot the results
    png(paste(outf,"/out_psuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
    par(mar=c(3,3,1,2))
    rsx <- eco[[1]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
    plot(wrld_simpl,add=T)
    grid(lwd=1.5)
    dev.off()
    
    png(paste(outf,"/out_tsuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
    par(mar=c(3,3,1,2))
    rsx <- eco[[2]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
    plot(wrld_simpl,add=T)
    grid(lwd=1.5)
    dev.off()
    
    png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
    par(mar=c(3,3,1,2))
    rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
    plot(wrld_simpl,add=T)
    grid(lwd=1.5)
    dev.off()
    
    rm(eco); gc(T)
  }
}



######################################################################
#Assess accuracy of each parameter set
test <- read.csv(paste(dataDir,"/test.csv",sep=""))
test <- cbind(test[,"LON"], test[,"LAT"])

train <- read.csv(paste(dataDir,"/train.csv",sep=""))
train <- cbind(train[,"LON"], train[,"LAT"])

parList <- read.csv(paste(dataDir,"/parameter_sets.csv",sep=""))
rwList <- unique(parList$RUN)
for (rw in rwList[1:26]) {
  #rw <- rwList[1]
  #pList <- parList[which(parList$RUN==rw),]
  for (suf in c("_p","_t","_")) {
    #suf <- "_p"
    cat("RUN:", rw, "- SUF:", suf, "\n")
    s_rs <- raster(paste(runDir,"/run_",rw,"/", crop_name, suf, "suitability.tif", sep=""))
    tem <- accMetrics(s_rs, test) #doing with test data
    trm <- accMetrics(s_rs, train) #doing with training data
    
    #make data frame with this raster's results
    #removed the entropy stuff, as it seemed useless (JRV)
    resrow <- data.frame(RUN=rw, TYPE=paste(suf, "suitability", sep=""), 
                         TEST.AV.SUIT=tem$METRICS$SUIT, TEST.SD.SUIT=tem$METRICS$SUITSD, 
                         TEST.MAX.SUIT=tem$METRICS$SUITX, TEST.MIN.SUIT=tem$METRICS$SUITN, 
                         TEST.OMISSION.RATE=tem$METRICS$OMISSION_RATE, 
                         TEST.ERROR=tem$METRICS$RMSQE, TEST.ERR.DIST=tem$METRICS$ERR_DIST,
                         TRAIN.AV.SUIT=trm$METRICS$SUIT, TRAIN.SD.SUIT=trm$METRICS$SUITSD,
                         TRAIN.MAX.SUIT=trm$METRICS$SUITX, TRAIN.MIN.SUIT=trm$METRICS$SUITN, 
                         TRAIN.OMISSION.RATE=trm$METRICS$OMISSION_RATE, 
                         TRAIN.ERROR=trm$METRICS$RMSQE, TRAIN.ERR.DIST=trm$METRICS$ERR_DIST) 
    
    #append results to single data frame
    if (rw == rwList[1] & suf == "_p") {
      rres <- resrow
    } else {
      rres <- rbind(rres, resrow)
    }
  }
}
#write result
write.csv(rres, paste(dataDir,"/skill.csv",sep=""), row.names=F)

#plot accuracy metrics here
plot.acc <- rres[which(rres$TYPE=="_suitability"),]
tiff(paste(imgDir,"/skill.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(4.5,4,1,1),cex=1)
plot(plot.acc$TEST.OMISSION.RATE,plot.acc$TEST.ERROR,
     xlim=c(0,1),ylim=c(0,1),pch=21,xlab="OR",ylab="RMSE")
abline(h=0.5,lwd=1.2,lty=2)
abline(v=0.1,lwd=1.2,lty=2)
grid()
dev.off()


######################################################################
#Assess accuracy of each parameter set using AUC
#draw some pseudo absences from the whole analysis domain
if (!file.exists(paste(mskDir,"/mask_30s.tif",sep=""))) {
  ind_msk <- readShapePoly("/nfs/a102/eejarv/CMIP5/assessment/input-data/adm-data/SAS/IND_adm/IND3.shp")
  ind_msk <- rasterize(ind_msk,msk,field=1)
  writeRaster(ind_msk,paste(mskDir,"/mask_30s.tif",sep=""),format="GTiff")
} else {
  ind_msk <- raster(paste(mskDir,"/mask_30s.tif",sep=""))
}

ind_xy <- as.data.frame(xyFromCell(ind_msk,which(!is.na(ind_msk[]))))
npa <- 10000
set.seed(1234)
pab <- as.matrix(ind_xy[sample(1:nrow(ind_xy),npa),])
rownames(pab) <- 1:nrow(pab)

#looping the model outputs
for (rw in rwList) {
  #rw <- rwList[1]
  #pList <- parList[which(parList$RUN==rw),]
  for (suf in c("_p","_t","_")) {
    #suf <- "_"
    cat("RUN:", rw, "- SUF:", suf, "\n")
    s_rs <- raster(paste(runDir,"/run_",rw,"/", crop_name, suf, "suitability.tif", sep=""))
    
    #calculating auc
    auc <- eco_roc_eval(s_rs,test,train,pab)
    auc <- cbind(RUN=rw, TYPE=paste(suf, "suitability", sep=""),auc)
    
    #append results to single data frame
    if (rw == rwList[1] & suf == "_p") {
      all_auc <- auc
    } else {
      all_auc <- rbind(all_auc, auc)
    }
  }
}
write.csv(all_auc, paste(dataDir,"/auc_evaluation.csv",sep=""), row.names=F)







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
#run details (parameters)
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
######################################################################
#2. run historical simulations of EcoCrop (1966-1993)
iitm_dir <- paste(r_dir,"/climate-data/IND-TropMet_mon",sep="")
cru_dir <- paste(r_dir,"/climate-data/CRU_TS_v3-1_data",sep="")
clm_dir <- paste(ec_dir,"/climate/yearly",sep="")

#loop years to produce the datasets for running the model
for (yr in 1966:1993) {
  cat("processing year",yr,"\n")
  oclim_dir <- copy_clim_data(clm_type=yr,
                              iitm_dir=paste(iitm_dir,"/",yr,sep=""),
                              cru_dir=paste(cru_dir,"/monthly_grids",sep=""),
                              oclm_dir=clm_dir,cru_prefix=yr)
}


#run the model using each year's climate data
#run details (1966-1993 climatology)
gs <- 1
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
gs_type <- "tmean"
gs_loc <- which(p$VARIABLE == gs_type)

#loop through years and calculate suitability
met_all <- data.frame()
for (yr in 1966:1993) {
  cat("processing year",yr,"\n")
  eco <- suitCalc(climPath=paste(clm_dir,"/",yr,sep=""), 
                  Gmin=120,Gmax=120,Tkmp=p$KILL[gs_loc],Tmin=p$MIN[gs_loc],Topmin=p$OPMIN[gs_loc],
                  Topmax=p$OPMAX[gs_loc],Tmax=p$MAX[gs_loc],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                  Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                  outfolder=paste(ec_dir,"/analyses/runs_eg/yearly/",yr,sep=""), 
                  cropname=paste(gs,"-",crop_name,"-",gs_type,sep=""),ext=".tif")
  
  #use yearly harv. area data to assess EcoCrop
  aha_dir <- paste(crop_dir,"/harvested_area/raster/gridded",sep="")
  aha_rs <- raster(paste(aha_dir,"/raw-",yr,".asc",sep=""))
  aha_rs[which(aha_rs[] > 0)] <- 1
  
  met_yr <- eval_ecocrop(rsl=eco[[5]],eval_rs=aha_rs)
  met_yr <- cbind(YEAR=yr,met_yr)
  met_all <- rbind(met_all,met_yr)
}
write.csv(met_all,paste(omet_dir,"/yearly_metrics.csv",sep=""),quote=T,row.names=F)

#plot accuracy metrics here
#true positive rate
tiff(paste(ec_dir,"/analyses/img/historical_TPR.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=1)
plot(met_all$YEAR,met_all$TPR,ylim=c(0.75,1),pch=20,ty="l",xlab=NA,ylab="TPR")
abline(h=met$TPR[1],lwd=1.2,lty=2,col="red")
grid()
dev.off()

#false positive rate
tiff(paste(ec_dir,"/analyses/img/historical_FPR.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=1)
plot(met_all$YEAR,met_all$FPR,ylim=c(0,0.3),pch=20,ty="l",xlab=NA,ylab="FPR")
abline(h=met$FPR[1],lwd=1.2,lty=2,col="red")
grid()
dev.off()

#true negative rate
tiff(paste(ec_dir,"/analyses/img/historical_TNR.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=1)
plot(met_all$YEAR,met_all$TNR,ylim=c(0.5,0.90),pch=20,ty="l",xlab=NA,ylab="TNR")
abline(h=met$TNR[1],lwd=1.2,lty=2,col="red")
grid()
dev.off()



