#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#summarise and make charts of skill of randomised optimisation runs

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
maxiter <- 15
version <- "c"
selection <- "v6"
base_exp <- 33 #change if you have done any other experiment

#run <- 1
#expID <- "10"

#eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
# maxiter <- 10
# version <- "d"
# selection <- "v4"
# base_exp <- 31 #change if you have done any other experiment

source(paste(src.dir,"/glam/glam-randomiser-functions.R",sep=""))

cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(NA,sample(1:9999,49))
#seeds <- c(NA)

expIDs <- c(base_exp:((base_exp-1)+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)

#output summary folders
sum_out <- paste(cropDir,"/calib/results_exp",sep="")
set_odir <- paste(sum_out,"/summary_exp_",runs_ref$EXPID[1],"-",runs_ref$EXPID[nrow(runs_ref)],sep="")


#here loop through zones
for (z in 1:5) {
  cat("summarising zone",z,"...\n")
  z_odir <- paste(set_odir,"/z",z,"_rfd_irr",sep="")
  
  #create output folder
  sum_spatial <- paste(z_odir,"/spatial",sep="")
  if (!file.exists(sum_spatial)) {dir.create(sum_spatial,recursive=T)}
  
  ### get summaries and do some plots
  if (!file.exists(paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""))) {
    skill <- data.frame()
    scount <- 1
    for (seed in seeds) {
      #seed <- seeds[2]
      if (is.na(seed)) {
        idexp <- runs_ref$EXPID[which(is.na(runs_ref$SEED))][1]
      } else {
        idexp <- runs_ref$EXPID[which(runs_ref$SEED == seed)][1]
      }
      
      cal_dir <- paste(cropDir,"/calib/exp-",idexp,"_outputs",sep="")
      
      #load calib_all_cells.csv
      cal_data <- read.csv(paste(cal_dir,"/general/calib_all_cells.csv",sep=""))
      gcl_data <- read.csv(paste(cropDir,"/inputs/calib-cells-selection-v6.csv",sep=""))
      cal_data$ZONE <- gcl_data$ZONE
      cal_data <- cal_data[cal_data$ZONE==z,]
      
      r_ym <- cor.test(cal_data$Y_OBS,cal_data$Y_PRED,na.rm=T)$estimate
      p_ym <- cor.test(cal_data$Y_OBS,cal_data$Y_PRED,na.rm=T)$p.value
      
      r_ys <- cor.test(cal_data$YSD_OBS,cal_data$YSD_PRED,na.rm=T)$estimate
      p_ys <- cor.test(cal_data$YSD_OBS,cal_data$YSD_PRED,na.rm=T)$p.value
      
      odf <- data.frame(EXPID=idexp,CCOEF_Y=r_ym,PVAL_Y=p_ym,CCOEF_YSD=r_ys,PVAL_YSD=p_ys)
      skill <- rbind(skill,odf)
      
      #making a data.frame from which the best parameter set can be further
      #calculated and a k-s or whatever test performed
      ccoef_mx <- data.frame(CELL=cal_data$CELL,X=cal_data$X,Y=cal_data$Y,METRIC="CCOEF",
                             VALUE=cal_data$CCOEF)
      names(ccoef_mx)[ncol(ccoef_mx)] <- paste("EXP.",idexp,sep="")
      rmse_mx <- data.frame(CELL=cal_data$CELL,X=cal_data$X,Y=cal_data$Y,METRIC="RMSE",
                            VALUE=cal_data$RMSE)
      names(rmse_mx)[ncol(rmse_mx)] <- paste("EXP.",idexp,sep="")
      prmse_mx <- data.frame(CELL=cal_data$CELL,X=cal_data$X,Y=cal_data$Y,METRIC="P_RMSE",
                            VALUE=cal_data$P_RMSE)
      names(prmse_mx)[ncol(prmse_mx)] <- paste("EXP.",idexp,sep="")
      
      skill_res <- rbind(ccoef_mx,rmse_mx,prmse_mx)
      
      if (scount==1) {
        skill_all <- skill_res
      } else {
        skill_all <- merge(skill_all,skill_res,by=intersect(names(skill_all), names(skill_res)),sort=F)
      }
      
      #produce density plots of each skill parameter
      dp <- density(cal_data$YGP,na.rm=T)
      dp$y <- dp$y/max(dp$y)
      if (scount==1) {
        tiff(paste(sum_spatial,"/ygp.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
        par(mar=c(5,5,1,1))
        plot(dp$x,dp$y,ty="l",xlab="YGP",ylab="Normalised PDF",col="grey 60",lwd=0.75)
        dev.next()
      } else {
        dev.set(which=2)
        lines(dp$x,dp$y,col="grey 60",lwd=0.75)
      }
      
      dp <- density(cal_data$CCOEF,na.rm=T)
      dp$y <- dp$y/max(dp$y)
      if (scount==1) {
        tiff(paste(sum_spatial,"/ccoef.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
        par(mar=c(5,5,1,1))
        plot(dp$x,dp$y,ty="l",xlab="CCOEF",ylab="Normalised PDF",col="grey 60",lwd=0.75)
        dev.next()
      } else {
        dev.set(which=3)
        lines(dp$x,dp$y,col="grey 60",lwd=0.75)
      }
      
      dp <- density(cal_data$RMSE,na.rm=T)
      dp$y <- dp$y/max(dp$y)
      if (scount==1) {
        tiff(paste(sum_spatial,"/rmse.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
        par(mar=c(5,5,1,1))
        plot(dp$x,dp$y,ty="l",xlab="RMSE (kg/ha)",ylab="Normalised PDF",col="grey 60",lwd=0.75)
        dev.next()
      } else {
        dev.set(which=4)
        lines(dp$x,dp$y,col="grey 60",lwd=0.75)
      }
      
      dp <- density(cal_data$P_RMSE,na.rm=T)
      dp$y <- dp$y/max(dp$y)
      if (scount==1) {
        tiff(paste(sum_spatial,"/prmse.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
        par(mar=c(5,5,1,1))
        plot(dp$x,dp$y,ty="l",xlab="RMSE (%)",ylab="Normalised PDF",col="grey 60",lwd=0.75)
        dev.next()
      } else {
        dev.set(which=5)
        lines(dp$x,dp$y,col="grey 60",lwd=0.75)
      }
      
      scount <- scount+1
    }
    
    for (gdev in 2:5) {
      dev.set(which=gdev)
      grid(lwd=0.5)
    }
    graphics.off()
    
    #plot the spread of randomised optimisation runs in a scattergram
    tiff(paste(sum_spatial,"/random_skill_xy.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
    par(mar=c(5,5,1,1))
    plot(skill$CCOEF_Y,skill$CCOEF_YSD,pch=20,cex=0.75,xlab="CCOEF (Mean yield)",ylab="CCOEF (SD yield)")
    grid(lwd=0.75)
    dev.off()
    
    dp <- density(skill$CCOEF_YSD,na.rm=T)
    dp$y <- dp$y/max(dp$y)
    tiff(paste(sum_spatial,"/random_skill_pdf-sd.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
    par(mar=c(5,5,1,1))
    plot(dp$x,dp$y,lwd=1,lty=1,ty="l",xlab="CCOEF (spatial)",ylab="Normalised PDF",xlim=c(min(dp$x),max(dp$x)))
    grid(lwd=0.5)
    dev.off()
    
    dp <- density(skill$CCOEF_Y,na.rm=T)
    dp$y <- dp$y/max(dp$y)
    tiff(paste(sum_spatial,"/random_skill_pdf-mean.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
    par(mar=c(5,5,1,1))
    plot(dp$x,dp$y,lwd=1,lty=1,ty="l",xlab="CCOEF (spatial)",ylab="Normalised PDF",xlim=c(min(dp$x),max(dp$x)))
    grid(lwd=0.5)
    dev.off()
    
    write.csv(skill,paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""),quote=T,row.names=F)
    write.csv(skill_all,paste(sum_spatial,"/skill_spatial.csv",sep=""),quote=T,row.names=F)
  } else {
    skill <- read.csv(paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""))
    skill_all <- read.csv(paste(sum_spatial,"/skill_spatial.csv",sep=""))
  }
}


