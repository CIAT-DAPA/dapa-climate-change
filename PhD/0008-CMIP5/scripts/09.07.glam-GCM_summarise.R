#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

library(raster)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#cmipDir <- "V:/eejarv/CMIP5"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "/nfs/a102/eejarv/CMIP5"

#sourcing functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))

#input directories and model
cropName <- "gnut"
runs_set <- "gcm_runs"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")
runs_odir <- paste(glam_dir,"/model-runs/gcm_runs",sep="")


#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
procList <- data.frame(GCM=gcmList)


#load cell details
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#get the mask needed (to which data will be appended)
ncFile <- paste(bDir,"/../climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
ydDir <- paste(bDir,"/climate-signals-yield/GNUT/raster/gridded",sep="")

metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)
msk[] <- NA

#method of yield detrending
method <- "lin"

#load irrigation rates
irDir <- paste(cDir,"/irrigated_ratio",sep="")
ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))


###select process
#this_proc <- 1

### wrapper function to summarise the YGP optimisation
wrapper_summarise_GCM_cal <- function(this_proc) {
  
  #sourcing functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir,"/climateSignals-functions.R",sep=""))
  
  #get gcm and ensemble member names
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  cal_dir <- paste(runs_odir,"/",gcm,"_",ens,sep="")
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",gcm,"-",ens,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  #select cell
  #cell <- cells$CELL[1]
  if (!file.exists(paste(cal_dir,"/calib_all_cells.csv",sep=""))) {
    # loop through gridcells
    for (cell in cells$CELL) {
      cat("\nprocessing gridcell",paste(cell),"\n")
      #get the required metrics:
      #1. mean predicted yield (taken from best value of YGP)
      #2. mean standard deviation of yield (taken from best value of YGP)
      #3. yield gap parameter
      #4. R pearson and pvalue
      #5. RMSE
      #6. RMSE / mean obs. yield * 100
      #7. initial planting date
      
      run_dir <- paste(cal_dir,"/calib_",cell,sep="")
      if (!file.exists(run_dir)) {
        ygp <- NA
        r_val <- NA
        p_val <- NA
        rmse <- NA
        prmse <- NA
        yp_mean <- NA
        yp_stdv <- NA
        yo_mean <- NA
        yo_stdv <- NA
      } else {
        ygpDir <- paste(run_dir,"/iter-ygp",sep="")
        
        #get best ygp, and mean predicted yield from that run
        #you need to read irrigated and rainfed, and also iratios
        cat("getting optimal ygp\n")
        load(paste(ygpDir,"/output.RData",sep=""))
        ygp <- optimal$YGP
        opt_pos <- which(optimised$YGP$VALUE == ygp)
        
        #load rainfed yields
        cat("load rainfed yields\n")
        data_dir <- paste(run_dir,"/iter-ygp/ygp/RFD_run-",opt_pos,"_",ygp,"/output",sep="")
        outfile <- list.files(data_dir,pattern="\\.out")
        pred <- read.table(paste(data_dir,"/",outfile,sep=""),header=F,sep="\t")
        names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                         "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                         "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                         "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                         "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
        y_rfd <- pred$YIELD
        
        #load irrigated yields
        cat("load irrigated yields\n")
        data_dir <- paste(run_dir,"/iter-ygp/ygp/IRR_run-",opt_pos,"_",ygp,"/output",sep="")
        if (file.exists(data_dir)) {
          outfile <- list.files(data_dir,pattern="\\.out")
          pred <- read.table(paste(data_dir,"/",outfile,sep=""),header=F,sep="\t")
          names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                           "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                           "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                           "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                           "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
          y_irr <- pred$YIELD
        } else {
          y_irr <- rep(0,times=length(y_rfd))
        }
        
        #get irrigation ratio
        #extract irrigation rates
        cat("get irrigation rates\n")
        ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==cell)],Y=cells$Y[which(cells$CELL==cell)]))
        ir_vls <- as.numeric(ir_vls)
        ir_vls[which(ir_vls > 1)] <- 1
        
        #put all this into a data.frame
        cat("calculate 'true' predicted yield\n")
        y_pred <- data.frame(YEAR=1966:1993,RFD=y_rfd,IRR=y_irr,IRATIO=ir_vls)
        y_pred$PRED <- y_pred$RFD*(1-y_pred$IRATIO) + y_pred$IRR*y_pred$IRATIO
        
        #load observed yields
        cat("load observed yields\n")
        yfil <- paste(cDir,"/inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
        y_o <- read.fortran(yfil,format=c("A12","F8"),n=28)
        y_pred$OBS <- y_o$V2
        
        #get anything -99 to NA
        y_pred$OBS[which(y_pred$OBS < -90)] <- NA
        
        #remove all lines that are NA in OBS and calculate 'n' (number of observations)
        y_pred <- y_pred[which(!is.na(y_pred$OBS)),]
        n <- nrow(y_pred)
        
        #4. R pearson and pvalue
        if (n >= 2) {
          cat("calculate final metrics\n")
          r_val <- cor.test(y_pred$PRED,y_pred$OBS)$estimate
          p_val <- cor.test(y_pred$PRED,y_pred$OBS)$p.value
        } else {
          r_val <- NA
          p_val <- NA
        }
        
        #5. RMSE
        rmse <- sqrt(sum((y_pred$OBS-y_pred$PRED)^2) / nrow(y_pred))
        
        #6. RMSE / mean obs. yield * 100
        prmse <- rmse/mean(y_pred$OBS)*100
        
        #mean and standard deviation of predicted and observed yield
        yp_mean <- mean(y_pred$PRED)
        yp_stdv <- sd(y_pred$PRED)
        yo_mean <- mean(y_pred$OBS)
        yo_stdv <- sd(y_pred$OBS)
        
        #remove optimal and optimised
        rm(optimal); rm(optimised)
      }
      
      #output data frame
      out_row <- data.frame(CELL=cell,X=cells$X[which(cells$CELL == cell)],
                            Y=cells$Y[which(cells$CELL == cell)],YGP=ygp,Y_OBS=yo_mean,YSD_OBS=yo_stdv,
                            Y_PRED=yp_mean,YSD_PRED=yp_stdv,CCOEF=r_val,PVAL=p_val,RMSE=rmse,P_RMSE=prmse,
                            SOW_DATE=NA,N=n)
      
      if (cell == cells$CELL[1]) {
        out_all <- out_row
      } else {
        out_all <- rbind(out_all,out_row)
      }
    }
    write.csv(out_all,paste(cal_dir,"/calib_all_cells.csv",sep=""),quote=T,row.names=F)
  } else {
    out_all <- read.csv(paste(cal_dir,"/calib_all_cells.csv",sep=""))
  }
  
  #now create the rasters
  out_rs_dir <- paste(cal_dir,"/calib_results_spat",sep="")
  if (!file.exists(out_rs_dir)) {dir.create(out_rs_dir)}
  
  library(maptools); data(wrld_simpl)
  
  rnames <- names(out_all)[4:13]
  for (rn in rnames) {
    if (!file.exists(paste(out_rs_dir,"/",tolower(rn),".asc",sep=""))) {
      cat("output of",rn,"\n")
      rs <- raster(msk)
      rs[out_all$CELL] <- out_all[,rn]
      rs <- writeRaster(rs,paste(out_rs_dir,"/",tolower(rn),".asc",sep=""),format="ascii",overwrite=T)
      rm(rs); g=gc(); rm(g)
    } else {
      cat(rn,"already exists\n")
    }
  }
  
  #ratio of observed to predicted mean yields
  if (!file.exists(paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""))) {
    yo <- raster(paste(out_rs_dir,"/y_obs.asc",sep=""))
    yp <- raster(paste(out_rs_dir,"/y_pred.asc",sep=""))
    yo_yp <- yo/yp
    yo_yp <- writeRaster(yo_yp,paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""),format="ascii",overwrite=T)
  } else {
    yo_yp <- raster(paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""))
  }
  
  #ratio of observed to predicted sd yields
  if (!file.exists(paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""))) {
    sdo <- raster(paste(out_rs_dir,"/ysd_obs.asc",sep=""))
    sdp <- raster(paste(out_rs_dir,"/ysd_pred.asc",sep=""))
    sdo_sdp <- sdo/sdp
    sdo_sdp <- writeRaster(sdo_sdp,paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""),format="ascii",overwrite=T)
  } else {
    sdo_sdp <- raster(paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""))
  }
}


#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>10) {ncpus <- 10}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=10)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("bDir")
sfExport("cmipDir")
sfExport("cropName")
sfExport("runs_set")
sfExport("cDir")
sfExport("glam_dir")
sfExport("input_dir")
sfExport("runs_odir")
sfExport("gcmChars")
sfExport("gcmList")
sfExport("procList")
sfExport("cells")
sfExport("msk")
sfExport("method")
sfExport("irDir")
sfExport("ir_stk")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),wrapper_summarise_GCM_cal))

#stop the cluster
sfStop()



