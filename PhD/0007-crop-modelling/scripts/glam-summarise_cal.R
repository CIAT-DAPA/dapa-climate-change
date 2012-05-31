#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

library(raster)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"

#eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"

#sourcing functions
source(paste(src.dir,"/climateSignals-functions.R",sep=""))

#input directories and model
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
cal_dir <- paste(cDir,"/calib",sep="")

#load cell details
#cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
cells <- read.csv(paste(cDir,"/inputs/calib-cells-selection.csv",sep=""))

#get the mask needed (to which data will be appended)
ncFile <- paste(bDir,"/../climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
ydDir <- paste(bDir,"/climate-signals-yield/GNUT/raster/gridded",sep="")

metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)
msk[] <- NA

#method of yield detrending
method <- "lin"

#select cell
#cell <- cells$CELL[1]
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
  
  run_dir <- paste(cal_dir,"/fcal_",cell,sep="")
  ygpDir <- paste(run_dir,"/iter-ygp",sep="")
  sowDir <- paste(run_dir,"/iter-ipdate",sep="")
  
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
  irDir <- paste(cDir,"/irrigated_ratio",sep="")
  ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
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
  
  #get best initial planting date
  load(paste(sowDir,"/output.RData",sep=""))
  sow_date <- optimal$IPDATE
  
  #output data frame
  out_row <- data.frame(CELL=cell,X=cells$X[which(cells$CELL == cell)],
                        Y=cells$Y[which(cells$CELL == cell)],YGP=ygp,Y_OBS=yo_mean,YSD_OBS=yo_stdv,
                        Y_PRED=yp_mean,YSD_PRED=yp_stdv,CCOEF=r_val,PVAL=p_val,RMSE=rmse,P_RMSE=prmse,
                        SOW_DATE=sow_date,N=n)
  
  if (cell == cells$CELL[1]) {
    out_all <- out_row
  } else {
    out_all <- rbind(out_all,out_row)
  }
}


