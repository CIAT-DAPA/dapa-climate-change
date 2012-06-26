#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

### wrapper function to summarise the YGP optimisation
wrapper_summarise_GCM_cal <- function(this_proc) {
  library(raster)
  
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
  
  if (file.exists(cal_dir)) {
    if (!file.exists(paste(cal_dir,"/calib_all_cells.csv",sep=""))) {
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
          n <- NA
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
}


### wrapper function to optimize the ygp using the GCM data
wrapper_GCM_glam_optimise_ygp <- function(this_proc) {
  
  #source all needed functions
  source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/climateSignals-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #get gcm and ensemble member names
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",gcm,"-",ens,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  
  #create an output control directory if it does not exist yet
  octr_dir <- paste(runs_odir,"/x.proc",sep="")
  if (!file.exists(octr_dir)) {dir.create(octr_dir)}
  
  cFile <- paste(octr_dir,"/",gcm,"_",ens,".proc",sep="")
  
  if (!file.exists(cFile)) {
    #ci <- 1
    ciList <- 1:nrow(cells)
    for (ci in ciList) {
      #get run setup
      #files that were generated
      setup <- list()
      setup$BDIR <- bDir
      setup$CELL <- cells$CELL[ci]
      setup$METHOD <- "lin"
      setup$CROPNAME <- "gnut"
      setup$CAL_DIR <- paste(runs_odir,"/",gcm,"_",ens,sep="")
      setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
      setup$YGP_FILE <- "nofile"
      setup$SOW_FILE_RFD <- paste(input_dir,"/ascii/sow/opt_calib_",setup$CELL,".txt",sep="")
      setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
      setup$WTH_DIR_RFD <- paste(input_dir,"/ascii/wth_fut/",gcm,"_ENS_",ens,"/rfd_",setup$CELL,sep="")
      setup$WTH_DIR_IRR <- paste(input_dir,"/ascii/wth_fut/",gcm,"_ENS_",ens,"/irr_",setup$CELL,sep="")
      setup$WTH_ROOT <- "ingc"
      setup$SOL_FILE <- paste(input_dir,"/ascii/soil/soiltypes2.txt",sep="")
      setup$SOL_GRID <- paste(input_dir,"/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
      setup$SIM_NAME <- paste("calib_",setup$CELL,sep="")
      setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
      
      cat("\nprocessing cell",setup$CELL,"\n")
      
      nw_irr <- length(list.files(setup$WTH_DIR_IRR,pattern="\\.wth"))
      nw_rfd <- length(list.files(setup$WTH_DIR_RFD,pattern="\\.wth"))
      
      if (nw_rfd == 28 & nw_irr == 28) {
        #get defaults (parameter set)
        params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
        params$glam_param.mod_mgt$ISYR <- 1966 #start year
        params$glam_param.mod_mgt$IEYR <- 1993 #end year
        params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
        params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
        params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
        params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
        
        #extract irrigation rates
        irDir <- paste(cDir,"/irrigated_ratio",sep="")
        library(raster)
        ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
        ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
        ir_vls <- as.numeric(ir_vls)
        ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
        ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
        
        ######################################################
        # final calibration of YGP
        ######################################################
        #run the optimiser for YGP, 20 steps
        parname <- "YGP"
        where <- "glam_param.ygp"
        nstep <- 20
        
        if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
          # reset lists of output parameters
          optimal <- list(); optimised <- list()
          
          optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                    param=parname,n.steps=20,iter=tolower(parname),
                                                    iratio=ir_vls)
          
          optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
          cat(parname,":",optimal[[parname]],"\n")
          if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
          
          save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
          
          #now make the plot
          plotsDir <- paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/plots",sep="")
          if (!file.exists(plotsDir)) {dir.create(plotsDir)}
          
          tiff(paste(plotsDir,"/",tolower(parname),".tif",sep=""),res=300,compression="lzw",height=1000,
               width=1250,pointsize=8)
          par(mar=c(3,3,2,1))
          plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",
               main=paste(parname," :: ",optimal[[parname]],sep=""),
               xlab="Parameter value",ylab="RMSE (kg/ha)")
          grid(nx=10,ny=10)
          abline(v=optimal[[parname]],col="red",lty=2,lwd=0.8)
          dev.off()
        }
      } else {
        warning("incomplete set of weather files for ",gcm," - ",ens,"\n")
      }
    }
    
    #write the control file
    ff <- file(cFile,"w")
    cat("this model run was finished on",date(),"\n",file=ff)
    close(ff)
  } else {
    cat("process is already done\n")
  }
}

