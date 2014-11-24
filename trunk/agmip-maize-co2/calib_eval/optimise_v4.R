#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#optimise parameters using growth data from Germany FACE study
#change constraints so that instead of 0.5 * TLIMSIL i get 1.0 * TLIMSIL (at the start)
#and 0.0 * TLIMSIL (at the end)

#Note: do only for "best" seed (4510)
#      improved version of model (added KJN's bugfixes)

#list of constraints for optimisation:
#1. 100 % through ISTG=2 == DOY 200 [2007] OR DOY 207 [2008]
#2. TLIMJUV + 4 DAYS + 1.0 * TLIMSIL = 692.4 [2007]
#   TLIMJUV + 4 DAYS + 1.0 * TLIMSIL = 678.6 [2008]
#3. 0.0 * TLIMSIL + 0.90 * TLIMGFP = 594.6 [2007]
#   0.0 * TLIMSIL + 0.95 * TLIMGFP = 581.05 [2008]
#4. LAI, BMASS, YIELD on specified dates
#5. Soil water content

#dont forget
# maximum rooting depth = 60 cm
# YGP=1
# PPSENS=0
# hardate=274 [2007] & hardate=272 [2008]
# sowdate=120 [2007] & sowdate=130 [2008]
# SMLON=10.45

#soil hydrological properties
#RLL=0.05, DUL=0.2167, SAT=0.4048
#Initial available soil water = 1*(DUL-RLL) --->ASWS=1

###
#notes: calculate RMSE divided by mean of measurement, then add all of them up
#       parameters to calibrate are in ./parameters/parameter_list_glam.dat
###

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))

#i/o directories
wd <- "~/Leeds-work/AgMIP-maize-phase-2"
runs_dir <- paste(wd,"/model_runs/laimax_stg3_optim_v4",sep="")
wth_dir <- paste(wd,"/weather",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c_improved",sep="")
par_dir <- paste(wd,"/parameters",sep="")
obs_dir <- paste(wd,"/observed",sep="")

#do the seed thing here
set.seed(5829) #fixed seed to make it replicable
seed_list <- c(0,round(runif(50, 1000, 9999),0))
#seed_list <- seed_list[22]

for (seed in seed_list) {
  #seed <- seed_list[1]
  calib_dir <- paste(runs_dir,"/optim_seed-",seed,sep="")
  
  #load list of parameters and ranges (./data/model_data/parameter_list.txt)
  param_orig <- read.csv(paste(par_dir,"/parameter_list_glam.txt",sep=""),sep="\t",header=T)
  
  #resample parameter order only if seed value is not zero
  if (seed != 0) {
    set.seed(seed)
    param_orig <- param_orig[sample((1:nrow(param_orig)),nrow(param_orig)),]
    row.names(param_orig) <- 1:nrow(param_orig)
  }
  
  #load observed data
  growth_data <- read.csv(paste(obs_dir,"/growth_data.txt",sep=""),sep="\t",header=T)
  growth_data$LAI_MIN <- apply(growth_data[,paste("LAI_r",1:6,sep="")],1,function(x) {min(x,na.rm=T)})
  growth_data$LAI_MAX <- apply(growth_data[,paste("LAI_r",1:6,sep="")],1,function(x) {max(x,na.rm=T)})
  growth_data$BMASS_MIN <- apply(growth_data[,paste("BMASS_r",1:6,sep="")],1,function(x) {min(x*100*100/1000,na.rm=T)})
  growth_data$BMASS_MAX <- apply(growth_data[,paste("BMASS_r",1:6,sep="")],1,function(x) {max(x*100*100/1000,na.rm=T)})
  growth_data$YIELD_MIN <- apply(growth_data[,paste("YIELD_r",1:6,sep="")],1,function(x) {if (length(which(is.na(x))) == length(x)) {return(NA)} else {min(x*100*100/1000,na.rm=T)}})
  growth_data$YIELD_MAX <- apply(growth_data[,paste("YIELD_r",1:6,sep="")],1,function(x) {if (length(which(is.na(x))) == length(x)) {return(NA)} else {max(x*100*100/1000,na.rm=T)}})
  growth_data <- growth_data[,c("YEAR","DOY","LAI","LAI_MIN","LAI_MAX","BMASS","BMASS_MIN",
                                "BMASS_MAX","YIELD","YIELD_MIN","YIELD_MAX")]
  names(growth_data)[c(3,6,9)] <- c("LAI_OBS","BIOMASS_OBS","YIELD_OBS")
  swater_data <- read.csv(paste(obs_dir,"/swater_data.txt",sep=""),sep="\t",header=T)
  
  #load raw weather data
  wth_data <- read.csv(paste(wth_dir,"/weather_all.txt",sep=""),sep="\t")
  wth_data$DATE <- paste(wth_data$DATE)
  wth_data$TBAR <- (wth_data$TMAX + wth_data$TMIN) * 0.5
  names(wth_data)[1:2] <- c("DOY","YEAR")
  wth_data$DOY <- as.numeric(substr(wth_data$DOY,5,7))
  
  #load parameter set and put values in there
  base_params <- GLAM_get_par(parFile=paste(par_dir,"/maize_param_base.txt",sep=""))
  this_params <- base_params
  
  #make directories for runs
  dir_r2007 <- create_dirs(glam_dir=paste(calib_dir,"/wet_amb_2007",sep=""))
  dir_r2008 <- create_dirs(glam_dir=paste(calib_dir,"/wet_amb_2008",sep=""))
  
  #copy weather and executable, write filenames.txt file
  for (year in 2007:2008) {
    #year <- 2007
    run_dir <- get(paste("dir_r",year,sep=""))
    
    #copy weather
    wthfil <- list.files(paste(wth_dir,"/wet_amb_",year,sep=""),pattern="\\.wth")
    if (length(wthfil) == 1) {
      cpfil <- lapply(wthfil, function(x) file.copy(paste(wth_dir,"/wet_amb_",year,"/",x,sep=""),paste(run_dir,"/inputs/ascii/wth",sep=""),overwrite=T))
    }
    
    #copy model
    system(paste("cp -fp ",bin_dir,"/glam-maiz ",run_dir,"/.",sep=""))
    
    #write filenames file
    fn <- file(paste(run_dir,"/filenames.txt",sep=""),"w")
    cat(sprintf("%-41s","maize_param_run.txt"),"\n",sep="",file=fn)
    cat(sprintf("%-41s","inputs/ascii/wth/gebr"),"\n",sep="",file=fn)
    cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
    cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
    cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
    cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
    cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
    close(fn)
  }
  
  #ttcalc function
  ttcalc <- function(tbar) {
    if (tbar<34 & tbar>8) {
      teff <- tbar-8
    } else if (tbar>=34) {
      teff <- 34-8
    } else if (tbar<=8) {
      teff <- 0
    }
    return(teff)
  }
  
  #loop parameters to be optimised
  if (!file.exists(paste(calib_dir,"/optimisation.RData",sep=""))) {
    param_all <- data.frame()
    raw_all <- list()
    for (iter in 1:15) {
      #iter <- 1
      raw_param <- data.frame()
      for (i in 1:nrow(param_orig)) {
        #i <- 1
        prev_params <- this_params
        
        #parameter details
        param <- paste(param_orig$PARAM[i])
        sect <- paste(param_orig$WHERE[i])
        nsteps <- param_orig$NSTEPS[i]
        min_val <- param_orig$MIN[i]
        max_val <- param_orig$MAX[i]
        vals <- seq(min_val, max_val, length.out=nsteps)
        
        #loop values for this parameter
        param_err <- data.frame()
        for (j in 1:nsteps) {
          #j <- 1
          cat("performing run ",j," value = ",vals[j]," (",param,"), iter=",iter,sep="","\n")
          val <- vals[j]
          
          #assign value to parameter set
          if (param %in% c("SLA_INI","NDSLA")) {
            this_params[[sect]][[param]] <- val
          } else {
            this_params[[sect]][[param]][,"Value"] <- val
          }
          
          #loop years to run
          #cat("...running the model for WET 2007-2008\n")
          seas_all <- data.frame()
          daily_all <- data.frame()
          for (year in 2007:2008) {
            #year <- 2007
            run_dir <- get(paste("dir_r",year,sep=""))
            
            #set sowing/harvest date and other conditions in the file
            this_params$glam_param.mod_mgt$ISYR <- year
            this_params$glam_param.mod_mgt$IEYR <- year
            if (year == 2007) {
              this_params$glam_param.spt_mgt$IPDATE$Value <- 120-1
              this_params$glam_param.spt_mgt$IHDATE$Value <- 274-120
            } else {
              this_params$glam_param.spt_mgt$IPDATE$Value <- 130-1
              this_params$glam_param.spt_mgt$IHDATE$Value <- 272-130
            }
            
            #re-write parameter file
            parfile <- GLAM_create_parfile(this_params, paste(run_dir,"/maize_param_run.txt",sep=""))
            
            #run model
            thisdir <- getwd(); setwd(run_dir); system("./glam-maiz"); setwd(thisdir)
            
            #read in output
            seas_out <- read.table(paste(run_dir,"/output/maize1.out",sep=""),header=F,sep="\t")
            names(seas_out) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                                 "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                                 "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                                 "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                                 "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT",
                                 "IPLANT","LETHAL_YIELD","LETHAL_HI","LETHAL_BMASS","LETHAL_BMASS","LETHAL_DAP",
                                 "SWFAC_TOT","SWFAC_MEAN","SWFAC_COUNT")
            
            daily_out <- read.table(paste(run_dir,"/output/daily/maize_gebr001001",year,"_1.out",sep=""),header=F,sep="\t")
            names(daily_out) <- c("DAP","STG","RLV_M","LAI","YIELD","BIOMASS","SLA","HI","CUM_RAIN","SRAD",
                                  "PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                                  "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                                  "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                                  "DUR","VPDTOT")
            daily_out <- cbind(YEAR=year,DOY=(daily_out$DAP+this_params$glam_param.spt_mgt$IPDATE$Value+1),daily_out)
            
            #append to global objects
            seas_all <- rbind(seas_all, seas_out)
            daily_all <- rbind(daily_all, daily_out)
            
            #delete output
            system(paste("rm -f ",run_dir,"/output/daily/maize_gebr001001",year,"_1.out",sep=""))
            system(paste("rm -f ",run_dir,"/output/maize1.out",sep=""))
            system(paste("rm -f ",run_dir,"/glam.inf",sep=""))
            system(paste("rm -f ",run_dir,"/fort.80",sep=""))
          }
          
          #calculate errors
          #cat("...calculating error values\n")
          #1. 90 % through STG=2 eq. 200 in 2007
          #   90 % through STG=2 eq. 207 in 2007
          flwr_per_2007 <- daily_all[which(daily_all$STG==2 & daily_all$YEAR==2007),c("YEAR","DOY","DAP")]
          flwr_per_2008 <- daily_all[which(daily_all$STG==2 & daily_all$YEAR==2008),c("YEAR","DOY","DAP")]
          flwr_doy_2007 <- flwr_per_2007$DOY[round(nrow(flwr_per_2007) * 1.0)]
          flwr_doy_2008 <- flwr_per_2008$DOY[round(nrow(flwr_per_2008) * 1.0)]
          flwr_err <- sqrt(((flwr_doy_2007-200)^2+(flwr_doy_2008-207)^2)/2)/mean(c(200,207))
          
          #2. TT[STG==0] + TT[STG==1] + 0.5 * TT[STG==2] = 692.4 [2007]
          #   TT[STG==0] + TT[STG==1] + 0.5 * TT[STG==2] = 678.6 [2008]
          pflwr <- daily_all[which(daily_all$STG<=2 & daily_all$YEAR==2007),c("YEAR","DOY","DAP","STG")]
          pflwr <- merge(pflwr, wth_data[,c("DOY","YEAR","TBAR")], by=c("DOY","YEAR"),all.x=T,all.y=F)
          pflwr$TT <- unlist(lapply(pflwr$TBAR, ttcalc))
          pflwr_s0 <- pflwr[which(pflwr$STG == 0),]; if (nrow(pflwr_s0) == 0) {pflwr_s0 <- 0} else {pflwr_s0 <- cumsum(pflwr_s0$TT)[nrow(pflwr_s0)]}
          pflwr_s1 <- pflwr[which(pflwr$STG == 1),]; if (nrow(pflwr_s1) == 0) {pflwr_s1 <- 0} else {pflwr_s1 <- cumsum(pflwr_s1$TT)[nrow(pflwr_s1)]}
          pflwr_s2 <- pflwr[which(pflwr$STG == 2),]; if (nrow(pflwr_s2) == 0) {pflwr_s2 <- 0} else {pflwr_s2 <- cumsum(pflwr_s2$TT)[nrow(pflwr_s2)]}
          tthalf_2007 <- pflwr_s0 + pflwr_s1 + 1.0 * pflwr_s2
          
          pflwr <- daily_all[which(daily_all$STG<=2 & daily_all$YEAR==2008),c("YEAR","DOY","DAP","STG")]
          pflwr <- merge(pflwr, wth_data[,c("DOY","YEAR","TBAR")], by=c("DOY","YEAR"),all.x=T,all.y=F)
          pflwr$TT <- unlist(lapply(pflwr$TBAR, ttcalc))
          pflwr_s0 <- pflwr[which(pflwr$STG == 0),]; if (nrow(pflwr_s0) == 0) {pflwr_s0 <- 0} else {pflwr_s0 <- cumsum(pflwr_s0$TT)[nrow(pflwr_s0)]}
          pflwr_s1 <- pflwr[which(pflwr$STG == 1),]; if (nrow(pflwr_s1) == 0) {pflwr_s1 <- 0} else {pflwr_s1 <- cumsum(pflwr_s1$TT)[nrow(pflwr_s1)]}
          pflwr_s2 <- pflwr[which(pflwr$STG == 2),]; if (nrow(pflwr_s2) == 0) {pflwr_s2 <- 0} else {pflwr_s2 <- cumsum(pflwr_s2$TT)[nrow(pflwr_s2)]}
          tthalf_2008 <- pflwr_s0 + pflwr_s1 + 1.0 * pflwr_s2
          
          tthalf_err <- sqrt(((tthalf_2007-687.325)^2+(tthalf_2008-671.4)^2)/2)/mean(c(687.325,671.4))
          
          #3. 0.5 * TT[STG==2] + TT[STG==3] + 0.90 * (TT[STG==4] + TT[STG==5]) = 594.6 [2007]
          #   0.5 * TT[STG==2] + TT[STG==3] + 0.95 * (TT[STG==4] + TT[STG==5]) = 581.05 [2008]
          pmat <- daily_all[which(daily_all$STG>=2 & daily_all$YEAR==2007),c("YEAR","DOY","DAP","STG")]
          pmat <- merge(pmat, wth_data[,c("DOY","YEAR","TBAR")], by=c("DOY","YEAR"),all.x=T,all.y=F)
          pmat$TT <- unlist(lapply(pmat$TBAR, ttcalc))
          pmat_s2 <- pmat[which(pmat$STG == 2),]; if (nrow(pmat_s2) == 0) {pmat_s2 <- 0} else {pmat_s2 <- cumsum(pmat_s2$TT)[nrow(pmat_s2)]}
          pmat_s3 <- pmat[which(pmat$STG == 3),]; if (nrow(pmat_s3) == 0) {pmat_s3 <- 0} else {pmat_s3 <- cumsum(pmat_s3$TT)[nrow(pmat_s3)]}
          pmat_s4 <- pmat[which(pmat$STG == 4),]; if (nrow(pmat_s4) == 0) {pmat_s4 <- 0} else {pmat_s4 <- cumsum(pmat_s4$TT)[nrow(pmat_s4)]}
          pmat_s5 <- pmat[which(pmat$STG == 5),]; if (nrow(pmat_s5) == 0) {pmat_s5 <- 0} else {pmat_s5 <- cumsum(pmat_s5$TT)[nrow(pmat_s5)]}
          ttmat_2007 <- 0.0 * pmat_s2 + pmat_s3 + 0.9 * (pmat_s4 + pmat_s5)
          
          pmat <- daily_all[which(daily_all$STG>=2 & daily_all$YEAR==2008),c("YEAR","DOY","DAP","STG")]
          pmat <- merge(pmat, wth_data[,c("DOY","YEAR","TBAR")], by=c("DOY","YEAR"),all.x=T,all.y=F)
          pmat$TT <- unlist(lapply(pmat$TBAR, ttcalc))
          pmat_s2 <- pmat[which(pmat$STG == 2),]; if (nrow(pmat_s2) == 0) {pmat_s2 <- 0} else {pmat_s2 <- cumsum(pmat_s2$TT)[nrow(pmat_s2)]}
          pmat_s3 <- pmat[which(pmat$STG == 3),]; if (nrow(pmat_s3) == 0) {pmat_s3 <- 0} else {pmat_s3 <- cumsum(pmat_s3$TT)[nrow(pmat_s3)]}
          pmat_s4 <- pmat[which(pmat$STG == 4),]; if (nrow(pmat_s4) == 0) {pmat_s4 <- 0} else {pmat_s4 <- cumsum(pmat_s4$TT)[nrow(pmat_s4)]}
          pmat_s5 <- pmat[which(pmat$STG == 5),]; if (nrow(pmat_s5) == 0) {pmat_s5 <- 0} else {pmat_s5 <- cumsum(pmat_s5$TT)[nrow(pmat_s5)]}
          ttmat_2008 <- 0.0 * pmat_s2 + pmat_s3 + 0.95 * (pmat_s4 + pmat_s5)
          
          ttmat_err <- sqrt(((ttmat_2007-589.525)^2+(ttmat_2008-573.85)^2)/2)/mean(c(589.525,573.85))
          
          #4. LAI, BMASS, YIELD on specified dates
          gwth_all <- daily_all[,c("YEAR","DOY","DAP","LAI","BIOMASS","YIELD")]
          gwth_all <- merge(growth_data, gwth_all, by=c("YEAR","DOY"), all.x=T, all.y=F)
          lai_err <- sqrt(sum((gwth_all$LAI_OBS-gwth_all$LAI)^2,na.rm=T) / (length(which(!is.na(gwth_all$LAI_OBS))))) / mean(gwth_all$LAI_OBS, na.rm=T)
          bmass_err <- sqrt(sum((gwth_all$BIOMASS_OBS-gwth_all$BIOMASS)^2,na.rm=T) / (length(which(!is.na(gwth_all$BIOMASS_OBS))))) / mean(gwth_all$BIOMASS_OBS, na.rm=T)
          yield_err <- sqrt(sum((gwth_all$YIELD_OBS-gwth_all$YIELD)^2,na.rm=T) / (length(which(!is.na(gwth_all$YIELD_OBS))))) / mean(gwth_all$YIELD_OBS, na.rm=T)
          
          #5. Soil water content (=PESW in GLAM)
          swat_all <- daily_all[,c("YEAR","DOY","DAP","PESW")]
          swat_all <- merge(swater_data, swat_all, by=c("YEAR","DOY"), all.x=T, all.y=F)
          swat_all$PESW <- swat_all$PESW*10 #cm to mm
          swat_err <- sqrt(sum((swat_all$SW_mm-swat_all$PESW)^2,na.rm=T) / (length(which(!is.na(swat_all$SW_mm))))) / mean(swat_all$SW_mm, na.rm=T)
          
          #calculate total error
          tot_err <- sum(c(flwr_err,tthalf_err,ttmat_err,lai_err,bmass_err,yield_err,swat_err))
          men_err <- tot_err / 7
          
          #output row
          out_row <- data.frame(SEQ=j,VALUE=val,FLWR_ERR=flwr_err, TTHALF_ERR=tthalf_err, TTMAT_ERR=ttmat_err,
                                LAI_ERR=lai_err, BMASS_ERR=bmass_err, YIELD_ERR=yield_err, SWAT_ERR=swat_err,
                                TOT_ERR=tot_err, MEAN_ERR=men_err)
          param_err <- rbind(param_err, out_row)
        }
        
        #here determine the optimum value
        opt_val <- param_err$VALUE[which(param_err$TOT_ERR == min(param_err$TOT_ERR))]
        if (length(opt_val) == length(vals)) {
          if (param %in% c("SLA_INI","NDSLA")) {
            opt_val <- prev_params[[sect]][[param]]
          } else {
            opt_val <- prev_params[[sect]][[param]][,"Value"] <- val
          }
        } else if (length(opt_val) > 1) {
          opt_val <- opt_val[ceiling(length(opt_val)/2)]
        }
        
        #put parameter value in parameter set
        if (param %in% c("SLA_INI","NDSLA")) {
          this_params[[sect]][[param]] <- opt_val
        } else {
          this_params[[sect]][[param]]$Value <- opt_val
        }
        
        param_err <- cbind(ITER=iter, PARAM=param, param_err)
        param_all <- rbind(param_all,param_err[which(param_err$VALUE == opt_val),])
        raw_param <- rbind(raw_param, param_err)
      }
      raw_all[[iter]] <- raw_param
    }
    save(list=c("raw_all","param_all"),file=paste(calib_dir,"/optimisation.RData",sep=""))
  } else {
    load(file=paste(calib_dir,"/optimisation.RData",sep=""))
  }
  
  ###
  #produce sensitivity plots for each parameter
  fig_dir <- paste(calib_dir,"/parameter_responses",sep="")
  if (!file.exists(fig_dir)) {dir.create(fig_dir)}
  
  raw_param <- raw_all[[15]]
  for (i in 1:nrow(param_orig)) {
    #i <- 1
    param <- paste(param_orig$PARAM[i])
    
    #select sensitivity output from raw_param
    param_err <- raw_param[which(raw_param$PARAM == param),]
    
    #plot
    pdf(file=paste(fig_dir,"/param_",i,"_",param,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5)
    plot(param_err$VALUE, param_err$MEAN_ERR, ty="l", 
         xlim=c(param_err$VALUE[1],param_err$VALUE[nrow(param_err)]),
         ylim=c(0.05,0.5), xlab=paste("Value of ",param,sep=""), ylab="Mean error (normalised)")
    grid()
    dev.off()
  }
  
  
  ###
  #produce simulations with optimal parameter set and grab output
  #update parameter set
  best_params <- base_params
  opt_param <- param_all[which(param_all$ITER==15),]
  for (i in 1:nrow(opt_param)) {
    #i <- 1
    param <- paste(opt_param$PARAM[i])
    sect <- paste(param_orig$WHERE[which(param_orig$PARAM == param)])
    val <- opt_param$VALUE[i]
    
    #assign value to parameter set
    if (param %in% c("SLA_INI","NDSLA")) {
      best_params[[sect]][[param]] <- val
    } else {
      best_params[[sect]][[param]][,"Value"] <- val
    }
  }
  write.csv(opt_param, paste(calib_dir,"/optimum_values.csv",sep=""),row.names=F,quote=T)
  
  #run the two years if not run before
  if (!file.exists(paste(calib_dir,"/optimal_run.RData",sep=""))) {
    seas_all <- data.frame()
    daily_all <- data.frame()
    for (year in 2007:2008) {
      #year <- 2007
      run_dir <- get(paste("dir_r",year,sep=""))
      
      #set sowing/harvest date and other conditions in the file
      best_params$glam_param.mod_mgt$ISYR <- year
      best_params$glam_param.mod_mgt$IEYR <- year
      if (year == 2007) {
        best_params$glam_param.spt_mgt$IPDATE$Value <- 120-1
        best_params$glam_param.spt_mgt$IHDATE$Value <- 274-120
      } else {
        best_params$glam_param.spt_mgt$IPDATE$Value <- 130-1
        best_params$glam_param.spt_mgt$IHDATE$Value <- 272-130
      }
      
      #re-write parameter file
      parfile <- GLAM_create_parfile(best_params, paste(run_dir,"/maize_param_run.txt",sep=""))
      
      #run model
      thisdir <- getwd(); setwd(run_dir); system("./glam-maiz"); setwd(thisdir)
      
      #read in output
      seas_out <- read.table(paste(run_dir,"/output/maize1.out",sep=""),header=F,sep="\t")
      names(seas_out) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                           "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                           "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                           "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                           "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT",
                           "IPLANT","LETHAL_YIELD","LETHAL_HI","LETHAL_BMASS","LETHAL_BMASS","LETHAL_DAP",
                           "SWFAC_TOT","SWFAC_MEAN","SWFAC_COUNT")
      
      daily_out <- read.table(paste(run_dir,"/output/daily/maize_gebr001001",year,"_1.out",sep=""),header=F,sep="\t")
      names(daily_out) <- c("DAP","STG","RLV_M","LAI","YIELD","BIOMASS","SLA","HI","CUM_RAIN","SRAD",
                            "PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                            "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                            "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                            "DUR","VPDTOT")
      daily_out <- cbind(YEAR=year,DOY=(daily_out$DAP+best_params$glam_param.spt_mgt$IPDATE$Value+1),daily_out)
      
      #append to global objects
      seas_all <- rbind(seas_all, seas_out)
      daily_all <- rbind(daily_all, daily_out)
      
      #delete output
      system(paste("rm -f ",run_dir,"/output/daily/maize_gebr001001",year,"_1.out",sep=""))
      system(paste("rm -f ",run_dir,"/output/maize1.out",sep=""))
      system(paste("rm -f ",run_dir,"/glam.inf",sep=""))
      system(paste("rm -f ",run_dir,"/fort.80",sep=""))
    }
    save(list=c("seas_all","daily_all","best_params"),file=paste(calib_dir,"/optimal_run.RData",sep=""))
  } else {
    load(file=paste(calib_dir,"/optimal_run.RData",sep=""))
  }
  
  ###
  #produce plots of measured quantities, LAI, BIOMASS, YIELD, PESW
  efig_dir <- paste(calib_dir,"/evaluation_plots",sep="")
  if (!file.exists(efig_dir)) {dir.create(efig_dir)}
  
  gwth_all <- daily_all[,c("YEAR","DOY","DAP","LAI","BIOMASS","YIELD")]
  gwth_all <- merge(growth_data, gwth_all, by=c("YEAR","DOY"), all.x=T, all.y=F)
  
  swat_all <- daily_all[,c("YEAR","DOY","DAP","PESW")]
  swat_all <- merge(swater_data, swat_all, by=c("YEAR","DOY"), all.x=T, all.y=F)
  swat_all$PESW <- swat_all$PESW*10 #cm to mm
  
  for (year in 2007:2008) {
    #year <- 2007
    gwth_yr <- gwth_all[which(gwth_all$YEAR == year),]
    swat_yr <- swat_all[which(swat_all$YEAR == year),]
    daily_yr <- daily_all[which(daily_all$YEAR == year),c("YEAR","DOY","DAP","LAI","BIOMASS","YIELD","PESW")]
    
    #biomass
    pdf(file=paste(efig_dir,"/biomass_yield_",year,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5, las=1)
    plot(daily_yr$DOY, daily_yr$BIOMASS*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
         xlab="Day of year",ylab="Biomass or Yield (t/ha)")
    points(gwth_yr$DOY, gwth_yr$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
    for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$BMASS_MIN[jm]*.001,gwth_yr$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(daily_yr$DOY, daily_yr$YIELD*.001, lty=2, lwd=1.25)
    points(gwth_yr$DOY, gwth_yr$YIELD_OBS*.001, pch=15, cex=1.25, col="red")
    for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$YIELD_MIN[jm]*.001,gwth_yr$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    grid()
    dev.off()
    
    #lai
    pdf(file=paste(efig_dir,"/lai_",year,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5, las=1)
    plot(daily_yr$DOY, daily_yr$LAI, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
         xlab="Day of year",ylab="Leaf area index (m2/m2)")
    points(gwth_yr$DOY, gwth_yr$LAI_OBS, pch=20, cex=1.25, col="red")
    for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$LAI_MIN[jm],gwth_yr$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
    grid()
    dev.off()
    
    #pesw
    pdf(file=paste(efig_dir,"/swater_",year,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5, las=1)
    plot(daily_yr$DOY, daily_yr$PESW*10, ty="l", xlim=c(120,290), ylim=c(30,110),lwd=1.25,
         xlab="Day of year",ylab="Available soil water (mm)")
    points(swat_yr$DOY, swat_yr$SW_mm, pch=20, cex=1.05, col="red")
    grid()
    dev.off()
  }
}





