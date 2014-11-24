#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#goes with:
#    * optimise_v4.R
#    * co2_response_perturbations_v4.R
#    * co2_response_evaluation_v4.R

#iteratively improves the CO2 parameterisation using the Manderscheid et al. (2014) 
#observations

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))

#i/o directories
wd <- "~/Leeds-work/AgMIP-maize-phase-2"
runs_dir <- paste(wd,"/model_runs/laimax_stg3_co2_parameterisation_v4",sep="") #to be changed
wth_dir <- paste(wd,"/weather",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c_improved",sep="")
par_dir <- paste(wd,"/parameters",sep="")
obs_dir <- paste(wd,"/observed",sep="")
rep_dir <- paste(runs_dir,"/amb_face_reporting",sep="")

#output dir
out_dir <- paste(runs_dir,"/amb_face_model_improvement",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#list of seeds
set.seed(5829) #fixed seed to make it replicable
seed_list <- c(0,round(runif(50, 1000, 9999),0))

# #list of skilful seeds
# seed_skill <- data.frame()
# for (seed in seed_list) {
#   #seed <- seed_list[1]
#   skill <- read.csv(paste(wd,"/model_runs/laimax_stg3_optim_v4/optim_seed-",seed,"/optimum_values.csv",sep=""))
#   seed_skill <- rbind(seed_skill, data.frame(SEED=seed,MEAN_ERR=mean(skill$MEAN_ERR)))
#   rm(skill)
# }
# seed_best <- seed_skill$SEED[which(seed_skill$MEAN_ERR == min(seed_skill$MEAN_ERR))]
seed_best <- seed_list[22]

#get the parameter file for this run (target object: "best_params")
load(file=paste(wd,"/model_runs/laimax_stg3_optim_v4/optim_seed-",seed_best,"/optimal_run.RData",sep=""))
rm(list=c("daily_all","seas_all"))

#load parameter_response_ratios.csv
rratios <- read.csv(paste(wd,"/parameters/scaled_response/parameter_response_ratios.csv",sep=""))
photo_min <- rratios$q05[which(rratios$property == "photo" & rratios$cond == "dry")]
photo_max <- rratios$q95[which(rratios$property == "photo" & rratios$cond == "dry")]
trans_min <- min(rratios$q05[which(rratios$property == "trans")])
trans_max <- max(rratios$q95[which(rratios$property == "trans")])

###
#perform the runs as needed
#create directories for runs
rd_wet_amb2007 <- create_dirs(glam_dir=paste(out_dir,"/wet_amb_2007",sep=""))
rd_wet_amb2008 <- create_dirs(glam_dir=paste(out_dir,"/wet_amb_2008",sep=""))
rd_wet_face2007 <- create_dirs(glam_dir=paste(out_dir,"/wet_face_2007",sep=""))
rd_wet_face2008 <- create_dirs(glam_dir=paste(out_dir,"/wet_face_2008",sep=""))
rd_dry_amb2007 <- create_dirs(glam_dir=paste(out_dir,"/dry_amb_2007",sep=""))
rd_dry_amb2008 <- create_dirs(glam_dir=paste(out_dir,"/dry_amb_2008",sep=""))
rd_dry_face2007 <- create_dirs(glam_dir=paste(out_dir,"/dry_face_2007",sep=""))
rd_dry_face2008 <- create_dirs(glam_dir=paste(out_dir,"/dry_face_2008",sep=""))

#df of all blind simulations
blindsim_df <- data.frame(WTH=c("dry","dry","wet","wet"),CO2=c("amb","face","amb","face"))

#turn off tds
best_params$glam_param.sparer$HIMIN$Value <- -99.0
best_params$glam_param.sparer$SWC_FAC$Value <- -99.0

#loop through all simulations to prepare inputs
for (i in 1:nrow(blindsim_df)) {
  #i <- 1
  wth <- paste(blindsim_df$WTH[i])
  co2 <- paste(blindsim_df$CO2[i])
  
  #copy weather and executable, write filenames.txt file
  for (year in 2007:2008) {
    #year <- 2007
    run_dir <- get(paste("rd_",wth,"_",co2,year,sep=""))
    
    #copy weather
    wthfil <- list.files(paste(wth_dir,"/",wth,"_",co2,"_",year,sep=""),pattern="\\.wth")
    if (length(wthfil) == 1) {
      cpfil <- lapply(wthfil, function(x) file.copy(paste(wth_dir,"/",wth,"_",co2,"_",year,"/",x,sep=""),paste(run_dir,"/inputs/ascii/wth",sep=""),overwrite=T))
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
}

#step by step analysis
#run all simulations: a sequence of 20 values between 0.80 and 1.20 of the ranges
photo_seq <- seq((photo_min-(photo_max-photo_min)*.5), (photo_max+(photo_max-photo_min)*.5), length.out=20)
trans_seq <- seq((trans_min-(trans_max-trans_min)*.5), (trans_max+(trans_max-trans_min)*.5), length.out=20)

photo_seq <- c(photo_seq, photo_seq, 1.35, 1.4, 1.45, 1.50, 1.55, 1.60, 1.65)
trans_seq <- c(trans_seq, rev(trans_seq), 0.765, 0.765, 0.765, 0.765, 0.765, 0.765, 0.765)

#loop through all simulations to prepare inputs
for (i in 1:nrow(blindsim_df)) {
  #i <- 2
  wth <- paste(blindsim_df$WTH[i])
  co2 <- paste(blindsim_df$CO2[i])
  
  cat("processing condition=",wth,"and co2=",co2,"\n")
  
  #only if CO2=face
  if (co2 == "face") {
    if (!file.exists(paste(out_dir,"/",wth,"_",co2,"_2008/output_perturbations.RData",sep=""))) {
      sim_daily <- list(sim_2007=list(), sim_2008=list())
      sim_seas <- list(sim_2007=list(), sim_2008=list())
      for (ipert in 1:length(photo_seq)) {
        #ipert <- 1
        cat("...processing perturbation=",ipert,"\n")
        
        #get parameter set
        this_params <- best_params
        
        #baseline parameters
        this_params$glam_param.sim_ctr$IC02 <- 1
        this_params$glam_param.hts_fut$B_TE$Value <- this_params$glam_param.bmass$TE$Value
        this_params$glam_param.hts_fut$B_TEN_MAX$Value <- this_params$glam_param.bmass$TEN_MAX$Value
        
        #Tfac=0 for all FACE runs, C4 crop
        this_params$glam_param.hts_fut$TENFAC$Value <- 0 
        
        #stimulated parameters (TE and P_TRANS_MAX)
        te_fac <- photo_seq[ipert]
        tr_fac <- trans_seq[ipert]
        this_params$glam_param.bmass$TE$Value <- this_params$glam_param.bmass$TE$Value * te_fac
        this_params$glam_param.evap$P_TRANS_MAX$Value <- this_params$glam_param.evap$P_TRANS_MAX$Value * tr_fac
        
        #TEN_MAX as baseline since tfac=0
        this_params$glam_param.bmass$TEN_MAX$Value <- (1-this_params$glam_param.hts_fut$TENFAC$Value) * this_params$glam_param.hts_fut$B_TEN_MAX$Value + this_params$glam_param.hts_fut$TENFAC$Value * this_params$glam_param.bmass$TE$Value * (this_params$glam_param.hts_fut$B_TEN_MAX$Value / this_params$glam_param.hts_fut$B_TE$Value)
        
        #loop years
        for (year in 2007:2008) {
          #year <- 2007
          run_dir <- get(paste("rd_",wth,"_",co2,year,sep=""))
          
          #set sowing/harvest date and other conditions in the file
          this_params$glam_param.mod_mgt$ISYR <- year
          this_params$glam_param.mod_mgt$IEYR <- year
          if (year == 2007) {
            this_params$glam_param.spt_mgt$IPDATE$Value <- 120-1
            this_params$glam_param.spt_mgt$IHDATE$Value <- -99 #274-120
          } else {
            this_params$glam_param.spt_mgt$IPDATE$Value <- 130-1
            this_params$glam_param.spt_mgt$IHDATE$Value <- -99 #272-130
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
          
          #append to lists
          sim_seas[[paste("sim_",year,sep="")]][[ipert]] <- seas_out
          sim_daily[[paste("sim_",year,sep="")]][[ipert]] <- daily_out
          
          #delete output
          system(paste("rm -f ",run_dir,"/output/daily/maize_gebr001001",year,"_1.out",sep=""))
          system(paste("rm -f ",run_dir,"/output/maize1.out",sep=""))
          system(paste("rm -f ",run_dir,"/glam.inf",sep=""))
          system(paste("rm -f ",run_dir,"/fort.80",sep=""))
        }
      }
      sim_seas_2007 <- sim_seas$sim_2007; sim_seas_2008 <- sim_seas$sim_2008
      sim_daily_2007 <- sim_daily$sim_2007; sim_daily_2008 <- sim_daily$sim_2008
      save(list=c("sim_seas_2007","sim_daily_2007"),file=paste(out_dir,"/",wth,"_",co2,"_2007/output_perturbations.RData",sep=""))
      save(list=c("sim_seas_2008","sim_daily_2008"),file=paste(out_dir,"/",wth,"_",co2,"_2008/output_perturbations.RData",sep=""))
      rm(list=c("sim_seas","sim_daily"))
    }
  } else {
    #get parameter set
    this_params <- best_params
    
    #loop years
    for (year in 2007:2008) {
      #year <- 2007
      run_dir <- get(paste("rd_",wth,"_",co2,year,sep=""))
      
      if (!file.exists(paste(run_dir,"/output.RData",sep=""))) {
        #set sowing/harvest date and other conditions in the file
        this_params$glam_param.mod_mgt$ISYR <- year
        this_params$glam_param.mod_mgt$IEYR <- year
        if (year == 2007) {
          this_params$glam_param.spt_mgt$IPDATE$Value <- 120-1
          this_params$glam_param.spt_mgt$IHDATE$Value <- -99 #274-120
        } else {
          this_params$glam_param.spt_mgt$IPDATE$Value <- 130-1
          this_params$glam_param.spt_mgt$IHDATE$Value <- -99 #272-130
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
        save(list=c("seas_out","daily_out","this_params"),file=paste(run_dir,"/output.RData",sep=""))
        
        #remove objects
        rm(list=c("daily_out","seas_out"))
      }
    }
  }
}


########### produce plots for each of the 40 iterations
########### 

#load obs data
growth_data <- read.table(paste(obs_dir,"/growth_data_high.txt",sep=""),header=T)
growth_data$LAI_MIN <- apply(growth_data[,paste("LAI_r",1:6,sep="")],1,function(x) {min(x,na.rm=T)})
growth_data$LAI_MAX <- apply(growth_data[,paste("LAI_r",1:6,sep="")],1,function(x) {max(x,na.rm=T)})
growth_data$BMASS_MIN <- apply(growth_data[,paste("BMASS_r",1:6,sep="")],1,function(x) {min(x*100*100/1000,na.rm=T)})
growth_data$BMASS_MAX <- apply(growth_data[,paste("BMASS_r",1:6,sep="")],1,function(x) {max(x*100*100/1000,na.rm=T)})
growth_data$YIELD_MIN <- apply(growth_data[,paste("YIELD_r",1:6,sep="")],1,function(x) {if (length(which(is.na(x))) == length(x)) {return(NA)} else {min(x*100*100/1000,na.rm=T)}})
growth_data$YIELD_MAX <- apply(growth_data[,paste("YIELD_r",1:6,sep="")],1,function(x) {if (length(which(is.na(x))) == length(x)) {return(NA)} else {max(x*100*100/1000,na.rm=T)}})
growth_data <- growth_data[,c("TREATMENT","YEAR","DOY","LAI","LAI_MIN","LAI_MAX","BMASS","BMASS_MIN",
                              "BMASS_MAX","YIELD","YIELD_MIN","YIELD_MAX")]
names(growth_data)[c(4,7,10)] <- c("LAI_OBS","BIOMASS_OBS","YIELD_OBS")

swater_data <- read.table(paste(obs_dir,"/swater_data_high.txt",sep=""),header=T)

fig_dir <- paste(out_dir, "/improve_figs",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir)}

#plot each perturbation and calculate rmse for each perturbation and all treatments
rmse_df <- data.frame()
for (ipert in 1:length(photo_seq)) {
  #ipert <- 1
  cat("...processing perturbation=",ipert,"\n")
  
  ttitle <- paste("PN_fac=",round(photo_seq[ipert],3)," / TR_fac=",round(trans_seq[ipert],3),sep="")
  
  #plot per year
  for (year in 2007:2008) {
    #year <- 2007
    #cat("processing year=",year,"\n")
    
    #dry_face 
    load(paste(out_dir,"/dry_face_",year,"/output_perturbations.RData",sep=""))
    dry_face <- get(paste("sim_daily_",year,sep=""))
    dry_face <- dry_face[[ipert]]; rm(list=c(paste("sim_daily_",year,sep=""),paste("sim_seas_",year,sep="")))
    dry_face_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "DRY_ELE"),]
    dry_face_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "DRY_ELE"),]
    
    #dry_amb
    load(paste(out_dir,"/dry_amb_",year,"/output.RData",sep=""))
    dry_amb <- daily_out; rm(list=c("seas_out","daily_out","this_params"))
    dry_amb_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "DRY_AMB"),]
    dry_amb_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "DRY_AMB"),]
    
    #wet_face
    load(paste(out_dir,"/wet_face_",year,"/output_perturbations.RData",sep=""))
    wet_face <- get(paste("sim_daily_",year,sep=""))
    wet_face <- wet_face[[ipert]]; rm(list=c(paste("sim_daily_",year,sep=""),paste("sim_seas_",year,sep="")))
    wet_face_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "WET_ELE"),]
    wet_face_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "WET_ELE"),]
    
    #wet_amb
    load(paste(out_dir,"/wet_amb_",year,"/output.RData",sep=""))
    wet_amb <- daily_out; rm(list=c("seas_out","daily_out","this_params"))
    wet_amb_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "WET_AMB"),]
    wet_amb_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "WET_AMB"),]
    
    #calculate rmse values
    for (treat in c("dry_face","dry_amb","wet_face","wet_amb")) {
      #treat <- "dry_face"
      obs_df <- get(paste(treat,"_obs",sep=""))
      sim_df <- get(treat)
      all_df <- merge(obs_df[,c("DOY","LAI_OBS","BIOMASS_OBS","YIELD_OBS")], sim_df[,c("DOY","LAI","YIELD","BIOMASS")], by="DOY", all.x=T, all.y=F)
      
      lai_rmse <- sqrt(sum((all_df$LAI_OBS - all_df$LAI)^2) / nrow(all_df)) / mean(all_df$LAI_OBS) * 100
      bms_rmse <- sqrt(sum((all_df$BIOMASS_OBS - all_df$BIOMASS)^2) / nrow(all_df)) / mean(all_df$BIOMASS_OBS) * 100
      yld_rmse <- sqrt(sum((all_df$YIELD_OBS - all_df$YIELD)^2,na.rm=T) / 1) / mean(all_df$YIELD_OBS,na.rm=T) * 100
      
      #swater
      obs_df <- get(paste(treat,"_swat",sep=""))
      all_df <- merge(obs_df[,c("DOY","SW_mm")], sim_df[,c("DOY","PESW")], by="DOY", all.x=T, all.y=F)
      all_df$PESW <- all_df$PESW * 10
      swt_rmse <- sqrt(sum((all_df$SW_mm - all_df$PESW)^2) / nrow(all_df)) / mean(all_df$SW_mm) * 100
      
      #output row and append
      out_row <- data.frame(PERTURBATION=ipert, TREATMENT=treat,YEAR=year, LAI=lai_rmse, BIOMASS=bms_rmse, YIELD=yld_rmse, SWATER=swt_rmse)
      rmse_df <- rbind(rmse_df, out_row)
    }
    
    
    ### biomass and yield 
    #wet plots
    pdf(file=paste(fig_dir,"/biomass_yield_",year,"_wet_",ipert,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5, las=1)
    plot(wet_amb$DOY, wet_amb$BIOMASS*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
         xlab="Day of year",ylab="Biomass or Yield (t/ha)",main=ttitle)
    points(wet_amb_obs$DOY, wet_amb_obs$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
    for (jm in 1:nrow(wet_amb_obs)) {lines(x=c(wet_amb_obs$DOY[jm],wet_amb_obs$DOY[jm]),y=c(wet_amb_obs$BMASS_MIN[jm]*.001,wet_amb_obs$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(wet_amb$DOY, wet_amb$YIELD*.001, lty=1, lwd=1.25)
    points(wet_amb_obs$DOY, wet_amb_obs$YIELD_OBS*.001, pch=15, cex=1.25, col="red")
    for (jm in 1:nrow(wet_amb_obs)) {lines(x=c(wet_amb_obs$DOY[jm],wet_amb_obs$DOY[jm]),y=c(wet_amb_obs$YIELD_MIN[jm]*.001,wet_amb_obs$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(wet_face$DOY, wet_face$BIOMASS*.001, lty=2, lwd=1.25)
    points(wet_face_obs$DOY, wet_face_obs$BIOMASS_OBS*.001, pch=21, cex=1, col="red")
    for (jm in 1:nrow(wet_face_obs)) {lines(x=c(wet_face_obs$DOY[jm],wet_face_obs$DOY[jm]),y=c(wet_face_obs$BMASS_MIN[jm]*.001,wet_face_obs$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(wet_face$DOY, wet_face$YIELD*.001, lty=2, lwd=1.25)
    points(wet_face_obs$DOY, wet_face_obs$YIELD_OBS*.001, pch=22, cex=1, col="red")
    for (jm in 1:nrow(wet_face_obs)) {lines(x=c(wet_face_obs$DOY[jm],wet_face_obs$DOY[jm]),y=c(wet_face_obs$YIELD_MIN[jm]*.001,wet_face_obs$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    grid(lwd=1)
    legend(120,25,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
    legend(120,19.5,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
    dev.off()
    
    
    #dry plots
    pdf(file=paste(fig_dir,"/biomass_yield_",year,"_dry_",ipert,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5, las=1)
    plot(dry_amb$DOY, dry_amb$BIOMASS*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
         xlab="Day of year",ylab="Biomass or Yield (t/ha)",main=ttitle)
    points(dry_amb_obs$DOY, dry_amb_obs$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
    for (jm in 1:nrow(dry_amb_obs)) {lines(x=c(dry_amb_obs$DOY[jm],dry_amb_obs$DOY[jm]),y=c(dry_amb_obs$BMASS_MIN[jm]*.001,dry_amb_obs$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(dry_amb$DOY, dry_amb$YIELD*.001, lty=1, lwd=1.25)
    points(dry_amb_obs$DOY, dry_amb_obs$YIELD_OBS*.001, pch=15, cex=1.25, col="red")
    for (jm in 1:nrow(dry_amb_obs)) {lines(x=c(dry_amb_obs$DOY[jm],dry_amb_obs$DOY[jm]),y=c(dry_amb_obs$YIELD_MIN[jm]*.001,dry_amb_obs$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(dry_face$DOY, dry_face$BIOMASS*.001, lty=2, lwd=1.25)
    points(dry_face_obs$DOY, dry_face_obs$BIOMASS_OBS*.001, pch=21, cex=1, col="red")
    for (jm in 1:nrow(dry_face_obs)) {lines(x=c(dry_face_obs$DOY[jm],dry_face_obs$DOY[jm]),y=c(dry_face_obs$BMASS_MIN[jm]*.001,dry_face_obs$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    lines(dry_face$DOY, dry_face$YIELD*.001, lty=2, lwd=1.25)
    points(dry_face_obs$DOY, dry_face_obs$YIELD_OBS*.001, pch=22, cex=1, col="red")
    for (jm in 1:nrow(dry_face_obs)) {lines(x=c(dry_face_obs$DOY[jm],dry_face_obs$DOY[jm]),y=c(dry_face_obs$YIELD_MIN[jm]*.001,dry_face_obs$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
    grid(lwd=1)
    legend(120,25,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
    legend(120,19.5,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
    dev.off()
    
    
    ### LAI
    #wet plots
    pdf(file=paste(fig_dir,"/lai_",year,"_wet_",ipert,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5)
    plot(wet_amb$DOY, wet_amb$LAI, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
         xlab="Day of year",ylab="Leaf area index (m2/m2)",main=ttitle)
    points(wet_amb_obs$DOY, wet_amb_obs$LAI_OBS, pch=20, cex=1.25, col="red")
    for (jm in 1:nrow(wet_amb_obs)) {lines(x=c(wet_amb_obs$DOY[jm],wet_amb_obs$DOY[jm]),y=c(wet_amb_obs$LAI_MIN[jm],wet_amb_obs$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
    lines(wet_face$DOY, wet_face$LAI, lty=2, lwd=1.25)
    points(wet_face_obs$DOY, wet_face_obs$LAI_OBS, pch=21, cex=1, col="red")
    for (jm in 1:nrow(wet_face_obs)) {lines(x=c(wet_face_obs$DOY[jm],wet_face_obs$DOY[jm]),y=c(wet_face_obs$LAI_MIN[jm],wet_face_obs$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
    grid(lwd=1)
    legend(120,6,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
    legend(120,4.7,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
    dev.off()
    
    #dry plots
    pdf(file=paste(fig_dir,"/lai_",year,"_dry_",ipert,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5)
    plot(dry_amb$DOY, dry_amb$LAI, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
         xlab="Day of year",ylab="Leaf area index (m2/m2)",main=ttitle)
    points(dry_amb_obs$DOY, dry_amb_obs$LAI_OBS, pch=20, cex=1.25, col="red")
    for (jm in 1:nrow(dry_amb_obs)) {lines(x=c(dry_amb_obs$DOY[jm],dry_amb_obs$DOY[jm]),y=c(dry_amb_obs$LAI_MIN[jm],dry_amb_obs$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
    lines(dry_face$DOY, dry_face$LAI, lty=2, lwd=1.25)
    points(dry_face_obs$DOY, dry_face_obs$LAI_OBS, pch=21, cex=1, col="red")
    for (jm in 1:nrow(dry_face_obs)) {lines(x=c(dry_face_obs$DOY[jm],dry_face_obs$DOY[jm]),y=c(dry_face_obs$LAI_MIN[jm],dry_face_obs$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
    grid(lwd=1)
    legend(120,6,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
    legend(120,4.7,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
    dev.off()
    
    
    ### PESW
    #wet plots
    pdf(file=paste(fig_dir,"/swater_",year,"_wet_",ipert,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5)
    plot(wet_amb$DOY, wet_amb$PESW*10, ty="l", xlim=c(120,290), ylim=c(0,120),lwd=1.25,
         xlab="Day of year",ylab="Available soil water (mm)",main=ttitle)
    points(wet_amb_swat$DOY, wet_amb_swat$SW_mm, pch=20, cex=1.05, col="red")
    lines(wet_face$DOY, wet_face$PESW*10, lty=2, lwd=1.25)
    points(wet_face_swat$DOY, wet_face_swat$SW_mm, pch=21, cex=1.05, col="red")
    grid(lwd=1)
    legend(120,47.5,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
    legend(120,25,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
    dev.off()
    
    #dry plots
    pdf(file=paste(fig_dir,"/swater_",year,"_dry_pert_",ipert,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
    par(mar=c(5,5,1,1), lwd=1.5)
    plot(dry_amb$DOY, dry_amb$PESW*10, ty="l", xlim=c(120,290), ylim=c(0,120),lwd=1.25,
         xlab="Day of year",ylab="Available soil water (mm)",main=ttitle)
    points(dry_amb_swat$DOY, dry_amb_swat$SW_mm, pch=20, cex=1.05, col="red")
    lines(dry_face$DOY, dry_face$PESW*10, lty=2, lwd=1.25)
    points(dry_face_swat$DOY, dry_face_swat$SW_mm, pch=21, cex=1.05, col="red")
    grid(lwd=1)
    legend(120,47.5,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
    legend(120,25,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
    dev.off()
  }
}

write.csv(rmse_df, paste(out_dir,"/rmse_all_treatments_perturbations.csv",sep=""), row.names=F, quote=T)

###
#calculate gain in RMSE for each perturbation
rmse_orig <- read.csv(paste(runs_dir,"/amb_face_eval_plots_full_obs/rmse_all_treatments.csv",sep=""))
#rmse_orig <- aggregate(rmse_orig[,c(3:ncol(rmse_orig))], by=list(TREATMENT=rmse_orig$TREATMENT), FUN=function(x) {mean(x,na.rm=T)})

rmse_impr <- read.csv(paste(out_dir,"/rmse_all_treatments_perturbations.csv",sep=""))
#rmse_impr <- aggregate(rmse_impr[,c(4:ncol(rmse_impr))], by=list(PERTURBATION=rmse_impr$PERTURBATION,TREATMENT=rmse_impr$TREATMENT), FUN=function(x) {mean(x,na.rm=T)})

names(rmse_orig)[3:ncol(rmse_orig)] <- paste("ORIG_",names(rmse_orig)[3:ncol(rmse_orig)],sep="")
names(rmse_impr)[4:ncol(rmse_impr)] <- paste("IMPR_",names(rmse_impr)[4:ncol(rmse_impr)],sep="")

rmse_all <- merge(rmse_impr, rmse_orig, by=c("TREATMENT","YEAR"))
#rmse_all <- rmse_all[order(rmse_all$PERTURBATION),]

rmse_all$GAIN_LAI <- (rmse_all$IMPR_LAI - rmse_all$ORIG_LAI) / rmse_all$ORIG_LAI
rmse_all$GAIN_BIOMASS <- (rmse_all$IMPR_BIOMASS - rmse_all$ORIG_BIOMASS) / rmse_all$ORIG_BIOMASS
rmse_all$GAIN_YIELD <- (rmse_all$IMPR_YIELD - rmse_all$ORIG_YIELD) / rmse_all$ORIG_YIELD
rmse_all$GAIN_SWATER <- (rmse_all$IMPR_SWATER - rmse_all$ORIG_SWATER) / rmse_all$ORIG_SWATER

#only for face treatments since i am only performing the modifications on these
wet_face_2007 <- rmse_all[which(rmse_all$TREATMENT == "wet_face" & rmse_all$YEAR == 2007),]
wet_face_2007 <- wet_face_2007[order(wet_face_2007$PERTURBATION),]
wet_face_2008 <- rmse_all[which(rmse_all$TREATMENT == "wet_face" & rmse_all$YEAR == 2008),]
wet_face_2008 <- wet_face_2008[order(wet_face_2008$PERTURBATION),]

dry_face_2007 <- rmse_all[which(rmse_all$TREATMENT == "dry_face" & rmse_all$YEAR == 2007),]
dry_face_2007 <- dry_face_2007[order(dry_face_2007$PERTURBATION),]
dry_face_2008 <- rmse_all[which(rmse_all$TREATMENT == "dry_face" & rmse_all$YEAR == 2008),]
dry_face_2008 <- dry_face_2008[order(dry_face_2008$PERTURBATION),]

# barplot(wet_face$GAIN_LAI, names.arg=wet_face$PERTURBATION, xlim=c(-15,15), horiz=T); box(); grid()
# barplot(wet_face$GAIN_BIOMASS, names.arg=wet_face$PERTURBATION, xlim=c(-10,10), horiz=T); box(); grid()
# barplot(wet_face$GAIN_YIELD, names.arg=wet_face$PERTURBATION, xlim=c(-10,10), horiz=T); box(); grid()
# barplot(wet_face$GAIN_SWATER, names.arg=wet_face$PERTURBATION, xlim=c(-10,10), horiz=T); box(); grid()
# 
# barplot(dry_face$GAIN_LAI, names.arg=dry_face$PERTURBATION, xlim=c(-10,10), horiz=T); box(); grid()
# barplot(dry_face$GAIN_BIOMASS, names.arg=dry_face$PERTURBATION, xlim=c(-10,10), horiz=T); box(); grid()
# barplot(dry_face$GAIN_YIELD, names.arg=dry_face$PERTURBATION, xlim=c(-10,10), horiz=T); box(); grid()
# barplot(dry_face$GAIN_SWATER, names.arg=dry_face$PERTURBATION, xlim=c(-15,15), horiz=T); box(); grid()

#par(las=1,mar=c(5,5,1,1))
#barplot(wet_face$GAIN_ALL, names.arg=wet_face$PERTURBATION, xlim=c(-5,5), horiz=T); box(); grid()
#barplot(dry_face$GAIN_ALL, names.arg=dry_face$PERTURBATION, xlim=c(-5,5), horiz=T); box(); grid()

gain_all <- (wet_face_2007$GAIN_SWATER + wet_face_2008$GAIN_SWATER + dry_face_2007$GAIN_SWATER + 
               dry_face_2008$GAIN_SWATER + dry_face_2008$GAIN_LAI + dry_face_2008$GAIN_BIOMASS +
               dry_face_2008$GAIN_YIELD) / 7
par(las=1,mar=c(5,5,1,1))
barplot(gain_all, names.arg=1:length(photo_seq), xlim=c(-.5,.5), horiz=T,
        xlab="Original minus improved mean relative RMSE (%)", 
        ylab="Parameter perturbation")
box(); grid()


### selected perturbation: 43
#plot each perturbation and calculate rmse for each perturbation and all treatments
rep_dir <- paste(out_dir,"/reporting",sep="")
if (!file.exists(rep_dir)) {dir.create(rep_dir)}

ipert <- 43
cat("...processing perturbation=",ipert,"\n")

#plot per year
for (year in 2007:2008) {
  #year <- 2007
  #cat("processing year=",year,"\n")
  
  #dry_face 
  load(paste(out_dir,"/dry_face_",year,"/output_perturbations.RData",sep=""))
  dry_face <- get(paste("sim_daily_",year,sep=""))
  dry_face <- dry_face[[ipert]]; rm(list=c(paste("sim_daily_",year,sep=""),paste("sim_seas_",year,sep="")))
  write.table(dry_face,paste(rep_dir,"/dry_face_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  
  #dry_amb
  load(paste(out_dir,"/dry_amb_",year,"/output.RData",sep=""))
  dry_amb <- daily_out; rm(list=c("seas_out","daily_out","this_params"))
  write.table(dry_amb,paste(rep_dir,"/dry_amb_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  
  #wet_face
  load(paste(out_dir,"/wet_face_",year,"/output_perturbations.RData",sep=""))
  wet_face <- get(paste("sim_daily_",year,sep=""))
  wet_face <- wet_face[[ipert]]; rm(list=c(paste("sim_daily_",year,sep=""),paste("sim_seas_",year,sep="")))
  write.table(wet_face,paste(rep_dir,"/wet_face_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  
  #wet_amb
  load(paste(out_dir,"/wet_amb_",year,"/output.RData",sep=""))
  wet_amb <- daily_out; rm(list=c("seas_out","daily_out","this_params"))
  write.table(wet_amb,paste(rep_dir,"/wet_amb_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
}




