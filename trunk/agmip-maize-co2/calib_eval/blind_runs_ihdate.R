#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#get best parameter set, use that one to run the two FACE WET simulations (2007, 2008)
#and the four DRY treatments: DRY_AMB, DRY_FACE * (2007, 2008)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))

#i/o directories
wd <- "~/Leeds-work/AgMIP-maize-phase-2"
runs_dir <- paste(wd,"/model_runs/laimax_stg3_blind_ihdate",sep="") #to be changed
wth_dir <- paste(wd,"/weather",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c",sep="")
par_dir <- paste(wd,"/parameters",sep="")
obs_dir <- paste(wd,"/observed",sep="")

if (!file.exists(runs_dir)) {dir.create(runs_dir,recursive=T)}

#list of seeds
set.seed(5829) #fixed seed to make it replicable
seed_list <- c(0,round(runif(50, 1000, 9999),0))

#list of skilful seeds
seed_skill <- data.frame()
for (seed in seed_list) {
  #seed <- seed_list[1]
  skill <- read.csv(paste(wd,"/model_runs/laimax_stg3_optim/optim_seed-",seed,"/optimum_values.csv",sep=""))
  seed_skill <- rbind(seed_skill, data.frame(SEED=seed,MEAN_ERR=mean(skill$MEAN_ERR)))
  rm(skill)
}
seed_best <- seed_skill$SEED[which(seed_skill$MEAN_ERR == min(seed_skill$MEAN_ERR))]

#get the parameter file for this run (target object: "best_params")
load(file=paste(wd,"/model_runs/laimax_stg3_optim/optim_seed-",seed_best,"/optimal_run.RData",sep=""))
rm(list=c("daily_all","seas_all"))

###
#perform the runs as needed
#create directories for runs
rd_wet_amb2007 <- create_dirs(glam_dir=paste(runs_dir,"/wet_amb_2007",sep=""))
rd_wet_amb2008 <- create_dirs(glam_dir=paste(runs_dir,"/wet_amb_2008",sep=""))
rd_wet_face2007 <- create_dirs(glam_dir=paste(runs_dir,"/wet_face_2007",sep=""))
rd_wet_face2008 <- create_dirs(glam_dir=paste(runs_dir,"/wet_face_2008",sep=""))
rd_dry_amb2007 <- create_dirs(glam_dir=paste(runs_dir,"/dry_amb_2007",sep=""))
rd_dry_amb2008 <- create_dirs(glam_dir=paste(runs_dir,"/dry_amb_2008",sep=""))
rd_dry_face2007 <- create_dirs(glam_dir=paste(runs_dir,"/dry_face_2007",sep=""))
rd_dry_face2008 <- create_dirs(glam_dir=paste(runs_dir,"/dry_face_2008",sep=""))

#df of all blind simulations
blindsim_df <- data.frame(WTH=c("dry","dry","wet","wet"),CO2=c("amb","face","amb","face"))

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


#run all simulations
#loop through all simulations to prepare inputs
for (i in 1:nrow(blindsim_df)) {
  #i <- 2
  wth <- paste(blindsim_df$WTH[i])
  co2 <- paste(blindsim_df$CO2[i])
  
  cat("processing condition=",wth,"and co2=",co2,"\n")
  
  #get parameter set
  this_params <- best_params
  
  #if FACE then i need to do some modifications
  #nothing in the literature about SLA --also, due to issues in meaning of SLA in GLAM
  #i avoided changing SLA for FACE runs
  if (co2 == "face") {
    this_params$glam_param.sim_ctr$IC02 <- 1
    this_params$glam_param.hts_fut$B_TE$Value <- this_params$glam_param.bmass$TE$Value
    this_params$glam_param.hts_fut$B_TEN_MAX$Value <- this_params$glam_param.bmass$TEN_MAX$Value
    this_params$glam_param.hts_fut$TENFAC$Value <- 0 #Tfac=0 for all FACE runs, C4 crop
    this_params$glam_param.bmass$TE$Value <- this_params$glam_param.bmass$TE$Value * 1.23 #WUE increased by 23$ @ 550 ppm, literature
    this_params$glam_param.bmass$TEN_MAX$Value <- (1-this_params$glam_param.hts_fut$TENFAC$Value) * this_params$glam_param.hts_fut$B_TEN_MAX$Value + this_params$glam_param.hts_fut$TENFAC$Value * this_params$glam_param.bmass$TE$Value * (this_params$glam_param.hts_fut$B_TEN_MAX$Value / this_params$glam_param.hts_fut$B_TE$Value)
    this_params$glam_param.evap$P_TRANS_MAX$Value <- this_params$glam_param.evap$P_TRANS_MAX$Value * (1+(-0.22 * (550-370)/(750-370))) #Kim et al. (2006) suggest 22 % reduction @ 750 ppm (baseline 370ppm), linear scaling applied
  }
  
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
      
      #plot(daily_out$DOY,daily_out$BIOMASS,ty="l",ylim=c(0,25000))
      #lines(opt_2007$DOY,opt_2007$BIOMASS,col="red")
      save(list=c("seas_out","daily_out","this_params"),file=paste(run_dir,"/output.RData",sep=""))
    } else {
      load(file=paste(run_dir,"/output.RData",sep=""))
    }
  }
}

#figure dir
fig_dir <- paste(runs_dir,"/amb_face_figs",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir)}

#reporting dir
rep_dir <- paste(runs_dir,"/amb_face_reporting",sep="")
if (!file.exists(rep_dir)) {dir.create(rep_dir)}

#load and plot all model runs
for (year in 2007:2008) {
  #year <- 2007
  
  #load optimised run (WET_AMB)
  load(file=paste(runs_dir,"/wet_amb_",year,"/output.RData",sep=""))
  wet_amb <- daily_out
  rm(list=c("seas_out","this_params"))
  #wet_amb <- get(paste("opt_",year,sep=""))
  
  #load WET_FACE
  load(file=paste(runs_dir,"/wet_face_",year,"/output.RData",sep=""))
  wet_face <- daily_out
  rm(list=c("seas_out","this_params"))
  
  #load DRY_AMB
  load(file=paste(runs_dir,"/dry_amb_",year,"/output.RData",sep=""))
  dry_amb <- daily_out
  rm(list=c("seas_out","this_params"))
  
  #load DRY_FACE
  load(file=paste(runs_dir,"/dry_face_",year,"/output.RData",sep=""))
  dry_face <- daily_out
  rm(list=c("seas_out","this_params"))
  
  #write data for reporting back
  write.table(wet_amb,paste(rep_dir,"/wet_amb_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  write.table(wet_face,paste(rep_dir,"/wet_face_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  write.table(dry_amb,paste(rep_dir,"/dry_amb_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  write.table(dry_face,paste(rep_dir,"/dry_face_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
  
  #produce plot (BIOMASS and YIELD)
  pdf(file=paste(fig_dir,"/biomass_yield_",year,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(wet_amb$DOY, wet_amb$BIOMASS*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or yield (t/ha)")
  lines(wet_face$DOY, wet_face$BIOMASS*.001, lty=2, lwd=1.25)
  lines(dry_amb$DOY, dry_amb$BIOMASS*.001, lty=1, col="red", lwd=1.25)
  lines(dry_face$DOY, dry_face$BIOMASS*.001, lty=2, col="red", lwd=1.25)
  lines(wet_amb$DOY, wet_amb$YIELD*.001, lty=1, lwd=1.25)
  lines(wet_face$DOY, wet_face$YIELD*.001, lty=2, lwd=1.25)
  lines(dry_amb$DOY, dry_amb$YIELD*.001, lty=1, col="red", lwd=1.25)
  lines(dry_face$DOY, dry_face$YIELD*.001, lty=2, col="red", lwd=1.25)
  grid()
  legend(120,25,legend=c("WET_AMB","WET_FACE","DRY_AMB","DRY_FACE"),bg="white",cex=0.75,
         lty=c(1,2,1,2),col=c("black","black","red","red"))
  dev.off()
  
  #produce plot (LAI)
  pdf(file=paste(fig_dir,"/lai_",year,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(wet_amb$DOY, wet_amb$LAI, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
       xlab="Day of year",ylab="Leaf area index (m2/m2)")
  lines(wet_face$DOY, wet_face$LAI, lty=2, lwd=1.25)
  lines(dry_amb$DOY, dry_amb$LAI, lty=1, col="red", lwd=1.25)
  lines(dry_face$DOY, dry_face$LAI, lty=2, col="red", lwd=1.25)
  grid()
  legend(120,6,legend=c("WET_AMB","WET_FACE","DRY_AMB","DRY_FACE"),bg="white",cex=0.75,
         lty=c(1,2,1,2),col=c("black","black","red","red"))
  dev.off()
  
  #produce plot (PESW)
  pdf(file=paste(fig_dir,"/swater_",year,".pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(wet_amb$DOY, wet_amb$PESW*10, ty="l", xlim=c(120,290), ylim=c(0,110),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  lines(wet_face$DOY, wet_face$PESW*10, lty=2, lwd=1.25)
  lines(dry_amb$DOY, dry_amb$PESW*10, lty=1, col="red", lwd=1.25)
  lines(dry_face$DOY, dry_face$PESW*10, lty=2, col="red", lwd=1.25)
  grid()
  legend(120,30,legend=c("WET_AMB","WET_FACE","DRY_AMB","DRY_FACE"),bg="white",cex=0.75,
         lty=c(1,2,1,2),col=c("black","black","red","red"))
  dev.off()
}



