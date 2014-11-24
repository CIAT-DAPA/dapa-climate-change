#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#goes with optimise_v4.R

#get best parameter set, use that one to run the two FACE WET simulations (2007, 2008)
#and the four DRY treatments: DRY_AMB, DRY_FACE * (2007, 2008)

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

if (!file.exists(runs_dir)) {dir.create(runs_dir,recursive=T)}

#figure dir
fig_dir <- paste(runs_dir,"/amb_face_figs",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir)}

#reporting dir
rep_dir <- paste(runs_dir,"/amb_face_reporting",sep="")
if (!file.exists(rep_dir)) {dir.create(rep_dir)}

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

#list of 1000 seeds for replicability
base_seed <- 4627
set.seed(base_seed)
seed_list <- round(runif(1000, 1000, 9999))

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


#run all simulations
#loop through all simulations to prepare inputs
for (i in 1:nrow(blindsim_df)) {
  #i <- 2
  wth <- paste(blindsim_df$WTH[i])
  co2 <- paste(blindsim_df$CO2[i])
  
  cat("processing condition=",wth,"and co2=",co2,"\n")
  
  #only if CO2=face
  if (co2 == "face") {
    if (!file.exists(paste(runs_dir,"/",wth,"_",co2,"_2008/output_perturbations.RData",sep=""))) {
      sim_daily <- list(sim_2007=list(), sim_2008=list())
      sim_seas <- list(sim_2007=list(), sim_2008=list())
      for (ipert in 1:length(seed_list)) {
        #ipert <- 1
        if (ipert%%10 == 0 | ipert == 1) {cat("...processing perturbation=",ipert,"\n")}
        
        #get parameter set
        this_params <- best_params
        
        #baseline parameters
        this_params$glam_param.sim_ctr$IC02 <- 1
        this_params$glam_param.hts_fut$B_TE$Value <- this_params$glam_param.bmass$TE$Value
        this_params$glam_param.hts_fut$B_TEN_MAX$Value <- this_params$glam_param.bmass$TEN_MAX$Value
        
        #Tfac=0 for all FACE runs, C4 crop
        this_params$glam_param.hts_fut$TENFAC$Value <- 0 
        
        #stimulated parameters (TE and P_TRANS_MAX)
        set.seed(seed_list[ipert]); te_fac <- runif(1, photo_min, photo_max)
        set.seed(seed_list[ipert]); tr_fac <- runif(1, trans_min, trans_max)
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
      save(list=c("sim_seas_2007","sim_daily_2007"),file=paste(runs_dir,"/",wth,"_",co2,"_2007/output_perturbations.RData",sep=""))
      save(list=c("sim_seas_2008","sim_daily_2008"),file=paste(runs_dir,"/",wth,"_",co2,"_2008/output_perturbations.RData",sep=""))
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


#calculate factors from seasonal files and assess the output against biomass and yield data
#for both wet and dry

##### dry runs
#load the seasonal output from face perturbations
load(file=paste(runs_dir,"/dry_face_2007/output_perturbations.RData",sep=""))
load(file=paste(runs_dir,"/dry_face_2008/output_perturbations.RData",sep=""))
rm(list=c("sim_daily_2007","sim_daily_2008"))

dry_f2007 <- do.call(rbind, sim_seas_2007)
dry_f2007 <- cbind(PERT=1:length(seed_list), dry_f2007)

dry_f2008 <- do.call(rbind, sim_seas_2008)
dry_f2008 <- cbind(PERT=1:length(seed_list), dry_f2008)

rm(list=c("sim_seas_2007","sim_seas_2008"))

#load the ambient output
load(file=paste(runs_dir,"/dry_amb_2007/output.RData",sep=""))
dry_a2007 <- seas_out
dry_a2007_day <- daily_out
rm(daily_out)

load(file=paste(runs_dir,"/dry_amb_2008/output.RData",sep=""))
dry_a2008 <- seas_out
dry_a2008_day <- daily_out
rm(daily_out)

#calculate yield and biomass response in simulations
ydry_rr2007 <- dry_f2007$YIELD / dry_a2007$YIELD
ydry_rr2008 <- dry_f2008$YIELD / dry_a2008$YIELD

bdry_rr2007 <- dry_f2007$BMASS / dry_a2007$BMASS
bdry_rr2008 <- dry_f2008$BMASS / dry_a2008$BMASS

dry_swf <- data.frame(Y2007=dry_f2007$SWFAC_MEAN,Y2008=dry_f2008$SWFAC_MEAN)
dry_vpd <- data.frame(Y2007=dry_f2007$VPDTOT,Y2008=dry_f2008$VPDTOT)

#RR observations
ydry_min <- rratios$q05[which(rratios$property == "yield" & rratios$cond == "dry")]
ydry_max <- rratios$q95[which(rratios$property == "yield" & rratios$cond == "dry")]
bdry_min <- rratios$q05[which(rratios$property == "biomass" & rratios$cond == "dry")]
bdry_max <- rratios$q95[which(rratios$property == "biomass" & rratios$cond == "dry")]
ydry_min <- min(c(ydry_min,bdry_min))
bdry_min <- min(c(ydry_min,bdry_min))

##### wet runs
#load the seasonal output from face perturbations
load(file=paste(runs_dir,"/wet_face_2007/output_perturbations.RData",sep=""))
load(file=paste(runs_dir,"/wet_face_2008/output_perturbations.RData",sep=""))
rm(list=c("sim_daily_2007","sim_daily_2008"))

wet_f2007 <- do.call(rbind, sim_seas_2007)
wet_f2007 <- cbind(PERT=1:length(seed_list), wet_f2007)

wet_f2008 <- do.call(rbind, sim_seas_2008)
wet_f2008 <- cbind(PERT=1:length(seed_list), wet_f2008)

rm(list=c("sim_seas_2007","sim_seas_2008"))

#load the ambient output
load(file=paste(runs_dir,"/wet_amb_2007/output.RData",sep=""))
wet_a2007 <- seas_out
wet_a2007_day <- daily_out
rm(daily_out)

load(file=paste(runs_dir,"/wet_amb_2008/output.RData",sep=""))
wet_a2008 <- seas_out
wet_a2008_day <- daily_out
rm(daily_out)

#calculate yield and biomass response in simulations
ywet_rr2007 <- wet_f2007$YIELD / wet_a2007$YIELD
ywet_rr2008 <- wet_f2008$YIELD / wet_a2008$YIELD

bwet_rr2007 <- wet_f2007$BMASS / wet_a2007$BMASS
bwet_rr2008 <- wet_f2008$BMASS / wet_a2008$BMASS

wet_swf <- data.frame(Y2007=wet_f2007$SWFAC_MEAN,Y2008=wet_f2008$SWFAC_MEAN)
wet_vpd <- data.frame(Y2007=wet_f2007$VPDTOT,Y2008=wet_f2008$VPDTOT)

#RR observations
ywet_min <- rratios$q05[which(rratios$property == "yield" & rratios$cond == "wet")]
ywet_max <- rratios$q95[which(rratios$property == "yield" & rratios$cond == "wet")]
bwet_min <- rratios$q05[which(rratios$property == "biomass" & rratios$cond == "wet")]
bwet_max <- rratios$q95[which(rratios$property == "biomass" & rratios$cond == "wet")]
ywet_min <- min(c(ywet_min,bwet_min))
bwet_min <- min(c(ywet_min,bwet_min))

###plots here (histograms)
##yield histogram
h_ydry2007 <- hist(ydry_rr2007, breaks=seq(0.94,1.30, by=0.01),plot=F)
h_ydry2008 <- hist(ydry_rr2008, breaks=seq(0.94,1.30, by=0.01),plot=F)
h_ywet2007 <- hist(ywet_rr2007, breaks=seq(0.94,1.30, by=0.01),plot=F)
h_ywet2008 <- hist(ywet_rr2008, breaks=seq(0.94,1.30, by=0.01),plot=F)

pdf(paste(fig_dir,"/freq_line_RR_yield.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),lwd=1.25)
plot(h_ydry2007$mids, h_ydry2007$counts/sum(h_ydry2007$counts), ty="l", xlim=c(0.95, 1.25), 
     ylim=c(0,0.40), xlab="Response ratio", ylab="Fractional count", col="red")
lines(h_ydry2008$mids, h_ydry2008$counts/sum(h_ydry2008$counts),lty=2, col="red")
lines(h_ywet2007$mids, h_ywet2007$counts/sum(h_ywet2007$counts),lty=1, col="blue")
lines(h_ywet2008$mids, h_ywet2008$counts/sum(h_ywet2008$counts),lty=2, col="blue")
abline(v=1, lty=2)
polygon(x=c(ydry_min,ydry_min,ydry_max,ydry_max),y=c(0,1,1,0),
        col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
        border=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255))
polygon(x=c(ywet_min,ywet_min,ywet_max,ywet_max),y=c(0,1,1,0),
        col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
        border=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255))
grid()
legend(1.18,0.4,legend=c("WET_2007","WET_2008","DRY_2007","DRY_2008"),bg="white",cex=0.75,
       lty=c(1,2,1,2),col=c("blue","blue","red","red"))
dev.off()

##biomass histogram
h_bdry2007 <- hist(bdry_rr2007, breaks=seq(0.94,1.30, by=0.01),plot=F)
h_bdry2008 <- hist(bdry_rr2008, breaks=seq(0.94,1.30, by=0.01),plot=F)
h_bwet2007 <- hist(bwet_rr2007, breaks=seq(0.94,1.30, by=0.01),plot=F)
h_bwet2008 <- hist(bwet_rr2008, breaks=seq(0.94,1.30, by=0.01),plot=F)

pdf(paste(fig_dir,"/freq_line_RR_biomass.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),lwd=1.25)
plot(h_bdry2007$mids, h_bdry2007$counts/sum(h_bdry2007$counts), ty="l", xlim=c(0.95, 1.25), 
     ylim=c(0,0.4), xlab="Response ratio", ylab="Fractional count",col="red")
lines(h_bdry2008$mids, h_bdry2008$counts/sum(h_bdry2008$counts),lty=2, col="red")
lines(h_bwet2007$mids, h_bwet2007$counts/sum(h_bwet2007$counts),lty=1, col="blue")
lines(h_bwet2008$mids, h_bwet2008$counts/sum(h_bwet2008$counts),lty=2, col="blue")
abline(v=1, lty=2)
polygon(x=c(bdry_min,bdry_min,bdry_max,bdry_max),y=c(0,1,1,0),
        col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
        border=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255))
polygon(x=c(bwet_min,bwet_min,bwet_max,bwet_max),y=c(0,1,1,0),
        col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
        border=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255))
grid()
legend(1.18,0.4,legend=c("WET_2007","WET_2008","DRY_2007","DRY_2008"),bg="white",cex=0.75,
       lty=c(1,2,1,2),col=c("blue","blue","red","red"))
dev.off()

####
#report response ratios for biomass and yield, both years, dry and wet
#wet
plotdf <- data.frame(year=2007,param="biomass",rr=bwet_rr2007)
plotdf <- rbind(plotdf, data.frame(year=2008,param="biomass",rr=bwet_rr2008))
plotdf <- rbind(plotdf, data.frame(year=2007,param="yield",rr=ywet_rr2007))
plotdf <- rbind(plotdf, data.frame(year=2008,param="yield",rr=ywet_rr2008))

pdf(paste(fig_dir,"/boxplot_RR_wet.pdf",sep=""), height=6,width=6,pointsize=16)
par(mar=c(3,5,1,1))
boxplot(plotdf$rr ~ plotdf$year*plotdf$param,ylab="Response ratio",pch=20,ylim=c(0.95,1.08),
        horizontal=F,outcol="red",medcol="red",boxcol="blue",col="white",border="black",las=2,axes=F)
axis(1, at=c(1:4), labels=c("2007","2008","2007","2008"), pos=NA, lty=1, las=1)
axis(2, at=seq(0.90,1.10,by=0.02), labels=sprintf("%.2f",seq(0.90,1.10,by=0.02)), pos=NA, lty=1, las=1)
abline(h=1); abline(v=2.5)
box()
grid()
polygon(x=c(0,2.5,2.5,0),y=c(bwet_min,bwet_min,bwet_max,bwet_max),lwd=1.5,
        col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
        border=rgb(red=0,green=0,blue=255,alpha=100,maxColorValue=255))
polygon(x=c(2.5,5,5,2.5),y=c(ywet_min,ywet_min,ywet_max,ywet_max),lwd=1.5,
        col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
        border=rgb(red=0,green=0,blue=255,alpha=100,maxColorValue=255))
text(1.5, 1.075, "Biomass"); text(3.5, 1.075, "Yield")
dev.off()


#dry
plotdf <- data.frame(year=2007,param="biomass",rr=bdry_rr2007)
plotdf <- rbind(plotdf, data.frame(year=2008,param="biomass",rr=bdry_rr2008))
plotdf <- rbind(plotdf, data.frame(year=2007,param="yield",rr=ydry_rr2007))
plotdf <- rbind(plotdf, data.frame(year=2008,param="yield",rr=ydry_rr2008))

pdf(paste(fig_dir,"/boxplot_RR_dry.pdf",sep=""), height=6,width=6,pointsize=16)
par(mar=c(3,5,1,1))
boxplot(plotdf$rr ~ plotdf$year*plotdf$param,ylab="Response ratio",pch=20,ylim=c(0.95,1.26),
        horizontal=F,outcol="red",medcol="red",boxcol="blue",col="white",border="black",las=2,axes=F)
axis(1, at=c(1:4), labels=c("2007","2008","2007","2008"), pos=NA, lty=1, las=1)
axis(2, at=seq(0.90,1.30,by=0.04), labels=sprintf("%.2f",seq(0.90,1.30,by=0.04)), pos=NA, lty=1, las=1)
abline(h=1); abline(v=2.5)
box()
grid()
polygon(x=c(0,2.5,2.5,0),y=c(bdry_min,bdry_min,bdry_max,bdry_max),lwd=1.5,
        col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
        border=rgb(red=255,green=0,blue=0,alpha=100,maxColorValue=255))
polygon(x=c(2.5,5,5,2.5),y=c(ydry_min,ydry_min,ydry_max,ydry_max),lwd=1.5,
        col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
        border=rgb(red=255,green=0,blue=0,alpha=100,maxColorValue=255))
text(1.5, 1.255, "Biomass"); text(3.5, 1.255, "Yield")
dev.off()


#### get seeds and factors corresponding to rr_wet==1
sel_seeds0 <- seed_list[which(bdry_rr2007 >= 0 & bdry_rr2008 >= 0)]
sel_seeds1 <- seed_list[which(bwet_rr2007 >= 0.999 & bwet_rr2007 <= 1.001)]
sel_seeds2 <- seed_list[which(bwet_rr2008 >= 0.999 & bwet_rr2008 <= 1.001)]
sel_seeds <- unique(c(sel_seeds0,sel_seeds1,sel_seeds2)[duplicated(c(sel_seeds0,sel_seeds1,sel_seeds2))])

te_fact <- c(); tr_fact <- c()
for (seed in sel_seeds) {
  set.seed(seed); te_fac <- runif(1, photo_min, photo_max); te_fact <- c(te_fact, te_fac)
  set.seed(seed); tr_fac <- runif(1, trans_min, trans_max); tr_fact <- c(tr_fact, tr_fac)
}

#plot factors
plotdf <- data.frame(param="TE",rr=te_fact)
plotdf <- rbind(plotdf, data.frame(param="TTmax_p",rr=tr_fact))

pdf(paste(fig_dir,"/boxplot_RR_parameters.pdf",sep=""), height=6,width=4,pointsize=16)
par(mar=c(3,5,1,1))
boxplot(plotdf$rr ~ plotdf$param,ylab="Response ratio",pch=20,ylim=c(0.80,1.25),
        horizontal=F,outcol="red",medcol="red",boxcol="blue",col="white",border="black",las=1,axes=T)
abline(h=1)
box()
grid()
dev.off()


#calculate mean response of variables of interest for reporting and plotting
#for face
for (year in 2007:2008) {
  #year <- 2007
  for (wth in c("dry","wet")) {
    #wth <- "dry"
    cat("processing wth=",wth,"and year=",year,"\n")
    
    #face output
    load(file=paste(runs_dir,"/",wth,"_face_",year,"/output_perturbations.RData",sep=""))
    rm(list=paste("sim_seas_",year,sep=""))
    daily_df <- get(paste("sim_daily_",year,sep=""))
    daily_df <- daily_df[which(seed_list %in% sel_seeds)]
    daily_df <- lapply(1:length(daily_df), FUN=function(i) cbind(pert=i,daily_df[[i]]))
    daily_df <- do.call(rbind, daily_df)
    daily_df <- aggregate(daily_df[,c(4:ncol(daily_df))], by=list(YEAR=daily_df$YEAR, DOY=daily_df$DOY), FUN=function(x) {mean(x,na.rm=T)})
    write.table(daily_df,paste(rep_dir,"/",wth,"_face_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
    
    #assign output
    assign(paste(wth,"_face",sep=""),daily_df); rm(daily_df)
    
    #amb output
    load(file=paste(runs_dir,"/",wth,"_amb_",year,"/output.RData",sep=""))
    write.table(daily_out,paste(rep_dir,"/",wth,"_amb_",year,".txt",sep=""),sep="\t",quote=F,row.names=F)
    assign(paste(wth,"_amb",sep=""),daily_out)
    rm(list=c("seas_out","this_params","daily_out"))
  }
  
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





