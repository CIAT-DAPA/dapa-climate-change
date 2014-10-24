#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#load the amb_face_reporting output and assess it against observations in 
#growth_data_high.txt and swater_data_high.txt. Produce:
#1. RMSE values for each variable
#2. Plots as previously produced

#source functions
#src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
#source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
#source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
#source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))

#i/o directories
wd <- "~/Leeds-work/AgMIP-maize-phase-2"
runs_dir <- paste(wd,"/model_runs/laimax_stg3_co2_parameterisation",sep="") #input dir
obs_dir <- paste(wd,"/observed",sep="")
rep_dir <- paste(runs_dir,"/amb_face_reporting",sep="")
out_dir <- paste(runs_dir, "/amb_face_eval_plots_full_obs",sep="")

if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}

#load obs data
obs_growth <- read.table(paste(obs_dir,"/growth_data_high.txt",sep=""),header=T)
obs_swater <- read.table(paste(obs_dir,"/swater_data_high.txt",sep=""),header=T)

#calculate mean response of variables of interest for reporting and plotting
#for face
for (year in 2007:2008) {
  #year <- 2007
  cat("processing year=",year,"\n")
    
  #dry_face
  dry_face <- read.table(paste(rep_dir,"/dry_face_",year,".txt",sep=""), header=T)
  
  #dry_amb
  dry_amb <- read.table(paste(rep_dir,"/dry_amb_",year,".txt",sep=""), header=T)
  
  #wet_face
  wet_face <- read.table(paste(rep_dir,"/wet_face_",year,".txt",sep=""), header=T)
  
  #wet_amb
  wet_amb <- read.table(paste(rep_dir,"/wet_amb_",year,".txt",sep=""), header=T)
  
  
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




