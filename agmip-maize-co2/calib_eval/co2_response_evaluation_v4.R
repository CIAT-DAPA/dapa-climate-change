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
runs_dir <- paste(wd,"/model_runs/laimax_stg3_co2_parameterisation_v4",sep="") #input dir
obs_dir <- paste(wd,"/observed",sep="")
rep_dir <- paste(runs_dir,"/amb_face_reporting",sep="")
out_dir <- paste(runs_dir, "/amb_face_eval_plots_full_obs",sep="")

if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}

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

#plot and rmse calculation per year
rmse_df <- data.frame()
for (year in 2007:2008) {
  #year <- 2007
  cat("processing year=",year,"\n")
  
  #dry_face
  dry_face <- read.table(paste(rep_dir,"/dry_face_",year,".txt",sep=""), header=T)
  dry_face_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "DRY_ELE"),]
  dry_face_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "DRY_ELE"),]
  
  #dry_amb
  dry_amb <- read.table(paste(rep_dir,"/dry_amb_",year,".txt",sep=""), header=T)
  dry_amb_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "DRY_AMB"),]
  dry_amb_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "DRY_AMB"),]
  
  #wet_face
  wet_face <- read.table(paste(rep_dir,"/wet_face_",year,".txt",sep=""), header=T)
  wet_face_obs <- growth_data[which(growth_data$YEAR == year & growth_data$TREATMENT == "WET_ELE"),]
  wet_face_swat <- swater_data[which(swater_data$YEAR == year & swater_data$TREATMENT == "WET_ELE"),]
  
  #wet_amb
  wet_amb <- read.table(paste(rep_dir,"/wet_amb_",year,".txt",sep=""), header=T)
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
    out_row <- data.frame(TREATMENT=treat,YEAR=year, LAI=lai_rmse, BIOMASS=bms_rmse, YIELD=yld_rmse, SWATER=swt_rmse)
    rmse_df <- rbind(rmse_df, out_row)
  }
  
  ### biomass and yield 
  #wet plots
  pdf(file=paste(out_dir,"/biomass_yield_",year,"_wet.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(wet_amb$DOY, wet_amb$BIOMASS*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or Yield (t/ha)")
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
  pdf(file=paste(out_dir,"/biomass_yield_",year,"_dry.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(dry_amb$DOY, dry_amb$BIOMASS*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or Yield (t/ha)")
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
  pdf(file=paste(out_dir,"/lai_",year,"_wet.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(wet_amb$DOY, wet_amb$LAI, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
       xlab="Day of year",ylab="Leaf area index (m2/m2)")
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
  pdf(file=paste(out_dir,"/lai_",year,"_dry.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(dry_amb$DOY, dry_amb$LAI, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
       xlab="Day of year",ylab="Leaf area index (m2/m2)")
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
  pdf(file=paste(out_dir,"/swater_",year,"_wet.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(wet_amb$DOY, wet_amb$PESW*10, ty="l", xlim=c(120,290), ylim=c(0,120),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  points(wet_amb_swat$DOY, wet_amb_swat$SW_mm, pch=20, cex=1.05, col="red")
  lines(wet_face$DOY, wet_face$PESW*10, lty=2, lwd=1.25)
  points(wet_face_swat$DOY, wet_face_swat$SW_mm, pch=21, cex=1.05, col="red")
  grid(lwd=1)
  legend(120,47.5,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
  legend(120,25,legend=c("WET_AMB","WET_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
  dev.off()
  
  #dry plots
  pdf(file=paste(out_dir,"/swater_",year,"_dry.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5)
  plot(dry_amb$DOY, dry_amb$PESW*10, ty="l", xlim=c(120,290), ylim=c(0,120),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  points(dry_amb_swat$DOY, dry_amb_swat$SW_mm, pch=20, cex=1.05, col="red")
  lines(dry_face$DOY, dry_face$PESW*10, lty=2, lwd=1.25)
  points(dry_face_swat$DOY, dry_face_swat$SW_mm, pch=21, cex=1.05, col="red")
  grid(lwd=1)
  legend(120,47.5,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,lty=c(1,2),col=c("black","black"))
  legend(120,25,legend=c("DRY_AMB","DRY_FACE"),bg="white",cex=0.75,pch=c(20,21),col=c("red","red"))
  dev.off()
}

write.csv(rmse_df, paste(out_dir,"/rmse_all_treatments.csv",sep=""), row.names=F, quote=T)



