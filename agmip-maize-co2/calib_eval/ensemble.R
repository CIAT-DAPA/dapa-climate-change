#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#load each of the "optimal_run.RData"
#plot all individual ensemble members for all variables (YIELD, BIOMASS, LAI, PESW)
#plot only those with mean err<0.1

#i/o directories
wd <- "~/Leeds-work/AgMIP-maize-phase-2"
runs_dir <- paste(wd,"/model_runs/laimax_stg3_optim",sep="")
#runs_dir <- paste(wd,"/model_runs/laimax_stg2_optim",sep="")
wth_dir <- paste(wd,"/weather",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c",sep="")
par_dir <- paste(wd,"/parameters",sep="")
obs_dir <- paste(wd,"/observed",sep="")

#figure dir
fig_dir <- paste(runs_dir,"/ensemble_figs",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir)}

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

#list of seeds
set.seed(5829) #fixed seed to make it replicable
seed_list <- c(0,round(runif(50, 1000, 9999),0))

#list of skilful seeds
seed_skill <- data.frame()
for (seed in seed_list) {
  #seed <- seed_list[1]
  skill <- read.csv(paste(runs_dir,"/optim_seed-",seed,"/optimum_values.csv",sep=""))
  seed_skill <- rbind(seed_skill, data.frame(SEED=seed,MEAN_ERR=mean(skill$MEAN_ERR)))
  rm(skill)
}
skill_limit <- quantile(seed_skill$MEAN_ERR, probs=c(0.1))
seed_skilful <- seed_skill$SEED[which(seed_skill$MEAN_ERR <= skill_limit)]

### frequency plot of model skill across ensemble members
hist_all <- hist(seed_skill$MEAN_ERR, breaks=seq(0.07,0.16,by=0.005), plot=F)
pdf(file=paste(fig_dir,"/frequency_plot_skill.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
par(mar=c(5,5,1,1), lwd=1.5, las=1)
plot(hist_all$mids, hist_all$counts/sum(hist_all$counts)*100, ty="l", xlim=c(0.07,0.16), ylim=c(0,50),
     xlab="Mean error (normalised)", ylab="Percentage of ensemble members (%)")
grid()
dev.off()

###
#gather output from *ALL* ensemble members
for (seed in seed_list) {
  #seed <- seed_list[1]
  load(file=paste(runs_dir,"/optim_seed-",seed,"/optimal_run.RData",sep=""))
  rm(list=c("best_params","seas_all"))
  
  daily_all <- daily_all[,c("YEAR","DOY","DAP","LAI","BIOMASS","YIELD","PESW")]
  names(daily_all)[4:7] <- paste(c("LAI","BIOMASS","YIELD","PESW"),".",seed,sep="")
  
  if (seed == seed_list[1]) {
    daily_merged <- daily_all
  } else {
    daily_merged <- merge(daily_merged, daily_all, by=c("YEAR","DOY","DAP"))
  }
  rm(daily_all)
}

#ensemble means params
fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("BIOMASS.",names(daily_merged))])
bmass_all <- daily_merged[,fields]
bmass_all$MEAN <- rowMeans(bmass_all[,4:ncol(bmass_all)], na.rm=T)
bmass_all$SD <- apply(bmass_all[,(4:(ncol(bmass_all)-1))],1,FUN=function(x) {sd(x)})

fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("YIELD.",names(daily_merged))])
yield_all <- daily_merged[,fields]
yield_all$MEAN <- rowMeans(yield_all[,4:ncol(yield_all)], na.rm=T)
yield_all$SD <- apply(yield_all[,(4:(ncol(yield_all)-1))],1,FUN=function(x) {sd(x)})

fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("LAI.",names(daily_merged))])
lai_all <- daily_merged[,fields]
lai_all$MEAN <- rowMeans(lai_all[,4:ncol(lai_all)], na.rm=T)
lai_all$SD <- apply(lai_all[,(4:(ncol(lai_all)-1))],1,FUN=function(x) {sd(x)})

fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("PESW.",names(daily_merged))])
pesw_all <- daily_merged[,fields]
pesw_all$MEAN <- rowMeans(pesw_all[,4:ncol(pesw_all)], na.rm=T)
pesw_all$SD <- apply(pesw_all[,(4:(ncol(pesw_all)-1))],1,FUN=function(x) {sd(x)})

#loop for plotting individual years
for (year in 2007:2008) {
  #year <- 2007
  bmass_yr <- bmass_all[which(bmass_all$YEAR == year),]
  yield_yr <- yield_all[which(yield_all$YEAR == year),]
  lai_yr <- lai_all[which(lai_all$YEAR == year),]
  pesw_yr <- pesw_all[which(pesw_all$YEAR == year),]
  gwth_yr <- growth_data[which(growth_data$YEAR == year),]
  swat_yr <- swater_data[which(swater_data$YEAR == year),]
  
  #with individual lines
  pdf(file=paste(fig_dir,"/biomass_yield_",year,"_ensemble_lines_all.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(bmass_yr$DOY, bmass_yr$MEAN*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or Yield (t/ha)")
  for (seed in seed_list) {
    lines(bmass_yr$DOY, bmass_yr[,paste("BIOMASS.",seed,sep="")]*.001,col="grey 80",lwd=0.75)
  }
  lines(bmass_yr$DOY, bmass_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$BMASS_MIN[jm]*.001,gwth_yr$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  for (seed in seed_list) {
    lines(yield_yr$DOY, yield_yr[,paste("YIELD.",seed,sep="")]*.001,col="grey 80",lwd=0.75)
  }
  lines(yield_yr$DOY, yield_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$YIELD_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$YIELD_MIN[jm]*.001,gwth_yr$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #with shading
  pdf(file=paste(fig_dir,"/biomass_yield_",year,"_ensemble_shading_all.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(bmass_yr$DOY, bmass_yr$MEAN*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or Yield (t/ha)")
  polygon(x=c(bmass_yr$DOY, rev(bmass_yr$DOY)), y=c((bmass_yr$MEAN*0.001+bmass_yr$SD*0.002),rev((bmass_yr$MEAN*0.001-bmass_yr$SD*0.002))),col="grey 80",border="grey 80")
  polygon(x=c(bmass_yr$DOY, rev(bmass_yr$DOY)), y=c((bmass_yr$MEAN*0.001+bmass_yr$SD*0.001),rev((bmass_yr$MEAN*0.001-bmass_yr$SD*0.001))),col="grey 60",border="grey 60")
  lines(bmass_yr$DOY, bmass_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$BMASS_MIN[jm]*.001,gwth_yr$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  polygon(x=c(yield_yr$DOY, rev(yield_yr$DOY)), y=c((yield_yr$MEAN*0.001+yield_yr$SD*0.002),rev((yield_yr$MEAN*0.001-yield_yr$SD*0.002))),col="grey 80",border="grey 80")
  polygon(x=c(yield_yr$DOY, rev(yield_yr$DOY)), y=c((yield_yr$MEAN*0.001+yield_yr$SD*0.001),rev((yield_yr$MEAN*0.001-yield_yr$SD*0.001))),col="grey 60",border="grey 60")
  lines(yield_yr$DOY, yield_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$YIELD_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$YIELD_MIN[jm]*.001,gwth_yr$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #lai
  pdf(file=paste(fig_dir,"/lai_",year,"_ensemble_lines_all.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(lai_yr$DOY, lai_yr$MEAN, ty="l", xlim=c(120,290), ylim=c(0,6.5),lwd=1.25,
       xlab="Day of year",ylab="LAI (m2/m2)")
  for (seed in seed_list) {
    lines(lai_yr$DOY, lai_yr[,paste("LAI.",seed,sep="")],col="grey 80",lwd=0.75)
  }
  lines(lai_yr$DOY, lai_yr$MEAN,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$LAI_OBS, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$LAI_MIN[jm],gwth_yr$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #with shading
  pdf(file=paste(fig_dir,"/lai_",year,"_ensemble_shading_all.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(lai_yr$DOY, lai_yr$MEAN, ty="l", xlim=c(120,290), ylim=c(0,6.5),lwd=1.25,
       xlab="Day of year",ylab="Leaf area index (m2/m2)")
  polygon(x=c(lai_yr$DOY, rev(lai_yr$DOY)), y=c((lai_yr$MEAN+lai_yr$SD*2),rev((lai_yr$MEAN-lai_yr$SD*2))),col="grey 80",border="grey 80")
  polygon(x=c(lai_yr$DOY, rev(lai_yr$DOY)), y=c((lai_yr$MEAN+lai_yr$SD),rev((lai_yr$MEAN-lai_yr$SD))),col="grey 60",border="grey 60")
  lines(lai_yr$DOY, lai_yr$MEAN,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$LAI_OBS, pch=20, cex=1.5, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$LAI_MIN[jm],gwth_yr$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #pesw
  pdf(file=paste(fig_dir,"/swater_",year,"_ensemble_lines_all.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(pesw_yr$DOY, pesw_yr$MEAN*10, ty="l", xlim=c(120,290), ylim=c(20,110),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  for (seed in seed_list) {
    lines(pesw_yr$DOY, pesw_yr[,paste("PESW.",seed,sep="")]*10,col="grey 80",lwd=0.75)
  }
  lines(pesw_yr$DOY, pesw_yr$MEAN*10,col="black",lwd=1.25)
  points(swat_yr$DOY, swat_yr$SW_mm, pch=20, cex=1.5, col="red")
  grid()
  dev.off()
  
  #with shading
  pdf(file=paste(fig_dir,"/swater_",year,"_ensemble_shading_all.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(pesw_yr$DOY, pesw_yr$MEAN*10, ty="l", xlim=c(120,290), ylim=c(20,110),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  polygon(x=c(pesw_yr$DOY, rev(pesw_yr$DOY)), y=c((pesw_yr$MEAN*10+pesw_yr$SD*20),rev((pesw_yr$MEAN*10-pesw_yr$SD*20))),col="grey 80",border="grey 80")
  polygon(x=c(pesw_yr$DOY, rev(pesw_yr$DOY)), y=c((pesw_yr$MEAN*10+pesw_yr$SD*10),rev((pesw_yr$MEAN*10-pesw_yr$SD*10))),col="grey 60",border="grey 60")
  lines(pesw_yr$DOY, pesw_yr$MEAN*10,col="black",lwd=1.25)
  points(swat_yr$DOY, swat_yr$SW_mm, pch=20, cex=1.5, col="red")
  grid()
  dev.off()
}


###
#gather output from *SKILFUL* ensemble members
for (seed in seed_skilful) {
  #seed <- seed_list[1]
  load(file=paste(runs_dir,"/optim_seed-",seed,"/optimal_run.RData",sep=""))
  rm(list=c("best_params","seas_all"))
  
  daily_all <- daily_all[,c("YEAR","DOY","DAP","LAI","BIOMASS","YIELD","PESW")]
  names(daily_all)[4:7] <- paste(c("LAI","BIOMASS","YIELD","PESW"),".",seed,sep="")
  
  if (seed == seed_skilful[1]) {
    daily_merged <- daily_all
  } else {
    daily_merged <- merge(daily_merged, daily_all, by=c("YEAR","DOY","DAP"))
  }
  rm(daily_all)
}

#ensemble means params
fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("BIOMASS.",names(daily_merged))])
bmass_all <- daily_merged[,fields]
bmass_all$MEAN <- rowMeans(bmass_all[,4:ncol(bmass_all)], na.rm=T)
bmass_all$SD <- apply(bmass_all[,(4:(ncol(bmass_all)-1))],1,FUN=function(x) {sd(x)})

fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("YIELD.",names(daily_merged))])
yield_all <- daily_merged[,fields]
yield_all$MEAN <- rowMeans(yield_all[,4:ncol(yield_all)], na.rm=T)
yield_all$SD <- apply(yield_all[,(4:(ncol(yield_all)-1))],1,FUN=function(x) {sd(x)})

fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("LAI.",names(daily_merged))])
lai_all <- daily_merged[,fields]
lai_all$MEAN <- rowMeans(lai_all[,4:ncol(lai_all)], na.rm=T)
lai_all$SD <- apply(lai_all[,(4:(ncol(lai_all)-1))],1,FUN=function(x) {sd(x)})

fields <- c("YEAR","DOY","DAP",names(daily_merged)[grep("PESW.",names(daily_merged))])
pesw_all <- daily_merged[,fields]
pesw_all$MEAN <- rowMeans(pesw_all[,4:ncol(pesw_all)], na.rm=T)
pesw_all$SD <- apply(pesw_all[,(4:(ncol(pesw_all)-1))],1,FUN=function(x) {sd(x)})

#loop for plotting individual years
for (year in 2007:2008) {
  #year <- 2007
  bmass_yr <- bmass_all[which(bmass_all$YEAR == year),]
  yield_yr <- yield_all[which(yield_all$YEAR == year),]
  lai_yr <- lai_all[which(lai_all$YEAR == year),]
  pesw_yr <- pesw_all[which(pesw_all$YEAR == year),]
  gwth_yr <- growth_data[which(growth_data$YEAR == year),]
  swat_yr <- swater_data[which(swater_data$YEAR == year),]
  
  pdf(file=paste(fig_dir,"/biomass_yield_",year,"_ensemble_lines_skilful.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(bmass_yr$DOY, bmass_yr$MEAN*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or Yield (t/ha)")
  for (seed in seed_skilful) {
    lines(bmass_yr$DOY, bmass_yr[,paste("BIOMASS.",seed,sep="")]*.001,col="grey 80",lwd=0.75)
  }
  lines(bmass_yr$DOY, bmass_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$BMASS_MIN[jm]*.001,gwth_yr$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  
  for (seed in seed_skilful) {
    lines(yield_yr$DOY, yield_yr[,paste("YIELD.",seed,sep="")]*.001,col="grey 80",lwd=0.75)
  }
  lines(yield_yr$DOY, yield_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$YIELD_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$YIELD_MIN[jm]*.001,gwth_yr$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #with shading
  pdf(file=paste(fig_dir,"/biomass_yield_",year,"_ensemble_shading_skilful.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(bmass_yr$DOY, bmass_yr$MEAN*.001, ty="l", xlim=c(120,290), ylim=c(0,25),lwd=1.25,
       xlab="Day of year",ylab="Biomass or Yield (t/ha)")
  polygon(x=c(bmass_yr$DOY, rev(bmass_yr$DOY)), y=c((bmass_yr$MEAN*0.001+bmass_yr$SD*0.002),rev((bmass_yr$MEAN*0.001-bmass_yr$SD*0.002))),col="grey 80",border="grey 80")
  polygon(x=c(bmass_yr$DOY, rev(bmass_yr$DOY)), y=c((bmass_yr$MEAN*0.001+bmass_yr$SD*0.001),rev((bmass_yr$MEAN*0.001-bmass_yr$SD*0.001))),col="grey 60",border="grey 60")
  lines(bmass_yr$DOY, bmass_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$BIOMASS_OBS*.001, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$BMASS_MIN[jm]*.001,gwth_yr$BMASS_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  polygon(x=c(yield_yr$DOY, rev(yield_yr$DOY)), y=c((yield_yr$MEAN*0.001+yield_yr$SD*0.002),rev((yield_yr$MEAN*0.001-yield_yr$SD*0.002))),col="grey 80",border="grey 80")
  polygon(x=c(yield_yr$DOY, rev(yield_yr$DOY)), y=c((yield_yr$MEAN*0.001+yield_yr$SD*0.001),rev((yield_yr$MEAN*0.001-yield_yr$SD*0.001))),col="grey 60",border="grey 60")
  lines(yield_yr$DOY, yield_yr$MEAN*.001,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$YIELD_OBS*.001, pch=20, cex=1.5, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$YIELD_MIN[jm]*.001,gwth_yr$YIELD_MAX[jm]*.001),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #lai
  pdf(file=paste(fig_dir,"/lai_",year,"_ensemble_lines_skilful.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(lai_yr$DOY, lai_yr$MEAN, ty="l", xlim=c(120,290), ylim=c(0,6),lwd=1.25,
       xlab="Day of year",ylab="LAI (m2/m2)")
  for (seed in seed_skilful) {
    lines(lai_yr$DOY, lai_yr[,paste("LAI.",seed,sep="")],col="grey 80",lwd=0.75)
  }
  lines(lai_yr$DOY, lai_yr$MEAN,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$LAI_OBS, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$LAI_MIN[jm],gwth_yr$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #with shading
  pdf(file=paste(fig_dir,"/lai_",year,"_ensemble_shading_skilful.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(lai_yr$DOY, lai_yr$MEAN, ty="l", xlim=c(120,290), ylim=c(0,6.5),lwd=1.25,
       xlab="Day of year",ylab="Leaf area index (m2/m2)")
  polygon(x=c(lai_yr$DOY, rev(lai_yr$DOY)), y=c((lai_yr$MEAN+lai_yr$SD*2),rev((lai_yr$MEAN-lai_yr$SD*2))),col="grey 80",border="grey 80")
  polygon(x=c(lai_yr$DOY, rev(lai_yr$DOY)), y=c((lai_yr$MEAN+lai_yr$SD),rev((lai_yr$MEAN-lai_yr$SD))),col="grey 60",border="grey 60")
  lines(lai_yr$DOY, lai_yr$MEAN,col="black",lwd=1.25)
  points(gwth_yr$DOY, gwth_yr$LAI_OBS, pch=20, cex=1.25, col="red")
  for (jm in 1:nrow(gwth_yr)) {lines(x=c(gwth_yr$DOY[jm],gwth_yr$DOY[jm]),y=c(gwth_yr$LAI_MIN[jm],gwth_yr$LAI_MAX[jm]),lty=1,col="red",lwd=1.25)}
  grid()
  dev.off()
  
  #pesw
  pdf(file=paste(fig_dir,"/swater_",year,"_ensemble_lines_skilful.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(pesw_yr$DOY, pesw_yr$MEAN*10, ty="l", xlim=c(120,290), ylim=c(20,110),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  for (seed in seed_skilful) {
    lines(pesw_yr$DOY, pesw_yr[,paste("PESW.",seed,sep="")]*10,col="grey 80",lwd=0.75)
  }
  lines(pesw_yr$DOY, pesw_yr$MEAN*10,col="black",lwd=1.25)
  points(swat_yr$DOY, swat_yr$SW_mm, pch=20, cex=1.5, col="red")
  grid()
  dev.off()
  
  #with shading
  pdf(file=paste(fig_dir,"/swater_",year,"_ensemble_shading_skilful.pdf",sep=""),height=4,width=6,pointsize=12,family="Helvetica")
  par(mar=c(5,5,1,1), lwd=1.5, las=1)
  plot(pesw_yr$DOY, pesw_yr$MEAN*10, ty="l", xlim=c(120,290), ylim=c(20,110),lwd=1.25,
       xlab="Day of year",ylab="Available soil water (mm)")
  polygon(x=c(pesw_yr$DOY, rev(pesw_yr$DOY)), y=c((pesw_yr$MEAN*10+pesw_yr$SD*20),rev((pesw_yr$MEAN*10-pesw_yr$SD*20))),col="grey 80",border="grey 80")
  polygon(x=c(pesw_yr$DOY, rev(pesw_yr$DOY)), y=c((pesw_yr$MEAN*10+pesw_yr$SD*10),rev((pesw_yr$MEAN*10-pesw_yr$SD*10))),col="grey 60",border="grey 60")
  lines(pesw_yr$DOY, pesw_yr$MEAN*10,col="black",lwd=1.25)
  points(swat_yr$DOY, swat_yr$SW_mm, pch=20, cex=1.5, col="red")
  grid()
  dev.off()
}


