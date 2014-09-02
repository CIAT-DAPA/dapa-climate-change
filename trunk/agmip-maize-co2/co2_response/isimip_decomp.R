#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#borrows from Ed Hawkins' unccrop.m script and from AKK's script
#signal_noise_analysis_difffits_forJulian, but plots more individual unc. components

#load libraries
library(maptools); data(wrld_simpl); library(raster);
library(rgdal); library(rasterVis); library(sp); library(ggplot2);
library(ncdf4); library(lubridate); library(matrixStats)

##directories
data_dir <- "~/Leeds-work/AgMIP-maize-phase-2/isimip_output"
plot_dir <- paste(data_dir,"/plots",sep="")
if (!file.exists(plot_dir)) {dir.create(plot_dir)}

#create a dummy mask from some of the simulations
msk <- stack(paste(data_dir,"/",c("pdssat","lpjml"),"/",c("pdssat","lpjml"),"_gfdl-esm2m_hist_ssp2_co2_firr_yield_mai_annual_1971_2005.nc",sep=""))
msk[which(msk[] < 0.1)] <- NA
msk <- calc(msk, fun = mean, na.rm=F)

#factors of analysis
gcm_list <- c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc-esm-chem","noresm1-m")
rcp_list <- c("rcp2p6","rcp4p5","rcp6p0","rcp8p5")
co2_list <- c("co2","noco2")
cgm_list <- c("lpjml","pdssat")

#define baseline
sim_baseline <- 1971 #start of simulations, for the fitting only
sim_lastyear <- 2099 #last simulated year
ipcc_baseline <- 1986 #IPCC start, for analyses

#total number of years for the fitting and range of years for the fits
yrstot <- length(sim_baseline:sim_lastyear)+(sim_baseline-1900)-1
sim_fitrange <- (sim_baseline-1900):yrstot
ipcc_fitrange <- (ipcc_baseline-1900):yrstot


####
#1. create a list of rasterStacks for historical and future runs
his_stk <- list(); fut_stk <- list()

#for hist data
for (gcm_i in gcm_list) {
  #gcm_i <- gcm_list[1]
  his_stk[[gcm_i]] <- list()
  fut_stk[[gcm_i]] <- list()
  
  #loop crop models
  for (cgm_i in cgm_list) {
    #cgm_i <- cgm_list[1]
    his_stk[[gcm_i]][[cgm_i]] <- list()
    fut_stk[[gcm_i]][[cgm_i]] <- list()
    
    #loop CO2 response
    for (co2_i in co2_list) {
      #co2_i <- co2_list[1]
      cat("gcm=",gcm_i,", crop_model=",cgm_i,", co2_resp=",co2_i,"\n",sep="")
      fut_stk[[gcm_i]][[cgm_i]][[co2_i]] <- list()
      
      #define baseline and future ranges for data loading
      if (gcm_i == "hadgem2-es") {
        start_hist <- sim_baseline; end_hist <- 2004; start_fut <- 2005; end_fut <- 2099
      } else {
        start_hist <- sim_baseline; end_hist <- 2005; start_fut <- 2006; end_fut <- 2099
      }
      
      #read in historical
      his_stk[[gcm_i]][[cgm_i]][[co2_i]] <- stack(paste(data_dir,"/",cgm_i,"/",cgm_i,"_",gcm_i,"_hist_ssp2_",co2_i,"_noirr_yield_mai_annual_",start_hist,"_",end_hist,".nc",sep=""))
      
      #read in future
      for (rcp_i in rcp_list) {
        #rcp_i <- rcp_list[1]
        fut_stk[[gcm_i]][[cgm_i]][[co2_i]][[rcp_i]] <- stack(paste(data_dir,"/",cgm_i,"/",cgm_i,"_",gcm_i,"_",rcp_i,"_ssp2_",co2_i,"_noirr_yield_mai_annual_",start_fut,"_",end_fut,".nc",sep=""))
      }
    }
  }
}


#dimensions of arrays
Nco2 <- length(co2_list) #number of co2 responses
Ncgm <- length(cgm_list) #number of crop models
NG <- Nco2*Ncgm #crop component, i.e. CO2*CROP_MODEL
Ngcm <- length(gcm_list) #number of GCMs
Nrcp <- length(rcp_list) #number of RCPs
NM <- Ngcm*Nrcp #climate component, i.e. GCM*RCP

### extract data from yield array and calculate models
#initialise yield array, for data extraction
yield <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nco2,yrstot)) ##this yield matrix is needed for each grid cell

#initialise analysis arrays
yfit <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nco2,yrstot))
climyield <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nco2))
yieldanom <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nco2,yrstot))

#list of grid cells to average or to extract
loc_list <- which(!is.na(msk[]))[5000:5200]

#types of models
#1-3: loess; 4: poly2; 5: poly3
model_list <- data.frame(model=c(1:5),span=c(0.75,1,1,NA,NA),degree=c(1,1,2,NA,NA),
                         name=c("span075_deg1","span1_deg1","span1_deg2","poly2","poly3"))

#chosen model
this_model <- 4

#### calculate fits
#loop crop models
for (cgm in 1:Ncgm) {
  #cgm <- 1
  cgm_i <- cgm_list[cgm]
  
  #loop co2 responses
  for (co2 in 1:Nco2) {
    #co2 <- 1
    co2_i <- co2_list[co2]
    
    #loop climate models
    for (gcm in 1:Ngcm) {
      #gcm <- 1
      gcm_i <- gcm_list[gcm]
      
      #gather data
      data_his <- his_stk[[gcm_i]][[cgm_i]][[co2_i]]
      data_his <- as.data.frame(extract(data_his, loc_list))
      
      #average locations as needed
      #note, this bit would need modification for e.g. a weighted average
      if (length(loc_list) > 1) {data_his <- colMeans(data_his, na.rm=T)}
      
      #loop rcps
      for (rcp in 1:Nrcp) {
        #rcp <- 1
        rcp_i <- rcp_list[rcp]
        cat("gcm=",gcm_i,", rcp=",rcp_i,", co2_resp=",co2_i,", crop_model=",cgm_i,"\n",sep="")
        
        #gather data
        data_fut <- fut_stk[[gcm_i]][[cgm_i]][[co2_i]][[rcp_i]]
        data_fut <- as.data.frame(extract(data_fut, loc_list))
        
        #average locations as needed
        #note, this bit would need modification for e.g. a weighted average
        if (length(loc_list) > 1) {data_fut <- colMeans(data_fut, na.rm=T)}
        
        #fill the hist time period - for different rcps it is the same
        yield[gcm,rcp,cgm,co2,(sim_baseline-1900):((sim_baseline-1900)+length(data_his)-1)] <- as.numeric(data_his)
        
        #fill the future time period - differs for different rcps
        yield[gcm,rcp,cgm,co2,((sim_baseline-1900)+length(data_his)):yrstot] <- as.numeric(data_fut)
        
        #fitting regressions
        x <- sim_fitrange
        y <- yield[gcm,rcp,cgm,co2,sim_fitrange]
        
        #fit the model
        if (this_model <= 3) {
          model_fit <- loess(y~x,
                             span=model_list$span[model_list$model == this_model],
                             degree=model_list$degree[model_list$model == this_model])
        } else if (this_model == 4) {
          model_fit <- lm(y~poly(x,2,raw=TRUE))
        } else if (this_model == 5) {
          model_fit <- lm(y~poly(x,3,raw=TRUE))
        } else {
          stop("wrong model choice")
        }
        
        #predict values within range
        yfit_dummy <- predict(model_fit,data.frame(x=sim_fitrange))
        #plot(x,y); lines(x,yfit_dummy)
        
        #fill in yfit
        yfit[gcm,rcp,cgm,co2,sim_fitrange] <- yfit_dummy
        
        #climatological mean
        #the climyield is only over the IPCC baseline, not over the whole historical time period
        #that was used for the fit
        climyield[gcm,rcp,cgm,co2] <- mean(yfit[gcm,rcp,cgm,co2,(ipcc_baseline-1900):((ipcc_baseline-1900)+20-1)])
        
        #percentage deviation from predicted (i.e. anomaly)
        yieldanom_dummy <- 100 * ((yield[gcm,rcp,cgm,co2,] / climyield[gcm,rcp,cgm,co2]) - 1)
        yieldanom[gcm,rcp,cgm,co2,] <- yieldanom_dummy
        
        #percentage yield
        yfit_dummy <- 100 * ((yfit[gcm,rcp,cgm,co2,] / climyield[gcm,rcp,cgm,co2]) - 1)
        yfit[gcm,rcp,cgm,co2,] <- yfit_dummy
      }
    }
  }
}


### calculate signal and noise
#initialise arrays
yfitcomb <- array(NA,dim=c(NM,NG,yrstot)) #signal

#pooled uncertainty components
clim_comp <- array(NA,dim=c(yrstot))
crop_comp <- array(NA,dim=c(yrstot))

#average predicted yield per dimension, used to calculate uncertainty components
ycgm <- array(NA,dim=c(Ncgm,yrstot)) #crop_model
yco2 <- array(NA,dim=c(Nco2,yrstot)) #co2_resp
ygcm <- array(NA,dim=c(Ngcm,yrstot)) #gcm
yrcp <- array(NA,dim=c(Nrcp,yrstot)) #rcp

#uncertainty components
cgm_comp <- array(NA,dim=c(yrstot))
co2_comp <- array(NA,dim=c(yrstot))
gcm_comp <- array(NA,dim=c(yrstot))
rcp_comp <- array(NA,dim=c(yrstot))

#put together dimensions GCM*RCP and CROP_MODEL*CO2_RESP
yfitcomb[,,] <- array(yfit[,,,,],dim=c(NM,NG,yrstot))

#calculate yield [%] means across dimensions 1 (climate) and 3 (time)
#and then calculate s.d. across climate
clim_comp[] <- colSds(apply(yfitcomb[,,],c(1,3),mean))

#calculate yield [%] means across dimensions 2 (crop) and 3 (time)
#and thencalculate s.d. across crop
crop_comp[] <- colSds(apply(yfitcomb[,,],c(2,3),mean))

#crop model uncertainty: take average of all other dimensions across crop_model and time
for (i in 1:Ncgm) {ycgm[i,] <- colMeans(array(yfit[,,i,,],c(NM*Nco2,yrstot)))}

#co2_resp uncertainty
for (i in 1:Nco2) {yco2[i,] <- colMeans(array(yfit[,,,i,],c(NM*Ncgm,yrstot)))}

#GCM uncertainty
for (i in 1:Ngcm) {ygcm[i,] <-colMeans(array(yfit[i,,,,],c(NG*Nrcp,yrstot)))}

#RCP uncertainty
for (i in 1:Nrcp) {yrcp[i,] <- colMeans(array(yfit[,i,,,],c(NG*Ngcm,yrstot)),na.rm=TRUE)}

#calculate s.d. (unc.) due to each source
cgm_comp[] <- colSds(ycgm[,])
co2_comp[] <- colSds(yco2[,])
gcm_comp[] <- colSds(ygcm[,])
rcp_comp[] <- colSds(yrcp[,]) 


### calculate variability
variability <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nco2,yrstot))
vres <- array(dim=c(NG*NM,length(ipcc_fitrange)))
vresdec <- array(NA,dim=c(NM*NG,(length(ipcc_fitrange)-9)))

#fill variability array
variability[,,,,] <- yieldanom[,,,,] - yfit[,,,,]
vres[,] <- array(variability[,,,,ipcc_fitrange], dim=c(NG*NM,length(ipcc_fitrange)))

#decadal - mean over 10 year periods (moving average)
for (tt in 1:105) {vresdec[,tt] <- rowMeans(vres[,tt:(tt+9)])}


### calculate total uncertainty, signal and fit variability
varifit <- array(NA,dim=c(yrstot))
varifitconst <- array(NA,dim=c(yrstot))
total_err <- array(NA,dim=c(yrstot)) #total uncertainty
tot_mean <- array(NA,dim=c(yrstot)) #signal

#fit decadal variability to time
x <- 10:(length(ipcc_fitrange)-9)
y <- colSds(vresdec[,10:(length(ipcc_fitrange)-9)])
fit1 <- lm(y~poly(x,1,raw=TRUE)) 

#predict trend of decadal variance in time
#the whole vector needs to be length(x)==yrstot
varifit[] <- predict(fit1,data.frame(x=(length(ipcc_fitrange) - yrstot + 1):(length(ipcc_fitrange))))
varifitconst[] <- rep(sqrt(mean(rowVars(vresdec[,]))),yrstot)

#total error (unc) is climate (gcm+rcp) + crop (co2 only) + decadal variance trend
total_err[] <- clim_comp[]^2 + crop_comp[]^2 + varifit[]^2

#mean signal
tot_mean[] <- colMeans(array(yfitcomb[,,], dim=c(NM*NG,yrstot)))


### plots
#determine max y for plot
sigfac <- 1
ymx <- sigfac * max(sqrt(total_err[sim_fitrange]))

############ plot 1: variance fraction plot: clim vs. crop vs. var
#total error within fitrange
total_err_t <- total_err[sim_fitrange]

#calculate varifit as total.error-varifit
varifit_t <- sqrt((total_err_t) * (1 - ((clim_comp[sim_fitrange]^2 + crop_comp[sim_fitrange]^2) / total_err_t)))

pdf(paste(plot_dir,"/decomp_var_fraction_pooled.pdf",sep=""),height=5,width=8)
par(mar=c(5,5,1,1),las=1,lwd=2)
plot(1900+sim_fitrange,clim_comp[sim_fitrange]^2 / total_err[sim_fitrange],
     type="n",col="blue",xlim=c(2005,2100),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2005,2100,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

#climate
plotline1 <- clim_comp[sim_fitrange]^2 / total_err_t
lines(1900+sim_fitrange, plotline1, col="blue")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline1,rep(0,length(sim_fitrange))),
        col="blue",border=NA)

#crop (but needs to be clim+crop since it's cumulative)
plotline2 <- (clim_comp[sim_fitrange]^2 + crop_comp[sim_fitrange]^2) / total_err_t
lines((1900+sim_fitrange), plotline2, col="seagreen")
polygon(c((1900+sim_fitrange),rev(1900+sim_fitrange)),c(plotline1,rev(plotline2)),col="seagreen",border=NA)

#total of all others, but could have done ok with just a square in the background
plotline3 <- (clim_comp[sim_fitrange]^2 + crop_comp[sim_fitrange]^2 + varifit_t^2) / total_err_t
lines(1900+sim_fitrange, plotline3, col="orange")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline3,rev(plotline2)),col="orange",border=NA)

dev.off()


############# plot 2: variance fraction plot: gcm vs. rcp vs. crop_model vs. co2 vs. var
#total error within fitrange
total_err_t <- gcm_comp[sim_fitrange]^2 + rcp_comp[sim_fitrange]^2 + cgm_comp[sim_fitrange]^2 + co2_comp[sim_fitrange]^2 + varifit[sim_fitrange]^2

#calculate varifit as total.error-varifit
varifit_t <- sqrt((total_err_t) * (1 - ((gcm_comp[sim_fitrange]^2 + rcp_comp[sim_fitrange]^2 + cgm_comp[sim_fitrange]^2 + co2_comp[sim_fitrange]^2) / total_err_t)))

pdf(paste(plot_dir,"/decomp_var_fraction_individual.pdf",sep=""),height=5,width=8)
par(mar=c(5,5,1,1),las=1,lwd=2)
plot(1900+sim_fitrange,gcm_comp[sim_fitrange]^2 / total_err_t,
     type="n",col="blue",xlim=c(2005,2100),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2005,2100,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

#gcm
plotline1 <- gcm_comp[sim_fitrange]^2 / total_err_t
lines(1900+sim_fitrange, plotline1, col="blue")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline1,rep(0,length(sim_fitrange))),
        col="blue",border=NA)

#rcp
plotline2 <- (gcm_comp[sim_fitrange]^2 + rcp_comp[sim_fitrange]^2) / total_err_t
lines((1900+sim_fitrange), plotline2, col="purple")
polygon(c((1900+sim_fitrange),rev(1900+sim_fitrange)),c(plotline1,rev(plotline2)),col="purple",border=NA)

#crop_model
plotline3 <- (gcm_comp[sim_fitrange]^2 + rcp_comp[sim_fitrange]^2 + cgm_comp[sim_fitrange]^2) / total_err_t
lines(1900+sim_fitrange, plotline3, col="seagreen")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline3,rev(plotline2)),col="seagreen",border=NA)

#co2_resp
plotline4 <- (gcm_comp[sim_fitrange]^2 + rcp_comp[sim_fitrange]^2 + cgm_comp[sim_fitrange]^2 + co2_comp[sim_fitrange]^2) / total_err_t
lines(1900+sim_fitrange, plotline4, col="light green")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline4,rev(plotline3)),col="light green",border=NA)

#variability
plotline5 <- (gcm_comp[sim_fitrange]^2 + rcp_comp[sim_fitrange]^2 + cgm_comp[sim_fitrange]^2 + co2_comp[sim_fitrange]^2 + varifit_t^2) / total_err_t
lines(1900+sim_fitrange, plotline5, col="orange")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline5,rev(plotline4)),col="orange",border=NA)

dev.off()

############# plot 3: uncertainties, total unc. and signal
pdf(paste(plot_dir,"/decomp_unc_signal_pooled.pdf",sep=""),height=5,width=8)
par(mar=c(5,5,1,1),las=1,lwd=2)

plot(sim_fitrange+1900, sigfac * sqrt(total_err[sim_fitrange]), col="black", type="l", 
     lty=1,lwd=2,xlab="Year",ylab="Uncertainty [% yield]",xlim=c(2005,2098),ylim=c(0,ymx),
     axes=F) #total
axis(side=1,at=seq(2005,2100,by=10))
axis(side=2,at=seq(0,2*ymx,by=2),labels=seq(0,2*ymx,by=2))
box()

lines(sim_fitrange+1900, sigfac * clim_comp[sim_fitrange], col="blue", lwd=2) #climate component
lines(sim_fitrange+1900, sigfac * crop_comp[sim_fitrange], col="seagreen", lwd=2) #crop component
lines(sim_fitrange+1900, sigfac * varifit[sim_fitrange], col="orange", lwd=2) #variability
lines(sim_fitrange+1900, tot_mean[sim_fitrange], col="black", lty=2, lwd=2) #signal
grid(lwd=1)
legend("topleft",c("Total","Climate","Crop","Variability","Signal"),
       lty=c(1,1,1,1,2), col=c("black","blue","seagreen","orange","black"), bty="n", cex=1)
dev.off()


############# plot 4: uncertainties, total unc. and signal with all individual components
pdf(paste(plot_dir,"/decomp_unc_signal_individual.pdf",sep=""),height=5,width=8)
par(mar=c(5,5,1,1),las=1,lwd=2)

plot(sim_fitrange+1900, sigfac * sqrt(total_err[sim_fitrange]), col="black", type="l", 
     lty=1,lwd=2,xlab="Year",ylab="Uncertainty [% yield]",xlim=c(2005,2098),ylim=c(0,ymx),
     axes=F) #total
axis(side=1,at=seq(2005,2100,by=10))
axis(side=2,at=seq(0,2*ymx,by=2),labels=seq(0,2*ymx,by=2))
box()

lines(sim_fitrange+1900, sigfac * gcm_comp[sim_fitrange], col="blue", lwd=2) #gcm component
lines(sim_fitrange+1900, sigfac * rcp_comp[sim_fitrange], col="purple", lwd=2) #rcp component
lines(sim_fitrange+1900, sigfac * cgm_comp[sim_fitrange], col="seagreen", lwd=2) #crop_model component
lines(sim_fitrange+1900, sigfac * co2_comp[sim_fitrange], col="light green", lwd=2) #co2_resp component
lines(sim_fitrange+1900, sigfac * varifit[sim_fitrange], col="orange", lwd=2) #variability
lines(sim_fitrange+1900, tot_mean[sim_fitrange], col="black", lty=2, lwd=2) #signal
grid(lwd=1)
legend("topleft",c("Total","GCM","RCP","Crop model","CO2 resp.","Variability","Signal"),
       lty=c(1,1,1,1,1,1,2), col=c("black","blue","purple","seagreen","light green","orange","black"), 
       bty="n", cex=1)

dev.off()



