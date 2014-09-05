#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#plot co2_response ratio fo the three areas analysed for each RCP

#load libraries
library(maptools); data(wrld_simpl); library(raster);
library(rgdal); library(rasterVis); library(sp); library(ggplot2);
library(ncdf4); library(lubridate); library(matrixStats)

##directories
base_dir <- "~/Leeds-work/AgMIP-maize-phase-2/co2_resp_analysis"
data_dir <- paste(base_dir,"/isimip_output",sep="")
out_dir <- paste(base_dir,"/yield_rr",sep="")
plot_dir <- paste(base_dir,"/figures/co2_rr",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir, recursive=T)}
if (!file.exists(plot_dir)) {dir.create(plot_dir, recursive=T)}

#get masks for each country
load(paste(base_dir,"/areas_data/masks.RData",sep=""))

#select iso and get mask
iso <- "ge"
msk <- get(paste("msk_",iso,sep=""))

#factors of analysis
gcm_list <- c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc-esm-chem","noresm1-m")
rcp_list <- c("rcp8p5") #c("rcp2p6","rcp4p5","rcp6p0","rcp8p5")
irr_list <- c("noirr","firr")
co2_list <- c("co2","noco2")
cgm_list <- c("epic","lpjml","pdssat","pegasus")

#dimensions of arrays
Ncgm <- length(cgm_list) #number of crop models
Nco2 <- length(co2_list) #number of co2 responses
Nirr <- length(irr_list) #number of irr treatments
Ngcm <- length(gcm_list) #number of GCMs
Nrcp <- length(rcp_list) #number of RCPs

#define baseline
sim_baseline <- 1980 #start of simulations
sim_lastyear <- 2099 #last simulated year

#total number of years for the fitting and range of years for the fits
yrstot <- length(sim_baseline:sim_lastyear)+(sim_baseline-1900)-1
sim_fitrange <- (sim_baseline-1900):yrstot


####
if (!file.exists(paste(out_dir,"/yield_rr_",iso,".RData",sep=""))) {
  #create a list of rasterStacks for historical and future runs
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
        fut_stk[[gcm_i]][[cgm_i]][[co2_i]] <- list()
        his_stk[[gcm_i]][[cgm_i]][[co2_i]] <- list()
        
        for (irr_i in irr_list) {
          #irr_i <- irr_list[1]
          cat("gcm=",gcm_i,", crop_model=",cgm_i,", co2_resp=",co2_i,", irr=",irr_i,"\n",sep="")
          fut_stk[[gcm_i]][[cgm_i]][[co2_i]][[irr_i]] <- list()
          
          #define baseline and future ranges for data loading
          if (gcm_i == "hadgem2-es") {
            start_hist <- 1971; end_hist <- 2004; start_fut <- 2005; end_fut <- 2099
          } else {
            start_hist <- 1971; end_hist <- 2005; start_fut <- 2006; end_fut <- 2099
          }
          
          if (cgm_i == "epic") {
            start_hist <- sim_baseline; end_hist <- 2010; start_fut <- 2005; end_fut <- 2099
          }
          
          #read in historical
          his_stk[[gcm_i]][[cgm_i]][[co2_i]][[irr_i]] <- stack(paste(data_dir,"/",cgm_i,"/",cgm_i,"_",gcm_i,"_hist_ssp2_",co2_i,"_",irr_i,"_yield_mai_annual_",start_hist,"_",end_hist,".nc",sep=""))
          
          #read in future
          for (rcp_i in rcp_list) {
            #rcp_i <- rcp_list[1]
            fut_stk[[gcm_i]][[cgm_i]][[co2_i]][[irr_i]][[rcp_i]] <- stack(paste(data_dir,"/",cgm_i,"/",cgm_i,"_",gcm_i,"_",rcp_i,"_ssp2_",co2_i,"_",irr_i,"_yield_mai_annual_",start_fut,"_",end_fut,".nc",sep=""))
          }
        }
      }
    }
  }
  
  ### extract data from yield array
  #initialise yield array, for data extraction
  yield <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nco2,Nirr,yrstot)) ##this yield matrix represents one location or mean
  
  #list of grid cells to average or to extract
  loc_list <- as.data.frame(xyFromCell(msk, which(!is.na(msk[]))))
  loc_list$ahar <- extract(msk, loc_list[,c("x","y")])
  
  #### get yield data
  #loop crop models
  for (cgm in 1:Ncgm) {
    #cgm <- 1
    cgm_i <- cgm_list[cgm]
    
    #loop co2 responses
    for (co2 in 1:Nco2) {
      #co2 <- 1
      co2_i <- co2_list[co2]
      
      #loop irrigation runs
      for (irr in 1:Nirr) {
        #irr <- 1
        irr_i <- irr_list[irr]
        
        #loop climate models
        for (gcm in 1:Ngcm) {
          #gcm <- 1
          gcm_i <- gcm_list[gcm]
          
          #gather data
          data_his <- his_stk[[gcm_i]][[cgm_i]][[co2_i]][[irr_i]]
          data_his <- as.data.frame(extract(data_his, loc_list[,c("x","y")]))
          
          #average locations as needed
          #note, this bit would need modification for e.g. a weighted average
          if (nrow(loc_list) > 1) {
            #data_his <- colMeans(data_his, na.rm=T) #unweighted
            #weighted by growing area
            calc_wmean <- function(x) {
              #x <- as.numeric(vpd_jja[1,2:ncol(vpd_jja)])
              nna <- which(!is.na(x))
              x <- x[nna]; har_frac <- loc_list$ahar[nna]
              yval <- sum(x * har_frac) / sum(har_frac)
              return(yval)
            }
            data_his <- apply(data_his, 2, calc_wmean)
          }
          
          #loop rcps
          for (rcp in 1:Nrcp) {
            #rcp <- 1
            rcp_i <- rcp_list[rcp]
            cat("gcm=",gcm_i,", rcp=",rcp_i,", co2_resp=",co2_i,", crop_model=",cgm_i,", irr=",irr_i,"\n",sep="")
            
            #gather data
            data_fut <- fut_stk[[gcm_i]][[cgm_i]][[co2_i]][[irr_i]][[rcp_i]]
            data_fut <- as.data.frame(extract(data_fut, loc_list[,c("x","y")]))
            
            #average locations as needed
            #note, this bit would need modification for e.g. a weighted average
            if (nrow(loc_list) > 1) {
              #data_fut <- colMeans(data_fut, na.rm=T) #unweighted
              #weighted by growing area
              data_fut <- apply(data_fut, 2, calc_wmean)
            }
            
            #fill the hist time period - for different rcps it is the same
            if (cgm_i == "epic") {
              yield[gcm,rcp,cgm,co2,irr,(sim_baseline-1900):((sim_baseline-1900)+length(data_his)-1)] <- as.numeric(data_his)
            } else {
              yield[gcm,rcp,cgm,co2,irr,(1971-1900):((1971-1900)+length(data_his)-1)] <- as.numeric(data_his)
            }
            
            #fill the future time period - differs for different rcps
            if (cgm_i == "epic") {
              yield[gcm,rcp,cgm,co2,irr,((sim_baseline-1900)+length(data_his)-4):yrstot] <- as.numeric(data_fut)
            } else {
              yield[gcm,rcp,cgm,co2,irr,((1971-1900)+length(data_his)):yrstot] <- as.numeric(data_fut)
            }
          }
        }
      }
    }
  }
  
  
  #calculate actual and smoothed response ratios
  yield_rr <- syield_rr <- array(NA,dim=c(Ngcm,Nrcp,Ncgm,Nirr,yrstot)) ##this rr matrix represents one location or mean
  
  for (gcm in 1:Ngcm) {
    #gcm <- 1
    gcm_i <- gcm_list[gcm]
    
    #loop rcps
    for (rcp in 1:Nrcp) {
      #rcp <- 1
      rcp_i <- rcp_list[rcp]
      
      for (cgm in 1:Ncgm) {
        #cgm <- 1
        cgm_i <- cgm_list[cgm]
        
        for (irr in 1:Nirr) {
          #irr <- 1
          irr_i <- irr_list[irr]
          cat("gcm=",gcm_i,", rcp=",rcp_i,", crop_model=",cgm_i,", irr=",irr_i,"\n",sep="")
          
          yco2 <- yield[gcm,rcp,cgm,1,irr,] #yield with co2 effects
          ynoco2 <- yield[gcm,rcp,cgm,2,irr,] #yield with no co2 effects
          yield_rr[gcm,rcp,cgm,irr,] <- yco2 / ynoco2
          
          #testing whether fitting would be appropriate
          x <- sim_fitrange
          y <- yield_rr[gcm,rcp,cgm,irr,sim_fitrange]
          #model_fit <- lm(y~poly(x,4,raw=TRUE))
          model_fit <- loess(y~x,span=0.75,degree=2)
          yfit_dummy <- predict(model_fit,data.frame(x=sim_fitrange))
          syield_rr[gcm,rcp,cgm,irr,sim_fitrange] <- yfit_dummy
        }
      }
    }
  }
  save(list=c("yield_rr","syield_rr","yield"),file=paste(out_dir,"/yield_rr_",iso,".RData",sep=""))
} else {
  load(file=paste(out_dir,"/yield_rr_",iso,".RData",sep=""))
}

#calculate average rr for each cgm, only for RCP so as to be able to make some relationship
#with CO2 concentrations
rr_mean <- rr_min <- rr_max <- array(NA,dim=c(Ncgm,Nirr,yrstot))
srr_mean <- srr_min <- srr_max <- array(NA,dim=c(Ncgm,Nirr,yrstot))

for (i in 1:Ncgm) {
  for (j in 1:Nirr) {
    rr_mean[i,j,] <- colMedians(array(yield_rr[,,i,j,],c(Ngcm*Nrcp,yrstot)), na.rm=T)
    srr_mean[i,j,] <- colMedians(array(syield_rr[,,i,j,],c(Ngcm*Nrcp,yrstot)), na.rm=T)
  }
}

for (i in 1:Ncgm) {
  for (j in 1:Nirr) {
    rr_min[i,j,] <- colQuantiles(array(yield_rr[,,i,j,],c(Ngcm*Nrcp,yrstot)), probs=0.10, na.rm=T)
    srr_min[i,j,] <- colQuantiles(array(syield_rr[,,i,j,],c(Ngcm*Nrcp,yrstot)), probs=0.10, na.rm=T)
  }
}

for (i in 1:Ncgm) {
  for (j in 1:Nirr) {
    rr_max[i,j,] <- colQuantiles(array(yield_rr[,,i,j,],c(Ngcm*Nrcp,yrstot)) , probs=0.90, na.rm=T)
    srr_max[i,j,] <- colQuantiles(array(syield_rr[,,i,j,],c(Ngcm*Nrcp,yrstot)) , probs=0.90, na.rm=T)
  }
}

#load CO2 concentrations
co2_conc <- read.table(paste(base_dir,"/co2_concentrations/RCP85_MIDYEAR_CONCENTRATIONS.tab",sep=""), header=T, sep="\t")
co2_conc <- co2_conc[,c("YEARS","CO2")]
co2_conc$D550 <- abs(co2_conc$CO2 - 550)
year550 <- co2_conc$YEARS[which(co2_conc$D550 == min(co2_conc$D550))[1]]

### produce plot (actual)
pdf(paste(plot_dir,"/yield_rr_",iso,".pdf",sep=""),height=5,width=8)
par(mar=c(5,5,1,1),las=1,lwd=2)

#initialise plot
plot(sim_fitrange+1900, rr_mean[1,1,sim_fitrange], ty="l", axes=F, col=NA, ylim=c(1,2.0),
     xlab="Year", ylab="Yield response ratio")
axis(side=1,at=seq(1950,2100,by=10))
axis(side=2,at=seq(1,3,by=0.2),labels=sprintf("%.1f",seq(1,3,by=0.2)))
box()

#start cgm=1, irr=1 (epic, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[1,1,sim_fitrange],rev(rr_max[1,1,sim_fitrange])),
       col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, rr_mean[1,1,sim_fitrange], lwd=1.5, col="red")

#cgm=1, irr=2 (epic, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[1,2,sim_fitrange],rev(rr_max[1,2,sim_fitrange])),
       col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, rr_mean[1,2,sim_fitrange], lwd=1.5, col="red", lty=2)

#cgm=2, irr=1 (lpjml, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[2,1,sim_fitrange],rev(rr_max[2,1,sim_fitrange])),
        col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, rr_mean[2,1,sim_fitrange], lwd=1.5, col="blue")

#cgm=2, irr=2 (lpjml, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[2,2,sim_fitrange],rev(rr_max[2,2,sim_fitrange])),
        col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, rr_mean[2,2,sim_fitrange], lwd=1.5, col="blue", lty=2)

#cgm=3, irr=1 (pdssat, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[3,1,sim_fitrange],rev(rr_max[3,1,sim_fitrange])),
        col=rgb(red=0,green=255,blue=155,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, rr_mean[3,1,sim_fitrange], lwd=1.5, col="dark green")

#cgm=3, irr=2 (pdssat, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[3,2,sim_fitrange],rev(rr_max[3,2,sim_fitrange])),
        col=rgb(red=0,green=255,blue=155,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, rr_mean[3,2,sim_fitrange], lwd=1.5, col="dark green", lty=2)

#cgm=4, irr=1 (pegasus, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[4,1,sim_fitrange],rev(rr_max[4,1,sim_fitrange])),
        col=rgb(red=255,green=255,blue=0,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, rr_mean[4,1,sim_fitrange], lwd=1.5, col="orange")

#cgm=4, irr=2 (pegasus, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(rr_min[4,2,sim_fitrange],rev(rr_max[4,2,sim_fitrange])),
        col=rgb(red=255,green=255,blue=0,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, rr_mean[4,2,sim_fitrange], lwd=1.5, col="orange", lty=2)

grid(lwd=1)
abline(v=year550, lty=2, lwd=1.5)
legend("topleft",c("EPIC","LPJmL","pDSSAT","PEGASUS","Rainfed","Irrigated"),lty=c(1,1,1,1,1,2), col=c("red","blue","dark green","orange","black","black"), bty="n", cex=1)
dev.off()



### produce plot (smoothed)
pdf(paste(plot_dir,"/syield_rr_",iso,".pdf",sep=""),height=5,width=8)
par(mar=c(5,5,1,1),las=1,lwd=2)

#initialise plot
plot(sim_fitrange+1900, srr_mean[1,1,sim_fitrange], ty="l", axes=F, col=NA, ylim=c(1,2.0),
     xlab="Year", ylab="Yield response ratio")
axis(side=1,at=seq(1950,2100,by=10))
axis(side=2,at=seq(1,3,by=0.2),labels=sprintf("%.1f",seq(1,3,by=0.2)))
box()

#start cgm=1, irr=1 (epic, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[1,1,sim_fitrange],rev(srr_max[1,1,sim_fitrange])),
        col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, srr_mean[1,1,sim_fitrange], lwd=1.5, col="red")

#cgm=1, irr=2 (epic, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[1,2,sim_fitrange],rev(srr_max[1,2,sim_fitrange])),
        col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, srr_mean[1,2,sim_fitrange], lwd=1.5, col="red", lty=2)

#cgm=2, irr=1 (lpjml, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[2,1,sim_fitrange],rev(srr_max[2,1,sim_fitrange])),
        col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, srr_mean[2,1,sim_fitrange], lwd=1.5, col="blue")

#cgm=2, irr=2 (lpjml, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[2,2,sim_fitrange],rev(srr_max[2,2,sim_fitrange])),
        col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, srr_mean[2,2,sim_fitrange], lwd=1.5, col="blue", lty=2)

#cgm=3, irr=1 (pdssat, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[3,1,sim_fitrange],rev(srr_max[3,1,sim_fitrange])),
        col=rgb(red=0,green=255,blue=155,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, srr_mean[3,1,sim_fitrange], lwd=1.5, col="dark green")

#cgm=3, irr=2 (pdssat, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[3,2,sim_fitrange],rev(srr_max[3,2,sim_fitrange])),
        col=rgb(red=0,green=255,blue=155,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, srr_mean[3,2,sim_fitrange], lwd=1.5, col="dark green", lty=2)

#cgm=4, irr=1 (pegasus, noirr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[4,1,sim_fitrange],rev(srr_max[4,1,sim_fitrange])),
        col=rgb(red=255,green=255,blue=0,alpha=50,maxColorValue=255),border=NA)
lines(sim_fitrange+1900, srr_mean[4,1,sim_fitrange], lwd=1.5, col="orange")

#cgm=4, irr=2 (pegasus, firr)
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)), c(srr_min[4,2,sim_fitrange],rev(srr_max[4,2,sim_fitrange])),
        col=rgb(red=255,green=255,blue=0,alpha=50,maxColorValue=255), border=NA)
lines(sim_fitrange+1900, srr_mean[4,2,sim_fitrange], lwd=1.5, col="orange", lty=2)

grid(lwd=1)
abline(v=year550, lty=2, lwd=1.5)
legend("topleft",c("EPIC","LPJmL","pDSSAT","PEGASUS","Rainfed","Irrigated"),lty=c(1,1,1,1,1,2), col=c("red","blue","dark green","orange","black","black"), bty="n", cex=1)
dev.off()

