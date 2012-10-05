#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#September 2012

library(raster)
library(maptools); data(wrld_simpl)

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
cmip5_dir <- "V:/eejarv/CMIP5"

#eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
# src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
# bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
#cmip5_dir <- "/nfs/a102/eejarv/CMIP5"

#sourcing scripts
source(paste(src.dir,"/cmip5/05.analyse_chg_rcp45-functions.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/watbal.R",sep=""))

#name of crop and other details
cropName <- "gnut"
selection <- "v6"
gsi <- 152 #1st of june
gsf <- 243 #31st of august

yh_i <- 1966
yh_f <- 1993
yf_i <- 2021
yf_f <- 2049

#other directories
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
wthDir_obs <- paste(cropDir,"/inputs/ascii/wth",sep="")
wthDir_hist <- paste(cropDir,"/inputs/ascii/wth-cmip5_hist",sep="")
wthDir_rcp <- paste(cropDir,"/inputs/ascii/wth-cmip5_rcp45",sep="")
out_dir <- paste(bDir,"/climate-change",sep=""); if (!file.exists(out_dir)) {dir.create(out_dir)}
histDir <- paste(cmip5_dir,"/baseline",sep="")
rcpDir <- paste(cmip5_dir,"/rcp45",sep="")


#list of gcms hist and future
gcmList_hist <- list.files(wthDir_hist,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp,pattern="_ENS_")
gcm_outHist <- list()
gcm_outRcp <- list()

#merge both lists
gcmList <- gcmList_rcp[which(gcmList_rcp %in% gcmList_hist)]

#matrix gridcells
cells <- read.csv(paste(cropDir,"/inputs/calib-cells-selection-",selection,".csv",sep=""))

######################################################################
######################################################################
#cells to analyse:
#      1. optimisation gridcells: 435, 565, 644, 720, 960
#      2. additional gridcell southern Gujarat: 635
#      3. some additional gridcells 676, 711, 888, 889, 920, 361, 429, 465
loc <- 465 #this is in southern Gujarat, one of the highest ccoef gridcells
######################################################################
######################################################################


######################################################################
######################################################################
####### first part: extract data and create charts for each GCM
######################################################################
######################################################################
for (i in 1:length(gcmList)) {
  #i=1
  gcm <- gcmList[i]
  cat("\nprocessing",gcm,"\n")
  
  #output directories
  out_gcmDir <- paste(out_dir,"/",gcm,sep=""); if (!file.exists(out_gcmDir)) {dir.create(out_gcmDir)}
  out_cellDir <- paste(out_gcmDir,"/cell-",loc,sep=""); if (!file.exists(out_cellDir)) {dir.create(out_cellDir)}
  out_dataDir <- paste(out_cellDir,"/data",sep=""); if (!file.exists(out_dataDir)) {dir.create(out_dataDir)}
  out_figDir <- paste(out_cellDir,"/figures",sep=""); if (!file.exists(out_figDir)) {dir.create(out_figDir)}
  
  #1. load all years of data into a data.frame for all GCMs
  cat("loading data\n")
  wthFil_obs <- paste(wthDir_obs,"/rfd_",loc,"/ingc001001",yh_i:yh_f,".wth",sep="")
  wthFil_hist <- paste(wthDir_hist,"/",gcm,"/rfd_",loc,"/ingc001001",yh_i:yh_f,".wth",sep="")
  wthFil_rcp <- paste(wthDir_rcp,"/",gcm,"/rfd_",loc,"/ingc001001",yf_i:yf_f,".wth",sep="")
  
  # a. one data.frame for obs
  list_obs <- lapply(wthFil_obs,FUN=get_wth,1900)
  names(list_obs) <- paste("Y.",yh_i:yh_f,sep="")
  
  # b. one data.frame for hist
  list_hist <- lapply(wthFil_hist,FUN=get_wth,1900)
  names(list_hist) <- paste("Y.",yh_i:yh_f,sep="")
  
  # c. one data.frame for rcp45
  list_rcp <- lapply(wthFil_rcp,FUN=get_wth,2000)
  names(list_rcp) <- paste("Y.",yf_i:yf_f,sep="")
  
  #2. calculate few specific metrics for each of these time series (per year).
  #run the water balance first
  cat("calculations\n")
  list_obs_wbal <- lapply(list_obs,FUN=do_wbal)
  list_hist_wbal <- lapply(list_hist,FUN=do_wbal)
  list_rcp_wbal <- lapply(list_rcp,FUN=do_wbal)
  
  #calculate all metrics
  obs_mets <- do.call("rbind",lapply(list_obs_wbal,FUN=do_metrics,gsi,gsf))
  obs_mets <- cbind(GCM=gcm,obs_mets); row.names(obs_mets) <- 1:nrow(obs_mets)
  hist_mets <- do.call("rbind",lapply(list_hist_wbal,FUN=do_metrics,gsi,gsf))
  hist_mets <- cbind(GCM=gcm,hist_mets); row.names(hist_mets) <- 1:nrow(hist_mets)
  rcp_mets <- do.call("rbind",lapply(list_rcp_wbal,FUN=do_metrics,gsi,gsf))
  rcp_mets <- cbind(GCM=gcm,rcp_mets); row.names(rcp_mets) <- 1:nrow(rcp_mets)
  
  #put everything into result object
  gcm_outHist[[gcm]] <- hist_mets
  gcm_outRcp[[gcm]] <- rcp_mets
  
  #write csv with the data
  write.csv(obs_mets,paste(out_dataDir,"/obs_metrics.csv",sep=""),quote=T,row.names=F)
  write.csv(hist_mets,paste(out_dataDir,"/hist_metrics.csv",sep=""),quote=T,row.names=F)
  write.csv(rcp_mets,paste(out_dataDir,"/rcp_metrics.csv",sep=""),quote=T,row.names=F)
  
  #3. construct PDFs for these metrics
  metList <- names(obs_mets)[5:(ncol(obs_mets))]
  
  #making graphs for this gcm all metrics
  cat("constructing graphs\n")
  for (imet in 1:length(metList)) {
    #met <- metList[1]
    met <- metList[imet]
    #make the pdfs
    obs_pdf <- density(obs_mets[,met])
    obs_pdf_p1 <- density(obs_mets[which(obs_mets$YEAR <= 1979),met])
    obs_pdf_p2 <- density(obs_mets[which(obs_mets$YEAR >= 1980),met])
    
    hist_pdf <- density(hist_mets[,met])
    hist_pdf_p1 <- density(hist_mets[which(hist_mets$YEAR <= 1979),met])
    hist_pdf_p2 <- density(hist_mets[which(hist_mets$YEAR >= 1980),met])
    
    rcp_pdf <- density(rcp_mets[,met])
    
    #observed historical
    tiff(paste(out_figDir,"/obs_",tolower(met),".tif",sep=""),height=1000,width=1200,
         compression="lzw",res=300,pointsize=6.5)
    par(mar=c(5,5,1,8),xpd=T)
    plot(obs_pdf,xlim=c(min(c(obs_pdf$x,obs_pdf_p1$x,obs_pdf_p2$x)),max(c(obs_pdf$x,obs_pdf_p1$x,obs_pdf_p2$x))),
         ylim=c(0,max(c(obs_pdf$y,obs_pdf_p1$y,obs_pdf_p2$y))),lwd=1.5,col="grey 75",
         main=NA,xlab=tolower(met),ylab="Density")
    lines(obs_pdf_p1,lty=2,lwd=1.5)
    lines(obs_pdf_p2,lty=3,lwd=1.5)
    legend("topright",inset=c(-0.25,0),
           legend=c("1966-1993","1966-1979","1980-1993"),
           col=c("grey 75","black","black"),lty=c(1,2,3),cex=0.8)
    par(xpd=F)
    grid(lwd=0.75)
    dev.off()
    
    #gcm historical and rcp
    tiff(paste(out_figDir,"/pred_",tolower(met),".tif",sep=""),height=1000,width=1200,
         compression="lzw",res=300,pointsize=6.5)
    par(mar=c(5,5,1,8),xpd=T)
    plot(hist_pdf,ylim=c(0,max(c(hist_pdf$y,hist_pdf_p1$y,hist_pdf_p2$y,rcp_pdf$y))),
         xlim=c(min(c(hist_pdf$x,hist_pdf_p1$x,hist_pdf_p2$x,rcp_pdf$x)),max(c(hist_pdf$x,hist_pdf_p1$x,hist_pdf_p2$x,rcp_pdf$x))),
         lwd=1.5,col="blue",main=NA,xlab=tolower(met),ylab="Density")
    lines(hist_pdf_p1,lty=2,lwd=1.5,col="blue")
    lines(hist_pdf_p2,lty=3,lwd=1.5,col="blue")
    lines(rcp_pdf,lwd=1.5,col="red")
    legend("topright",inset=c(-0.25,0),
           legend=c("1966-1993","1966-1979","1980-1993","2020-2049"),
           col=c("blue","blue","blue","red"),lty=c(1,2,3,1),cex=0.75)
    par(xpd=F)
    grid(lwd=0.75)
    dev.off()
  }
}




######################################################################
######################################################################
#### summary of all GCMs
######################################################################
######################################################################
out_sum <- paste(out_dir,"/summary",sep=""); if (!file.exists(out_sum)) {dir.create(out_sum)}
out_sum_cell <- paste(out_sum,"/cell-",loc,sep=""); if (!file.exists(out_sum_cell)) {dir.create(out_sum_cell)}
save(list=c("gcm_outHist","gcm_outRcp"),file=paste(out_sum_cell,"/hist_rcp_metrics.RData",sep=""))

#met <- metList[1]
for (met in metList) {
  cat("processing variable",met,"\n")
  #build data.frame with all models for this metric
  #raw empty data frames
  hist <- data.frame(YEAR=yh_i:yh_f)
  rcp <- data.frame(YEAR=yf_i:yf_f)
  
  for (gcm in names(gcm_outHist)) {
    #gcm <- names(gcm_outHist)[1]
    hist$VALUES <- as.numeric(unlist(gcm_outHist[[gcm]][met]))
    names(hist)[ncol(hist)] <- paste(gcm)
    rcp$VALUES <- as.numeric(unlist(gcm_outRcp[[gcm]][met]))
    names(rcp)[ncol(rcp)] <- paste(gcm)
  }
  
  #multi-model mean
  hist_m <- apply(hist[2:ncol(hist)],1,FUN=function(x) {mean(x,na.rm=T)})
  hist_sd <- apply(hist[2:ncol(hist)],1,FUN=function(x) {sd(x,na.rm=T)})
  hist_n <- apply(hist[2:ncol(hist)],1,FUN=function(x) {min(x,na.rm=T)})
  hist_x <- apply(hist[2:ncol(hist)],1,FUN=function(x) {max(x,na.rm=T)})
  
  rcp_m <- apply(rcp[2:ncol(rcp)],1,FUN=function(x) {mean(x,na.rm=T)})
  rcp_sd <- apply(rcp[2:ncol(rcp)],1,FUN=function(x) {sd(x,na.rm=T)})
  rcp_n <- apply(rcp[2:ncol(rcp)],1,FUN=function(x) {min(x,na.rm=T)})
  rcp_x <- apply(rcp[2:ncol(rcp)],1,FUN=function(x) {max(x,na.rm=T)})
  
  xlims <- c(min(c(hist_n,rcp_n)),max(c(hist_x,rcp_x)))
  xlims[2] <- xlims[2]+xlims[2]*0.15
  
  #mmm densities
  pdf_hist <- density(hist_m,from=xlims[1],to=xlims[2])
  pdf_rcp <- density(rcp_m,from=xlims[1],to=xlims[2])
  
  #densities per gcm
  dp_hist <- data.frame(X=pdf_hist$x)
  dp_rcp <- data.frame(X=pdf_rcp$x)
  
  for (gcm in names(gcm_outHist)) {
    dp_hist$VALUES <- as.numeric(density(as.numeric(unlist(hist[gcm])),from=xlims[1],to=xlims[2])$y)
    names(dp_hist)[ncol(dp_hist)] <- gcm
    
    dp_rcp$VALUES <- as.numeric(density(as.numeric(unlist(rcp[gcm])),from=xlims[1],to=xlims[2])$y)
    names(dp_rcp)[ncol(dp_rcp)] <- gcm
  }
  
  hist_yx <- apply(dp_hist[2:ncol(dp_hist)],1,FUN=function(x) {max(x,na.rm=T)})
  rcp_yx <- apply(dp_rcp[2:ncol(dp_rcp)],1,FUN=function(x) {max(x,na.rm=T)})
  ylims <- c(0,max(c(hist_yx,rcp_yx)))
  
  #mean densities
  hist_ym <- apply(dp_hist[2:ncol(dp_hist)],1,FUN=function(x) {mean(x,na.rm=T)})
  hist_ysd <- apply(dp_hist[2:ncol(dp_hist)],1,FUN=function(x) {sd(x,na.rm=T)})
  hist_ytop <- hist_ym+hist_ysd
  hist_ybot <- hist_ym-hist_ysd
  
  rcp_ym <- apply(dp_rcp[2:ncol(dp_rcp)],1,FUN=function(x) {mean(x,na.rm=T)})
  rcp_ysd <- apply(dp_rcp[2:ncol(dp_rcp)],1,FUN=function(x) {sd(x,na.rm=T)})
  rcp_ytop <- rcp_ym+rcp_ysd
  rcp_ybot <- rcp_ym-rcp_ysd
  
  #cutx <- (range(xlims)[2]-range(xlims)[1])*.50
  #xlims[2] <- xlims[2]-cutx
  
  #do the plot
  tiff(paste(out_sum_cell,"/chg_",tolower(met),".tif",sep=""),height=1000,width=1200,
       compression="lzw",res=300,pointsize=6.5)
  par(mar=c(5,5,1,1))
  plot(pdf_hist,xlim=c(xlims[1],xlims[2]),lty=2,
       ylim=c(0,max(c(hist_ytop,rcp_ytop,pdf_hist$y,pdf_rcp$y))),
       lwd=1.5,col="blue",main=NA,xlab=tolower(met),ylab="Density")
  
  #pick colours
  blue <- rgb(red=180,green=200,blue=255,alpha=200,maxColorValue=255)
  red <- rgb(red=255,green=200,blue=200,alpha=100,maxColorValue=255)
  
  polygon(c(dp_hist$X,rev(dp_hist$X)),c(hist_ytop,rev(hist_ybot)),col=blue,border=blue)
  lines(dp_hist$X,hist_ym,col="blue")
  polygon(c(dp_rcp$X,rev(dp_rcp$X)),c(rcp_ytop,rev(rcp_ybot)),col=red,border=red)
  lines(dp_rcp$X,rcp_ym,col="red")
  
  #plot the MMM pdf
  lines(pdf_hist,col="blue",lwd=1.5,lty=2)
  lines(pdf_rcp,col="red",lwd=1.5,lty=2)
  grid(lwd=0.75)
  dev.off()
  
  #plot individual GCMs should it be needed
  # for (i in 2:ncol(dp_hist)) {
  #   lines(dp_hist$X,dp_hist[,i],lwd=1,col="light blue")
  #   lines(dp_rcp$X,dp_rcp[,i],lwd=1,col="pink")
  # }
  
  write.csv(dp_hist,paste(out_sum_cell,"/pdf_data_hist_",tolower(met),".csv",sep=""),row.names=F)
  write.csv(dp_rcp,paste(out_sum_cell,"/pdf_data_rcp_",tolower(met),".csv",sep=""),row.names=F)
  
  save(list=c("pdf_hist","pdf_rcp"),file=paste(out_sum_cell,"/pdf_mean_",tolower(met),".RData",sep=""))
}



######################################################################
######################################################################
####### second part: plot changes in growing season temperature and 
#######              total rainfall per GCM and the MMM, using 
######################################################################
######################################################################

#output directories
out_mapDir <- paste(out_dir,"/maps",sep=""); if (!file.exists(out_mapDir)) {dir.create(out_mapDir)}
out_mapData <- paste(out_mapDir,"/data",sep=""); if (!file.exists(out_mapData)) {dir.create(out_mapData)}
out_mapFigs <- paste(out_mapDir,"/figures",sep=""); if (!file.exists(out_mapFigs)) {dir.create(out_mapFigs)}

#load mask of india
rds <- raster(paste(src.dir3,"/data/mask.tif",sep=""))
rds[which(!is.na(rds[]))] <- 1

#months i'm interested in
mths <- c(1:12) #change this thing here

#output for those months
odata_mth <- paste(out_mapData,"/season_",mths[1],"-",mths[length(mths)],sep="")
if (!file.exists(odata_mth)) {dir.create(odata_mth)}

ofigs_mth <- paste(out_mapFigs,"/season_",mths[1],"-",mths[length(mths)],sep="")
if (!file.exists(ofigs_mth)) {dir.create(ofigs_mth)}


######################################################################
## part a: data loading ##############################################
pr_all <- list(); tas_all <- list(); tasmin_all <- list(); tasmax_all <- list()
#loop through this
for (i in 1:length(gcmList)) {
  #gcm to analyse
  #i <- 1
  gcm_ens <- gcmList[i]
  cat("\nloading data for gcm",gcm_ens,"i=",i,"of",length(gcmList),"\n")
  
  #name of model and ensemble
  gcm <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[2]
  
  #names of input directories
  h_gcmDir <- paste(histDir,"/",gcm,"/",ens,"_climatology",sep="")
  f_gcmDir <- paste(rcpDir,"/",gcm,"/",ens,"_climatology",sep="")
  
  #loading data
  chg_data <- loadData(h_gcmDir,f_gcmDir,months=mths,vars=c("pr","tas","tasmin","tasmax"),
                       chg_type=c("p","a","a","a"),calc_type=c("sum","mean","mean","mean"),
                       ext=".tif",msk=rds)
  
  pr_all[[gcm_ens]] <- chg_data$pr
  tas_all[[gcm_ens]] <- chg_data$tas
  tasmin_all[[gcm_ens]] <- chg_data$tasmin
  tasmax_all[[gcm_ens]] <- chg_data$tasmax
}

pr_stk <- stack(pr_all)
tas_stk <- stack(tas_all)
tasmin_stk <- stack(tasmin_all)
tasmax_stk <- stack(tasmax_all)

#calculate summaries for each variable
pr_sum <- calc_summary(pr_stk)
tas_sum <- calc_summary(tas_stk)
tasmin_sum <- calc_summary(tasmin_stk)
tasmax_sum <- calc_summary(tasmax_stk)

save(list=c("pr_stk","tas_stk","tasmin_stk","tasmax_stk","pr_sum","tas_sum","tasmin_sum","tasmax_sum"),
     file=paste(odata_mth,"/rs_data.RData",sep=""))

######################################################################
## part b: plotting changes ##########################################

########
#details for figures
nbrks <- 20

#colours
cols <- list()
#cols$pr <- colorRampPalette(c(rgb(190,130,25,1,1,255),rgb(225,157,35,1,1,255),
#                              rgb(245,170,30,1,1,255),rgb(100,200,250,1,1,255),
#                              rgb(90,185,222,1,1,255),rgb(85,155,181,1,1,255)))(nbrks)
cols$pr <- rev(colorRampPalette(c("blue","light blue","light green","yellow","orange"))(nbrks))
cols$tas <- colorRampPalette(c(rgb(250,240,20,1,1,255),rgb(245,175,35,1,1,255),
                               rgb(245,130,35,1,1,255),rgb(250,105,45,1,1,255),
                               rgb(235,45,45,1,1,255),rgb(180,25,95,1,1,255)))(nbrks)
cols$tasmin <- colorRampPalette(c(rgb(250,240,20,1,1,255),rgb(245,175,35,1,1,255),
                               rgb(245,130,35,1,1,255),rgb(250,105,45,1,1,255),
                               rgb(235,45,45,1,1,255),rgb(180,25,95,1,1,255)))(nbrks)
cols$tasmax <- colorRampPalette(c(rgb(250,240,20,1,1,255),rgb(245,175,35,1,1,255),
                               rgb(245,130,35,1,1,255),rgb(250,105,45,1,1,255),
                               rgb(235,45,45,1,1,255),rgb(180,25,95,1,1,255)))(nbrks)
cols$unc <- colorRampPalette(c("blue","light blue","yellow","orange","red"))(nbrks)

#breaks
brks <- list()
#brks$pr_chg <- seq(max(abs(pr_stk[]),na.rm=T)*-1,max(pr_stk[],na.rm=T),length.out=nbrks)
brks$pr_chg <- seq(min(pr_stk[],na.rm=T),max(pr_stk[],na.rm=T),length.out=nbrks)
brks$pr_sd <- seq(min(pr_sum$SD[],na.rm=T),max(pr_sum$SD[],na.rm=T),length.out=nbrks)
brks$pr_cv <- seq(0,100,length.out=nbrks)
brks$pr_ag <- seq(0,100,length.out=nbrks)

brks$tas_chg <- seq(0,max(tas_stk[],na.rm=T),length.out=nbrks)
brks$tas_sd <- seq(min(tas_sum$SD[],na.rm=T),max(tas_sum$SD[],na.rm=T),length.out=nbrks)
brks$tas_cv <- seq(0,100,length.out=nbrks)
brks$tas_ag <- seq(0,100,length.out=nbrks)

brks$tasmin_chg <- seq(0,max(tasmin_stk[],na.rm=T),length.out=nbrks)
brks$tasmin_sd <- seq(min(tasmin_sum$SD[],na.rm=T),max(tasmin_sum$SD[],na.rm=T),length.out=nbrks)
brks$tasmin_cv <- seq(0,100,length.out=nbrks)
brks$tasmin_ag <- seq(0,100,length.out=nbrks)

brks$tasmax_chg <- seq(0,max(tasmax_stk[],na.rm=T),length.out=nbrks)
brks$tasmax_sd <- seq(min(tasmax_sum$SD[],na.rm=T),max(tasmax_sum$SD[],na.rm=T),length.out=nbrks)
brks$tasmax_cv <- seq(0,100,length.out=nbrks)
brks$tasmax_ag <- seq(0,100,length.out=nbrks)


#plot here all summaries
for (vn in c("pr","tas","tasmin","tasmax")) {
  cat("plotting",vn,"\n")
  tName <- paste(ofigs_mth,"/",vn,"_chg_MMM.tif",sep="")
  make_spplot(prs=get(paste(vn,"_sum",sep=""))$MEAN,pcols=cols[[vn]],
              pbrks=brks[[paste(vn,"_chg",sep="")]],ht=1000,tiffName=tName)
  
  tName <- paste(ofigs_mth,"/",vn,"_chg_MMQ1.tif",sep="")
  make_spplot(prs=get(paste(vn,"_sum",sep=""))$Q1,pcols=cols[[vn]],
              pbrks=brks[[paste(vn,"_chg",sep="")]],ht=1000,tiffName=tName)
  
  tName <- paste(ofigs_mth,"/",vn,"_chg_MMQ4.tif",sep="")
  make_spplot(prs=get(paste(vn,"_sum",sep=""))$Q4,pcols=cols[[vn]],
              pbrks=brks[[paste(vn,"_chg",sep="")]],ht=1000,tiffName=tName)
  
  tName <- paste(ofigs_mth,"/",vn,"_chg_MMSD.tif",sep="")
  make_spplot(prs=get(paste(vn,"_sum",sep=""))$SD,pcols=cols$unc,
              pbrks=brks[[paste(vn,"_sd",sep="")]],ht=1000,tiffName=tName)
  
  tName <- paste(ofigs_mth,"/",vn,"_chg_MMCV.tif",sep="")
  make_spplot(prs=get(paste(vn,"_sum",sep=""))$CV,pcols=cols$unc,
              pbrks=brks[[paste(vn,"_cv",sep="")]],ht=1000,tiffName=tName)
  
  if (vn == "pr") {
    tName <- paste(ofigs_mth,"/",vn,"_chg_MMAG.tif",sep="")
    make_spplot(prs=get(paste(vn,"_sum",sep=""))$AG,pcols=cols$unc,
                pbrks=brks[[paste(vn,"_ag",sep="")]],ht=1000,tiffName=tName)
  }
}


#plot here the GCM specific stuff
for (i in 1:length(gcmList)) {
  gcm <- gcmList[i]
  cat("\nplotting",gcm,"\n")
  for (vn in c("pr","tas","tasmin","tasmax")) {
    cat(vn,"\n")
    tName <- paste(ofigs_mth,"/",vn,"_chg_",gcm,".tif",sep="")
    make_spplot(prs=get(paste(vn,"_stk",sep=""))[[i]],pcols=cols[[vn]],
                pbrks=brks[[paste(vn,"_chg",sep="")]],ht=1000,tiffName=tName)
    
  }
}







