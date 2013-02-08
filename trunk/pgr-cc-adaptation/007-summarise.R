#Julian Ramirez-Villegas
#Jan 2012

library(raster); library(sfsmisc); library(maptools)
data(wrld_simpl)

src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/pgr-cc-adaptation"
source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))

#base directories
gcmDir <- "/mnt/a102/eejarv/CMIP5/Amon"
bDir <- "/mnt/a17/eejarv/pgr-cc-adaptation"
scratch <- "~/Workspace/pgr_analogues"

#input directories
hisDir <- paste(gcmDir,"/historical_amon",sep="")
rcpDir <- paste(gcmDir,"/rcp45_amon",sep="")
cfgDir <- paste(bDir,"/config",sep="")
cruDir <- paste(bDir,"/cru-data",sep="")

#output directories
out_bDir <- paste(bDir,"/outputs",sep="")

#configuration details and initial data
gcmList <- read.csv(paste(cfgDir,"/gcm_list.csv",sep=""))
gcmList <- gcmList[c(1:6,9,16,19,22),]
row.names(gcmList) <- 1:nrow(gcmList)

#cru raster to filter out NAs
if (!file.exists(paste(cfgDir,"/dum_rs.RData",sep=""))) {
  dum_rs <- raster(paste(cruDir,"/cru_ts_3_10.1966.2005.tmx.dat.nc",sep=""),band=0)
  save(dum_rs,file=paste(cfgDir,"/dum_rs.RData",sep=""))
} else {
  load(paste(cfgDir,"/dum_rs.RData",sep=""))
}

#crop
crop_name <- "RICE"

#gcm
all_ov <- list(); all_bf <- list(); all_cy <- list()

for (gcm_i in 1:nrow(gcmList)) {
  gcm <- paste(gcmList$GCM[gcm_i])
  ens <- paste(gcmList$ENS[gcm_i])
  
  #output GCM-specific directory
  gcm_outDir <- paste(out_bDir,"/",gcmList$GCM_ENS[gcm_i],sep="")
  
  #load overlaps
  load(paste(gcm_outDir,"/raster_outputs.RData",sep=""))
  over_rs <- out_rs
  rm(out_rs)
  
  load(paste(gcm_outDir,"/adap_raster_outputs.RData",sep=""))
  adap_rs <- out_rs
  rm(out_rs)
  
  ov_rs <- over_rs[[crop_name]]$P_2075; all_ov[[gcm_i]] <- ov_rs
  bf_rs <- adap_rs[[crop_name]]$BUF_2075; all_bf[[gcm_i]] <- bf_rs
  cy_rs <- adap_rs[[crop_name]]$CTR_2075; all_cy[[gcm_i]] <- cy_rs
  
  crop_data <- as.data.frame(xyFromCell(ov_rs,which(!is.na(ov_rs[]))))
  crop_data$OV <- ov_rs[which(!is.na(ov_rs[]))]
  crop_data$BF <- bf_rs[which(!is.na(ov_rs[]))]
  crop_data$CY <- cy_rs[which(!is.na(ov_rs[]))]
  
  #rule 1 overlap below 25%
  crop_data$R1 <- 0
  crop_data$R1[which(crop_data$OV < 0.25)] <- 1
  
  #rule 2 buffer adapt above 75%
  crop_data$R2 <- 0
  crop_data$R2[which(crop_data$BF > 0.75)] <- 1
  
  #rule 3 country adapt above 75%
  crop_data$R3 <- 0
  crop_data$R3[which(crop_data$CY > 0.75)] <- 1
  
  #total of rules
  crop_data$RT <- crop_data$R1 + crop_data$R2 + crop_data$R3
  
  #selected points
  #selp <- crop_data[which(crop_data$R1 == 1 & crop_data$R2 == 1),]
  #selp2 <- crop_data[which(crop_data$R1 == 1 & crop_data$R3 == 1),]
  
  #x11()
  #plot(ov_rs)
  #plot(wrld_simpl,add=T)
  #points(selp2$x,selp2$y,pch=20,cex=0.1,col="red")
  #points(selp$x,selp$y,pch=20,cex=0.1,col="blue")
  
  #some useful info
  cat("\n")
  cat("\nclimate model",gcm,"\n")
  cat("locations with <25% overlap:",sum(crop_data$R1)/nrow(crop_data)*100,"%\n")
  cat("of those how many have >75% overlap in buffer:",nrow(crop_data[which(crop_data$R1 == 1 & crop_data$R2 == 1),])/sum(crop_data$R1)*100,"%\n")
  cat("of those how many have >75% overlap in country:",nrow(crop_data[which(crop_data$R1 == 1 & crop_data$R3 == 1),])/sum(crop_data$R1)*100,"%\n")
}

#stacks
all_ov <- stack(all_ov)
all_bf <- stack(all_bf)
all_cy <- stack(all_cy)

#mean and sd
ov_mean <- calc(all_ov,fun=function(x) {mean(x,na.rm=T)})
ov_sd <- calc(all_ov,fun=function(x) {sd(x,na.rm=T)})

bf_mean <- calc(all_bf,fun=function(x) {mean(x,na.rm=T)})
bf_sd <- calc(all_bf,fun=function(x) {sd(x,na.rm=T)})

cy_mean <- calc(all_cy,fun=function(x) {mean(x,na.rm=T)})
cy_sd <- calc(all_cy,fun=function(x) {sd(x,na.rm=T)})

## save outputs
out_resDir <- paste(out_bDir,"/_results",sep="")
if (!file.exists(out_resDir)) {dir.create(out_resDir)}

save(list=c("all_ov","all_bf","all_cy","ov_mean","ov_sd","bf_mean","bf_sd","cy_mean","cy_sd"),
     file=paste(out_resDir,"/ensemble_",tolower(crop_name),".RData",sep=""))

#totals
crop_data <- as.data.frame(xyFromCell(ov_mean,which(!is.na(ov_mean[]))))
crop_data$OV_M <- ov_mean[which(!is.na(ov_mean[]))]
crop_data$OV_SD <- ov_sd[which(!is.na(ov_mean[]))]
crop_data$BF_M <- bf_mean[which(!is.na(ov_mean[]))]
crop_data$BF_SD <- bf_sd[which(!is.na(ov_mean[]))]
crop_data$CY_M <- cy_mean[which(!is.na(ov_mean[]))]
crop_data$CY_SD <- cy_sd[which(!is.na(ov_mean[]))]

selp <- crop_data[which(crop_data$OV_M < 0.5 & crop_data$CY_M > 0.75),]
selp2 <- crop_data[which(crop_data$OV_M < 0.5 & crop_data$BF_M > 0.75),]

#x11()
#plot(ov_mean)
#plot(wrld_simpl,add=T)
#points(selp2$x,selp2$y,pch=20,cex=0.1,col="red")
#points(selp$x,selp$y,pch=20,cex=0.1,col="blue")

###plotting
#general plot characteristics
xt <- extent(-130,160,-45,75)
rs <- ov_mean
rs <- crop(rs,xt)

ht <- 1000
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=30), norths=seq(-90,90,by=30))
grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)


############################################
brks <- seq(0,1,by=0.025)
brks.lab <- round(brks,2)
cols <- c(colorRampPalette(c("dark red","red","orange","yellow","light blue","blue","dark blue"))(length(brks)))

#points of buffer
xybuf <- cbind(x=crop_data$x[which(crop_data$OV_M < 0.5 & crop_data$CY_M > 0.75)],
               y=crop_data$y[which(crop_data$OV_M < 0.5 & crop_data$CY_M > 0.75)])
xybuf <- SpatialPoints(xybuf)
pts <- list("sp.points", xybuf, pch = 4, col = "black", cex=0.75, lwd=0.6,first=F)
layt <- list(pts,wld,grli)

#do the plot
figName <- paste(tolower(cropname),"_",tolower(period),sep="")
tiffName <- paste(fig_dir,"/",figName,".tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt)
x <- spplot(rs,sp.layout=layt,col.regions=cols,
            par.settings=list(fontsize=list(text=8)),
            at=brks,pretty=brks)
print(x)
dev.off()




