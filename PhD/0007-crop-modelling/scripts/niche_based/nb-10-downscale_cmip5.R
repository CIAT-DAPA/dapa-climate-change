#JRV May 2013
#process Andean occurrence data
stop("dont run")

##############################
#Procedure to follow
##############################

#1. list the GCMs that were used for GLAM
#2. for each variable: load the 
#   1966_1993 data (obs, hist) and 
#   2022-2049 data (rcp45)
#3. calculate / write baseline for: 
#   loci (bc but only for rain, i.e., replace obs temperature)
#   no need to write other baselines because that == obs
#4. calculate future climate for each method: del, bc, loci
#5. put the 'raw' in there as well (but running this may not be necessary)
#6. calculate the 'bio' variables for each one


##############################
##############################

#load relevant package(s)
library(raster); library(rgdal); library(maptools)

#source dir
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#source functions
source(paste(src.dir,"/scripts/niche_based/nb-10-sdm_downscale_cmip5-fun.R",sep=""))

#i/o directories
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
nbDir <- paste(bDir,"/niche-based",sep="")
envDir <- paste(nbDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate/imd_cru_climatology_1dd",sep="")
mthDir <- paste(clmDir,"/1966_1993",sep="")
bioDir <- paste(clmDir,"/1966_1993_bio",sep="")

gcmDir <- "/mnt/a102/eejarv/CMIP5"
hisDir <- paste(gcmDir,"/baseline",sep="")
rcpDir <- paste(gcmDir,"/rcp45",sep="")

outDir <- paste(envDir,"/climate/cmip5",sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

outLOCI_b <- paste(outDir,"/baseline_loci",sep="")
outraw_b <- paste(outDir,"/baseline_raw",sep="")
outLOCI_p <- paste(outDir,"/rcp_loci",sep="")
outdel_p <- paste(outDir,"/rcp_del",sep="")
outraw_p <- paste(outDir,"/rcp_raw",sep="")

if (!file.exists(outLOCI_b)) {dir.create(outLOCI_b)}
if (!file.exists(outraw_b)) {dir.create(outraw_b)}
if (!file.exists(outLOCI_p)) {dir.create(outLOCI_p)}
if (!file.exists(outdel_p)) {dir.create(outdel_p)}
if (!file.exists(outraw_p)) {dir.create(outraw_p)}


#1. list the GCMs (and ens members) that were used for GLAM (those which runs didnt fail)
gcm_list <- list.files(paste(bDir,"/GLAM/model-runs/GNUT/runs/cmip5_all/_outputs/raw_output_v2",sep=""),pattern="_ENS_")
gcm_list <- data.frame(GCM_ENS=gcm_list)
gcm_list$GCM <- sapply(gcm_list$GCM_ENS,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[1]})
gcm_list$ENS <- sapply(gcm_list$GCM_ENS,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[2]})

#select gcm
gcm_i <- 1
gcm <- gcm_list$GCM[gcm_i]
ens <- gcm_list$ENS[gcm_i]

#i/o gcm dirs
hisgcmDir <- paste(hisDir,"/",gcm,"/",ens,"_monthly",sep="")
rcpgcmDir <- paste(rcpDir,"/",gcm,"/",ens,"_monthly",sep="")

ogcmLOCI_b <- paste(outLOCI_b,"/",gcm,"_ENS_",ens,sep="")
ogcmraw_b <- paste(outraw_b,"/",gcm,"_ENS_",ens,sep="")
ogcmLOCI_p <- paste(outLOCI_p,"/",gcm,"_ENS_",ens,sep="")
ogcmdel_p <- paste(outdel_p,"/",gcm,"_ENS_",ens,sep="")
ogcmraw_p <- paste(outraw_p,"/",gcm,"_ENS_",ens,sep="")
if (!file.exists(ogcmLOCI_b)) {dir.create(ogcmLOCI_b)}
if (!file.exists(ogcmraw_b)) {dir.create(ogcmraw_b)}
if (!file.exists(ogcmLOCI_p)) {dir.create(ogcmLOCI_p)}
if (!file.exists(ogcmdel_p)) {dir.create(ogcmdel_p)}
if (!file.exists(ogcmraw_p)) {dir.create(ogcmraw_p)}


#2. for each variable: load the 1966_1993 data (obs, hist, rcp)
vn <- "prec"
msk <- raster(paste(envDir,"/mask/mask_1dd.tif",sep=""))

#observed
obs_stk <- stack(paste(mthDir,"/",vn,"_",1:12,".tif",sep=""))

#historical GCM
if (vn == "prec") {
  his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/pr_",sprintf("%02d",x),".tif",sep=""))})
} else if (vn == "tmax") {
  his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/tasmax_",sprintf("%02d",x),".tif",sep=""))})
} else if (vn == "tmin") {
  his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/tasmin_",sprintf("%02d",x),".tif",sep=""))})
} else if (vn == "tmean") {
  his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/tas_",sprintf("%02d",x),".tif",sep=""))})
}

his_stk <- lapply(his_stk,FUN=function(x) {calc(x,fun=function(y) {mean(y,na.rm=T)})})
his_stk <- lapply(his_stk,FUN=function(x) {rotate(x)})
his_stk <- lapply(his_stk,FUN=function(x,y) {resample(x,y,method="ngb")},msk)
his_stk <- stack(his_stk)
if (vn != "prec") {his_stk <- his_stk * 10} #to make temperatures comparable

#rcp45 GCM
if (vn == "prec") {
  rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/pr_",sprintf("%02d",x),".tif",sep=""))})
} else if (vn == "tmax") {
  rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/tasmax_",sprintf("%02d",x),".tif",sep=""))})
} else if (vn == "tmin") {
  rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/tasmin_",sprintf("%02d",x),".tif",sep=""))})
} else if (vn == "tmean") {
  rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/tas_",sprintf("%02d",x),".tif",sep=""))})
}

#load and crop
rcp_stk <- lapply(rcp_stk,FUN=function(x) {calc(x,fun=function(y) {mean(y,na.rm=T)})})
rcp_stk <- lapply(rcp_stk,FUN=function(x) {rotate(x)})
rcp_stk <- lapply(rcp_stk,FUN=function(x,y) {resample(x,y,method="ngb")},msk)
rcp_stk <- stack(rcp_stk)
if (vn != "prec") {rcp_stk <- rcp_stk * 10} #to make temperatures comparable

#3. write baseline for: 
#   loci (bc but only for rain, i.e., replace obs temperature)
#   no need to write other baselines because that == obs
if (vn == "prec") {wrtstk <- obs_stk} else {wrtstk <- his_stk}
for (m in 1:12) {
  if (!file.exists(paste(ogcmLOCI_b,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(wrtstk[[m]],paste(ogcmLOCI_b,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
  if (!file.exists(paste(ogcmraw_b,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(his_stk[[m]],paste(ogcmraw_b,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
}
rm(wrtstk)


#4. calculate future climate for each method: del, bc, loci
#calculate bias factors
del_stk <- list()
for (m in 1:12) {del_stk[[m]] <- biasfun(obs_stk[[m]],his_stk[[m]],rcp_stk[[m]],vn)}


#5. write everything put the 'raw' in there as well (but running this may not be necessary)
if (vn == "prec") {wrtstk <- del_stk} else {wrtstk <- rcp_stk}
for (m in 1:12) {
  if (!file.exists(paste(ogcmLOCI_p,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(wrtstk[[m]],paste(ogcmLOCI_p,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
  if (!file.exists(paste(ogcmdel_p,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(del_stk[[m]],paste(ogcmdel_p,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
  if (!file.exists(paste(ogcmraw_p,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(rcp_stk[[m]],paste(ogcmraw_p,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
}

#6. calculate the 'bio' variables for each one






