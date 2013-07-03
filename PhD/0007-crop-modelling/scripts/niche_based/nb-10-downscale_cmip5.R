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
source(paste(src.dir,"/scripts/niche_based/nb-10-downscale_cmip5-fun.R",sep=""))
source(paste(src.dir,"/scripts/niche_based/nb-06-calc_bio_sdm-fun.R",sep=""))

#i/o directories
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
nbDir <- paste(bDir,"/niche-based",sep="")
envDir <- paste(nbDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate/imd_cru_climatology_1dd",sep="")
mthDir <- paste(clmDir,"/1966_1993",sep="")
bioDir <- paste(clmDir,"/1966_1993_bio",sep="")

gcmDir <- "/mnt/a102/eejarv/CMIP5"
#gcmDir <- "/nfs/a102/eejarv/CMIP5"
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

#run wrapper
for (m_i in 8:20) {dst <- gcm_downscale(m_i)}




