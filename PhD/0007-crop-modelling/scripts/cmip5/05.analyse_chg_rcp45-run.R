#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#September 2012

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"

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
i <- 1 #gcm position to analyse

#other directories
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
wthDir_obs <- paste(cropDir,"/inputs/ascii/wth",sep="")
wthDir_hist <- paste(cropDir,"/inputs/ascii/wth-cmip5_hist",sep="")
wthDir_rcp <- paste(cropDir,"/inputs/ascii/wth-cmip5_rcp45",sep="")

#list of gcms hist and future
gcmList_hist <- list.files(wthDir_hist,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp,pattern="_ENS_")

#merge both lists
gcmList <- gcmList_rcp[which(gcmList_rcp %in% gcmList_hist)]

#matrix gridcells
cells <- read.csv(paste(cropDir,"/inputs/calib-cells-selection-",selection,".csv",sep=""))

#cell and gcm to analyse
loc <- 529 #this is in northern Gujarat, one of the highest ccoef gridcells
gcm <- gcmList[i]

#1. load all years of data into a data.frame for all GCMs
wthFil_obs <- paste(wthDir_obs,"/rfd_",loc,"/ingc001001",yh_i:yh_f,".wth",sep="")
wthFil_hist <- paste(wthDir_hist,"/",gcm,"/rfd_",loc,"/ingc001001",yh_i:yh_f,".wth",sep="")
wthFil_rcp <- paste(wthDir_rcp,"/",gcm,"/rfd_",loc,"/ingc001001",yf_i:yf_f,".wth",sep="")

#       a. one data.frame for obs
list_obs <- lapply(wthFil_obs,FUN=get_wth,1900)
names(list_obs) <- paste("Y.",yh_i:yh_f,sep="")

#       b. one data.frame for hist
list_hist <- lapply(wthFil_hist,FUN=get_wth,1900)
names(list_hist) <- paste("Y.",yh_i:yh_f,sep="")

#       c. one data.frame for rcp45
list_rcp <- lapply(wthFil_rcp,FUN=get_wth,2000)
names(list_rcp) <- paste("Y.",yf_i:yf_f,sep="")


#2. calculate few specific metrics for each of these time series (per year).

#run the water balance first
list_obs_wbal <- lapply(list_obs,FUN=do_wbal)
list_hist_wbal <- lapply(list_hist,FUN=do_wbal)
list_rcp_wbal <- lapply(list_rcp,FUN=do_wbal)

#calculate all metrics
obs_mets <- do.call("rbind",lapply(list_obs_wbal,FUN=do_metrics,gsi,gsf))
hist_mets <- do.call("rbind",lapply(list_hist_wbal,FUN=do_metrics,gsi,gsf))
rcp_mets <- do.call("rbind",lapply(list_rcp_wbal,FUN=do_metrics,gsi,gsf))


#3. construct PDFs for these metrics
metList <- names(obs_mets)[4:(ncol(obs_mets))]

met <- metList[1]
obs_pdf <- density(obs_mets[,met])
hist_pdf <- density(hist_mets[,met])
rcp_pdf <- density(rcp_mets[,met])

obs_pdf$y <- obs_pdf$y/max(obs_pdf$y)
hist_pdf$y <- hist_pdf$y/max(hist_pdf$y)
rcp_pdf$y <- rcp_pdf$y/max(rcp_pdf$y)

#plot(obs_mets[,met],hist_mets[,met],pch=20)

plot(obs_pdf,xlim=c(min(c(obs_pdf$x,hist_pdf$x,rcp_pdf$x)),
                    max(c(obs_pdf$x,hist_pdf$x,rcp_pdf$x))))
#plot(hist_pdf,col="blue")
lines(hist_pdf,col="blue")
lines(rcp_pdf,col="red")

met <- "EFF.SRAD"
range(obs_mets[,met]); range(hist_mets[,met]); range(rcp_mets[,met])

