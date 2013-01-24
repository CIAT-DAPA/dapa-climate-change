#Julian Ramirez-Villegas
#Jan 2012
#select GCMs from the whole pool taking into account that
#1. only one ensemble member is to be used, and 
#2. same GCMs need to be present in historical and future periods 
#   for the four variables: pr, tas, tasmin, tasmax

src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))

#input directories
gcmDir <- "/mnt/a102/eejarv/CMIP5/Amon"
hisDir <- paste(gcmDir,"/historical_amon",sep="")
rcpDir <- paste(gcmDir,"/rcp45_amon",sep="")

#output directories
out_bDir <- "/mnt/a17/eejarv/pgr-cc-adaptation"
oDir <- paste(out_bDir,"/config",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}


###############
#historical
mList_pr <- get_gcmList(vn="pr",data_dir=hisDir,yi=1965,yf=2005)
mList_tas <- get_gcmList(vn="tas",data_dir=hisDir,yi=1965,yf=2005)
mList_tasmin <- get_gcmList(vn="tasmin",data_dir=hisDir,yi=1965,yf=2005)
mList_tasmax <- get_gcmList(vn="tasmax",data_dir=hisDir,yi=1965,yf=2005)

#filter down historical
hList <- mList_pr[which(mList_pr$GCM_ENS %in% mList_tas$GCM_ENS),]
hList <- hList[which(hList$GCM_ENS %in% mList_tasmin$GCM_ENS),]
hList <- hList[which(hList$GCM_ENS %in% mList_tasmax$GCM_ENS),]

#rcp4.5
rList_pr <- get_gcmList(vn="pr",data_dir=rcpDir,yi=2020,yf=2099)
rList_tas <- get_gcmList(vn="tas",data_dir=rcpDir,yi=2020,yf=2099)
rList_tasmin <- get_gcmList(vn="tasmin",data_dir=rcpDir,yi=2020,yf=2099)
rList_tasmax <- get_gcmList(vn="tasmax",data_dir=rcpDir,yi=2020,yf=2099)

#filter down rcp4.5
fList <- rList_pr[which(rList_pr$GCM_ENS %in% rList_tas$GCM_ENS),]
fList <- fList[which(fList$GCM_ENS %in% rList_tasmin$GCM_ENS),]
fList <- fList[which(fList$GCM_ENS %in% rList_tasmax$GCM_ENS),]

#match historical and rcp4.5
geList <- fList[which(fList$GCM_ENS %in% hList$GCM_ENS),]

#leave only one ensemble member
gcmList <- unique(geList$GCM)
gcmList <- lapply(gcmList,FUN=get_single_ens,hList,geList)
gcmList <- as.data.frame(do.call("rbind",gcmList))
names(gcmList) <- c("GCM","ENS","YI_H","YF_H","YI_F","YF_F")
gcmList <- cbind(GCM_ENS=paste(gcmList$GCM,"_ENS_",gcmList$ENS,sep=""),gcmList)
gcmList$YI_H <- as.numeric(paste(gcmList$YI_H))
gcmList$YF_H <- as.numeric(paste(gcmList$YF_H))
gcmList$YI_F <- as.numeric(paste(gcmList$YI_F))
gcmList$YF_F <- as.numeric(paste(gcmList$YF_F))

#write final list of GCMs
write.csv(gcmList,paste(oDir,"/gcm_list.csv",sep=""),row.names=F,quote=T)


