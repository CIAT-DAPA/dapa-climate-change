#Julian Ramirez-Villegas
#Jan 2012
#functions for PGR paper

#find month given a Julian day 
find_month <- function(x) {
  #x <- vals[1]
  x <- round(x,0)
  if (x==0) {x <- 1}
  if (x>365) {x <- 365}
  dg <- createDateGrid(2000)
  dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
  dg$DOM <- as.numeric(substr(dg$MTH.DAY,5,6))
  m <- dg$MTH[which(dg$JD == x)]
  return(m)
}


#get only first listed ensemble member for each gcm
get_single_ens <- function(x,list1,list2) {
  gcm <- paste(x)
  ens <- paste(list2$ENS[which(list2$GCM == gcm)][1])
  yri_h <- list1$YI[which(list1$GCM == gcm & list1$ENS == ens)]
  yrf_h <- list1$YF[which(list1$GCM == gcm & list1$ENS == ens)]
  yri_f <- list2$YI[which(list2$GCM == gcm & list2$ENS == ens)]
  yrf_f <- list2$YF[which(list2$GCM == gcm & list2$ENS == ens)]
  yret <- c(gcm,ens,yri_h,yrf_h,yri_f,yrf_f)
  return(yret)
}


#get list of GCM and ensemble members
get_gcmList <- function(vn,data_dir,yi,yf) {
  rawList <- list.files(paste(data_dir,"/",vn,sep=""),recursive=T,include.dirs=F)
  mList <- lapply(rawList,FUN=get_details)
  mList <- as.data.frame(do.call("rbind",mList))
  names(mList) <- c("GCM","ENS","YI","YF")
  mList <- cbind(GCM_ENS=paste(mList$GCM,"_ENS_",mList$ENS,sep=""),mList)
  mList$YI <- as.numeric(paste(mList$YI))
  mList$YF <- as.numeric(paste(mList$YF))
  
  #eliminate repetitions (due to files being splitted in groups of years)
  uList <- paste(unique(mList$GCM_ENS))
  uList <- lapply(uList,FUN=get_final_details,mList)
  uList <- as.data.frame(do.call("rbind",uList))
  names(uList) <- c("GCM","ENS","YI","YF")
  uList <- cbind(GCM_ENS=paste(uList$GCM,"_ENS_",uList$ENS,sep=""),uList)
  uList$YI <- as.numeric(paste(uList$YI))
  uList$YF <- as.numeric(paste(uList$YF))
  
  #check if period includes yi (1965) and yf (2005)
  uList$PERIOD <- F
  uList$PERIOD[which(uList$YI <= yi & uList$YF >= yf)] <- T
  uList <- uList[which(uList$PERIOD),]
  uList$PERIOD <- NULL
  return(uList)
}


#get final details (without repetition)
get_final_details <- function(x,list2) {
  gcm <- unlist(strsplit(x,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(x,"_ENS_",fixed=T))[2]
  yri <- min(list2$YI[which(list2$GCM_ENS==x)])
  yrf <- max(list2$YF[which(list2$GCM_ENS==x)])
  yret <- c(gcm,ens,yri,yrf)
  return(yret)
}


#get details of listed files
get_details <- function(x) {
  y <- unlist(strsplit(x,"/",fixed=T))
  gcm <- y[1]
  ens <- y[2]
  fil <- y[3]
  yrs <- unlist(strsplit(fil,"_",fixed=T))
  yrs <- yrs[length(yrs)]
  yrs <- gsub(".nc","",yrs)
  yri <- as.numeric(substr(unlist(strsplit(yrs,"-",fixed=T))[1],1,4))
  yrf <- as.numeric(substr(unlist(strsplit(yrs,"-",fixed=T))[2],1,4))
  yret <- c(gcm,ens,yri,yrf)
  return(yret)
}

