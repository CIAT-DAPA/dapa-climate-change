


### Prepare WFD data
mthmat <- as.data.frame(cbind(c(paste(0,c(1:9),sep=""),paste(c(10:12))), c(1:12), c(31,28,31,30,31,30,31,31,30,31,30,31), c(31,29,31,30,31,30,31,31,30,31,30,31)))
names(mthmat) <- c("mth", "mthmod", "ndays", "ndays_leapyear")


iDir <- "S:/observed/gridded_products/wfd/raw"
oDir <- "D:/CIAT/Workspace/Rainf_daily_WFD_GPCC/tmp"

setwd(iDir)

if (var == "Rainf"){
  ncList <- list.files(paste(dirbase, "/raw/", var, "_daily_WFD_GPCC", sep=""), full.names = TRUE)
} else {
  ncList <- list.files(paste(dirbase, "/raw/", var, "_daily_WFD", sep=""), full.names = TRUE)
}

diroutvar <- paste(dirout, "/", tolower(var), "-daily", sep="")
if (!file.exists(diroutvar)) {dir.create(diroutvar)}

for (nc in ncList){
  
  ## Estract year and month from nc name
  if (var == "Rainf"){
    
    year <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 5), 1, 4)
    mth <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 5), 5, 6)
    
  } else {
    
    year <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 4), 1, 4)
    mth <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 4), 5, 6)
    
  }
   
}

system(paste("cdo -settaxis,", year, "-", mth, "-01,00:00:00,1day ", nc, " ", oDir, "/", nc, sep=""))





ncFiles <- list.files(path=paste0(iDir), full.names=FALSE)


for (nc in ncFiles){

  nc <- ncFiles[1]
  year <- as.integer(substr(strsplit(nc, '[_]')[[1]][length(strsplit(nc, '[_]')[[1]])],1,4))
  month <- as.integer(substr(strsplit(nc, '[_]')[[1]][length(strsplit(nc, '[_]')[[1]])],5,6))

  
  if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) {
  
    
    dates = seq(as.Date(paste0(year, "-", month, "-01"),as.Date(paste0(year, "-", month, "-01"),by=1)
    
  }
  
  
  
}
