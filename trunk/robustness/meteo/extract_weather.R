#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

###
#for a particular location, extract daily weather using cdo
###

#cellid: is the cell identifier from the robustness study. check initial_conditions_*.RData
#data_type: either obs or gcm
#sce: hist, rcp26, rcp45, rcp60, rcp85
#dataset: WFD, WFDEI, or name of GCM (e.g. gfdl-esm2m)
#years: range of years from which to extract

# #example
# #---------------------------------------------------------------
# #load libraries
# library(sp); library(raster); library(rgdal); library(maptools)
# 
# #directory
# wd <- "~/Leeds-work/quest-for-robustness"
# src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
# met_dir <- paste(wd,"/data/meteorology",sep="")
# mdata_dir <- paste(wd,"/data/model_data",sep="")
# 
# #source needed functions
# source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
# 
# #load xy_main
# load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))

# #run function
# extract_weather(cellid=xy_main$LOC[229], lon=xy_main$x[229], lat=xy_main$y[229], metDir, data_type="obs", dataset="WFD", sce="hist", years=1950:2001)
# extract_weather(cellid=xy_main$LOC[229], lon=xy_main$x[229], lat=xy_main$y[229], metDir, data_type="obs", dataset="WFDEI", sce="hist", years=1979:2010)
# extract_weather(cellid=xy_main$LOC[229], lon=xy_main$x[229], lat=xy_main$y[229], metDir, data_type="gcm", dataset="gfdl-esm2m", sce="hist", years=1950:2005)
# extract_weather(cellid=xy_main$LOC[229], lon=xy_main$x[229], lat=xy_main$y[229], metDir, data_type="gcm", dataset="gfdl-esm2m", sce="rcp26", years=2006:2099)
# #---------------------------------------------------------------

extract_weather <- function(cellid, lon, lat, met_dir, data_type="obs", dataset="WFD", sce="hist", years=1950:2005, write_wthfil=T) {
  #get arguments in proper format
  data_type <- tolower(data_type); sce <- tolower(sce)
  if (data_type == "obs") {dataset <- toupper(dataset)} else {dataset <- tolower(dataset)}
  
  #create temporary folder for daily met of location
  asc_dir <- paste(met_dir,"/ascii_extract_raw",sep="")
  if (!file.exists(asc_dir)) {dir.create(asc_dir)}
  
  #checking that the arguments make sense
  if (!data_type %in% c("obs","gcm")) {
    stop("data_type must be either obs or gcm")
  } else {
    if (tolower(data_type) == "obs") {
      if (is.na(dataset)) {stop("either WFD or WFDEI must be specified for obs")}
      if (tolower(sce) != "hist") {stop("period must be hist for obs")}
      if (!tolower(dataset) %in% c("wfd","wfdei")) {stop("dataset must be either WFD or WFDEI")}
      if (tolower(dataset) == "wfd") {
        if (length(which(!years %in% c(1950:2001))) > 0) {stop("specified years should be in the range 1950-2001 for WFD")}
      } else {
        if (length(which(!years %in% c(1979:2010))) > 0) {stop("specified years should be in the range 1979-2010 for WFD")}
      }
      vnames <- c("Rainf","SWdown","Tmax","Tmin")
    } else {
      if (!tolower(sce) %in% c("hist","rcp26","rcp45","rcp60","rcp85")) {stop("sce must be either hist, rcp26, rcp45, rcp60, rcp85")}
      if (!tolower(dataset) %in% c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc-esm-chem","noresm1-m")) {stop("dataset must be one of gfdl-esm2m, hadgem2-es, ipsl-cm5a-lr, miroc-esm-chem, noresm1-m")}
      if (tolower(dataset) == "hadgem2-es" & tolower(sce) == "hist") {yi <- 1950; yf <- 2004}
      if (sce == "hist") {
        if (tolower(dataset) == "hadgem2-es") {yi <- 1950; yf <- 2004} else {yi <- 1950; yf <- 2005}
      } else {
        if (tolower(dataset) == "hadgem2-es") {yi <- 2004; yf <- 2099} else {yi <- 2006; yf <- 2099}
      }
      if (length(which(!years %in% c(yi:yf))) > 0) {stop("specified years are not in the expected range")}
      vnames <- c("pr","rsds","tasmax","tasmin")
    }
  }
  
  #create folder of output
  out_adir <- paste(asc_dir,"/",data_type,"_",sce,"_",dataset,sep="")
  if (!file.exists(out_adir)) {dir.create(out_adir)}
  
  #output file
  out_file <- paste(out_adir,"/meteo_cell-",cellid,".met",sep="")
  
  if (!file.exists(out_file)) {
    #loop through files
    for (vname in vnames) {
      #vname <- vnames[1]
      
      #generate list of files from which to extract
      if (data_type == "obs") {
        #years_months
        ym_m <- expand.grid(YEAR=years,MONTH=sprintf("%1$02d",1:12))
        ym_m$YM <- paste(ym_m$YEAR,ym_m$MONTH,sep="")
        if (vname == "Rainf") {
          fnames <- paste(met_dir,"/baseline_climate/",vname,"_daily_",dataset,"_GPCC","/afr_",vname,"_daily_",dataset,"_GPCC_",ym_m$YM,".nc",sep="")
        } else {
          fnames <- paste(met_dir,"/baseline_climate/",vname,"_daily_",dataset,"/afr_",vname,"_daily_",dataset,"_",ym_m$YM,".nc",sep="")
        }
      } else {
        fnames <- list.files(paste(met_dir,"/future_climate/",dataset,"/",sce,sep=""),pattern=paste("afr_",vname,sep=""))
        fnames <- paste(met_dir,"/future_climate/",dataset,"/",sce,"/",fnames,sep="")
      }
      
      #loop files to extract data
      fdata <- data.frame(); fcount <- 1
      for (fname in fnames) {
        #fname <- fnames[1]
        #temporary output file
        odat <- paste(out_adir,"/",vname,"_cell-",cellid,"_",min(years),"-",max(years),".tab",sep="")
        
        #extract the data
        if (!file.exists(odat)) {system(paste("cdo -outputtab,year,month,day,lon,lat,value -remapnn,lon=",lon,"_lat=",lat," ",fname," > ",odat,sep=""))}
        
        #read in  the data
        idata <- read.table(odat,header=F)
        system(paste("rm -f ",odat,sep=""))
        names(idata) <- c("year","month","day","lon","lat",vname)
        
        if (tolower(dataset) == "wfd" | tolower(dataset) == "wfdei") {
          idata$year <- ym_m$YEAR[fcount]
          idata$month <- as.numeric(ym_m$MONTH[fcount])
          idata$day <- 1:nrow(idata)
        }
        fdata <- rbind(fdata, idata)
        fcount <- fcount+1
      }
      
      #select only years in question (only for gcm)
      if (tolower(data_type) == "gcm") {
        idata <- data.frame(year=years)
        idata <- merge(fdata, idata, by="year", all.x=F, all.y=F, sort=F)
      } else {
        idata <- fdata
      }
      
      #cbind all variables
      if (vname == vnames[1]) {
        metdata <- idata
      } else {
        metdata <- merge(metdata,idata,by=c("year","month","day","lon","lat"), sort=F)
      }
    }
    
    #remove 29th feb days in leap years
    metdata <- metdata[-which(metdata$month==2 & metdata$day==29),]
    
    #add julian day
    jdaym <- metdata[1:365,c("month","day")]
    jdaym$jday <- 1:365
    metdata <- merge(metdata, jdaym, by=c("month","day"), sort=F)
    metdata$month <- NULL; metdata$day <- NULL
    
    #sort met data
    metdata <- metdata[order(metdata$year, metdata$jday),]
    row.names(metdata) <- 1:nrow(metdata)
    names(metdata) <- c("year","lon","lat","pr","rsds","tasmax","tasmin","jday")
    
    #unit conversion for each variable
    metdata$pr <- metdata$pr * 3600 * 24 #kg m-2 s-1 to mm/day (GLAM takes mm and converts itself to cm)
    metdata$pr[which(metdata$pr < 0)] <- 0
    metdata$tasmax <- metdata$tasmax - 273.15 #K to C
    metdata$tasmin <- metdata$tasmin - 273.15 #K to C
    metdata$rsds <- metdata$rsds * 24 * 3600 / 1000000 #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
    
    #save file with all data
    write.table(metdata,file=out_file,col.names=T,row.names=F,sep="\t")
  }
  
  if (write_wthfil) {
    xin <- data.frame(CELL=cellid,X=lon,Y=lat)
    xout <- make_wth(x=xin,wthDir_in=out_adir,wthDir_out=paste(out_adir,"/loc-",cellid,sep=""),years=years,fields=list(CELL="CELL",X="X",Y="Y"))
  } else {
    xout <- out_file
  }
  return(xout)
}


