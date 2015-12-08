#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015

#Get outputs from bias_correct_met.R and write DSSAT formatted weather files

#source dir
src.dir <- "~/Repositories/dapa-climate-change/drybean-future-tpe"

#directories
wd <- "~/Leeds-work/drybean-future-tpe"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
dss_odir <- paste(wd,"/dssat_meteorology",sep="")
if (!file.exists(dss_odir)) {dir.create(dss_odir)}

source(paste(src.dir,"/make_wthfile.R",sep=""))

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

## variable, scenario, gcm and method list
varlist <- c("prec", "tmax", "tmin", "srad")
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
mthlist <- c("cf","del")
gcmlist <- list.files(paste(gcm_dir,"/loc_",gsub(".","",loc_list$id[1],fixed=T),"/gcm",sep=""))

#CO2 concentrations (2021-2045)
#scenario  period	    co2_ppm
#hist	     1981-2005	352.9
#rcp26	   2016-2040	446.5
#rcp45	   2016-2040	468.2
#rcp60	   2016-2040	452.5
#rcp85	   2016-2040	501.8

#canopy Pn response to CO2 (unc. bounds)
#!  65.0  2.05 .0116                         CCMP,CCMAX,CCEFF; CO2 EFFECT ON PGCAN (high)
#!  65.0  1.95 .0116                         CCMP,CCMAX,CCEFF; CO2 EFFECT ON PGCAN (low)

for (mth in mthlist) {
  #mth <- mthlist[1]
  for (rcp in rcplist) {
    #rcp <- rcplist[1]
    for (gcm in gcmlist) {
      #gcm <- gcmlist[1]
      if (!file.exists(paste(dss_odir,"/method_",mth,"_",rcp,"_",gcm,".tar.bz2",sep=""))) {
        gcm_odir <- paste(dss_odir,"/method_",mth,"_",rcp,"_",gcm,sep="")
        if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
        
        for (wst in loc_list$id) {
          #wst <- paste(loc_list$id[1])
          #weather stsation details
          lon <- loc_list$lon[which(loc_list$id == wst)]
          lat <- loc_list$lat[which(loc_list$id == wst)]
          ele <- loc_list$elev[which(loc_list$id == wst)]
          sta <- paste(loc_list$uf[which(loc_list$id == wst)])
          mun <- paste(loc_list$municipio[which(loc_list$id == wst)])
          stn <- paste(loc_list$file_name[which(loc_list$id == wst)])
          
          cat("writing method=",mth," / rcp=",rcp," / gcm=",gcm," / wst=",wst,"\n",sep="")
          
          #input folders
          wst_name <- gsub(".","",wst,fixed=T)
          wst_idir <- paste(gcm_dir,"/loc_",wst_name,sep="")
          
          if (file.exists(paste(wst_idir,"/obs",sep=""))) {
            #read in data for location
            for (vname in varlist) {
              #vname <- varlist[2]
              wsdata <- read.table(paste(wst_idir,"/",mth,"_ts_",rcp,"_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              wsdata <- wsdata[,c("date",gcm)]; names(wsdata) <- c("date",vname)
              wsdata$date <- paste(wsdata$date)
              if (vname == varlist[1]) {
                ws_data <- wsdata
              } else {
                ws_data <- merge(ws_data, wsdata, by="date")
              }
              rm(wsdata)
            }
            
            #srad w/m2 to MJ m-2 d-1 (below will do to KJ)
            ws_data$srad <- ws_data$srad * (24 * 60 * 60) / 10^6
            
            #get year (so each year has a file)
            ws_data$year <- as.numeric(sapply(paste(ws_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
            yearlist <- unique(ws_data$year)
            for (year in yearlist) {
              #year <- yearlist[1]
              yrdata <- ws_data[which(ws_data$year == year),]
              yrdata$year <- NULL
              names(yrdata) <- c("DATE","RAIN","TMAX","TMIN","SRAD")
              
              #retrieve header
              yrhdr <- retrieve_header(yrdata, xy_loc=data.frame(lon=lon,lat=lat,elev=ele,mun=mun),
                                       years=year,basename=stn)
              if (rcp == "rcp26") {yrhdr$CO2 <- 446.5}
              if (rcp == "rcp45") {yrhdr$CO2 <- 468.2}
              if (rcp == "rcp60") {yrhdr$CO2 <- 452.5}
              if (rcp == "rcp85") {yrhdr$CO2 <- 501.8}
              
              #make date field in DSSAT format
              yrdata$jday <- sprintf("%03d",as.numeric(format(as.Date(yrdata$DATE), "%j")))
              yrdata$year <- substr(paste(as.numeric(format(as.Date(yrdata$DATE), "%Y"))),3,4)
              yrdata$DATE <- paste(yrdata$year,yrdata$jday,sep="")
              
              #write file
              wth_fil <- paste(gcm_odir,"/",yrhdr$FNAME,sep="")
              wth_fil <- write_wth(yrdata,wth_fil,yrhdr,append=F)
            }
          }
        }
        
        #tar -cjvf
        setwd(dss_odir)
        system(paste("tar -cjf method_",mth,"_",rcp,"_",gcm,".tar.bz2"," method_",mth,"_",rcp,"_",gcm,sep=""))
        system(paste("rm -rf method_",mth,"_",rcp,"_",gcm,sep=""))
      } else {
        cat("writing method=",mth," / rcp=",rcp," / gcm=",gcm,", tar.bz2 file already exists \n",sep="")
      }
    }
  }
}




