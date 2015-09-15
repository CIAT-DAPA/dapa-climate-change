#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015

#Get outputs from bias_correct_met.R and write Oryza2000 formatted weather files

#source dir
src.dir <- "~/Repositories/dapa-climate-change/rice-future-tpe"

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
ory_odir <- paste(wd,"/oryza_meteorology",sep="")
if (!file.exists(ory_odir)) {dir.create(ory_odir)}

source(paste(src.dir,"/Oryza_v4.R",sep=""))

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

## variable, scenario, gcm and method list
varlist <- c("prec", "tmax", "tmin", "srad")
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
mthlist <- c("cf","del")
gcmlist <- list.files(paste(gcm_dir,"/loc_",gsub(".","",loc_list$id[1],fixed=T),"/gcm",sep=""))
gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]

for (mth in mthlist) {
  #mth <- mthlist[1]
  for (rcp in rcplist) {
    #rcp <- rcplist[1]
    for (gcm in gcmlist) {
      #gcm <- gcmlist[1]
      if (!file.exists(paste(ory_odir,"/method_",mth,"_",rcp,"_",gcm,".tar.bz2",sep=""))) {
        gcm_odir <- paste(ory_odir,"/method_",mth,"_",rcp,"_",gcm,sep="")
        if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
        
        for (wst in loc_list$id) {
          #wst <- paste(loc_list$id[1])
          #weather stsation details
          lon <- loc_list$lon[which(loc_list$id == wst)]
          lat <- loc_list$lat[which(loc_list$id == wst)]
          ele <- loc_list$elev[which(loc_list$id == wst)]
          sta <- paste(loc_list$uf[which(loc_list$id == wst)])
          mun <- paste(loc_list$municipio[which(loc_list$id == wst)])
          
          cat("writing method=",mth," / rcp=",rcp," / gcm=",gcm," / wst=",wst,"\n",sep="")
          
          #input folders
          wst_name <- gsub(".","",wst,fixed=T)
          wst_idir <- paste(gcm_dir,"/loc_",wst_name,sep="")
          
          if (file.exists(paste(wst_idir,"/obs",sep=""))) {
            #output folder
            wst_odir <- paste(gcm_odir,"/",sta,"/",tolower(gsub(" ","",mun)),sep="")
            if (!file.exists(wst_odir)) {dir.create(wst_odir, recursive=T)}
            
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
              
              #create file
              fname <- paste(wst_odir,"/",substr(toupper(gsub(" ","",mun)),1,5),"1.",sprintf("%03d",year%%2000),sep="")
              tfile <- file(fname,"w")
              
              #write header
              cabecalho(arquivo=tfile,ano=year,estacao=wst,municipio=mun,latitude=lat,longitude=lon,altitude=ele)
              
              for (i in 1:nrow(yrdata)) {
                #i <- 1
                writeLines(paste("1",year,as.integer(format(as.Date(yrdata$date[i], "%Y-%m-%d"),"%j")),
                                 round(yrdata$srad[i]*1000,3),round(yrdata$tmin[i],3),
                                 round(yrdata$tmax[i],3),"-99","-99",
                                 round(yrdata$prec[i],3),sep=','),tfile)
              }
              close(tfile)
            }
          }
        }
        
        #tar -cjvf
        setwd(ory_odir)
        system(paste("tar -cjf method_",mth,"_",rcp,"_",gcm,".tar.bz2"," method_",mth,"_",rcp,"_",gcm,sep=""))
        system(paste("rm -rf method_",mth,"_",rcp,"_",gcm,sep=""))
      } else {
        cat("writing method=",mth," / rcp=",rcp," / gcm=",gcm,", tar.bz2 file already exists \n",sep="")
      }
    }
  }
}




