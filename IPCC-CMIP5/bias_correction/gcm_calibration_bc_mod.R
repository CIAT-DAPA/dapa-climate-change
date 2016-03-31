######################################################################################################################
#### Author : Carlos Navarro
#### MOdified: Jaime Tarapues
#### Date   : May 2015
#### updated : January 2016
#### Contact: c.e.navarro@cgiar.org, jaime.tm8@gmail.com
#### OS     : Linux (all functions), Windows (might not work for extraction functions)
######################################################################################################################

######################################################################################################################
######################################## BIAS CORRECTION METHODOLOGIES ###############################################
########################### CHANGE FACTOR, BIAS CORRECTION AND QUANTILE MAPPING ######################################
######################################################################################################################

######################################################################################################################
### This script aims to calibrate daily observations (Reanalysis) and GCM projections using 'delta' (change factor),
### 'nudging' (bias correction) and quantile mapping approaches. These types of approach could be more widely 
### adopted for assessing calibration methodologies for crop modelling. 
######################################################################################################################
### Main References : 
### 1) Hawkins, E., Osborne, T. M., Ho, C. K., & Challinor, A. J. (2013). Calibration and bias correction of climate 
### projections for crop modelling: An idealised case study over Europe. Agricultural and Forest Meteorology, 
### 170(0), 19-31. http://doi.org/http://dx.doi.org/10.1016/j.agrformet.2012.04.007
### 2) Gudmundsson L; Bremnes JB; Haugen JE; Engen-Skaugen T. (2012). Technical Note: Downscaling RCM precipitation 
### to the station scale using statistical transformations - a comparison of methods.  Hydrology and Earth System 
### Sciences 16: 3383???3390. doi: 10.5194/hess-16-3383-2012.
######################################################################################################################
### Conventions
### OBS: Observation data
### GCM: Global Climate Model data
### TS: Time serie
### SH: Bias Correction approach excluding variability
### BC: Bias Correction approach including variability
### DEL: Change Factor approach excluding variability
### CF: Change Factor approach including variability
### QM: Quantile mappong approach including variability
######################################################################################################################
##  file structure station (fileStat):
## colnames = date  prec	tmin	tmax	hur	srad	swind
## Values =   19980101	0	17.3	27.2	85.9	10.13	0.59
## Units:
## prec->mm/day; temperature->C; hur->%; srad->MJ m-2 day-1; swind->m/s
######################################################################################################################
## Nota: Este script fue ajustado para funcionar en una plataforma online sobre un servidor, sin embargo es posible 
## ejecutarlo en una maquina local
######################################################################################################################

## Extract Observations Time Series Function
obs_extraction <- function(dataset="wfd", varmod="prec",yi=2007, yf=2013, lon=-73.5, lat=3.4, dirobs="U:/cropdata", dirout="D:/jetarapues/Request/Request_cnavarro/bc", dircdo="cdo"){
  
  ## Load libraries
  #library(raster); library(ncdf); library(rgdal);
  
  ## Create and set working directory
  dirtemp <- paste0(dirout, "/obs/", dataset)
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  setwd(dirtemp)
  #ts <- paste0(yi,'_',yf)
  ## Define end and start year from TS
  #yi <- substr(ts, 1, 4)
  # yf <- substr(ts, 6, 9)
  
  ## Change longitude in 0-360 x-axis
  if(dataset!="princeton-afr"){
    if (lon < 0){
      lonmod <- lon + 360
    }else{lonmod <- lon }
  }
  #####  Extract TS OBS  #####
  
  ## NetCDF observation file
  #ncvar <- paste0(dirobs,'/',dataset,"/daily/nc-files/", varmod, "_daily_ts_", tolower(dataset), "_", ts, ".nc")
  ncvarlis <- paste0(dirobs,'/',dataset,"/daily/nc-files")
  ncvar <- list.files(ncvarlis, pattern=paste0(varmod,"_daily_ts_", tolower(dataset), "_*" ),full.names = T,ignore.case=F)
  ncvar <- ncvar[sapply(strsplit(basename(ncvar), '[_]'), "[[", 1)==varmod]
  ## Define extraction output file
  odat <- paste0("obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  if (length(ncvar) > 0){
    if (!file.exists(odat)) {
      
      cat("\nExtracting observation data : ", " ", dataset, " ", varmod, " \n")
      #nc<-system(paste0("ncdump -h ",ncvar), intern=TRUE)
      ## CDO command line to extract daily TS
      system(paste0(dircdo," -s -outputtab,date,value -selyear,",yi,"/",yf," -remapnn,lon=", lon, "_lat=", lat, " ", ncvar, " > ", dirtemp, "/", odat))
      #system(paste("C:\\Python27\\ArcGIS10.1\\python.exe D:\\jetarapues\\_scripts\\bc_extract_gcm.py ",ncvar,' ',dirtemp, "/", odat,' ',yi,' ',yf,' ',lon,' ',lat,' NO cdo',sep=''),intern=TRUE)  
      
      ## Read and organize daily TS
      datobs <- read.table(odat,header=F,sep="")
      names(datobs) <- c("date","value")
      #         datobs <- datobs[which(datobs$year %in% yi:yf),]
      #         datobs$year <- NULL

      if(dataset=='nnrp'|| dataset=='princeton' || dataset=='wfd' || dataset=='wfdei'){
        ## Convert units to mm/day, W/m2 and Celsius Degrees
        if (varmod == "prec"){
          datobs$value <- datobs$value * 86400
          datobs$value[datobs$value>300] <- NA    
          datobs$value[datobs$value<0] <- NA            
        }else if (varmod == "srad") {
          datobs$value <- datobs$value * 0.0864  # W m-2 to MJ m-2 day-1
        }else if(varmod =="tmax" || varmod =="tmin") {
          datobs$value <- datobs$value - 273.15
          datobs$value[datobs$value>60] <- NA    
          datobs$value[datobs$value<50*-1] <- NA            
        }
      }
      
      ## Write extraction output file
      datobs <- write.table(datobs,odat,sep=" ",row.names=F, quote=F)
      
      cat("Done! \n")
      
    }
  }else{cat(paste0("\n No existe archivo: ",varmod,"_daily_ts_", tolower(dataset), "_",varmod,'*' ))}
}

## Extract GCM Time Series Function
gcm_extraction <- function(var="pr",varmod="prec",rcp="historical",yi=1980, yf=2010, gcmlist=c("bcc_csm1_1", "bcc_csm1_1_m", "bnu_esm", "cccma_cancm4", "cccma_canesm2"),lon=-73.5, lat=3.4, dirgcm="T:/gcm/cmip5/raw/daily", dirout="D:/jetarapues/Request/Request_cnavarro/bc/bc_38.35_-4.75"){
  
  ## Load libraries
  #library(raster); library(ncdf); library(rgdal);
  
  ## Path where are stored the daily GCM data 
  if (rcp == "historical"){dirrcp <- paste0(dirgcm, "/", rcp)} else {dirrcp <- paste0(dirgcm, "/", rcp)}
  
  ## Define end and start year from TS period
  #yi <- substr(ts, 1, 4)
  #yf <- substr(ts, 6, 9)
  
  # Loop through GCMs
  for (gcm in gcmlist){
    
    ## Create and set working directory
    dirtemp <- paste0(dirout, "/gcm/", basename(gcm))
    if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
    setwd(dirtemp)
    
    ## Change longitude in 0-360 x axis
    if (lon < 0){
      lonmod <- lon + 360
    }else{lonmod <- lon }
    
    
    #####  Extract TS GCM  #####
    
    ## NetCDF GCM daily file
    Hncvar <- list.files(path=paste0(dirgcm, "/historical/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
    ncvar <- list.files(path=paste0(dirrcp, "/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
    
    #     tmax=lapply(Hncvar, FUN=brick)
    #     tmaxx=brick(Hncvar)
    #     po=extract(tmaxx$X1985.04.21,cbind(lon-20,lat-5))
    #     names(tmaxx)
    #     tmax=stack(tmax)
    #     plot(shapefile("S:/admin_boundaries/adminFiles/world_adm0_no_antarctica.shp"),add=T)
    
    if (length(ncvar) > 0 && length(Hncvar) > 0){
      
      years <- sapply(strsplit(basename(ncvar), '[_]'), "[[", 6)  
      staYear <- sapply(strsplit(years, '[-]'), "[[", 1)
      endYear <- gsub(".nc","",substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 8))
      yearI<- strftime(as.Date(as.character(staYear), "%Y%m%d"),"%Y")
      yearF<- strftime(as.Date(as.character(endYear), "%Y%m%d"),"%Y")
      
      yearsH <- sapply(strsplit(basename(Hncvar), '[_]'), "[[", 6)  
      staYearH <- sapply(strsplit(yearsH, '[-]'), "[[", 1)
      endYearH <- gsub(".nc","",substr(sapply(strsplit(yearsH, '[-]'), "[[", 2), 1, 8))
      yearHI<- strftime(as.Date(as.character(staYearH), "%Y%m%d"),"%Y")
      yearHF<- strftime(as.Date(as.character(endYearH), "%Y%m%d"),"%Y")   
      
  
      checkHist=TRUE
      if (rcp == "historical"){
        if(yearHF<= yi){
          checkHist=FALSE
        }
      }else{
        odat_hist <- paste0("raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab") 
        if(!file.exists(odat_hist)){
          checkHist=FALSE
        }
      }
      
      if(yi <= yearF && checkHist==TRUE){
        
        if(yearI >= yi){
          yei = yearI
        }
        if (yearI <= yi){
          yei = yi
        }  
        if (yearF <= yf){
          yef = yearF
        }	
        if (yearF >= yf){
          yef =yf
        }
        yi<-yei
        yf<-yef
        
        ## Define extraction output file
        odat <- paste0("raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")    
        
        if (!file.exists(odat)) {
          
          cat("\nExtracting GCM data : ", " ", basename(gcm), " ", rcp,  " ", varmod, " \n")
          
          ### CDO command line to extract daily TS
                    if (varmod=="hur"){
                      system(paste0("cdo -s -outputtab,date,value -selyear,",yei,"/",yef," -remapnn,lon=", lonmod, "_lat=", lat, " -selname,", var, " -sellevel,85000 ",  ncvar[1], " > ", dirtemp, "/", odat))
                    } else {
                      system(paste0("cdo -s -outputtab,date,value -selyear,",yei,"/",yef," -remapnn,lon=", lonmod, "_lat=", lat, " -selname,", var, " ",  ncvar[1], " > ", dirtemp, "/", odat))
                    }
          #system(paste("C:\\Python27\\ArcGIS10.1\\python.exe D:\\jetarapues\\_scripts\\bc_extract_gcm.py ",ncvar[1],' ',dirtemp, "/", odat,' ',yi,' ',yf,' ',lon,' ',lat,' YES cdo',sep=''),intern=TRUE)  
          
          ## Read and organize daily TS
          datgcm <- read.table(odat, header=F, sep="")
          names(datgcm) <- c("date","value")
          #           datgcm <- datgcm[which(datgcm$year %in% yi:yf),]
          #           datgcm$year <- NULL
          
          ## Convert units to mm/day and celsius degrees
          if (varmod == "prec"){
            datgcm$value <- datgcm$value * 86400
            datgcm$value[datgcm$value>300] <- NA    
            datgcm$value[datgcm$value<0] <- NA                 
          } else if (varmod == "srad") {
            datgcm$value <- datgcm$value * 0.0864  # W m-2 to MJ m-2 day-1
          } else if(varmod =="tmax" || varmod =="tmin") {
            datgcm$value <- datgcm$value- 273.15 #as.numeric(as.character(datgcm$value))
            #datgcm$value =as.numeric(as.character(datgcm$value))
            datgcm$value[datgcm$value>60] <- NA    
            datgcm$value[datgcm$value<50*-1] <- NA  
          }
          
          ## Write extraction output file
          datgcm2 <- write.table(datgcm, odat,row.names=F, sep=" ", quote=F)
          
          cat("Done! \n")
          
        }
      }else{cat(paste0("\n Error con los periodos seleccionados del historico: ",yearsH))}
    }else{cat(paste0("\n No existe archivo: ",rcp, "/", gcm, "/r1i1p1/",var, "_day* or historical"))}
    
  }
  
} 

## Merge OBS and GCM in a Single Matrix
merge_extraction <- function(varmod="swind", rcp="rcp45", yi=1980, yf=1990, gcmlist=c("bnu_esm"), lon=9.883333, lat=-83.633333, dataset="station", dirbase="C:/Temp/bc/bc_2015-12-14_05_46_42",sepFile="\t",leep,typeData){
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/raw_merge")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}  
  
  ## Define extraction merged file (include OBS and GCM)
  odat <- paste0(dirtemp,"/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(odat)) {
    
    cat("\nMerging OBS and GCMs ",rcp ,varmod, " ... ")
    
    ## Define end and start year from TS period and 
    if(dataset!="station"){
      sepFile=" "
    }else{
      sepFile="\t"
    }
    
    # Load extraction files file names of OBS and GCMs
    ogcm <- paste0("raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    oobs <- paste0("obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    
    lista<-paste0(gcmlist, "/", ogcm)
    pos<-which(file.exists(paste0(dirbase, "/gcm/",gcmlist,'/',ogcm)))
    if(length(pos)>0){
      
      lista<-lista[pos]
      gcmlist<-gcmlist[pos]
  
      gcmdat <- lapply(paste0(dirbase, "/gcm/",gcmlist,'/',ogcm), function(x){read.table(x,header=T,sep=" ")})
      
      ## Create a sequence of dates at daily timestep for TS 
      dates <- format(seq(as.Date(paste0(yi,"/1/1")), as.Date(paste0(yf,"/12/31")), "days") ,"%Y-%m-%d")
      dates <- cbind.data.frame("date"=dates, NA)
      
      ## Insert GCM data in a data frame object
      gcmmat <- as.data.frame(matrix(NA, nrow(dates), length(gcmdat) + 1))
      for(j in 1:length(gcmdat)) {
        merge <- merge(dates, gcmdat[[j]], by="date", all.x=T)
        gcmmat[,j+1] <- merge[,3]
      }
      
      # Load and join observations to the matrix
      oobs <- read.table(paste0(dirbase,"/obs/",dataset, "/", oobs),header=T,sep=sepFile)
      #     oobs2 <- subset(oobs,strftime(oobs$date) >= strftime(yi) & strftime(oobs$date) <= strftime(yf))
      merge <- merge(dates, oobs, by="date", all.x=T)
      gcmmat[,1] <- merge[,3]
      
      # Organize matrix and add dates
      gcmmat <- cbind(dates, gcmmat)
      gcmmat <-gcmmat[,-2]
      names(gcmmat) <- c("date", "obs", gcmlist)
      
      if(rcp!="historical"){
        #gcmmat <- gcmmat[,-2]
      }
      
      if(leep==1){ # rellena los leep year con el promedio del dia antes y despues
        poslist=which(gcmmat$date %in% grep("02-29",gcmmat$date, value = TRUE))
        for(pos in poslist){
          gcmmat[pos,]
          gcmmat[pos,names(gcmmat)!="date"]=(gcmmat[pos-1,names(gcmmat)!="date"]+gcmmat[pos+1,names(gcmmat)!="date"])/2
        }
      }else if(leep==2){ # quita los leep year
        poslist=which(gcmmat$date %in% grep("02-29",gcmmat$date, value = TRUE))
        if(length(poslist)>0){
          gcmmat=gcmmat[-poslist,]
        }
        
      }
      cat()
      if(typeData==1){ # Remueve los NA si todos los tienen
        temp=which(is.na(gcmmat[names(gcmmat)!="date" & names(gcmmat)!="obs"]))
        if(length(temp)>0){
          gcmmat= gcmmat[-temp,]
        }
      }else if(typeData==2){ # remueve todos los NA 
        if(rcp=="historical"){
          gcmmat=na.omit(gcmmat) 
        }        
      }
      
      ## Write merged output file (include OBS and GCM)
      gcmmat2 <- write.table(gcmmat, odat, sep=" ",row.names=F, quote=F)
      cat("done!\n")  
    
    }
  }
}

## Bias Correction Calculation exluding variability (SH)
sh_calcs <- function(varmod="tmax", rcp="historical", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc",leep){
  
  ## Load libraries
  #library(lubridate)
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/Bias_Correction_no_variability")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  
  ## Define SH output file
  bcdat <- paste0(dirtemp,"/sh_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(bcdat)) {
    
    cat("\nBias Correction (excluding variability) calcs over: ", rcp, "\t", varmod, "\t", lon, "\t", lat," ... ")
    
    ## Load merged file
    odat <- paste0("raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat <- read.table(odat, header=T, sep=" ")
    
    if(leep==1){ # quita los leep year
      poslist=which(odat$date %in% grep("02-29",odat$date, value = TRUE))
      if(length(poslist)>0){
        odat=odat[-poslist,]
      }
    }
    years <- year(as.Date(odat$date))
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)    
    
    nyears <- max(years) - min(years) +1
    yearsLeap <- min(years):max(years)
    leepYears=yearsLeap[leap_year(yearsLeap)]
    
    ngcm <- length(odat)- 2    
    
    ## Calculate statistical metrics for OBS & GCM
    avgobs <- aggregate(odat$obs, by=list(months), FUN="mean", na.rm=T)
    avggcm <- aggregate(odat[3:length(odat)], by=list(months), FUN="mean", na.rm=T)
    
    ## Set replicates at the same length of OBS metrics
    avgobs_m <- rep(rep(avgobs[,2], nyears), nday[,3])
    
    ## Set replicates at the same length of GCM metrics (index by each GCM)
    avggcm_l <- list(); for (i in 1:ngcm) { avggcm_l[[i]] <- rep(rep(avggcm[,i+1], nyears), nday[,3] ) }
    
    ## Load GCMs future data if necessary
    if (rcp != "historical"){
      
      ## Load future GCM merged file
      odat_f <- paste0("raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
      odat_f <- read.table(odat_f, header=T, sep=" ")  
      
      if(leep==1){ # quita los leep year
        poslist=which(odat_f$date %in% grep("02-29",odat_f$date, value = TRUE))
        if(length(poslist)>0){
          odat_f=odat_f[-poslist,]
        }
      }
      
      ## Get GCM months and years, number of dates and years
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) +1
      
      
      yearsLeap_f <- min(years_f):max(years_f)      
      leepYears_f=yearsLeap_f[leap_year(yearsLeap_f)]
      
      
      ## Set replicates at the same length of OBS metrics
      avgobs_m_f <- rep(rep(avgobs[,2], nyears_f), nday_f[,3])
      
      ## Set replicates at the same length of GCMs metrics (index by each GCM)
      avggcm_l_f <- list(); for (i in 1:ngcm) { avggcm_l_f[[i]] <- rep(rep(avggcm[,i+1], nyears_f), nday_f[,3] ) }
      
    }
    
    
    #####  SH  Calcs  #####
    
    if (rcp == "historical"){
      
      ## Matrix to be filled with SH values
      bc_values <- matrix(NA, dim(odat)[1], ngcm)
      
      ## Looping through GCMs 
      for (j in 1:ngcm) {
        
        ## Main Bias Correction equation excluding variability (Hawkins et al., 2012)
        if (varmod == "prec" || varmod == "rsds"){ 
          bc_values[,j] <- odat[,j+2] * ( ( avgobs_m - avggcm_l[[j]] ) / avggcm_l[[j]] + 1 )
          bc_values[bc_values<0] <- 0
        } else {
          bc_values[,j] <- odat[,j+2] + (avgobs_m - avggcm_l[[j]])
        } 
        
        ## Output matrix with SH values
        gcmmat <- cbind(odat[,1:2],round(bc_values, digits = 4))
        
      }
      
    } else {
      
      ## Matrix to be filled with SH values
      bc_values <- matrix(NA, dim(odat_f)[1], ngcm)
      
      # Looping through GCMs 
      for (j in 1:ngcm) {
        
        ## Main Bias Correction equation excluding variability (Hawkins et al., 2012) for future
        if (varmod == "prec" || varmod == "rsds"){ 
          bc_values[,j] <- odat_f[,j+2] * ( ( avgobs_m_f - avggcm_l_f[[j]] ) / avggcm_l_f[[j]] + 1 )
          bc_values[bc_values<0] <- 0
        } else {
          bc_values[,j] <- odat_f[,j+2] + (avgobs_m_f - avggcm_l_f[[j]])
        } 
        
        ## Output matrix with SH values
        gcmmat <- cbind(odat_f[,1:2], round(bc_values, digits = 4))
        
      }
    }
    
    ## Write output matrix with SH values
    colnames(gcmmat) <- names(odat)
    if(rcp!="historical"){
      #gcmmat <- gcmmat[,-2]
    }
    
    if(leep==1){ # rellena los leep year con el promedio del dia antes y despues
      dates <- format(seq(as.Date(paste0(min(year(as.Date(gcmmat$date))),"/1/1")), as.Date(paste0(max(year(as.Date(gcmmat$date))),"/12/31")), "days") ,"%Y-%m-%d")
      dates <- cbind.data.frame("date"=dates)
      gcmmat <- merge(dates, gcmmat, by="date", all.x=T)       
      poslist=which(gcmmat$date %in% grep("02-29",gcmmat$date, value = TRUE))
      for(pos in poslist){
        gcmmat[pos,]
        gcmmat[pos,names(gcmmat)!="date"]=(gcmmat[pos-1,names(gcmmat)!="date"]+gcmmat[pos+1,names(gcmmat)!="date"])/2
      }
    }
    
    gcmmat2 <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat(" done!... ")    
    
  }
  
}

## Bias Correction Calculation including variability (BC)
bc_calcs <- function(varmod="prec", rcp="rcp45", lon=-73.5, lat=3.4, dirbase="C:/Temp/bc/bc_2015-12-14_05_46_42"){
  
  ## Load libraries
  #library(lubridate)
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/Bias_Correction_variability")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  
  ## Define BC output file
  bcdat <- paste0(dirtemp,"/bc_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(bcdat)) {
    
    cat("\nBias Correction (with variability) Calcs: ", rcp, "\t", varmod, "\t", lon, "\t", lat, " ... ")
    
    odat <- paste0("raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat <- read.table(odat, header=T, sep=" ")
    
    years <- year(as.Date(odat$date))
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)    
    nyears <- max(years) - min(years) +1
    
    ngcm <- length(odat)- 2    
    
    ## Std function
    fun <- function(x) { sd(x, na.rm=T) }
    
    ## Calculate statistical metrics for OBS & GCM
    avgobs <- aggregate(odat$obs, by=list(months), FUN="mean", na.rm=T)
    stdobs <- aggregate(odat$obs, by=list(months), FUN=fun)
    avggcm <- aggregate(odat[3:length(odat)], by=list(months), FUN="mean", na.rm=T)
    stdgcm <- aggregate(odat[3:length(odat)], by=list(months), FUN=fun)
    
    ## Set replicates at the same length of OBS metrics
    avgobs_m <- rep(rep(avgobs[,2], nyears), nday[,3])
    stdobs_m <- rep(rep(stdobs[,2], nyears), nday[,3])
    
    ## Set replicates at the same length of GCM metrics (index by each GCM)
    stdgcm_l <- list(); avggcm_l <- list()
    for (i in 1:ngcm) {
      stdgcm_l[[i]] <- rep(rep(stdgcm[,i+1], nyears), nday[,3])
      avggcm_l[[i]] <- rep(rep(avggcm[,i+1], nyears), nday[,3])    
    }
    
    ## Load GCMs future data if necessary
    if (rcp != "historical"){
      
      # Load future GCM merged file
      odat_f <- paste0("raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
      odat_f <- read.table(odat_f, header=T, sep=" ")
      
      ## Get GCM months and years, number of dates and years
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) +1
      
      # Calculate statistical metrics for future GCM
      avggcm <- aggregate(odat_f[3:length(odat_f)], by=list(months_f), FUN="mean", na.rm=T)
      stdgcm <- aggregate(odat_f[3:length(odat_f)], by=list(months_f), FUN=fun)
      
      # Set replicates at the same length of GCMs future metrics
      avgobs_m_f <- rep(rep(avgobs[,2], nyears_f), nday_f[,3])
      stdobs_m_f <- rep(rep(stdobs[,2], nyears_f), nday_f[,3])
      
      ## Set replicates at the same length of GCMs future metrics (index by each GCM)
      stdgcm_l_f <- list(); avggcm_l_f <- list()
      for (i in 1:ngcm) {
        stdgcm_l_f[[i]] <- rep(rep(stdgcm[,i+1], nyears_f), nday_f[,3] )
        avggcm_l_f[[i]] <- rep(rep(avggcm[,i+1], nyears_f), nday_f[,3] )    
      }
      
    }
    
    #####  BC  Calcs  #####
    
    if (rcp == "historical"){
      
      ## Matrix to be filled with BC values
      bc_values <- matrix(NA, dim(odat)[1], ngcm)
      
      # Looping through GCMs 
      for (j in 1:ngcm) {
        
        ## Main Bias Correction equation including variability (Hawkins et al., 2012)
        if (varmod == "prec" || varmod == "rsds"){
          bc_values[,j] <- avgobs_m *  (1 + ( stdobs_m / stdgcm_l[[j]] * ( odat[,j+2] - avggcm_l[[j]]) / avgobs_m ) )  
          # bc_values[,j] <- odat[,j+2] *  (1 + ( stdobs_m / stdgcm_l[[j]] * ( avggcm_l[[j]] - avgobs_m ) / avgobs_m ) )  ## Need double-check
          
          bc_values[bc_values<0] <- 0
        } else {
          bc_values[,j] <- avgobs_m + ( (stdobs_m / stdgcm_l[[j]]) * (odat[,j+2] - avggcm_l[[j]]))
        }  
        
      }
      
      ## Output matrix with BC values
      gcmmat <- cbind(odat[,1:2], round(bc_values, digits = 4))
      
      
    } else {
      
      ## Matrix to be filled with BC values
      bc_values <- matrix(NA, dim(odat_f)[1], ngcm)
      
      ## Looping through GCMs
      for (j in 1:ngcm) {
        
        ## Main Bias Correction equation including variability (Hawkins et al., 2012) for future
        if (varmod == "prec" || varmod == "rsds"){
          bc_values[,j] <- avgobs_m_f *  (1 + ( stdobs_m_f / stdgcm_l_f[[j]] * ( odat_f[,j+2] - avggcm_l_f[[j]]) / avgobs_m_f ) )
          # bc_values[,j] <- odat_f[,j+2] *  (1 + ( stdobs_m_f / stdgcm_l_f[[j]] * ( avggcm_l_f[[j]] - avgobs_m_f ) / avgobs_m_f ) ) ## Need double-check
          
          bc_values[bc_values<0] <- 0
        } else {
          bc_values[,j] <- avgobs_m_f + ( (stdobs_m_f / stdgcm_l_f[[j]]) * (odat_f[,j+2] - avggcm_l_f[[j]]))
        }
        
      }
      
      ## Output matrix with BC values
      gcmmat <- cbind(odat_f[,1:2], round(bc_values, digits = 4))
      
    }
    
    
    ## Write output matrix with SH values
    colnames(gcmmat) <- names(odat)
    if(rcp!="historical"){
      #gcmmat <- gcmmat[,-2]
    }
    
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat("done!")
    
  }
  
}

## Change Factor Calculation exluding variability (DEL)
del_calcs <- function(varmod="prec", rcp="rcp45", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc",leep){
  
  ## Load libraries
  #library(lubridate)
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/Change_Factor_no_variability")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  
  ## Define DEL output file
  bcdat <- paste0(dirtemp,"/del_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(bcdat)) {
    
    cat("\nChange Factor (without variability) Calcs: ", rcp, "\t", varmod, "\t", lon, "\t", lat, " ... ")
    
    # Load merged file (historical)
    odat <- paste0("raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat <- read.table(odat, header=T, sep=" ")
    # quita los leep year
    poslist=which(odat$date %in% grep("02-29",odat$date, value = TRUE))
    if(length(poslist)>0){
      odat=odat[-poslist,]
    }
    years <- year(as.Date(odat$date))
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)    
    nyears <- max(years) - min(years) +1    
    
    ## Load merged file (future)
    odat_f <- paste0("raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat_f <- read.table(odat_f, header=T, sep=" ")
    
    # quita los leep year
    poslist_f=which(odat_f$date %in% grep("02-29",odat_f$date, value = TRUE))
    if(length(poslist_f)>0){
      odat_f=odat_f[-poslist_f,]
    }
    
    months_f <- month(as.Date(odat_f$date))
    years_f <- year(as.Date(odat_f$date))
    nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
    nyears_f <- max(years_f) - min(years_f) +1
    
    ngcm <- length(odat)- 2
    
    ## Standarize length of future and historical periods by the middle point
    if (nyears > nyears_f){
      
      midyear <- (max(years) + min(years))/2
      yi <- round(midyear - nyears_f/2, 0)
      yf <- round(midyear + nyears_f/2, 0) - 1
      odat <- odat[which(year(as.Date(odat$date)) %in% yi:yf),]
      
      months <- month(as.Date(odat$date))
      years <- year(as.Date(odat$date))
      nday <- aggregate(odat[,1], list(months, years), length)
      nyears <- max(years) - min(years) + 1
      
    } else if (nyears < nyears_f) {
      
      midyear_f <- (max(years_f) + min(years_f))/2
      yi <- round(midyear_f - nyears/2, 0)
      yf <- yi+nyears-1#round(midyear_f + nyears/2, 0) - 1
      odat_f <- odat_f[which(year(as.Date(odat_f$date)) %in% yi:yf),]
      
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) + 1
      
    }
    
    ## Calculate statistical metrics for GCM
    avggcm <- aggregate(odat[3:length(odat)], by=list(months), FUN="mean", na.rm=T)
    avggcm_f <- aggregate(odat_f[3:length(odat_f)], by=list(months_f), FUN="mean", na.rm=T)
    
    ## Set replicates at the same length of GCM metrics (index by each GCM)
    avggcm_l <- list(); avggcm_l_f <- list()
    for (i in 1:ngcm) {
      avggcm_l[[i]] <- rep(rep(avggcm[,i+1], nyears), nday[,3] )
      avggcm_l_f[[i]] <- rep(rep(avggcm_f[,i+1], nyears), nday_f[,3] )
    }
    
    
    #####  DEL  Calcs  #####
    
    ## Matrix to be filled with DEL values
    bc_values <- matrix(NA, dim(odat_f)[1], ngcm)
    
    ## Looping through GCMs 
    for (j in 1:ngcm) {
      
      ## Main Change Factor equation excluding variability (Hawkins et al., 2012)
      if (varmod == "prec" || varmod == "rsds"){ 
        bc_values[,j] <- odat[,2] * ( (avggcm_l_f[[j]] - avggcm_l[[j]]) / avggcm_l[[j]] + 1 )
        bc_values[bc_values<0] <- 0
      } else {
        bc_values[,j] <- odat[,2] + ( avggcm_l_f[[j]] - avggcm_l[[j]])
      } 
      
    }
    
    ## Write output matrix with DEL values
    gcmmat <- cbind(odat_f[,1:2], round(bc_values, digits = 4))
    colnames(gcmmat) <- names(odat_f)
    if(rcp!="historical"){
      #gcmmat <- gcmmat[,-2]
    }
    if(leep==1){ # rellena los leep year con el promedio del dia antes y despues
      dates <- format(seq(as.Date(paste0(min(year(as.Date(gcmmat$date))),"/1/1")), as.Date(paste0(max(year(as.Date(gcmmat$date))),"/12/31")), "days") ,"%Y-%m-%d")
      dates <- cbind.data.frame("date"=dates)
      gcmmat <- merge(dates, gcmmat, by="date", all.x=T)       
      poslist=which(gcmmat$date %in% grep("02-29",gcmmat$date, value = TRUE))
      for(pos in poslist){
        gcmmat[pos,]
        gcmmat[pos,names(gcmmat)!="date"]=(gcmmat[pos-1,names(gcmmat)!="date"]+gcmmat[pos+1,names(gcmmat)!="date"])/2
      }
    }
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat("done!")
    
  }
  
}

## Change Factor approach with variability
cf_calcs <- function(varmod="tmin", rcp="rcp45", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc",leep){
  
  ## Load libraries
  #library(lubridate)
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/Change_Factor_variability")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  
  ## Define DEL output file
  bcdat <- paste0(dirtemp,"/cf_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(bcdat)) {
    
    cat("\nChange Factor (with variability) Calcs: ", rcp, "\t", varmod, "\t", lon, "\t", lat, " ... ")
    
    # Load merged file (historical)
    odat <- paste0("raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat <- read.table(odat, header=T, sep=" ")
    # quita los leep year
    poslist=which(odat$date %in% grep("02-29",odat$date, value = TRUE))
    if(length(poslist)>0){
      odat=odat[-poslist,]
    }
    
    years <- year(as.Date(odat$date))
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)    
    nyears <- max(years) - min(years) +1     
    
    # Load merged file (future)
    odat_f <- paste0("raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat_f <- read.table(odat_f, header=T, sep=" ")
    # quita los leep year
    poslist_f=which(odat_f$date %in% grep("02-29",odat_f$date, value = TRUE))
    if(length(poslist_f)>0){
      odat_f=odat_f[-poslist_f,]
    }
    months_f <- month(as.Date(odat_f$date))
    years_f <- year(as.Date(odat_f$date))
    nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
    nyears_f <- max(years_f) - min(years_f) +1    
    ngcm <- length(odat)- 2
    
    # Standarize length of future and historical periods by the middle point
    if (nyears > nyears_f){
      midyear <- (max(years) + min(years))/2
      yi <- round(midyear - nyears_f/2, 0)
      yf <- round(midyear + nyears_f/2, 0) - 1
      odat <- odat[which(year(as.Date(odat$date)) %in% yi:yf),]
      
      months <- month(as.Date(odat$date))
      years <- year(as.Date(odat$date))
      nday <- aggregate(odat[,1], list(months, years), length)
      nyears <- max(years) - min(years) + 1
      
    } else if (nyears < nyears_f){
      midyear_f <- (max(years_f) + min(years_f))/2
      yi <- round(midyear_f - nyears/2, 0)
      yf <- yi+nyears-1#round(midyear_f + nyears/2, 0) - 1
      odat_f <- odat_f[which(year(as.Date(odat_f$date)) %in% yi:yf),]
      
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) + 1
    }
    
    ## Std function
    fun <- function(x) { sd(x, na.rm=T) }
    
    ## Calculate statistical metrics for GCM
    avggcm <- aggregate(odat[3:length(odat)], by=list(months), FUN="mean", na.rm=T)
    stdgcm <- aggregate(odat[3:length(odat)], by=list(months), FUN=fun)
    avggcm_f <- aggregate(odat_f[3:length(odat_f)], by=list(months_f), FUN="mean", na.rm=T)
    stdgcm_f <- aggregate(odat_f[3:length(odat_f)], by=list(months_f), FUN=fun)
    
    ## Set replicates at the same length of GCM metrics (index by each GCM)
    stdgcm_l <- list(); avggcm_l <- list(); stdgcm_l_f <- list(); avggcm_l_f <- list()
    for (i in 1:ngcm) {
      stdgcm_l[[i]] <- rep(rep(stdgcm[,i+1], nyears), nday[,3] )
      avggcm_l[[i]] <- rep(rep(avggcm[,i+1], nyears), nday[,3] )
      stdgcm_l_f[[i]] <- rep(rep(stdgcm_f[,i+1], nyears), nday[,3] )
      avggcm_l_f[[i]] <- rep(rep(avggcm_f[,i+1], nyears), nday[,3] )
    }
    
    ## Matrix to be filled with CF values
    bc_values <- matrix(NA, dim(odat)[1], ngcm)
    
    
    #####  CF  Calcs  #####
    
    # Looping through GCMs 
    for (j in 1:ngcm) {
      
      ## Main Change Factor equation including variability (Hawkins et al., 2012)
      if (varmod == "prec" || varmod == "rsds"){
        bc_values[,j] <-  avggcm_l_f[[j]] * (1 + ( stdgcm_l_f[[j]] / stdgcm_l[[j]]  * ( odat[,2] - avggcm_l[[j]] ) / avggcm_l_f[[j]] ) )
        # bc_values[,j] <- odat[,2] * (1 + ( stdgcm_l_f[[i]] / stdgcm_l[[i]]  * ( avggcm_l_f[[j]] - avggcm_l[[j]] ) / avggcm_l[[j]] ) ) ## Need double check
        bc_values[bc_values<0] <- 0
      }else{
        bc_values[,j] <- avggcm_l_f[[j]] + ( stdgcm_l_f[[i]] / stdgcm_l[[i]] * (odat[,2] - avggcm_l[[j]]) )        
      }
      
    }
    
    
    ## Write output matrix with CF values
    gcmmat <- cbind(odat_f[,1:2], round(bc_values, digits = 4))
    colnames(gcmmat) <- names(odat_f)
    if(rcp!="historical"){
      #gcmmat <- gcmmat[,-2]
    }
    if(leep==1){ # rellena los leep year con el promedio del dia antes y despues
      dates <- format(seq(as.Date(paste0(min(year(as.Date(gcmmat$date))),"/1/1")), as.Date(paste0(max(year(as.Date(gcmmat$date))),"/12/31")), "days") ,"%Y-%m-%d")
      dates <- cbind.data.frame("date"=dates)
      gcmmat <- merge(dates, gcmmat, by="date", all.x=T)       
      poslist=which(gcmmat$date %in% grep("02-29",gcmmat$date, value = TRUE))
      for(pos in poslist){
        gcmmat[pos,]
        gcmmat[pos,names(gcmmat)!="date"]=(gcmmat[pos-1,names(gcmmat)!="date"]+gcmmat[pos+1,names(gcmmat)!="date"])/2
      }
    }
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat("done!")
    
  }
  
}

## Quantile mapping approach
qm_calcs <- function(varmod="tmax", rcp="rcp85", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc",leep){
  
  ## Load libraries
  #library(qmap); library(lubridate);
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/quantile_mapping")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  
  ## Define QM output file
  bcdat <- paste0(dirtemp,"/qm_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  ## Define QM output file (future)
  bcdat_f <- paste0(dirtemp,"/qm_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(bcdat) || !file.exists(bcdat_f)) {
    
    cat("\nQuantile MApping Calcs: ", rcp, " ", varmod, " ... ")
    
    ## Load merged file (historical)
    odatALL <- paste0("raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odatALL <- read.table(odatALL, header=T, sep=" ")
    #odatALL <- odatALL[1:5,]
    ngcm <- length(odatALL)- 2
    
    if(leep==1){ # quita los leep year
      poslist=which(odatALL$date %in% grep("02-29",odatALL$date, value = TRUE))
      if(length(poslist)>0){
        odatALL=odatALL[-poslist,]
      }
    }
    qm_histALL <- array(NA, dim = c(length(odatALL$date), ngcm))
    datesALL <- cbind.data.frame("date"=odatALL$date)

    if (rcp != "historical"){
      ## Load merged file (future)
      odat_fALL <- paste0("raw_merge/raw_ts_", rcp, "_",varmod,"_lon_",lon,"_lat_",lat,".tab")
      odat_fALL <- read.table(odat_fALL, header=T, sep=" ")
      #odat_fALL <- odat_fALL[1:5,]
      if(leep==1){ # quita los leep year
        poslist=which(odat_fALL$date %in% grep("02-29",odat_fALL$date, value = TRUE))
        if(length(poslist)>0){
          odat_fALL=odat_fALL[-poslist,]
        }
      }
      qm_futALL <- array(NA, dim = c(length(odat_fALL$date), ngcm))
      dates_fALL <- cbind.data.frame("date"=odat_fALL$date)      
    }    
    
    for (i in 1:ngcm) {
      #odat <- odatALL[1:5,c(c(1,2),(2+i))]
      odat <- odatALL[,c(c(1,2),(2+i))]
      odat=na.omit(odat) 
      years <- year(as.Date(odat$date))
      months <- month(as.Date(odat$date))
      nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)    
      nyears <- max(years) - min(years) +1 
      ## Get GCM dates, months, days and number of GCMs (historical)
      dates <- odat$date
      dates_nonleap <- seq(as.Date('2001-01-01'),as.Date('2001-12-31'),by=1)  # example non_leap year  
      days <- day(as.Date(odat$date)) 
      ## Array of dates (rows) and GCMs (columns)
      qm_hist <- array(NA, dim = c(length(dates), 1))
      
      ### FUTURE ###
      #odat_f <- odat_fALL[1:5,c(c(1,2),(2+i))]
      odat_f <- odat_fALL[,c(c(1,2),(2+i))]
      temp=which(is.na(odat_f[,3]))
      if(length(temp)>0){
        odat_f= odat_f[-temp,]
      }      
      ## Get GCM dates, months and days (future)
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) +1
      dates_f <- odat_f$date
      days_f <- day(as.Date(odat_f$date))
      ## Array of dates (rows) and GCMs (columns) for future
      qm_fut <- array(NA, dim = c(length(dates_f), 1))      
      
      #####  QM  Calcs  #####
      # Loop through days of year selecting 30-day moving window around each day (do not fit Feb. 29, NA for GCM's)
      for (k in 1:365) {
        
        # Need to find indices of all days "k" and then 15 days before and after by historical dates
        ind_k = which(months == month(dates_nonleap[k]) & days == mday(dates_nonleap[k]))
        if(length(ind_k)>0){
          for (d in 1:length(ind_k))  {
            if (d == 1) {
              ind_all <- (ind_k[d]-15):(ind_k[d]+15)
            } else {
              ind_all <- c(ind_all, (ind_k[d]-15):(ind_k[d]+15))
            }
          }
          
          # Get rid of values outside of historical range
          ind_all = ind_all[ind_all > 0 & ind_all < length(dates)]  
          
          # Indices for future
          if (rcp != "historical"){ind_k_f = which(months_f == month(dates_nonleap[k]) & days_f == mday(dates_nonleap[k]))}
          
          # Fit model with historical observations and historical GCM data 
          #for (1 in 1:1) {
            
            error.p = tryCatch( { #keep going if can't apply model (all zeros in obs)  
              
              if (varmod == "prec"){
                qm_hist_fit = fitQmap(obs = odat[ind_all, 2], mod = odat[ind_all, 1+2], method="RQUANT", qstep=0.01, wet.day=T, na.rm=T)  
              } else {
                qm_hist_fit = fitQmap(obs = odat[ind_all, 2], mod = odat[ind_all, 1+2], method="RQUANT", qstep=0.01, wet.day=F, na.rm=T)  
              }
              
              #  Apply model to GCM past & future (if necessary)
              qm_hist[ind_k, 1] = doQmap(x = odat[ind_k, 1+2], qm_hist_fit, type="linear")
              if (rcp != "historical"){
                qm_fut[ind_k_f, 1] = doQmap(x = odat_f[ind_k_f, 1+2], qm_hist_fit, type="linear")
              }
            }
            , error=function(e) e
            )
            if(inherits(error.p,'Error'))  next
          #}
        }
      }
      
      # Write output matrix with QM values (historical)
      qm_hist <- cbind(odat[,1:2], round(qm_hist, digits = 4))
      qm_hist <- merge(datesALL, qm_hist, by="date", all.x=T)   
      qm_histALL[,i] =qm_hist[,3]
      
      #Future
      qm_fut <- cbind(odat_f[,1:2], round(qm_fut, digits = 4))
      qm_fut <- merge(dates_fALL, qm_fut, by="date", all.x=T)   
      qm_futALL[,i] =qm_fut[,3]      
    }#gcm
 
    qm_histALL <- cbind(odatALL[,1:2], qm_histALL)
    colnames(qm_histALL) <- names(odatALL)
    
    if(leep==1){ # rellena los leep year con el promedio del dia antes y despues
      dates <- format(seq(as.Date(paste0(min(year(as.Date(qm_histALL$date))),"/1/1")), as.Date(paste0(max(year(as.Date(qm_histALL$date))),"/12/31")), "days") ,"%Y-%m-%d")
      dates <- cbind.data.frame("date"=dates)
      qm_histALL <- merge(dates, qm_histALL, by="date", all.x=T)       
      poslist=which(qm_histALL$date %in% grep("02-29",qm_histALL$date, value = TRUE))
      for(pos in poslist){
        qm_histALL[pos,]
        qm_histALL[pos,names(qm_histALL)!="date"]=(qm_histALL[pos-1,names(qm_histALL)!="date"]+qm_histALL[pos+1,names(qm_histALL)!="date"])/2
      }
    }    

    qm_hist2 <- write.table(qm_histALL, bcdat, sep=" ", row.names=F, quote=F)
    
    # Write output matrix with QM values for (future)
    if (rcp != "historical"){
      qm_futALL <- cbind(odat_fALL[,1:2], qm_futALL)
      colnames(qm_futALL) <- names(odat_fALL)

      #qm_futALL <- qm_futALL[,-2]
      if(leep==1){ # rellena los leep year con el promedio del dia antes y despues
        dates <- format(seq(as.Date(paste0(min(year(as.Date(qm_futALL$date))),"/1/1")), as.Date(paste0(max(year(as.Date(qm_futALL$date))),"/12/31")), "days") ,"%Y-%m-%d")
        dates <- cbind.data.frame("date"=dates)
        qm_futALL <- merge(dates, qm_futALL, by="date", all.x=T)       
        poslist=which(qm_futALL$date %in% grep("02-29",qm_futALL$date, value = TRUE))
        for(pos in poslist){
          qm_futALL[pos,]
          qm_futALL[pos,names(qm_futALL)!="date"]=(qm_futALL[pos-1,names(qm_futALL)!="date"]+qm_futALL[pos+1,names(qm_futALL)!="date"])/2
        }
      }      
      qm_fut2 <- write.table(qm_futALL, bcdat_f, sep=" ", row.names=F, quote=F)
    }
    
    cat("done!")
  }
  
}

## Comparison methods (plots)
bc_stats <- function(varmod="prec", rcp="historical",yi=1998, yf=2013, lon=-73.5, lat=3.4, dirbase="C:/Temp/bc/bc_31.45_-0.65",stat){
  
  ## Load libraries
  library(lubridate); library(ggplot2); library(reshape)
  
  dirsBC=c("Bias_Correction_no_variability","Bias_Correction_variability","Change_Factor_no_variability","Change_Factor_variability","quantile_mapping","raw_merge")
  listBC=list.dirs(dirbase,recursive=F,full.names=F)
  
  dirsel=intersect(dirsBC,listBC)  
  
  ## Set working directory
  setwd(dirbase)
  
  ## Set and create output directory
  dirout <- paste0(dirbase, "/statistics")
  if (!file.exists(dirout)) {dir.create(dirout, recursive=T)}
  
  ## Define end and start year to plot
  #yi <- substr(ts, 1, 4)
  #yf <- substr(ts, 6, 9)
  
  
  listbcAll=list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0(rcp,"_",varmod, "_*"))
  #   listbcAll=c(listbcAll,list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0("historical_",varmod, "_*")))
  
  ## Define methods to plot
  if (rcp == "historical"){
    methods <- c("sh_","bc_", "qm_", "raw_")
    listbc=grep(paste(methods,collapse="|"),listbcAll, value = TRUE)
    methods_ln <-unique(dirname(listbc))  #c("BC Var", "BC", "QM", "RAW") # 
  } else {
    methods <- c("sh_","bc_","del_","cf_","qm_","raw_")
    listbc=grep(paste(methods,collapse="|"),listbcAll, value = TRUE)
    methods_ln <- unique(dirname(listbc))  # c("CF Var", "CF", "BC Var", "BC", "QM", "RAW") #
  }  
  
  ## Load all bias corrected data
  odat <- lapply(listbcAll, function(x){read.table(x,header=T,sep=" ")})
  
  # Get GCMs names and length
  ngcm <- length(odat[[1]]) - 2
  gcmlist <- names(odat[[1]])[3:length(odat[[1]])]
  
  # Merge all data in a single table
  merge <- c()
  for(j in 1:length(odat)) { merge <- rbind(cbind("method"=rep(methods_ln[j],nrow( odat[[j]] )), (odat[[j]])), merge) }
  
  rownames(merge) <- NULL
  
  # Y-axis labels by variable
  if(varmod == "prec"){
    ylabel <- "Precipitation (mm/day)"; flabel <- "Rainfall Frequency (days/month)";limit = c(0,250);
  }else if(varmod == "tmin"){
    ylabel <- "Min. Temperature (C)"; limit = c(-10, 25);
  }else if(varmod == "tmax"){
    ylabel <- "Max. Temperature (C)"; flabel <- "Hot days Frequency (days/month)"; limit = c(0, 40);
  }else if(varmod == "srad"){
    ylabel <- "Shortwave Sol. Radiation ((MJ/m2 day)"; limit = c(0, 400);
  }else if(varmod == "hur"){
    ylabel <- "Relative Humidity (%)"; limit = c(0, 100);
  }else if(varmod == "swind"){
    ylabel <- "Wind speed (m/s)"; limit = c(0, 400);
  }else if(varmod == "tmean"){
    ylabel <- "Mean Temperature (C)"; limit = c(0, 100);
  }
  
  # Personalized colors 
  gray='gray50';blue="#122F6B";blue2="#1F78B4";blue3="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  # Long name months list
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  ######################
  
  ##### Timeseries Line Plot for all methods
  
  ## Define enviroment to plot ggplot functions in command line
  merge_mod <- merge[which(year(as.Date(merge$date)) %in% yi:yf),]
  assign("merge_mod", merge_mod,  envir = .GlobalEnv)
  
  ## Looping through GCMs 
  for (i in 1:ngcm){
    
    assign("i", i,  envir = .GlobalEnv)
    
    ## GCM name
    gcm <- colnames(merge_mod)[i+3]
    
    ## Define output plot file
    ots <- paste0(dirout, "/ts_", rcp,"_",gcm,"_",varmod,"_lon_",lon,"_lat_",lat,".tif")
    
    if (!file.exists(ots)) {
      
      if (rcp == "historical"){  # Historical plot includes observations
        
        cat(paste0("\nTime series plot  ", rcp, " ", varmod, " ", gcm))
        
        tiff(ots, width=1200, height=1000, pointsize=8, compression='lzw',res=100)
        p <- ggplot(data=merge_mod) + 
          geom_line(aes(x=as.Date(date), y=merge_mod[,3], color=" OBS"), size=0.2, shape=1) +   # Observations
          geom_line(aes(x=as.Date(date), y=merge_mod[,i+3], colour=factor(method)), shape=1, size=0.2) +   # GCMs (historical)
          facet_wrap(~ method, ncol=1) +
          scale_color_manual(values=c(gray, red, red2, orange, green)) +
          theme(panel.background = element_rect(fill = 'gray92'), legend.title=element_blank()) +
          ggtitle(paste0("BC Methods  Model : ",gcm)) +
          labs(x="Date (days)", y=ylabel)
        
      } else {
        
        cat(paste0("\nTime series plot  ", rcp, " ", varmod, " ", gcm))
        
        tiff(ots, width=1200, height=1400, pointsize=8, compression='lzw',res=100)
        p <- ggplot(data=merge_mod) +  
          geom_line(aes(x=as.Date(date), y=merge_mod[,i+3], colour=factor(method)), shape=1, size=0.2)+   # GCMs (future)
          facet_wrap(~ method, ncol=1) + 
          scale_color_manual(values=c(green, orange, red, red2, blue, blue2)) +
          theme(panel.background = element_rect(fill = 'gray92'), legend.title=element_blank()) +
          ggtitle(paste0("BC Methods  Model : ", gcm)) +
          labs(x="Date (days)", y=ylabel) 
        
      }
      
      # Plot and save
      print(p)
      dev.off()
      
    }
    
  }
  
  
  
  ##### Spread Line Plot for all methods and all GCMs
  
  ## Melt all GCM in a single column
  merge_mod <- merge[which(year(as.Date(merge$date)) %in% yi:yf),]
  merge_mod$obs <- NULL
  merge_mod <- melt(merge_mod,id=c("method","date"))
  
  ## Define enviroment to plot ggplot functions in command line
  assign("merge_mod", merge_mod,  envir = .GlobalEnv)
  
  ## Define output plot file
  ots <- paste0(dirout, "/spread_", rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tif")
  
  if (!file.exists(ots)){
    
    if (rcp == "historical"){  # Historical plot includes observations
      
      cat(paste0("\nTime series plot  ", rcp, " ", varmod))
      
      tiff(ots, width=1200, height=1000, pointsize=8, compression='lzw',res=100)
      p <- ggplot() + 
        geom_point(data=merge_mod, aes(x=as.Date(date), y=value, colour=factor(method)), size=0.2) +
        scale_color_manual(values=c(green, orange, red, red2)) +
        facet_wrap(~ method, ncol=1) +
        theme(panel.background = element_rect(fill = 'gray92'), legend.title=element_blank()) +
        scale_y_continuous(limits = limit) +
        ggtitle(paste0("BC Methods Spread")) +
        labs(x="Date (days)", y=ylabel)
      
    } else {
      
      cat(paste0("\nTime series plot  ", rcp, " ", varmod, " ", gcm))
      
      tiff(ots, width=1200, height=1600, pointsize=8, compression='lzw',res=100)
      p <- ggplot() + 
        geom_point(data=merge_mod, aes(x=as.Date(date), y=value, colour=factor(method)), size=0.2) +
        scale_color_manual(values=c(green, orange2, red, red2, blue, blue3)) +
        facet_wrap(~ method, ncol=1) +
        theme(panel.background = element_rect(fill = 'gray92'), legend.title=element_blank()) +
        scale_y_continuous(limits = limit) +
        ggtitle(paste0("BC Methods Spread")) +
        labs(x="Date (days)", y=ylabel)
      
    }
    
    # Plot and save
    print(p)
    dev.off()
    
  }
  
  
  ##### Interannual Variability Boxplot
  
  ## Define output metrics file
  intannvar <- paste0(dirout, "/var_", rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".txt")
  
  if (!file.exists(intannvar)) {
    
    ## Get months and years from merged file
    months <- month(as.Date(merge$date))
    years <- year(as.Date(merge$date))
    
    ## Std calculation for all TS
    fun <- function(x) { sd(x, na.rm=T) }
    stdgcm <- aggregate(merge[3:length(merge)], by=list("method"=merge$method, "month"=months), FUN=fun)
    
    ## Rename Months 
    stdgcm$month=month.abb[stdgcm$month]
    stdgcm$month=factor(stdgcm$month,levels=month.abb)
    
    ## Set apart observations
    obs <- stdgcm[which(stdgcm$method == "Bias_Correction_variability"), ]$obs
    stdgcm$obs <- NULL
    
    if (rcp == "historical"){   # Historical plot includes observations
      
      # Join observations at the end of std GCM values
      obs <- matrix(1, length(obs)[1], ngcm) * obs
      colnames(obs) <- gcmlist
      stdgcm <- rbind(stdgcm, cbind("method"=rep("OBS", dim(obs)[1]),stdgcm[which(stdgcm$method == "Bias_Correction_variability"),][2], obs))
      rownames(stdgcm) <- NULL
      
      ## Define variables to plot ggplot functions in command line
      assign("stdgcm", stdgcm,  envir = .GlobalEnv)
      
      ## Loop through GCMs 
      for (i in 1:ngcm){
        
        ## GCM name
        gcm <- colnames(stdgcm)[i+2]
        
        ## Define variables to plot ggplot functions in command line
        assign("i", i,  envir = .GlobalEnv)
        
        cat(paste0("\nInterannual variability boxplot  ", rcp, " ", varmod, " ", gcm))
        
        tiff(paste0(dirout, "/var_", rcp,"_",gcm,"_",varmod,"_lon_",lon,"_lat_",lat,".tif"), width=1200, height=1600, pointsize=8, compression='lzw',res=100)
        p <- ggplot(data=stdgcm) +  
          geom_bar(aes(x=month, y=stdgcm[,i+2], fill=factor(method)), shape=1, size=1, width=.5, stat="identity") +  # GCMs (historical)
          facet_wrap(~ method, ncol=1) + 
          scale_fill_manual(values=c(green2, orange, red, red2, gray)) +
          theme(panel.background = element_rect(fill = 'gray92'), legend.title=element_blank()) +
          ggtitle(paste0("Interannual Variability (STD) BC Methods  Model : ",gcm)) +
          labs(x="Date (days)", y=ylabel)
        
        # Plot and save
        print(p)
        dev.off()
        
      }
      
      ## Write metrics data
      freq <- write.table(stdgcm, intannvar, sep=" ", row.names=F, quote=F)
      
    } else {
      
      ## Define variables to plot ggplot functions in command line
      assign("stdgcm", stdgcm,  envir = .GlobalEnv)
      
      for (i in 1:ngcm){
        
        ## GCM name
        gcm <- colnames(stdgcm)[i+2]
        
        ## Define variables to plot ggplot functions in command line
        assign("i", i,  envir = .GlobalEnv)
        
        cat(paste0("\nInterannual variability boxplot  ", rcp, " ", varmod, " ", gcm))
        
        tiff(paste0(dirout, "/var_", rcp,"_",gcm,"_",varmod,"_lon_",lon,"_lat_",lat,".tif"), width=1200, height=1900, pointsize=8, compression='lzw',res=100)
        p <- ggplot(data=stdgcm) +  
          geom_bar(aes(x=month, y=stdgcm[,i+2], fill=factor(method)), shape=1, size=1, width=.5, stat="identity")+   # GCMs (future)
          facet_wrap(~ method, ncol=1) + 
          scale_fill_manual(values=c(green2, orange, red, red2, blue, blue2)) +
          theme(panel.background = element_rect(fill = 'gray92'), legend.title=element_blank()) +
          ggtitle(paste0("Interannual Variability (STD) BC Methods  Model : ", gcm)) +
          labs(x="Date (days)", y=ylabel)     
        
        # Plot and save
        print(p)
        dev.off()
        
      }
      
      ## Write output metrics file
      stdgcm <- write.table(stdgcm, intannvar, sep=" ", row.names=F, quote=F)
      
    }
    
  }
  
  
  ##### Rainfall frequency and hot days frequency comparisson
  
  if (varmod == "prec" || varmod == "tmax"){
    
    ## Define output metrics file
    ofreq <- paste0(dirout, "/freq_", rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".txt")
    
    if (!file.exists(ofreq)) {
      
      ## Get months, years and methods from merged file
      months <- month(as.Date(merge$date))
      years <- year(as.Date(merge$date))
      methods <- year(as.Date(merge$date))
      
      ## Calculate frequencies of rainy and hot days
      merge_mod <- merge[,3:length(merge)]
      if (varmod == "prec"){merge_mod[merge_mod < 1] <- 0 ; merge_mod[merge_mod > 1] <- 1} 
      if (varmod == "tmax"){merge_mod[merge_mod < 30] <- 0 ; merge_mod[merge_mod > 30] <- 1}
      merge_mod <- cbind(merge[1:2], merge_mod)
      freq <- aggregate(merge_mod[3:length(merge_mod)], by=list("method"=merge_mod$method, "year"=years, "month"=months), FUN="sum", na.rm=T)
      
      ## Set apart observations
      obs <- freq[which(freq$method == levels(freq$method)[1]), ]$obs
      freq$obs <- NULL
      
      if (rcp == "historical"){ # Historical plot includes observations
        
        # Join observations at the end of std GCM values
        obs <- matrix(1, length(obs)[1], ngcm) * obs
        colnames(obs) <- gcmlist
        freq <- rbind(freq, cbind("method"=rep("OBS", dim(obs)[1]),freq[which(freq$method == levels(freq$method)[1]),][2:3], obs))
        rownames(freq) <- NULL
        
        ## Loop through GCMs
        for (i in 1:ngcm){
          
          if (varmod == "prec"){cat(paste0("\nRainfall freq boxplot : ", rcp, " ", gcmlist[i]))} else {cat(paste0("\nHot days freq boxplot : ", rcp, " ", gcmlist[i]))}
          
          freq_mod <- freq
          colnames(freq_mod)[i+3] <- "model"
          
          assign("freq_mod", freq_mod,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          tiff(paste0(dirout, "/freq_", rcp,"_",gcmlist[i],"_",varmod,"_lon_",lon,"_lat_",lat,".tif"), width=1200, height=300, pointsize=8, compression='lzw',res=100)
          f <- ggplot(data=freq_mod, aes(x=method, y=model, fill=method)) + 
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4)), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=c(green2, orange, red, red2, blue2, blue3, "white")) +
            geom_boxplot(outlier.size = 1) +
            labs(x="Months", y=flabel) +
            facet_grid(~month, scales="free_y", drop=T, labeller=f_labeller)
          
          # Plot 
          print(f)
          dev.off()
          
        }
        
        #Write plot data
        freq <- write.table(freq, ofreq, sep=" ", row.names=F, quote=F)
        
      } else {
        
        assign("freq", freq,  envir = .GlobalEnv)
        
        for (i in 1:ngcm){
          
          if (varmod == "prec"){cat(paste0("\nRainfall freq boxplot : ", rcp, " ", gcmlist[i]))} else {cat(paste0("\nHot days freq boxplot : ", rcp, " ", gcmlist[i]))}
          
          freq_mod <- freq
          colnames(freq_mod)[i+3] <- "model"
          
          assign("freq_mod", freq,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          tiff(paste0(dirout, "/freq_", rcp,"_",gcmlist[i],"_",varmod,"_lon_",lon,"_lat_",lat,".tif"), width=1200, height=300, pointsize=8, compression='lzw',res=100)
          f <- ggplot(data=freq_mod, aes(x=method, y=model, fill=method)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=c(green2, orange, red, red2, blue2, blue3)) +
            geom_boxplot(outlier.size = 1) +
            labs(x="Months", y=flabel) +
            facet_grid(~month, scales="free_y", drop=T, labeller=f_labeller)
          
          # Plot and save
          print(f)
          dev.off()
          
        }
        
        #Write output metrics file
        freq <- write.table(freq, ofreq, sep=" ", row.names=F, quote=F)
        
      }
      
    }
    
  }   
  
}

## Comparison methods (plots ALL)
bc_densityStats <- function(varmod="srad", rcpList="historical",yi=1980, yf=1985, lon=38.35, lat=-4.75, dirbase="D:/jetarapues/Request/Request_cnavarro/bc/bc_38.35_-4.75",stat){
  
  ## Load libraries
  #library(lubridate); library(ggplot2); library(reshape)
  
  dirsBC=c("Bias_Correction_no_variability","Bias_Correction_variability","Change_Factor_no_variability","Change_Factor_variability","quantile_mapping","raw_merge")
  listBC=list.dirs(dirbase,recursive=F,full.names=F)
  
  dirsel=intersect(dirsBC,listBC)  
  
  ## Set working directory
  setwd(dirbase)
  
  ## Set and create output directory
  dirout <- paste0(dirbase, "/statistics")
  if (!file.exists(dirout)) {dir.create(dirout, recursive=T)}
  
  ## Define end and start year to plot
  #yi <- substr(ts, 1, 4)
  #yf <- substr(ts, 6, 9)
  ofile=paste0(dirout,"/density_stat_",varmod,".tif")
  ofileDif=paste0(dirout,"/density_diff_stat_",varmod,".tif")
  
  if (!file.exists(ofile) || !file.exists(ofileDif)  ) {
    
    merge <- c()
    rcpLists=c("historical",rcpList)
    for(rcp in rcpLists){
      listbcAll=list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0(rcp,"_",varmod, "_*")) # 
      #   listbcAll=c(listbcAll,list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0("historical_",varmod, "_*")))
      
      ## Define methods to plot
      if (rcp == "historical"){
        methods <- c("sh_","bc_", "qm_", "raw_")
        listbc=grep(paste(methods,collapse="|"),listbcAll, value = TRUE)
        methods_ln <-unique(dirname(listbc))  #c("BC Var", "BC", "QM", "RAW") # 
      } else {
        methods <- c("sh_","bc_","del_","cf_","qm_","raw_")
        listbc=grep(paste(methods,collapse="|"),listbcAll, value = TRUE)
        methods_ln <- unique(dirname(listbc))  # c("CF Var", "CF", "BC Var", "BC", "QM", "RAW") #
      }  
      
      ## Load all bias corrected data
      odat <- lapply(listbcAll, function(x){read.table(x,header=T,sep=" ")})
      
      # Get GCMs names and length
      ngcm <- length(odat[[1]]) - 2
      gcmlist <- names(odat[[1]])[3:length(odat[[1]])]
      
      # Merge all data in a single table
      for(j in 1:length(odat)) { merge <- rbind(cbind("method"=rep(methods_ln[j],nrow( odat[[j]] )),"rcp"=rep(rcp,nrow( odat[[j]])), (odat[[j]])), merge) }
    }
    rownames(merge) <- NULL
    
    # Y-axis labels by variable
    if(varmod == "prec"){
      xlabel <- "Precipitation (mm/day)"; flabel <- "Rainfall Frequency (days/month)";#ylimit =c(0, 0.005);xlimit=c(0, 2000); 
      ylabel <- "Density"
    }else if(varmod == "tmin"){
      xlabel <- "Min. Temperature (C)"; #xlimit = c(-10, 25);ylimit = c(0, 0.2);
      ylabel <- "Density"
    }else if(varmod == "tmax"){
      xlabel <- "Max. Temperature (C)"; flabel <- "Hot days Frequency (days/month)";# ylimit = c(0, 0.2);xlimit=c(0, 50);
      ylabel <- "Density"
    }else if(varmod == "srad"){
      xlabel <- "Shortwave Sol. Radiation (MJ/m2 day)"; #ylimit = c(0, 0.2);xlimit = c(0, 400);
      ylabel <- "Density"
    }else if(varmod == "hur"){
      xlabel <- "Relative Humidity (%)"; #ylimit = c(0, 0.2);xlimit = c(0, 100);
      ylabel <- "Density"
    }else if(varmod == "swind"){
      xlabel <- "Wind speed (m/s)"; #ylimit = c(0, 0.2);xlimit = c(0, 400);
      ylabel <- "Density"
    }else if(varmod == "tmean"){
      xlabel <- "Mean Temperature (C)"; #xlimit = c(0, 100);ylimit = c(0, 0.2);
      ylabel <- "Density"
    }
    
    dataAll<- data.frame()
    
    
    filset <- names(merge[4:length(merge)])
    model <- names(merge[5:length(merge)])
    
    ##### Para calcular datos monthly
    #     if (varmod=="prec"){
    #       mongcm <- aggregate(merge[4:length(merge)], by=list("method"=merge$method,"rcp"=merge$rcp,"year"=year(as.Date(merge$date)), "month"=month(as.Date(merge$date))), sum) #,"year"=year(as.Date(merge$date))
    #     }else if(varmod=="tmax" || varmod=="tmin" || varmod=='tmean'||varmod=='hur'||varmod=='swind'||varmod=='srad'){
    #       mongcm <- aggregate(merge[4:length(merge)], by=list("method"=merge$method,"rcp"=merge$rcp,"year"=year(as.Date(merge$date)), "month"=month(as.Date(merge$date))), mean) #,"year"=year(as.Date(merge$date))
    #     }
    #     if(length(model)>1){
    #       mod=mongcm[names(mongcm)!="date"]
    #       m=mod[,6:length(mod)] #pos=which(is.na(m)==TRUE)
    #       gcm="ensemble"
    #       Value=rowMeans(m, na.rm = TRUE)
    #       dataset=mongcm[1]
    #       mongcm$date=paste(mongcm$year,mongcm$month,sep='-')
    #       rcp=mongcm$rcp
    #       dataAll <-rbind(dataAll,cbind(dataset,rcp,mongcm$date,gcm,Value))
    #     }
    #     for(i in 1:length(filset)){
    #       gcm=rep(filset[i],nrow(mongcm)) 
    #       Value=mongcm[i+4]
    #       dataset=mongcm[1]
    #       colnames(Value) <- c("Value")
    #       mongcm$date=paste(mongcm$year,mongcm$month,sep='-')
    #       rcp=mongcm$rcp
    #       dataAll <-rbind(dataAll,cbind(dataset,rcp,mongcm$date,gcm, Value))
    #     }
    
    ## Para datos diarios
    if(length(model)>1){
      m=merge[,5:length(merge)] #pos=which(is.na(m)==TRUE)
      gcm="ensemble"
      Value=rowMeans(m, na.rm = TRUE)
      dataset=merge[1]
      date=merge$date
      rcp=merge$rcp
      dataAll <-rbind(dataAll,cbind(dataset,rcp,date,gcm,Value))
      
    }
    for(i in 1:length(filset)){
      gcm=rep(filset[i],nrow(merge)) 
      Value=merge[i+3]
      dataset=merge[1]
      colnames(Value) <- c("Value")
      date=merge$date
      rcp=merge$rcp
      dataAll <-rbind(dataAll,cbind(dataset,rcp,date,gcm, Value))
    }    
    
    
    colnames(dataAll) <- c("Dataset","RCP", "Date","GCM","Value")
    dataValuesF=dataAll
    ymax=c()
    qdMax=c()
    qdMin=c()
    ens=data.frame()
    for(rcps in rcpLists){
      for(sets in levels(dataAll$Dataset)){
        if(length(model)>1){
          ens=subset(dataAll,dataAll$GCM=="ensemble"&dataAll$RCP==rcps&dataAll$Dataset==sets)
        }else{
          if(rcps!="historical"){
            ens=subset(dataAll,dataAll$GCM==levels(dataAll$GCM)[2]&dataAll$Dataset==sets&dataAll$RCP==rcps)
          }
        }
        if(nrow(ens)>0 & length(ens$Value)>0 & all(is.na(ens$Value))!=TRUE){ 
          d=density(as.numeric(na.omit(ens$Value)))
          ymax=c(ymax,d$y[which.max(d$y)])         
          valuesq=as.numeric(as.character(ens$Value))
          qdevMax=quantile(valuesq,0.95,na.rm=T)
          deMax=data.frame(id=names(qdevMax), values=unname(qdevMax), stringsAsFactors=FALSE)
          qdMax=c(qdMax,deMax$values)
          qdevMin=quantile(valuesq,0.03,na.rm=T)
          devMin=data.frame(id=names(qdevMin), values=unname(qdevMin), stringsAsFactors=FALSE)          
          qdMin=c(qdMin,devMin$values)          
        }
      }
    }
    if(length(ymax)>0){
      ymax=round(min(ymax)*2,digits = 3)
    }else{ymax=1}
    xmax=round(mean(qdMax)) #round(max(ens$Value))
    xmin=round(min(qdMin))
    
    xlimit = c(xmin,xmax);ylimit = c(0, ymax*2);
    
    #       p <- ggplot(data=dataValuesF,aes(x=Date, y=Value, group = factor(GCM), colour=factor(GCM))) + 
    #         geom_point() + geom_line() + facet_wrap(~ Dataset, ncol=1)
    #       plot(p)
    
    
    shortLevels=c(levels(dataValuesF$GCM)[c(-which(levels(dataValuesF$GCM)=="obs"),-which(levels(dataValuesF$GCM)=="ensemble"))],levels(dataValuesF$GCM)[which(levels(dataValuesF$GCM)=="obs")],levels(dataValuesF$GCM)[which(levels(dataValuesF$GCM)=="ensemble")])
    dataValuesF$GCM2=factor(dataValuesF$GCM,levels=shortLevels)      
    
    p1 <- ggplot(data = dataValuesF, aes(x=as.numeric(as.character(Value)), group=factor(GCM2), colour=factor(GCM2),size=factor(GCM2))) + #
      #                 theme(legend.title=element_blank(),panel.background=element_blank(),
      #                       axis.text=element_text(size=14),legend.text = element_text(colour="blue", size = 15, face = "bold"),
      #                       panel.grid.major = element_line(colour = "#81DAF5", size=0.2),panel.grid.minor = element_line(colour = "#CEECF5", size=0.1),   #,legend.position="bottom"
      #                       strip.text.x = element_text(size = 14),strip.text.y = element_text(size = 17),text = element_text(size = 19)#,panel.margin = unit(2, "lines")
      #                 )+
      theme_light() + theme(panel.margin = unit(1, "lines"),legend.text = element_text(size = 15, face = "bold"),legend.title=element_blank(),axis.text=element_text(size=11,face = "bold"),strip.text.x = element_text(size = 14,colour="black",face = "bold"),strip.text.y = element_text(size = 17,colour="black",face = "bold"),text = element_text(size = 19))+
      labs(x=xlabel, y=ylabel) +
      geom_line(stat="density") + #,size=1.2
      facet_grid(RCP ~ Dataset,scales="free_y", drop=T) +
      scale_size_manual(values=c(rep(0.8,nlevels(dataValuesF$GCM2)-2),1.2,1.2))+
      scale_color_manual(values=c(rep("#B2AFAF", nlevels(dataValuesF$GCM2)-2),"#000000","#D52ED7"))+
      #       scale_x_continuous(limits = c(20, 32),breaks=seq(20,32,by=1),expand = c(0, 0)) +
      #       scale_y_continuous(limits = c(0, 0.4),expand = c(0, 0))   
      scale_x_continuous(limits = xlimit,expand = c(0, 0)) + #breaks=seq(0,400,by=100), 
      scale_y_continuous(limits = ylimit,expand = c(0, 0))  # 
    
    #ggtitle(paste("Eta and WorldClim ->",var)) +
    #guides(colour = guide_legend(override.aes = list(size=4)))
    
    tiff(ofile, width=3000, height=1600, pointsize=8, compression='lzw',res=100)
    plot(p1)
    dev.off()
    cat(paste0("..done density: ",varmod,"\n"))
    
    ##======================== PLOT DE DIFRENCIAS ===================================
    if (!file.exists(ofileDif)) {
      dataDiff<- data.frame()
      dataDiffAll<- data.frame()
      for(sets in levels(merge$method)){
        sub1=subset(merge,merge$method==sets & merge$rcp=="historical")
        for(rcps in rcpList){
          sub2=subset(merge,merge$method==sets & merge$rcp==rcps)
          if(nrow(sub1)==nrow(sub2)){
            dif=sub2[,5:length(sub2)]-sub1[,5:length(sub1)]
            dataDiff <-rbind(dataDiff,cbind(sets,paste0("Diff_",rcps,"-Hist"),dif))
          }
        }
      }
      if(length(dataDiff)>0){
        colnames(dataDiff) <- c("Dataset","RCP",names(merge[,5:length(merge)]))
        if(length(model)>1){
          m=dataDiff[,3:length(dataDiff)] #pos=which(is.na(m)==TRUE)
          gcm="ensemble"
          Value=rowMeans(m, na.rm = TRUE)
          dataset=dataDiff[1]
          rcp=dataDiff$RCP
          dataDiffAll <-rbind(dataDiffAll,cbind(dataset,rcp,gcm,Value))
        }
        for(i in 1:length(model)){
          gcm=rep(model[i],nrow(dataDiff)) 
          Value=dataDiff[i+2]
          dataset=dataDiff[1]
          colnames(Value) <- c("Value")
          rcp=dataDiff$RCP
          dataDiffAll <-rbind(dataDiffAll,cbind(dataset,rcp,gcm, Value))
        }    
        colnames(dataDiffAll) <- c("Dataset","RCP", "GCM","Value")
        dataValuesF=dataDiffAll
        ymax=c()
        qdMax=c()
        qdMin=c()
        for(rcps in levels(dataValuesF$RCP)){
          for(sets in levels(dataValuesF$Dataset)){
            if(length(model)>1){
              ens=subset(dataValuesF,dataValuesF$GCM=="ensemble"&dataValuesF$RCP==rcps&dataValuesF$Dataset==sets)
            }else{
                ens=subset(dataAll,dataAll$Dataset==sets)
            }
            if(nrow(ens)>0 & length(ens$Value)>0){ 
              d=density(as.numeric(na.omit(ens$Value)))
              ymax=c(ymax,d$y[which.max(d$y)])
              #xmin=c(xmin,min(as.numeric(ens$Value)))
              
              valuesq=as.numeric(as.character(ens$Value))
              qdevMax=quantile(valuesq,0.95,na.rm=T)
              deMax=data.frame(id=names(qdevMax), values=unname(qdevMax), stringsAsFactors=FALSE)
              qdMax=c(qdMax,deMax$values)
              qdevMin=quantile(valuesq,0.03,na.rm=T)
              devMin=data.frame(id=names(qdevMin), values=unname(qdevMin), stringsAsFactors=FALSE)          
              qdMin=c(qdMin,devMin$values)
            }
          }
        }
        ymax=round(max(ymax)*2,digits = 3)
        xmax=round(max(qdMax)) #round(max(ens$Value))
        xmin=round(min(qdMin))
        
        xlimit = c(xmin,xmax);ylimit = c(0, ymax);
        
        #       p <- ggplot(data=dataValuesF,aes(x=Date, y=Value, group = factor(GCM), colour=factor(GCM))) + 
        #         geom_point() + geom_line() + facet_wrap(~ Dataset, ncol=1)
        #       plot(p)
        
        if(length(model)>1){
          shortLevels=c(levels(dataValuesF$GCM)[c(-which(levels(dataValuesF$GCM)=="obs"),-which(levels(dataValuesF$GCM)=="ensemble"))],levels(dataValuesF$GCM)[which(levels(dataValuesF$GCM)=="obs")],levels(dataValuesF$GCM)[which(levels(dataValuesF$GCM)=="ensemble")])
          dataValuesF$GCM2=factor(dataValuesF$GCM,levels=shortLevels)      
          
          p1 <- ggplot(data = dataValuesF, aes(x=as.numeric(as.character(Value)), group=factor(GCM2), colour=factor(GCM2),size=factor(GCM2))) + #
            theme_light() + theme(panel.margin = unit(1, "lines"),legend.text = element_text(size = 15, face = "bold"),legend.title=element_blank(),axis.text=element_text(size=11,face = "bold"),strip.text.x = element_text(size = 14,colour="black",face = "bold"),strip.text.y = element_text(size = 17,colour="black",face = "bold"),text = element_text(size = 19))+
            labs(x=xlabel, y=ylabel) +
            geom_line(stat="density") + #,size=1.2
            facet_grid(RCP ~ Dataset,scales="free_y", drop=T) +
            scale_size_manual(values=c(rep(0.8,nlevels(dataValuesF$GCM2)-1),1.2))+
            scale_color_manual(values=c(rep("#B2AFAF", nlevels(dataValuesF$GCM2)-1),"#D52ED7"))+
            scale_x_continuous(limits = xlimit,expand = c(0, 0)) + #breaks=seq(0,400,by=100), 
            scale_y_continuous(limits = ylimit,expand = c(0, 0))  # 
        }else{
  
          p1 <- ggplot(data = dataValuesF, aes(x=as.numeric(as.character(Value)), group=factor(GCM), colour=factor(GCM),size=factor(GCM))) + #
            theme_light() + theme(panel.margin = unit(1, "lines"),legend.text = element_text(size = 15, face = "bold"),legend.title=element_blank(),axis.text=element_text(size=11,face = "bold"),strip.text.x = element_text(size = 14,colour="black",face = "bold"),strip.text.y = element_text(size = 17,colour="black",face = "bold"),text = element_text(size = 19))+
            labs(x=xlabel, y=ylabel) +
            geom_line(stat="density") + #,size=1.2
            facet_grid(RCP ~ Dataset,scales="free_y", drop=T)
#             scale_size_manual(values=c(rep(0.8,nlevels(dataValuesF$GCM))))+
#             scale_color_manual(values=c(rep("#B2AFAF", nlevels(dataValuesF$GCM))))+
#             scale_x_continuous(limits = xlimit,expand = c(0, 0)) + #breaks=seq(0,400,by=100), 
#             scale_y_continuous(limits = ylimit,expand = c(0, 0))  #           
        }
        
        tiff(ofileDif, width=3000, height=1600, pointsize=8, compression='lzw',res=100)
        plot(p1)
        dev.off()
        cat(paste0("..done density diff: ",varmod,"\n"))
      }
    }
    #========================================================================================
    
    
  }
  
}

##indicadores
bc_statsInd <- function(varmod="prec", rcp="historical",yi=1998, yf=2013, lon=-73.5, lat=3.4, dirbase="C:/Temp/bc/bc_31.45_-0.65",stat){
  
  ## Load libraries
  library(lubridate); library(ggplot2); library(reshape)
  
  dirsBC=c("Bias_Correction_no_variability","Bias_Correction_variability","Change_Factor_no_variability","Change_Factor_variability","quantile_mapping","raw_merge")
  listBC=list.dirs(dirbase,recursive=F,full.names=F)
  
  dirsel=intersect(dirsBC,listBC)  
  
  ## Set working directory
  setwd(dirbase)
  
  ## Set and create output directory
  dirout <- paste0(dirbase, "/indicadores")
  if (!file.exists(dirout)) {dir.create(dirout, recursive=T)}
  
  ## Define end and start year to plot
  #yi <- substr(ts, 1, 4)
  #yf <- substr(ts, 6, 9)
  
  
  listbcAll_p=list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0(rcp,"_prec_*"))
  listbcAll_tmx=list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0(rcp,"_tmax_*"))
  listbcAll_tmn=list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0(rcp,"_tmin_*"))
  listbcAll_s=list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0(rcp,"_srad_*"))
  
  
  #   listbcAll=c(listbcAll,list.files(path = dirsel, full.names=TRUE,recursive = TRUE,pattern=paste0("historical_",varmod, "_*")))
  
  ## Define methods to plot
  if (rcp == "historical"){
    methods <- c("sh_","bc_", "qm_", "raw_")
    listbc=grep(paste(methods,collapse="|"),listbcAll, value = TRUE)
    methods_ln <-unique(dirname(listbc))  #c("BC Var", "BC", "QM", "RAW") # 
  } else {
    methods <- c("sh_","bc_","del_","cf_","qm_","raw_")
    listbc=grep(paste(methods,collapse="|"),listbcAll, value = TRUE)
    methods_ln <- unique(dirname(listbc))  # c("CF Var", "CF", "BC Var", "BC", "QM", "RAW") #
  }  
  
  ## Load all bias corrected data
  odat_p <- lapply(listbcAll_p, function(x){read.table(x,header=T,sep=" ")})
  odat_tmx <- lapply(listbcAll_tmx, function(x){read.table(x,header=T,sep=" ")})
  odat_tmn <- lapply(listbcAll_tmn, function(x){read.table(x,header=T,sep=" ")})
  odat_s <- lapply(listbcAll_s, function(x){read.table(x,header=T,sep=" ")})
  
  # Get GCMs names and length
  ngcm <- length(odat_p[[1]]) - 2
  gcmlist <- names(odat_p[[1]])[3:length(odat_p[[1]])]

  # Merge all data in a single table
#   merge <- c()
#   for(j in 1:length(odat)) { merge <- rbind(cbind("method"=rep(methods_ln[j],nrow( odat[[j]] )), (odat[[j]])), merge) }
#   rownames(merge) <- NULL

  ################################ SEASONS
  seasons <- c()
  seasons <- cbind(c("djf", "mam", "jja", "son"), c(12, 3, 6, 9), c(2, 5, 8, 11))
  colnames(seasons) <- c("season", "staMth", "endMth")  
  
  seasonALL_p=c()
  for(j in 1:length(odat_p)) { 
    for (k in 1:nrow(seasons)){
      if(k!=1){
        seasonStk <- subset(odat_p[[j]], as.numeric(format(as.Date(odat_p[[j]]$date), "%m")) %in% seasons[k,2]:seasons[k,3])
      }else{
        seasonStk <- subset(odat_p[[j]], as.numeric(format(as.Date(odat_p[[j]]$date), "%m")) %in% c(12,1,2))
      }
      seasonALL_p=rbind(cbind("method_p"=rep(methods_ln[j],nrow(seasonStk)),"rcp"=rep(rcp,nrow(seasonStk)),"season"=rep(seasons[k,1],nrow(seasonStk)),seasonStk),seasonALL_p)  
    }  
  }
  seasonALL_tmx=c()
  for(j in 1:length(odat_tmx)) { 
    for (k in 1:nrow(seasons)){
      if(k!=1){
        seasonStk <- subset(odat_tmx[[j]], as.numeric(format(as.Date(odat_tmx[[j]]$date), "%m")) %in% seasons[k,2]:seasons[k,3])
      }else{
        seasonStk <- subset(odat_tmx[[j]], as.numeric(format(as.Date(odat_tmx[[j]]$date), "%m")) %in% c(12,1,2))
      }
      seasonALL_tmx=rbind(cbind("method_tmx"=rep(methods_ln[j],nrow(seasonStk)),"rcp"=rep(rcp,nrow(seasonStk)),"season"=rep(seasons[k,1],nrow(seasonStk)),seasonStk),seasonALL_tmx)  
    }  
  }  
  seasonALL_tmn=c()
  for(j in 1:length(odat_tmn)) { 
    for (k in 1:nrow(seasons)){
      if(k!=1){
        seasonStk <- subset(odat_tmn[[j]], as.numeric(format(as.Date(odat_tmn[[j]]$date), "%m")) %in% seasons[k,2]:seasons[k,3])
      }else{
        seasonStk <- subset(odat_tmn[[j]], as.numeric(format(as.Date(odat_tmn[[j]]$date), "%m")) %in% c(12,1,2))
      }
      seasonALL_tmn=rbind(cbind("method_tmn"=rep(methods_ln[j],nrow(seasonStk)),"rcp"=rep(rcp,nrow(seasonStk)),"season"=rep(seasons[k,1],nrow(seasonStk)),seasonStk),seasonALL_tmn)  
    }  
  }
  seasonALL_s=c()
  for(j in 1:length(odat_s)) { 
    for (k in 1:nrow(seasons)){
      if(k!=1){
        seasonStk <- subset(odat_s[[j]], as.numeric(format(as.Date(odat_s[[j]]$date), "%m")) %in% seasons[k,2]:seasons[k,3])
      }else{
        seasonStk <- subset(odat_s[[j]], as.numeric(format(as.Date(odat_s[[j]]$date), "%m")) %in% c(12,1,2))
      }
      seasonALL_s=rbind(cbind("method_s"=rep(methods_ln[j],nrow(seasonStk)),"rcp"=rep(rcp,nrow(seasonStk)),"season"=rep(seasons[k,1],nrow(seasonStk)),seasonStk),seasonALL_s)  
    }  
  }  
  
  
  # Y-axis labels by variable
  if(varmod == "prec"){
    ylabel <- "Precipitation (mm/day)"; flabel <- "Rainfall Frequency (days/month)";limit = c(0,250);
  }else if(varmod == "tmin"){
    ylabel <- "Min. Temperature (C)"; limit = c(-10, 25);
  }else if(varmod == "tmax"){
    ylabel <- "Max. Temperature (C)"; flabel <- "Hot days Frequency (days/month)"; limit = c(0, 40);
  }else if(varmod == "srad"){
    ylabel <- "Shortwave Sol. Radiation ((MJ/m2 day)"; limit = c(0, 400);
  }else if(varmod == "hur"){
    ylabel <- "Relative Humidity (%)"; limit = c(0, 100);
  }else if(varmod == "swind"){
    ylabel <- "Wind speed (m/s)"; limit = c(0, 400);
  }else if(varmod == "tmean"){
    ylabel <- "Mean Temperature (C)"; limit = c(0, 100);
  }
  flabel_py <- "Rainfall Frequency (days/season)";limit = c(0,250);
  flabel_tx <- "ndays Max. Temperaure >35 (C)"; flabel <- "Hot days Frequency (days/season)"; limit = c(0, 40);
  flabel_tn <- "ndays Min. Temperature >22 (C)"; flabel <- "Hot days Frequency (days/season)"; limit = c(0, 40);
  flabel_p <- "Precipitation total (mm/season)"
  flabel_s <- "S. Radiation total (MJ/m2 season)"
  flabel_tmx <- "Mean Max.Temperature (C)"
  flabel_tmn <- "Mean Min.Temperature (C)"
  
  # Personalized colors 
  gray='gray50';blue="#122F6B";blue2="#1F78B4";blue3="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  # Long name months list
  f_names <- list("son"="SON", "jja"="JJA", "mam"="MAM", "djf"="DJF")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  ######################
  
  ##### Rainfall frequency and hot days frequency comparisson
  
    ## Define output metrics file
    ofreq <- paste0(dirout, "/freq_", rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".txt")
    
    if (!file.exists(ofreq)) {
      
      ## Get months, years and methods from merged file
      months <- month(as.Date(seasonALL$date))
      years_p <- year(as.Date(seasonALL_p$date))
      years_tmx <- year(as.Date(seasonALL_tmx$date))
      years_tmn <- year(as.Date(seasonALL_tmn$date))
      years_s <- year(as.Date(seasonALL_s$date))
      methods <- year(as.Date(seasonALL$date))
      
      ## Calculate frequencies of rainy and hot days
      merge_mod_p <- seasonALL_p[,5:length(seasonALL_p)]
      merge_mod_p[merge_mod_p < 1] <- 0 ; merge_mod_p[merge_mod_p > 1] <- 1  #ind-1
      merge_mod_p <- cbind(seasonALL_p[1:3], merge_mod_p)
      freq_py <- aggregate(merge_mod_p[4:length(merge_mod_p)], by=list("method_py"=merge_mod_p$method, "year"=years_p, "season"=merge_mod_p$season), FUN="sum", na.rm=T) 
      
      merge_mod_tmx <- seasonALL_tmx[,5:length(seasonALL_tmx)]
      merge_mod_tmx[merge_mod_tmx < 35] <- 0 ; merge_mod_tmx[merge_mod_tmx > 35] <- 1 #ind-2
      merge_mod_tmx <- cbind(seasonALL_tmx[1:3], merge_mod_tmx)
      freq_tx <- aggregate(merge_mod_tmx[4:length(merge_mod_tmx)], by=list("method_tmx"=merge_mod_tmx$method, "year"=years_tmx, "season"=merge_mod_tmx$season), FUN="sum", na.rm=T) 
      
      merge_mod_tmn <- seasonALL_tmn[,5:length(seasonALL_tmn)]
      merge_mod_tmn[merge_mod_tmn < 22] <- 0 ; merge_mod_tmn[merge_mod_tmn > 22] <- 1 #ind-3
      merge_mod_tmn <- cbind(seasonALL_tmn[1:3], merge_mod_tmn)
      freq_tn <- aggregate(merge_mod_tmn[4:length(merge_mod_tmn)], by=list("method_tmn"=merge_mod_tmn$method, "year"=years_tmn, "season"=merge_mod_tmn$season), FUN="sum", na.rm=T) 
      
      #ind-4
       freq_p <- aggregate(seasonALL_p[5:length(seasonALL_p)], by=list("method_p"=seasonALL_p$method, "year"=years_p, "season"=seasonALL_p$season), FUN="sum", na.rm=T) #ind-4,5
      #ind-5
        freq_s <- aggregate(seasonALL_s[5:length(seasonALL_s)], by=list("method_s"=seasonALL_s$method, "year"=years_s, "season"=seasonALL_s$season), FUN="sum", na.rm=T) #ind-4,5
      #ind-6
        freq_tmx <- aggregate(seasonALL_tmx[6:length(seasonALL_tmx)], by=list("method_tmx"=seasonALL_tmx$method, "year"=years_tmx, "season"=seasonALL_tmx$season), FUN="mean", na.rm=T)
      ##ind-7
        freq_tmn <- aggregate(seasonALL_tmn[6:length(seasonALL_tmn)], by=list("method_tmn"=seasonALL_tmn$method, "year"=years_tmn, "season"=seasonALL_tmn$season), FUN="mean", na.rm=T)

      
      ## Set apart observations
      obs <- freq_py[which(freq_py$method == "quantile_mapping"), ]$obs
      freq_py$obs <- NULL
      freq_tx$obs <- NULL
      freq_tn$obs <- NULL
      freq_p$obs <- NULL
      freq_s$obs <- NULL
      freq_tmx$obs <- NULL
      freq_tmn$obs <- NULL
      
      if (rcp == "historical"){ # Historical plot includes observations

        
        # Join observations at the end of std GCM values
        obs <- matrix(1, length(obs)[1], ngcm) * obs
        colnames(obs) <- gcmlist
        freq_p <- rbind(freq_p, cbind("method_p"=rep("OBS", dim(obs)[1]),freq_p[which(freq_p$method == "quantile_mapping"),][2:3], obs))
        freq_py <- rbind(freq_py, cbind("method_py"=rep("OBS", dim(obs)[1]),freq_py[which(freq_py$method == "quantile_mapping"),][2:3], obs))
        freq_tmx <- rbind(freq_tmx, cbind("method_tmx"=rep("OBS", dim(obs)[1]),freq_tmx[which(freq_tmx$method == "quantile_mapping"),][2:3], obs))
        freq_tmn <- rbind(freq_tmn, cbind("method_tmn"=rep("OBS", dim(obs)[1]),freq_tmn[which(freq_tmn$method == "quantile_mapping"),][2:3], obs))
        freq_tx <- rbind(freq_tx, cbind("method_tmx"=rep("OBS", dim(obs)[1]),freq_tx[which(freq_tx$method == "quantile_mapping"),][2:3], obs))
        freq_tn <- rbind(freq_tn, cbind("method_tmn"=rep("OBS", dim(obs)[1]),freq_tn[which(freq_tn$method == "quantile_mapping"),][2:3], obs))
        freq_s <- rbind(freq_s, cbind("method_s"=rep("OBS", dim(obs)[1]),freq_s[which(freq_s$method == "quantile_mapping"),][2:3], obs))
        
        rownames(freq_py) <- NULL
        
        ## Loop through GCMs
        for (i in 1:ngcm){
          
          if (varmod == "prec"){cat(paste0("\nRainfall freq boxplot : ", rcp, " ", gcmlist[i]))} else {cat(paste0("\nHot days freq boxplot : ", rcp, " ", gcmlist[i]))}
          assign("freq_py", freq_py,  envir = .GlobalEnv)
          assign("freq_tx", freq_tx,  envir = .GlobalEnv)
          assign("freq_tn", freq_tn,  envir = .GlobalEnv)
          assign("freq_p", freq_p,  envir = .GlobalEnv)
          assign("freq_s", freq_s,  envir = .GlobalEnv)
          assign("freq_tmx", freq_tmx,  envir = .GlobalEnv)
          assign("freq_tmn", freq_tmn,  envir = .GlobalEnv)
          
          freq_mod_p <- freq_p
          colnames(freq_mod_p)[i+3] <- "model_p"
          assign("freq_mod_p", freq_p,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_py <- freq_py
          colnames(freq_mod_py)[i+3] <- "model_py"
          assign("freq_mod_py", freq_py,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_tmx <- freq_tmx
          colnames(freq_mod_tmx)[i+3] <- "model_tmx"
          assign("freq_mod_tmx", freq_tmx,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_tmn <- freq_tmn
          colnames(freq_mod_tmn)[i+3] <- "model_tmn"
          assign("freq_mod_tmn", freq_tmn,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)          
          
          freq_mod_tx <- freq_tx
          colnames(freq_mod_tx)[i+3] <- "model_tx"
          assign("freq_mod_tx", freq_tx,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_tn <- freq_tn
          colnames(freq_mod_tn)[i+3] <- "model_tn"
          assign("freq_mod_tn", freq_tn,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)           
          
          freq_mod_s <- freq_s
          colnames(freq_mod_s)[i+3] <- "model_s"
          assign("freq_mod_s", freq_s,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)          
          
          fillColor=c(red, blue2, orange, red2, blue2, blue3, "white")
          
          tiff(paste0(dirout, "/boxplot_", rcp,"_",gcmlist[i],"_lon_",lon,"_lat_",lat,".tif"), width=1000, height=1800, pointsize=8, compression='lzw',res=100)
          vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(8, 1)))
          
          f_p <- ggplot(data=freq_mod_p, aes(x=method_p, y=model_p, fill=method_p)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_p) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none") 
          f_py <- ggplot(data=freq_mod_py, aes(x=method_py, y=model_py, fill=method_py)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_py) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none")           
          f_tmx <- ggplot(data=freq_mod_tmx, aes(x=method_tmx, y=model_tmx, fill=method_tmx)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tmx) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none") 
          f_tmn <- ggplot(data=freq_mod_tmn, aes(x=method_tmn, y=model_tmn, fill=method_tmn)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tmn) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller)+ #
            theme(legend.position="none") 
          f_tx <- ggplot(data=freq_mod_tx, aes(x=method_tmx, y=model_tx, fill=method_tmx)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tx) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none") 
          f_tn <- ggplot(data=freq_mod_tn, aes(x=method_tmn, y=model_tn, fill=method_tmn)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tn) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none")           
          f_s <- ggplot(data=freq_mod_s, aes(x=method_s, y=model_s, fill=method_s)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_s) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller)+
            theme(legend.position="none")           
          
          par(mai=c(0,0,0,0))
          plot.new()
          legend("bottom",ncol=3,legend=c("RAW","BC Quantile Mapping","OBS"),fill=c("red","blue2","orange"), title="Legend",inset=0.1)          
          
          print(f_p, vp = vplayout(1, 1))
          print(f_py, vp = vplayout(2, 1))
          print(f_tmx, vp = vplayout(3, 1))
          print(f_tmn, vp = vplayout(4, 1))
          print(f_tx, vp = vplayout(5, 1))
          print(f_tn, vp = vplayout(6, 1))          
          print(f_s, vp = vplayout(7, 1))
          
          
          dev.off()


          
        }
        
        
      } else {
        
        assign("freq_py", freq_py,  envir = .GlobalEnv)
        assign("freq_tx", freq_tx,  envir = .GlobalEnv)
        assign("freq_tn", freq_tn,  envir = .GlobalEnv)
        assign("freq_p", freq_p,  envir = .GlobalEnv)
        assign("freq_s", freq_s,  envir = .GlobalEnv)
        assign("freq_tmx", freq_tmx,  envir = .GlobalEnv)
        assign("freq_tmn", freq_tmn,  envir = .GlobalEnv)
        
        for (i in 1:ngcm){
          
          if (varmod == "prec"){cat(paste0("\nRainfall freq boxplot : ", rcp, " ", gcmlist[i]))} else {cat(paste0("\nHot days freq boxplot : ", rcp, " ", gcmlist[i]))}
          
          freq_mod_p <- freq_p
          colnames(freq_mod_p)[i+3] <- "model_p"
          assign("freq_mod_p", freq_p,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_py <- freq_py
          colnames(freq_mod_py)[i+3] <- "model_py"
          assign("freq_mod_py", freq_py,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_tmx <- freq_tmx
          colnames(freq_mod_tmx)[i+3] <- "model_tmx"
          assign("freq_mod_tmx", freq_tmx,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_tmn <- freq_tmn
          colnames(freq_mod_tmn)[i+3] <- "model_tmn"
          assign("freq_mod_tmn", freq_tmn,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)          

          freq_mod_tx <- freq_tx
          colnames(freq_mod_tx)[i+3] <- "model_tx"
          assign("freq_mod_tx", freq_tx,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)
          
          freq_mod_tn <- freq_tn
          colnames(freq_mod_tn)[i+3] <- "model_tn"
          assign("freq_mod_tn", freq_tn,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)           
          
          freq_mod_s <- freq_s
          colnames(freq_mod_s)[i+3] <- "model_s"
          assign("freq_mod_s", freq_s,  envir = .GlobalEnv)
          assign("i", i,  envir = .GlobalEnv)          

          fillColor=c(red, blue2, red, red2, blue2, blue3, "white")
          
          tiff(paste0(dirout, "/boxplot_", rcp,"_",gcmlist[i],"_lon_",lon,"_lat_",lat,".tif"), width=1000, height=1800, pointsize=8, compression='lzw',res=100)
          vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(8, 1)))
          
          f_p <- ggplot(data=freq_mod_p, aes(x=method_p, y=model_p, fill=method_p)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_p) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none") 
          f_py <- ggplot(data=freq_mod_py, aes(x=method_py, y=model_py, fill=method_py)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_py) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none")           
          f_tmx <- ggplot(data=freq_mod_tmx, aes(x=method_tmx, y=model_tmx, fill=method_tmx)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tmx) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none") 
          f_tmn <- ggplot(data=freq_mod_tmn, aes(x=method_tmn, y=model_tmn, fill=method_tmn)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tmn) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller)+ #
            theme(legend.position="none") 
          f_tx <- ggplot(data=freq_mod_tx, aes(x=method_tmx, y=model_tx, fill=method_tmx)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tx) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none") 
          f_tn <- ggplot(data=freq_mod_tn, aes(x=method_tmn, y=model_tn, fill=method_tmn)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_tn) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller) +#
            theme(legend.position="none")           
          f_s <- ggplot(data=freq_mod_s, aes(x=method_s, y=model_s, fill=method_s)) + # GCMs (historical)
            theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.y = element_text(size = rel(0.8))) +
            scale_fill_manual(values=fillColor) +
            geom_boxplot(outlier.size = NA,outline=FALSE) +
            labs(x="Seasons", y=flabel_s) +
            facet_grid(~season, scales="free_y", drop=T, labeller=f_labeller)+
            theme(legend.position="none")           

          par(mai=c(0,0,0,0))
          plot.new()
          legend("bottom",ncol=2,legend=c("RAW","BC Quantile Mapping"),fill=c("red","blue2"), title="Legend",inset=0.1)          
          
          print(f_p, vp = vplayout(1, 1))
          print(f_py, vp = vplayout(2, 1))
          print(f_tmx, vp = vplayout(3, 1))
          print(f_tmn, vp = vplayout(4, 1))
          print(f_tx, vp = vplayout(5, 1))
          print(f_tn, vp = vplayout(6, 1))          
          print(f_s, vp = vplayout(7, 1))

    
          dev.off()
          
        }

      }
      
    }
    
}

## Main function
bc_processing<- function(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat,sepFile,leep,typeData){

  ## Load libraries
  library(raster); library(ncdf); library(rgdal); library(lubridate); library(qmap); library(ggplot2);library(tools); library(reshape);require(grid) 
  
  dircdo <- "cdo"
  dateDownl= "bc_2015-12-14_05_46_42"  #  paste0("bc_",gsub("\\.",'-',gsub(':','_',gsub(" ", "_", as.character(Sys.time()))))) # 
  dirout <- paste0(dirWork,'/',dateDownl) 
  dataset <- tolower(dataset)

  dirHist <- paste0(dirgcm, "/historical")
  rcp_t=rcpList[1]
  dirrcp <- paste0(dirgcm, "/", rcp_t)
  a=list.dirs(dirHist,recursive=F,full.names=F)
  b=list.dirs(dirrcp,recursive=F,full.names=F)
  gcmlistIn=intersect(a,b)
  gcmlist=intersect(gcmlistIn,gcmlist)

  for (var in varlist){
    if  (var == "prec") {var <- "pr"} else if (var == "srad") {var <- "rsds"} else if (var == "tmax") {var <- "tasmax"} else if (var == "tmin") {var <- "tasmin"} else if (var == "tmean") {var <- "tas"} else if (var == "swind") {var <- "sfcWind"} 
    cat()
    if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"} else if (var == "tas") {varmod <- "tmean"} else if (var == "sfcWind") {varmod <- "swind"} else if (var == "hur") {varmod <- "hur"}
  
    checkncH=c()
    checkncF=c()
    check=c()
    gcmlistSel=c()
    for (gcm in gcmlist){
      ncvar <- list.files(path=paste0(dirrcp, "/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
      Hncvar <- list.files(path=paste0(dirgcm, "/historical/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
      checkncH=c(checkncH,ncvar)
      checkncF=c(checkncF,Hncvar)
      if (length(ncvar) > 0 && length(Hncvar) > 0){
        years <- sapply(strsplit(basename(ncvar), '[_]'), "[[", 6)  
        staYear <- sapply(strsplit(years, '[-]'), "[[", 1)
        endYear <- gsub(".nc","",substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 8))
        yearI<- strftime(as.Date(as.character(staYear), "%Y%m%d"),"%Y")
        yearF<- strftime(as.Date(as.character(endYear), "%Y%m%d"),"%Y")
        if(fuyf <= yearF){
          gcmlistSel=c(gcmlistSel,gcm)
        }             
      }  
    }
    if(length(gcmlistSel)>0){
      if(dataset=="station"){
        dirtemp <- paste0(dirout, "/obs/station")
        if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
        df = read.table(url(fileStat), header = TRUE,sep=sepFile)
        dateSta=strftime(as.Date(as.character(df$date[!is.na(df$date)] ), "%Y%m%d"),"%Y-%m-%d")
        station=data.frame(cbind(dateSta,df[,which(colnames(df)==varmod)]))   
        names(station)=c("date","value")
        if (!file.exists(paste0(dirtemp,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab"))){
          write.table(station,paste0(dirtemp,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab"), sep="\t",row.names=F,quote = FALSE)   
          check=c(check,'ok')
        }
        
      }else{
        ncvarlis <- paste0(dirobs,'/',dataset,"/daily/nc-files")
        ncvar <- list.files(ncvarlis, pattern=paste0(varmod,"_daily_ts_", tolower(dataset), "_*" ),full.names = T,ignore.case=F)
        ncvar <- ncvar[sapply(strsplit(basename(ncvar), '[_]'), "[[", 1)==varmod]   
        if(length(ncvar)>0){
          obs_extraction(dataset, varmod, Obyi,Obyf, lon, lat, dirobs, dirout,dircdo)  
          check=c(check,'ok')
        }
      } 
      cat()
      if(length(check)>0){
        gcm_extraction(var,varmod, "historical", Obyi,Obyf, gcmlistSel, lon, lat, dirgcm, dirout)
        
        merge_extraction(varmod, "historical",Obyi,Obyf, gcmlistSel, lon, lat, dataset, dirout,sepFile,leep,typeData)
        
        for(rcp in rcpList){
          gcm_extraction(var,varmod, rcp, fuyi,fuyf, gcmlistSel, lon, lat, dirgcm, dirout)
          merge_extraction(varmod, rcp,fuyi,fuyf, gcmlistSel, lon, lat, dataset, dirout,sepFile,leep,typeData)
          
          odat <- paste0(dirout,"/raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
          hdat <- paste0(dirout,"/raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
          
          if(file.exists(odat) && file.exists(hdat)){
            for(methBC in methBCList){
              if(methBC=='1'){
                sh_calcs(varmod, "historical", lon, lat, dirout,leep)
                sh_calcs(varmod, rcp, lon, lat, dirout,leep)  
              }else if(methBC=='2'){
                bc_calcs(varmod, "historical", lon, lat, dirout)   
                bc_calcs(varmod, rcp, lon, lat, dirout)          
              }else if(methBC=='3'){
                del_calcs(varmod, rcp, lon, lat, dirout,leep)
              }else if(methBC=='4'){
                cf_calcs(varmod, rcp, lon, lat, dirout,leep)
              }else{
                qm_calcs(varmod, rcp, lon, lat, dirout,leep)
              }
            }
            for(stat in statList){
              if(stat=='2' || stat=='3'){
                bc_stats(varmod, "historical",Obyi,Obyf, lon, lat, dirout,stat)
                bc_stats(varmod, rcp, fuyi,fuyf, lon, lat, dirout,stat)  
              }
            }  
          }
        }
        
        if(stat=='2' || stat=='3'){
          bc_densityStats(varmod,rcpList,Obyi,Obyf, lon, lat, dirout,stat)
        }
      }else{cat(paste0("no exite obs ",varmod,'\n'))}
    }else{cat(paste0("no exite gcm ",varmod,'\n'))}
  }
  
#   if(length(list.files(dirout,recursive=T))!=0){
#     if (file.exists(paste0(dirout,'/gcm'))) {system(paste0('rm -r ',dirout,'/gcm'),intern=TRUE)}
#     if (file.exists(paste0(dirout,'/obs'))) {system(paste0('rm -r ',dirout,'/obs'),intern=TRUE)}
#     system(paste('7za a -mx1 -mmt=2 ', paste(file.path(serverData, dateDownl),'.zip',sep=''),' ',dirout),intern=TRUE)
#     system(paste0('rm -r ',dirout),intern=TRUE)
#     
#     return(paste0(file.path(downData,dateDownl),'.zip'))
#   }
  
}

# bc_processing<- function(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat,sepFile,leep,typeData){
#   
#   library(raster); library(ncdf); library(rgdal); library(lubridate); library(qmap); library(ggplot2);library(tools); library(reshape);require(grid) 
#   
#   dircdo <- "cdo"
#   dateDownl= paste0("bc_",lon,"_",lat)
#   dirout <- paste0(dirWork,'/',dateDownl) 
#   dataset <- tolower(dataset)
#   bc_statsInd("varmod", "historical",Obyi,Obyf, lon, lat, dirout,stat)
#   bc_statsInd("varmod", 'rcp85', fuyi,fuyf, lon, lat, dirout,stat)    
#   
# }


############################################# Wrapper ##############################################
#===== SOLO MODIFICAR ESTOS PARAMETROS Y CORRER HASTA LA LINEA FINAL: =====

serverData= "/mnt/data_cluster_4/portals/ccafs_climate/download_data/files/data/bc_platform" # "S:/portals/ccafs_climate/download_data/files/data/bc_platform" #  si se corre local no es necesario modificar esta variable
downData="http://gisweb.ciat.cgiar.org/ccafs_climate/files/data/bc_platform" # si se corre local no es necesario modificar esta variable
dirWork=   "/home/temp" #"C:/Temp/bc" #"/home/jtarapues/request/request_oriana" #  directorio de salida 
dirgcm <-   "/mnt/data_cluster_2/gcm/cmip5/raw/daily" # "T:/gcm/cmip5/raw/daily" # 
dirobs <-     "/mnt/data_cluster_5/cropdata/" # "U:/cropdata" #
dataset <- "agcfsr"  #"station" wfd, wfdei, agmerra, grasp, agcfsr, princeton
methBCList <- c('1','2','3','4','5')  # 1=SH,2=BC,3=DEL,4=CF,5=QM c('1')# 
varlist <- c('pr','tasmax','tas','tasmin') #c('pr') # 
Obyi <- 1980#1998 #
Obyf <- 2010#1985#2013#
fuyi <- 2020
fuyf <- 2050#2025 #2035 # 
rcpList <- c("rcp85") # rcp26, rcp45, rcp60, rcp85
lon <- -76.355035#-83.633333  # 
lat <- 3.502600#9.883333 # 
#gcmlist <-c("bcc_csm1_1","bnu_esm","cccma_canesm2","gfdl_esm2g","inm_cm4","ipsl_cm5a_lr","miroc_miroc5","mpi_esm_mr","ncc_noresm1_m")#c("cesm1_cam5") # "ALL" #  bcc_csm1_1_m
gcmlist <-c("bcc_csm1_1","bnu_esm")
statList<-c('1','2','3') # 1=files bc, 2=tables, 3=graphics c('1')# 
fileStat<- "http://172.22.52.48/bias_tmp/1_Turrialba_daily_raw.txt" # para cargar archivos desde la plataforma no funciona para local
sepFile<-"tab"# tab,puntocoma,space,Comma
leep<-1 # 1=rellena los leep year con el promedio del dia antes y despues, 2=quita los dias leep year, 3=conserva los datos con leeps NA
typeData<-1 #1=Remueve los NA si todos los modelos lo???s tienen en comun, 2=remueve todos los datos con NA, 3=conserva los datos con leeps NA # 2 no funciona para qmap

#=======================================
# dirbase="C:/Temp/bc/bc_2015-12-14_05_46_42" #"/home/temp/bc_2015-12-14_05_46_42" # 
# varmod="prec"
# lon=9.883333
# lat=-83.633333
# yi=1979
# yf=2005
#=======================================

checkALL=gcmlist[which(gcmlist=="ALL")]
if(length(checkALL)==1){
  gcmlist <-  list.dirs(paste0(dirgcm,"/", rcpList), recursive = FALSE, full.names = FALSE) 
}
if(dataset=="station"){
  if(sepFile=="space"){sepFile=" "} else if(sepFile=="tab"){sepFile="\t"}else if(sepFile=="puntocoma"){sepFile=";"}else if(sepFile=="comma"){sepFile=","}
  df = read.table(url(fileStat), header = TRUE,sep=sepFile)
  dateSta=strftime(as.Date(as.character(df$date[!is.na(df$date)]), "%Y%m%d"),"%Y-%m-%d")
  varlist=colnames(df)[!colnames(df) %in% colnames(df)[1]]
  Obyi <-as.numeric(format(as.Date(min(dateSta)),'%Y'))
  Obyf <-as.numeric(format(as.Date(max(dateSta)),'%Y'))
  bc_processing(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat,sepFile,leep,typeData)
  
}else{
  bc_processing(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat='',sepFile='',leep,typeData)
}

####################################################
# CHECK FILE STATION
################

check_file(fileStat,sepFile){
  library(lubridate)
  if(sepFile=="space"){sepFile=" "} else if(sepFile=="tab"){sepFile="\t"}else if(sepFile=="puntocoma"){sepFile=";"}else if(sepFile=="comma"){sepFile=","}
  df = read.table(url(fileStat), header = TRUE,sep=sepFile)
  dateSta=strftime(as.Date(as.character(df$date[!is.na(df$date)]), "%Y%m%d"),"%Y-%m-%d")  
  colnamesok=c("date","prec","tmin","tmax","tmean")
  namescol=colnames(df)
  checkCol=namescol[colnamesok %in% namescol]
  code=c()
  if(length(colnames(df))<2 || length(colnames(df))>5){
    code=1 #cat("There is an error in the number of columns")
  }else if(length(namescol)!=length(checkCol)){
    code=2 #cat("The name columns are not correct")
  }else if(any(is.na(dateSta))){
    code=3 #cat("The date format is not correct")
  }else if(dateSta[1]>as.Date("2000-01-01")){
    code=4 #cat("Must have data less of year: 2000")
  }else{
    code=paste0(year(dateSta[1]),'-',year(dateSta[length(dateSta)]))
  }
  
}
