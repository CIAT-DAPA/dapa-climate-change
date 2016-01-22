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
######################################################################################################################


## Extract Observations Time Series Function
obs_extraction <- function(dataset="wfd", varmod="rsds",yi=1971, yf=1981, lon=-73.5, lat=3.4, dirobs="U:/cropdata", dirout="C:/Temp/bc", dircdo="cdo"){

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
  if (lon < 0){
    lonmod <- lon + 360
  }else{lonmod <- lon }

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
        
        ## CDO command line to extract daily TS
        system(paste0(dircdo," -s -outputtab,date,value -selyear,",yi,"/",yf," -remapnn,lon=", lon, "_lat=", lat, " ", ncvar, " > ", dirtemp, "/", odat))
        
        ## Read and organize daily TS
        datobs <- read.table(odat,header=F,sep="")
        names(datobs) <- c("date","value")
#         datobs <- datobs[which(datobs$year %in% yi:yf),]
#         datobs$year <- NULL
        
        ## Convert units to mm/day, W/m2 and Celsius Degrees
        if (varmod == "prec"){
          datobs$value <- datobs$value * 86400
        } else if (varmod == "srad") {
          datobs$value <- datobs$value * 0.0864  # W m-2 to MJ m-2 day-1
        } else if(varmod =="tmax" || varmod =="tmin") {
          datobs$value <- datobs$value - 273.15
        }
        
        ## Write extraction output file
        datobs <- write.table(datobs,odat,sep=" ",row.names=F, quote=F)
        
        cat("Done! \n")
        
     }
   }else{cat(paste0("\n No existe archivo: ",varmod,"_daily_ts_", tolower(dataset), "_",varmod,'*' ))}
}

## Extract GCM Time Series Function
gcm_extraction <- function(var="pr",varmod="prec",rcp="historical",yi=1985, yf=1987, gcmlist=c("bcc_csm1_1", "bcc_csm1_1_m", "bnu_esm", "cccma_cancm4", "cccma_canesm2"),lon=-73.5, lat=3.4, dirgcm="T:/gcm/cmip5/raw/daily", dirout="C:/Temp/bc"){
  
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

    
    if (length(ncvar) > 0 && length(Hncvar) > 0){
      
      
      
      years <- sapply(strsplit(basename(ncvar), '[_]'), "[[", 6)  
      staYear <- sapply(strsplit(years, '[-]'), "[[", 1)
      endYear <- gsub(".nc","",substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 8))
      
      yearI<- strftime(as.Date(as.character(staYear), "%Y%m%d"),"%Y")
      yearF<- strftime(as.Date(as.character(endYear), "%Y%m%d"),"%Y")
      
      if(yi <= yearF){
        
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
          
          ## CDO command line to extract daily TS
          if (varmod=="hur"){
            system(paste0("cdo -s -outputtab,date,value -selyear,",yei,"/",yef," -remapnn,lon=", lonmod, "_lat=", lat, " -selname,", var, " -sellevel,85000 ",  ncvar[1], " > ", dirtemp, "/", odat))
          } else {
            system(paste0("cdo -s -outputtab,date,value -selyear,",yei,"/",yef," -remapnn,lon=", lonmod, "_lat=", lat, " -selname,", var, " ",  ncvar[1], " > ", dirtemp, "/", odat))
          }
          
          ## Read and organize daily TS
          datgcm <- read.table(odat, header=F, sep="")
          names(datgcm) <- c("date","value")
#           datgcm <- datgcm[which(datgcm$year %in% yi:yf),]
#           datgcm$year <- NULL
          
          ## Convert units to mm/day and celsius degrees
          if (varmod == "prec"){
            datgcm$value <- datgcm$value * 86400
          } else if (varmod == "srad") {
            datgcm$value <- datgcm$value * 0.0864  # W m-2 to MJ m-2 day-1
          } else if(varmod =="tmax" || varmod =="tmin") {
            datgcm$value <- datgcm$value - 273.15
          }
          
          ## Write extraction output file
          datgcm <- write.table(datgcm, odat,row.names=F, sep=" ", quote=F)
          
          cat("Done! \n")
          
        }
      }
    }else{cat(paste0("\n No existe archivo: ",rcp, "/", gcm, "/r1i1p1/",var, "_day* or historical"))}

  }
  
} 

## Merge OBS and GCM in a Single Matrix
merge_extraction <- function(varmod="swind", rcp="rcp45", yi=2015, yf=2050, gcmlist=c("bcc_csm1_1"), lon=9.883333, lat=-83.633333, dataset="station", dirbase="C:/Temp/bc/bc_2015-12-14_05_46_42",sepFile="\t"){
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/raw_merge")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}  
  
  ## Define extraction merged file (include OBS and GCM)
  odat <- paste0(dirtemp,"/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(odat)) {
    
    cat("\nMerging OBS and GCMs ",rcp ,varmod, " ... ")
    
    ## Define end and start year from TS period and 
    #yi <- substr(ts, 1, 4)
    #yf <- substr(ts, 6, 9)
    if(dataset!="station"){
      sepFile=" "
    }

    # Load extraction files file names of OBS and GCMs
    ogcm <- paste0("raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    oobs <- paste0("obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    
#     templist=paste0(dirbase, "/gcm/",gcmlist)
#     listGCM=c()
#     for(t in templist){
#       if(length(list.files(t,full.names = F,ignore.case=F))==2){
#         listGCM=c(listGCM,t)
#       }
#     }
    #if(file.exists(paste0(dirbase, "/gcm/",gcmlist, "/", ogcm))){
      
      lista<-paste0(gcmlist, "/", ogcm)
      pos<-which(file.exists(paste0(dirbase, "/gcm/",gcmlist,'/',ogcm)))
      lista<-lista[pos]
      gcmlist<-gcmlist[pos]
    
      gcmdat <- lapply(paste0(dirbase, "/gcm/",gcmlist,'/',ogcm), function(x){read.table(x,header=T,sep=" ")})
      

#       for(m in gcmlist){
#         cat(m)
#         yu=read.table(paste0(dirbase, "/gcm/",m,'/',ogcm),header=T,sep=" ")
#       }
      

      #     gcmdat2 <- data.frame(gcmdat)
      #     yi=strftime(as.Date(as.character(gcmdat2$date[1]), "%Y-%m-%d"),"%Y")
      #     yf=strftime(as.Date(as.character(gcmdat2$date[length(gcmdat2$date)]), "%Y-%m-%d"),"%Y") 
      
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

      temp=which(is.na(gcmmat[names(gcmmat)!="date" & names(gcmmat)!="obs"]))

      gcmmat2= gcmmat[-temp,]

      if(rcp=="historical"){
        #gcmmat=na.omit(gcmmat) # remueve los na
      }
      ## Write merged output file (include OBS and GCM)
      gcmmat <- write.table(gcmmat2, odat, sep=" ",row.names=F, quote=F)
      
      cat("done!\n")      
    #}else{cat("\n No existen archivos para hacer merge")}
    
  }
  
}

## Bias Correction Calculation exluding variability (SH)
sh_calcs <- function(varmod="tmax", rcp="historical", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc"){
  
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
    years <- year(as.Date(odat$date))
    dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
    dates <- cbind.data.frame("date"=dates)
    odat <- merge(dates, odat, by="date", all.x=T)  
    
    
    years <- year(as.Date(odat$date))
    yearsLeap <- min(years):max(years)
    remove_leap=as.character(paste0(yearsLeap[leap_year(yearsLeap)],'-02-29'))
    odatt <- odat[-which(odat$date %in% remove_leap),] 
    if(nrow(odatt)>0){odat <-odatt}
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)
    nyears <- max(years) - min(years) + 1
    ngcm <- length(odat)- 2    
    years <- year(as.Date(odat$date))
    
    
#     years <- year(as.Date(odat$date))
#     dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
#     dates <- cbind.data.frame("date"=dates)
#     odat <- merge(dates, odat, by="date", all.x=T)    
#     months <- month(as.Date(odat$date))
#     
#     nyears <- max(years) - min(years) +1
#     ngcm <- length(odat)- 2
# 
#     dmon<- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "month") ,"%Y-%m-%d")
#     nday=c()
#     for(mon in  dmon){
#       nday=c(nday,numberOfDays(as.Date(mon, "%Y-%m-%d")))
#     }
    
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
      
      ## Get GCM months and years, number of dates and years
      years_f <- year(as.Date(odat_f$date))
      yearsLeap_f <- min(years_f):max(years_f)
      remove_leap_f=paste0(yearsLeap_f[leap_year(yearsLeap_f)],'-02-29')
      odatt_f <- odat_f[-which(odat_f$date %in% remove_leap_f),]  
      if(nrow(odatt_f)>0){odat_f <-odatt_f}
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) + 1
      years_f <- year(as.Date(odat_f$date))      
      
#       months_f <- month(as.Date(odat_f$date))
#       years_f <- year(as.Date(odat_f$date))
#       nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
#       nyears_f <- max(years_f) - min(years_f) +1
      
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
        gcmmat <- cbind(odat[,1:2], bc_values)
        
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
        gcmmat <- cbind(odat_f[,1:2], bc_values)
        
      }
    }
    
    ## Write output matrix with SH values
    colnames(gcmmat) <- names(odat)
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
#     tablist <- list.files(dirbase, pattern="sh_ts_*",full.names = FALSE)
#     file.copy(tablist, dirtemp)
    
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
    dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
    dates <- cbind.data.frame("date"=dates)
    odat <- merge(dates, odat, by="date", all.x=T)  
    # Get OBS months and years, number of dates, years and GCMs
    
    years <- year(as.Date(odat$date))
    yearsLeap <- min(years):max(years)
    remove_leap=as.character(paste0(yearsLeap[leap_year(yearsLeap)],'-02-29'))
    odatt <- odat[-which(odat$date %in% remove_leap),] 
    if(nrow(odatt)>0){odat <-odatt}
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)
    nyears <- max(years) - min(years) + 1
    ngcm <- length(odat)- 2    
    years <- year(as.Date(odat$date))
    #nday <- aggregate(odat[,1], list(months, years), length)
    
    # Load merged file
    
#     dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
#     dates <- cbind.data.frame("date"=dates)
#     odat <- merge(dates, odat, by="date", all.x=T)    
#     months <- month(as.Date(odat$date))
#     
#     nyears <- max(years) - min(years) +1
#     ngcm <- length(odat)- 2
#        
#     dmon<- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "month") ,"%Y-%m-%d")
#     nday=c()
#     for(mon in  dmon){
#       nday=c(nday,numberOfDays(as.Date(mon, "%Y-%m-%d")))
#     }

#     nday=rep(c(31,28,31,30,31,30,31,31,30,31,30,31),times = nyears)
    
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
      years_f <- year(as.Date(odat_f$date))
      yearsLeap_f <- min(years_f):max(years_f)
      remove_leap_f=paste0(yearsLeap_f[leap_year(yearsLeap_f)],'-02-29')
      odatt_f <- odat_f[-which(odat_f$date %in% remove_leap_f),]  
      if(nrow(odatt_f)>0){odat_f <-odatt_f}
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) + 1
      years_f <- year(as.Date(odat_f$date))      
      
#       months_f <- month(as.Date(odat_f$date))
#       years_f <- year(as.Date(odat_f$date))
#       nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
#       nyears_f <- max(years_f) - min(years_f) +1
      
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
      gcmmat <- cbind(odat[,1:2], bc_values)
      
      
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
      gcmmat <- cbind(odat_f[,1:2], bc_values)
      
    }
    
    
    ## Write output matrix with SH values
    colnames(gcmmat) <- names(odat)
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat("done!")
    
  }
  
}

## Change Factor Calculation exluding variability (DEL)
del_calcs <- function(varmod="prec", rcp="rcp45", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc"){
  
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
    years <- year(as.Date(odat$date))
    dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
    dates <- cbind.data.frame("date"=dates)
    odat <- merge(dates, odat, by="date", all.x=T)  
    
    ## Load merged file (future)
    odat_f <- paste0("raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat_f <- read.table(odat_f, header=T, sep=" ")

    years <- year(as.Date(odat$date))
    yearsLeap <- min(years):max(years)
    remove_leap=as.character(paste0(yearsLeap[leap_year(yearsLeap)],'-02-29'))
    odatt <- odat[-which(odat$date %in% remove_leap),] 
    if(nrow(odatt)>0){odat <-odatt}
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)
    nyears <- max(years) - min(years) + 1
    ngcm <- length(odat)- 2
    years <- year(as.Date(odat$date))
    
    ## Get GCM months and years, number of dates, years and GCMs (future)
    years_f <- year(as.Date(odat_f$date))
    yearsLeap_f <- min(years_f):max(years_f)
    remove_leap_f=paste0(yearsLeap_f[leap_year(yearsLeap_f)],'-02-29')
    odatt_f <- odat_f[-which(odat_f$date %in% remove_leap_f),]
    if(nrow(odatt_f)>0){odat_f <-odatt_f}
    months_f <- month(as.Date(odat_f$date))
    years_f <- year(as.Date(odat_f$date))
    nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
    nyears_f <- max(years_f) - min(years_f) + 1
    years_f <- year(as.Date(odat_f$date))

    ## Get GCM months and years, number of dates and years (future)
#     months_f <- month(as.Date(odat_f$date))
#     years_f <- year(as.Date(odat_f$date))
#     nday_f <- aggregate(odat_f[,1], list(months_f, year(as.Date(odat_f$date))), length)

#     dmon_f<- format(seq(as.Date(paste0(min(years_f),"/1/1")), as.Date(paste0(max(years_f),"/12/31")), "month") ,"%Y-%m-%d")
#     nday_f=c()
#     for(mon in  dmon_f){
#       nday_f=c(nday_f,numberOfDays(as.Date(mon, "%Y-%m-%d")))
#     }

#     nyears_f <- max(years_f) - min(years_f) + 1
    
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
    gcmmat <- cbind(odat_f[,1:2], bc_values)
    colnames(gcmmat) <- names(odat_f)
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat("done!")
    
  }
  
}

## Change Factor approach with variability
cf_calcs <- function(varmod="tmax", rcp="rcp45", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc"){
  
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
    years <- year(as.Date(odat$date))
    dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
    dates <- cbind.data.frame("date"=dates)
    odat <- merge(dates, odat, by="date", all.x=T)  
    
    # Load merged file (future)
    odat_f <- paste0("raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat_f <- read.table(odat_f, header=T, sep=" ")
    
    ## Get GCM months and years, number of dates, years and GCMs (historical)
    
    
    yearsLeap <- min(years):max(years)
    remove_leap=as.character(paste0(yearsLeap[leap_year(yearsLeap)],'-02-29'))
    odatt <- odat[-which(odat$date %in% remove_leap),] 
    if(nrow(odatt)>0){odat <-odatt}
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)
    nyears <- max(years) - min(years) + 1
    ngcm <- length(odat)- 2
    years <- year(as.Date(odat$date))
    
    ## Get GCM months and years, number of dates, years and GCMs (future)
    years_f <- year(as.Date(odat_f$date))
    yearsLeap_f <- min(years_f):max(years_f)
    remove_leap_f=paste0(yearsLeap_f[leap_year(yearsLeap_f)],'-02-29')
    odatt_f <- odat_f[-which(odat_f$date %in% remove_leap_f),]  
    if(nrow(odatt_f)>0){odat_f <-odatt_f}
    months_f <- month(as.Date(odat_f$date))
    years_f <- year(as.Date(odat_f$date))
    nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
    nyears_f <- max(years_f) - min(years_f) + 1
    years_f <- year(as.Date(odat_f$date))
    
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
    gcmmat <- cbind(odat_f[,1:2], bc_values)
    colnames(gcmmat) <- names(odat_f)
    gcmmat <- write.table(gcmmat, bcdat, sep=" ",row.names=F, quote=F)
    
    cat("done!")
    
  }
  
}

## Quantile mapping approach
qm_calcs <- function(varmod="tmax", rcp="rcp45", lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc"){
  
  ## Load libraries
  #library(qmap); library(lubridate);
  
  # Set working directory
  setwd(dirbase)
  
  dirtemp <- paste0(dirbase, "/quantile_mapping")
  if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
  
  ## Define QM output file
  bcdat <- paste0(dirtemp,"/qm_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
  
  if (!file.exists(bcdat)) {
    
    cat("\nQuantile MApping Calcs: ", rcp, " ", varmod, " ... ")
    
    ## Load merged file (historical)
    odat <- paste0("raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
    odat <- read.table(odat, header=T, sep=" ")
    years <- year(as.Date(odat$date))
    dates <- format(seq(as.Date(paste0(min(years),"/1/1")), as.Date(paste0(max(years),"/12/31")), "days") ,"%Y-%m-%d")
    dates <- cbind.data.frame("date"=dates)
    odat <- merge(dates, odat, by="date", all.x=T) 
    
    ## Get GCM dates, months, days and number of GCMs (historical)
#     dates <- odat$date
#     months <- month(as.Date(odat$date)) 
#     days <- day(as.Date(odat$date)) 
    dates_nonleap <- seq(as.Date('2001-01-01'),as.Date('2001-12-31'),by=1)  # example non_leap year  
#     ngcm <- length(odat) - 2
    
    yearsLeap <- min(years):max(years)
    remove_leap=as.character(paste0(yearsLeap[leap_year(yearsLeap)],'-02-29'))
    odatt <- odat[-which(odat$date %in% remove_leap),] 
    if(nrow(odatt)>0){odat <-odatt}
    months <- month(as.Date(odat$date))
    nday <- aggregate(odat[,1], list(months, year(as.Date(odat$date))), length)
    nyears <- max(years) - min(years) + 1
    ngcm <- length(odat)- 2
    years <- year(as.Date(odat$date))
    days <- day(as.Date(odat$date)) 
    dates <- odat$date
    
    ## Array of dates (rows) and GCMs (columns)
    qm_hist <- array(NA, dim = c(length(dates), ngcm))
    
    if (rcp != "historical"){
      
      ## Load merged file (future)
      odat_f <- paste0("raw_merge/raw_ts_", rcp, "_",varmod,"_lon_",lon,"_lat_",lat,".tab")
      odat_f <- read.table(odat_f, header=T, sep=" ")
      
      ## Get GCM dates, months and days (future)
      years_f <- year(as.Date(odat_f$date))
      yearsLeap_f <- min(years_f):max(years_f)
      remove_leap_f=paste0(yearsLeap_f[leap_year(yearsLeap_f)],'-02-29')
      odatt_f <- odat_f[-which(odat_f$date %in% remove_leap_f),]  
      if(nrow(odatt_f)>0){odat_f <-odatt_f}
      months_f <- month(as.Date(odat_f$date))
      years_f <- year(as.Date(odat_f$date))
      nday_f <- aggregate(odat_f[,1], list(months_f, years_f), length)
      nyears_f <- max(years_f) - min(years_f) + 1
      years_f <- year(as.Date(odat_f$date))      
      dates_f <- odat_f$date
#       months_f <- month(as.Date(odat_f$date)) 
      days_f <- day(as.Date(odat_f$date))
      
      ## Array of dates (rows) and GCMs (columns) for future
      qm_fut <- array(NA, dim = c(length(dates_f), ngcm))
      
      ## Define QM output file (future)
      bcdat_f <- paste0(dirtemp,"/qm_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
      
    }
    
    
    #####  QM  Calcs  #####
    
    # Loop through days of year selecting 30-day moving window around each day (do not fit Feb. 29, NA for GCM's)
    for (k in 1:365) {
      
      # Need to find indices of all days "k" and then 15 days before and after by historical dates
      ind_k = which(months == month(dates_nonleap[k]) & days == mday(dates_nonleap[k]))
      
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
      for (i in 1:ngcm) {
        
        error.p = tryCatch( { #keep going if can't apply model (all zeros in obs)  
          
          if (varmod == "prec"){
            qm_hist_fit = fitQmap(obs = odat[ind_all, 2], mod = odat[ind_all, i+2], method="RQUANT", qstep=0.01, wet.day=T, na.rm=T)  
          } else {
            qm_hist_fit = fitQmap(obs = odat[ind_all, 2], mod = odat[ind_all, i+2], method="RQUANT", qstep=0.01, wet.day=F, na.rm=T)  
          }
          
          #  Apply model to GCM past & future (if necessary)
          qm_hist[ind_k, i] = doQmap(x = odat[ind_k, i+2], qm_hist_fit, type="linear")
          if (rcp != "historical"){
            qm_fut[ind_k_f, i] = doQmap(x = odat_f[ind_k_f, i+2], qm_hist_fit, type="linear")
          }
          
        }
        
        , error=function(e) e
        
        )
        
        if(inherits(error.p,'Error'))  next
        
      }
      
    }
    
    # Write output matrix with QM values (historical)
    qm_hist <- cbind(odat[,1:2], qm_hist)
    colnames(qm_hist) <- names(odat)
    qm_hist <- write.table(qm_hist, bcdat, sep=" ", row.names=F, quote=F)
    
    # Write output matrix with QM values for (future)
    if (rcp != "historical"){
      qm_fut <- cbind(odat_f[,1:2], qm_fut)
      colnames(qm_fut) <- names(odat)
      qm_fut <- write.table(qm_fut, bcdat_f, sep=" ", row.names=F, quote=F)
    }

    cat("done!")
  }
  
}

## Comparison methods (plots)
bc_stats <- function(varmod="prec", rcp="historical",yi=1998, yf=2013, lon=-73.5, lat=3.4, dirbase="D:/CIAT/Workspace/bc",stat){
  
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
    ylabel <- "Shortwave Sol. Radiation (W/m2)"; limit = c(0, 400);
  }else if(varmod == "hur"){
    ylabel <- "Relative Humidity (%)"; limit = c(0, 400);
  }else if(varmod == "swind"){
    ylabel <- "Wind speed (m/s)"; limit = c(0, 400);
  }else if(varmod == "tmean"){
    ylabel <- "Mean Temperature (C)"; limit = c(0, 400);
  }
  
  # Personalized colors 
  gray='gray50';blue="#122F6B";blue2="#1F78B4";blue3="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  # Long name months list
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  
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
      obs <- freq[which(freq$method == "Bias_Correction_variability"), ]$obs
      freq$obs <- NULL
      
      if (rcp == "historical"){ # Historical plot includes observations
        
        # Join observations at the end of std GCM values
        obs <- matrix(1, length(obs)[1], ngcm) * obs
        colnames(obs) <- gcmlist
        freq <- rbind(freq, cbind("method"=rep("OBS", dim(obs)[1]),freq[which(freq$method == "Bias_Correction_variability"),][2:3], obs))
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

## Main function
bc_processing<- function(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat,sepFile){

  ## Load libraries
  library(raster); library(ncdf); library(rgdal); library(lubridate); library(qmap); library(ggplot2);library(tools); library(reshape); 
  
  dircdo <- "cdo"
  dateDownl= paste0("bc_",gsub("\\.",'-',gsub(':','_',gsub(" ", "_", as.character(Sys.time()))))) # "bc_2015-12-14_05_46_42"  #  
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
    
    gcmlistSel=c()
    for (gcm in gcmlist){
      ncvar <- list.files(path=paste0(dirrcp, "/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
      Hncvar <- list.files(path=paste0(dirgcm, "/historical/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
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
    

      if(dataset=="station"){
        dirtemp <- paste0(dirout, "/obs/station")
        if (!file.exists(dirtemp)) {dir.create(dirtemp, recursive=T)}
        df = read.table(url(fileStat), header = TRUE,sep='\t')
        
        dateSta=strftime(as.Date(as.character(df$date[!is.na(df$date)] ), "%Y%m%d"),"%Y-%m-%d")
        #valueSta=df$Value[!is.na(df$Value)] 
        
#         selvar=colnames(df)[!colnames(df) %in% colnames(df)[1]]
#         for(colvar in selvar){
          station=data.frame(cbind(dateSta,df[,which(colnames(df)==varmod)]))   
          names(station)=c("date","value")
          if (!file.exists(paste0(dirtemp,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab"))){
            write.table(station,paste0(dirtemp,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab"), sep="\t",row.names=F,quote = FALSE)            
#           }
        }
  
      }else{
        ncvarlis <- paste0(dirobs,'/',dataset,"/daily/nc-files")
        ncvar <- list.files(ncvarlis, pattern=paste0(varmod,"_daily_ts_", tolower(dataset), "_*" ),full.names = T,ignore.case=F)
        ncvar <- ncvar[sapply(strsplit(basename(ncvar), '[_]'), "[[", 1)==varmod]        
        obs_extraction(dataset, varmod, Obyi,Obyf, lon, lat, dirobs, dirout,dircdo)  
      }      
      gcm_extraction(var,varmod, "historical", Obyi,Obyf, gcmlistSel, lon, lat, dirgcm, dirout)
      
      merge_extraction(varmod, "historical",Obyi,Obyf, gcmlistSel, lon, lat, dataset, dirout,sepFile)
      
      for(rcp in rcpList){
        gcm_extraction(var,varmod, rcp, fuyi,fuyf, gcmlistSel, lon, lat, dirgcm, dirout)
        merge_extraction(varmod, rcp,fuyi,fuyf, gcmlistSel, lon, lat, dataset, dirout,sepFile)
        
        odat <- paste0(dirout,"/raw_merge/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab")
        hdat <- paste0(dirout,"/raw_merge/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab")
        
        if(file.exists(odat) && file.exists(hdat) ){
          for(methBC in methBCList){
            if(methBC=='1'){
              sh_calcs(varmod, "historical", lon, lat, dirout)
              sh_calcs(varmod, rcp, lon, lat, dirout)  
            }else if(methBC=='2'){
              bc_calcs(varmod, "historical", lon, lat, dirout)   
              bc_calcs(varmod, rcp, lon, lat, dirout)          
            }else if(methBC=='3'){
              del_calcs(varmod, rcp, lon, lat, dirout)
            }else if(methBC=='4'){
              cf_calcs(varmod, rcp, lon, lat, dirout)
            }else{
              qm_calcs(varmod, rcp, lon, lat, dirout)
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


############################################# Wrapper ##############################################

serverData= "/mnt/data_cluster_4/portals/ccafs_climate/download_data/files/data/bc_platform" # "S:/portals/ccafs_climate/download_data/files/data/bc_platform" #
downData="http://gisweb.ciat.cgiar.org/ccafs_climate/files/data/bc_platform"
dirWork=  "/home/jtarapues/request/request_oriana" # "C:/Temp/bc" # "/home/temp" #
dirgcm <-  "/mnt/data_cluster_2/gcm/cmip5/raw/daily" # "T:/gcm/cmip5/raw/daily" #
dirobs <-   "/mnt/data_cluster_5/cropdata/"
dataset <- "station" # wfd, wfdi, agmerra, grasp, agcfsr, princenton
methBCList <-  c('1','2','3','4','5')  # 1=SH,2=BC,3=DEL,4=CF,5=QM
varlist <- c("pr","tasmax")
Obyi <- 1985
Obyf <- 1987
fuyi <- 2015
fuyf <- 2050
rcpList <- c("rcp45")
lon <- 9.883333 # 
lat <- -83.633333 # 
gcmlist <-  c("bcc_csm1_1_m") # ALL #
statList<- c('1','2','3') #
fileStat<- "http://172.22.52.48/bias_tmp/file_1452865604.txt"
sepFile<-"tab"


checkALL=gcmlist[which(gcmlist=="ALL")]
if(length(checkALL)==1){
  gcmlist <-  list.dirs(paste0(dirgcm,"/", rcpList), recursive = FALSE, full.names = FALSE) 
}
if(dataset=="station"){
  if(sepFile=="space"){sepFile=" "} else if(sepFile=="tab"){sepFile="\t"}else if(sepFile=="puntocoma"){sepFile=";"}else if(sepFile=="Comma"){sepFile=","}
  df = read.table(url(fileStat), header = TRUE,sep=sepFile)
  dateSta=strftime(as.Date(as.character(df$date[!is.na(df$date)]), "%Y%m%d"),"%Y-%m-%d")
  varlist=colnames(df)[!colnames(df) %in% colnames(df)[1]]
  Obyi <-as.numeric(format(as.Date(min(dateSta)),'%Y'))
  Obyf <-as.numeric(format(as.Date(max(dateSta)),'%Y'))
  bc_processing(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat,sepFile)
  
}else{
  bc_processing(serverData,downData,dirWork,dirgcm,dirobs,dataset,methBCList,varlist,Obyi,Obyf,fuyi,fuyf,rcpList,lon,lat,gcmlist,statList,fileStat='',sepFile='')
}


