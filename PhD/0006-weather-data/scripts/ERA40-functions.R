#Julian Ramirez-Villegas
#CIAT / UoL / CCAFS
#April 2012


##############################################################
### wrapper to process a given year precip data
year_wrapper_prec <- function(year) {
  library(raster); library(rgdal); library(ncdf)
  setwd(wd)
  
  cat("\nProcessing year",year,"\n")
  oYrDir <- paste(oDir,"/",year,sep="")
  if (!file.exists(oYrDir)) {dir.create(oYrDir)}
  
  nd <- leap(year)
  if (year == 2002) {nd <- 243}
  for (day in 1:nd) {
    cat(day," ")
    if (!file.exists(paste(oYrDir,"/prec_",day,".asc",sep=""))) {
      posit <- findNCPos(year,day,as.numeric(substr(dates.table$START[i],1,4)))
      
      if (posit == 0) {
        posit_p <- findNCPos(year,day,as.numeric(substr(dates.table$START[i-1],1,4)))
        rs_00 <- raster(pfName,band=posit_p)
      } else {
        rs_00 <- raster(fName,band=posit)
      }
      rs_06 <- raster(fName,band=(posit+1))
      rs_12 <- raster(fName,band=(posit+2))
      rs_18 <- raster(fName,band=(posit+3))
      
      rs <- rs_00 + rs_06 + rs_12 + rs_18
      rs <- rotate(rs)
      
      #meters to millimeters
      rs <- rs*1000
      
      rs <- writeRaster(rs,paste(oYrDir,"/prec_",day,".asc",sep=""),format='ascii')
      rm(rs); g=gc(); rm(g)
      
      #plot(rs,col=rainbow(100))
      #plot(wrld_simpl,add=T)
    }
  }
  cat("\n")
}



##############################################################
### wrapper to process a given year mean temperature data
year_wrapper_tasm <- function(year) {
  library(raster); library(rgdal); library(ncdf)
  setwd(wd)
  
  cat("\nProcessing year",year,"\n")
  oYrDir <- paste(oDir,"/",year,sep="")
  if (!file.exists(oYrDir)) {dir.create(oYrDir)}
  
  nd <- leap(year)
  if (year == 2002) {nd <- 243}
  for (day in 1:nd) {
    cat(day," ")
    if (!file.exists(paste(oYrDir,"/tasm_",day,".asc",sep=""))) {
      posit <- findNCPos(year,day,as.numeric(substr(dates.table$START[i],1,4)))
      
      if (posit == 0) {
        posit_p <- findNCPos(year,day,as.numeric(substr(dates.table$START[i-1],1,4)))
        rs_00 <- raster(pfName,band=posit_p)
      } else {
        rs_00 <- raster(fName,band=posit)
      }
      rs_06 <- raster(fName,band=(posit+1))
      rs_12 <- raster(fName,band=(posit+2))
      rs_18 <- raster(fName,band=(posit+3))
      
      rs <- (rs_00 + rs_06 + rs_12 + rs_18)/4
      rs <- rotate(rs)
      
      #Tc = Tk - 272.15
      rs <- rs - 273.15
      
      rs <- writeRaster(rs,paste(oYrDir,"/tasm_",day,".asc",sep=""),format='ascii')
      rm(rs); g=gc(); rm(g)
      
      #plot(rs,col=rainbow(100))
      #plot(wrld_simpl,add=T)
    }
  }
  cat("\n")
}


##############################################################
### wrapper to process a given year maximum temperature data
year_wrapper_dtr <- function(year) {
  library(raster); library(rgdal); library(ncdf)
  setwd(wd)
  
  cat("\nProcessing year",year,"\n")
  oYrDir <- paste(oDir,"/",year,sep="")
  if (!file.exists(oYrDir)) {dir.create(oYrDir)}
  
  nd <- leap(year)
  if (year == 2002) {nd <- 243}
  for (day in 1:nd) {
    cat(day," ")
    if (!file.exists(paste(oYrDir,"/dtr_",day,".asc",sep=""))) {
      posit <- findNCPos(year,day,as.numeric(substr(dates.table$START[i],1,4)))
      
      if (posit == 0) {
        posit_p <- findNCPos(year,day,as.numeric(substr(dates.table$START[i-1],1,4)))
        rs_00 <- raster(pfName,band=posit_p)
      } else {
        rs_00 <- raster(fName,band=posit)
      }
      rs_06 <- raster(fName,band=(posit+1))
      rs_12 <- raster(fName,band=(posit+2))
      rs_18 <- raster(fName,band=(posit+3))
      
      #maximum
      rsa <- calc(stack(rs_00,rs_06,rs_12,rs_18),fun=function(x) {max(x,na.rm=T)})
      rsa <- rsa - 273.15
      
      #minimum
      rsb <- calc(stack(rs_00,rs_06,rs_12,rs_18),fun=function(x) {min(x,na.rm=T)})
      rsb <- rsb - 273.15
      
      #range
      rs <- rsa-rsb
      rs <- rotate(rs)
      
      rs <- writeRaster(rs,paste(oYrDir,"/dtr_",day,".asc",sep=""),format='ascii')
      rm(rs); g=gc(); rm(g)
      
      #plot(rs,col=rainbow(100))
      #plot(wrld_simpl,add=T)
    }
  }
  cat("\n")
}


##############################################################
### wrapper to process a given year solar radiation data
year_wrapper_srad <- function(year) {
  library(raster); library(rgdal); library(ncdf)
  setwd(wd)
  
  cat("\nProcessing year",year,"\n")
  oYrDir <- paste(oDir,"/",year,sep="")
  if (!file.exists(oYrDir)) {dir.create(oYrDir)}
  
  nd <- leap(year)
  if (year == 2002) {nd <- 243}
  for (day in 1:nd) {
    cat(day," ")
    if (!file.exists(paste(oYrDir,"/srad_",day,".asc",sep=""))) {
      posit <- findNCPos(year,day,as.numeric(substr(dates.table$START[i],1,4)))
      
      if (posit == 0) {
        posit_p <- findNCPos(year,day,as.numeric(substr(dates.table$START[i-1],1,4)))
        rs_00 <- raster(pfName,band=posit_p)
      } else {
        rs_00 <- raster(fName,band=posit)
      }
      rs_06 <- raster(fName,band=(posit+1))
      rs_12 <- raster(fName,band=(posit+2))
      rs_18 <- raster(fName,band=(posit+3))
      
      rs <- rs_00 + rs_06 + rs_12 + rs_18
      rs <- rotate(rs)
      
      #w/m2*s = J/m2 / 1000000 = MJ/m2/day
      rs <- rs/1000000
      
      rs <- writeRaster(rs,paste(oYrDir,"/srad_",day,".asc",sep=""),format='ascii')
      rm(rs); g=gc(); rm(g)
      
      #plot(rs,col=rainbow(100))
      #plot(wrld_simpl,add=T)
    }
  }
  cat("\n")
}



############################ function to find the position of a day in
############################ a netCDF file
findNCPos <- function(thisYear,thisDay,iniYear) {
  counter <- 0
  if (thisYear == iniYear & thisDay == 1) {
    counter <- 0
  } else {
    #loop years
    for (yr in iniYear:thisYear) {
      
      if (yr < thisYear) {
        nd <- leap(yr)
        for (day in 1:nd) {
          if (yr == iniYear & day == 1) {nt <- 3} else {nt <- 4}
          #look for time of day in that day
          for (tod in 1:nt) {
            counter <- counter+1
          }
        }
      } else {
        #i'm in this year, so just count until that day regardless of total ndays
        for (day in 1:thisDay) {
          
          if (day < thisDay) {
            #if we're on before the day we want, still need to count times of day
            if (yr == iniYear & day == 1) {nt <- 3} else {nt <- 4}
            #look for time of day in that day
            for (tod in 1:nt) {
              counter <- counter+1
            }
          } else {
            #we're on desired day, so we count only once (get to the 00)
            counter <- counter+1
          }
        }
      }
    }
  }
  return(counter)
}

