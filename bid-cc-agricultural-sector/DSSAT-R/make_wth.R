##############################################################################
############################### Make Weather .WTH ############################
##############################################################################
## Load Functions Necessary
# source("/home/jeisonmesa/Proyectos/BID/DSSAT-R/main_functions.R")
## to test
# path <- "/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR"
# modelo <- "bcc_csm1_1"
# 
# load(paste0(path, "/", modelo, "/Futuro/", "Temperatura.Rdat"))
# load(paste0(path, "/", modelo, "/Futuro/", "Srad.Rdat"))
# load(paste0(path, "/", modelo, "/Futuro/", "Precipitation.RDat"))
# Tmax <- Tmax[[1]][1, ] 
# Tmin <- Tmin[[1]][1, ]
# Srad <- Srad[[1]][[1]]
# Prec <- Prec[[1]][[1]]
# lat <- -72
# long <- 4
# wfd <- "model"  ## switch between "wfd" and "model"

#  to WFD
# load(paste0(path, "/wfd/", "ValorWFD.RDat"))
# Tmax <- TempMax[[2]][1, ]    ## [[year]][pixel, ]
# Tmin <- TempMin[[2]][1, ]    ## [[year]][pixel, ]
# Srad <- Srad[[2]][1, ]       ## [[year]][pixel, ]  
# Prec <- Prec[[2] ][1, ]      ## [[year]][pixel, ]
# lat <- -72                   ## Latitude
# long <- 4                    ## Longitude 
# wfd <- "wfd"  ## switch between "wfd" and "model"

# Climate Data Set
# climate_data <- list()
# climate_data$year <- 71:99       ## Years where they will simulate yields
# climate_data$Srad <- Srad        ## [[year]][pixel, ]   
# climate_data$Tmax <- TempMax     ## [[year]][pixel, ]
# climate_data$Tmin <- TempMin     ## [[year]][pixel, ]
# climate_data$Prec <- Prec        ## [[year]][pixel, ]
# climate_data$lat <- -72         ## You can include a vector of latitude
# climate_data$long <- 4          ## You can include a vector of longitude
# climate_data$wfd <- "wfd"       ## Switch between "wfd" and "model"

## Proof
## One WTH
# WriteWTH(, climate_data$Srad[[2]][1, ], climate_data$Tmax[[2]][1, ], climate_data$Tmin[[2]][1, ], 
#          climate_data$Prec[[2]][1, ], climate_data$lat, climate_data$long, climate_data$wfd)
## all "WTH" for a pixel 
# pixel <- 1               ## Necessarily to first extract the coordinates matches
# sapply(1:length(climate_data$year), function(i) {
# WriteWTH(climate_data$year[i], climate_data$Srad[[i]][pixel, ], climate_data$Tmax[[i]][pixel, ], climate_data$Tmin[[i]][pixel, ], 
#            climate_data$Prec[[i]][pixel, ], climate_data$lat, climate_data$long, climate_data$wfd)
# 
# })
# i<- 1
# pixel <- 4
# 
# year <- climate_data$year[i]
# Prec <-  climate_data$Prec[[i]][[pixel]]
# Srad <- climate_data$Srad[[i]][[pixel]]
# Tmax <- climate_data$Tmax[[i]][[pixel]]
# Tmin <- climate_data$Tmin[[i]][[pixel]]
# lat <- climate_data$lat[pixel]
# long <- climate_data$long[pixel]
# wfd <- "modelo"
# 
# 
# length(Prec)
# length(Srad)
# 
# 
# leap_year (72)
# 
# for(i in 1:26){
#   WriteWTH (climate_data$year[i], climate_data$Srad[[pixel]][[i]], climate_data$Tmax[[pixel]][[i]], climate_data$Tmin[[pixel]][[i]], climate_data$Prec[[pixel]][[i]], climate_data$lat[pixel], climate_data$long[pixel], wfd)
#   
#   
# }
# 
# setwd("/home//jeisonmesa/Proyectos/BID//DSSAT-R/")
# 
# 


WriteWTH <- function(year, Srad, Tmax, Tmin, Prec, lat, long, wfd) {  
  
  years <- leap_year(year) 
  ## Defining climatic data used (WFD = historical Years (1971, 1999), Model = Climate change models)
 
  
  if(wfd == "wfd") {
    
    Prec <- as.vector(Prec)
	
#     Prec <- Prec*86400
    Srad <- as.vector(Srad)
#     Srad <- Srad/11.5740741 
    Tmax <- as.vector(Tmax)
#     Tmax <- Tmax-273.15
    Tmin <- as.vector(Tmin)
#     Tmin <- Tmin-273.15

    
  }
  
  if(wfd == "model") {
       
    Prec <- as.vector(as.matrix(Prec))
	Prec[which(is.na(Prec))] <- 0
    Srad <- as.vector(as.matrix(Srad))
    Tmax <- Tmax   
    Tmin <- Tmin
  if(length(Tmax) != length(Prec)){
    
    Tmax = c(Tmax[1:59],Tmax[59:365])
    Tmin = c(Tmin[1:59],Tmin[59:365])

  }
      
  
  
  }
  
  if(year == 68){
	Prec <- c(Prec, Prec[365])
	Srad <- c(Srad, Srad[365])
    Tmax <- c(Tmax, Tmax[365])  
    Tmin <- c(Tmin, Tmin[365])
  }
    
  ## pf <- file(paste("JBID",yrs2[1],".WTH",sep=""),open="a",encoding="latin1")
  sink(paste("JBID", years$yrs2[1], ".WTH", sep = ""), append = T)
  ##cat(paste("*WEATHER DATA :"),paste(coordenadas[1,1]),paste(coordenadas[1,2]))
  cat(paste("*WEATHER DATA :"), paste("BID"))
  cat("\n")
  cat("\n")
  cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
  cat("\n")
  cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", "JBID", lat, long, -99,-99, -99.0, 0, 0))
  cat("\n")
  cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
  cat("\n")
  cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f", years$yrs, Srad, Tmax, Tmin, Prec)), sep = "\n")
  sink()
  
}












