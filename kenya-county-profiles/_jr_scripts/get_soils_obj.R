#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#function to grab soil data
get_soils_obj <- function(soil_data, xy_loc, soilgen_file) {
  #soil_data <- xy_soil
  #xy_loc <- hh_mill_xy[1,]
  #soilgen_file <- paste(ifprisoil_dir,"/NE.RData",sep="")
  #soil depths
  depths <- c(25,100,225,450,800,1500)
  
  #load generic soils
  load(file=soilgen_file)
  
  #find closest generic profile
  pdist <- pointDistance(p1=c(xy_loc$lon,xy_loc$lat),
                         p2=cbind(x=soilprof_df$LONG,y=soilprof_df$LAT),lonlat=T)
  tprof_head <- soilprof_df[which(pdist == min(pdist)),]
  tprof_data <- soilprof_ls[[paste(tprof_head$PROF_ID[1])]]
  
  ### soil object
  #take SLDR, SLALB from soilgen_file (header)
  #take SRGF and SLNI from soilgen_file (data)
  #take LAT, LONG from xy_loc
  #SLU1 = 10.
  #SDEPTH = 200
  #SLRO = 75
  soil_obj <- list()
  soil_obj$general <- data.frame(SITE=-99,COUNTRY="Niger",LAT=xy_loc$lat,LON=xy_loc$lon,SCSFAM="Niger Generic Soil")
  soil_obj$properties <- data.frame(SCOM="-99",SALB=tprof_head$SALB[1],SLU1=10,SLDR=tprof_head$SLDR[1],SLRO=75,SLNF=1,SLPF=1,SMHB="-99",SMPX="-99",SMKE="-99")
  
  #take SLLL, SDUL, SSAT, SBDM, SLOC, SLCL, SLSI, SLCF, SLHW, SCEC from soil_data
  #calculate SSKS following Saxton and Rawls (2006)
  soil_obj$profile <- data.frame(SLB=c(5,15,30,60,100,200)) #depth
  soil_obj$profile$SLMH <- tprof_data$SLMH
  soil_obj$profile$SLLL <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("WWP.d.",depths,sep="")]) * 0.01
  soil_obj$profile$SASW <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("AWCh2.d.",depths,sep="")]) * 0.01
  soil_obj$profile$SDUL <- soil_obj$profile$SLLL + soil_obj$profile$SASW
  soil_obj$profile$SASW <- NULL
  soil_obj$profile$SSAT <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("tetaS.d.",depths,sep="")]) * 0.01
  soil_obj$profile$SRGF <- tprof_data$SRGF
  soil_obj$profile$BVAL <- (log(1500) - log(33)) / (log(soil_obj$profile$SDUL) - log(soil_obj$profile$SLLL))
  soil_obj$profile$LAMBDA <- 1 / soil_obj$profile$BVAL
  soil_obj$profile$SSKS <- 1930 * ((soil_obj$profile$SSAT - soil_obj$profile$SDUL) ^ (3-soil_obj$profile$LAMBDA)) * 0.1
  soil_obj$profile$BVAL <- soil_obj$profile$LAMBDA <- NULL
  soil_obj$profile$SBDM <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("BLD_T.d.",depths,sep="")]) * 1000 / 100^3 #kg m-3 to g cm-3
  soil_obj$profile$SLOC <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("ORCDRC_T.d.",depths,sep="")]) * 0.1
  soil_obj$profile$SLCL <- NA
  soil_obj$profile$SLSI <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("SLTPPT_T.d.",depths,sep="")])
  soil_obj$profile$SLCL <- 100 - soil_obj$profile$SLSI - as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("SNDPPT_T.d.",depths,sep="")])
  soil_obj$profile$SLCF <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("CRFVOL_T.d.",depths,sep="")])
  soil_obj$profile$SLNI <- tprof_data$SLNI
  soil_obj$profile$SLHW <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("PHIHOX_T.d.",depths,sep="")]) * 0.1
  soil_obj$profile$SLHB <- -99
  soil_obj$profile$SCEC <- as.numeric(soil_data[which(soil_data$hhid == xy_loc$hhid),paste("CEC_T.d.",depths,sep="")])
  soil_obj$profile$SADC <- -99
  
  #SRGF below rdepth is zero
  rdepth <- soil_data$rdepth[which(soil_data$hhid == xy_loc$hhid)]
  rdepth <- max(c(rdepth,45)); rdepth <- min(c(rdepth,100)) #cross check
  soil_obj$profile$SRGF[which(soil_obj$profile$SLB > rdepth)] <- 0
  
  #check that SSAT > SDUL
  corr_lay <- soil_obj$profile$SLB[which(round(soil_obj$profile$SSAT,3) <= round(soil_obj$profile$SDUL,3))]
  if (length(corr_lay) > 0) {
    corr_fac <- mean((soil_obj$profile$SSAT[-which(round(soil_obj$profile$SSAT,3) <= round(soil_obj$profile$SDUL,3))]-soil_obj$profile$SDUL[-which(round(soil_obj$profile$SSAT,3) <= round(soil_obj$profile$SDUL,3))]),na.rm=T)
    soil_obj$profile$SSAT[which(soil_obj$profile$SLB %in% corr_lay)] <- soil_obj$profile$SDUL[which(soil_obj$profile$SLB %in% corr_lay)] + corr_fac
    soil_obj$profile$SSAT[which(soil_obj$profile$SLB %in% corr_lay)] <- soil_obj$profile$SSAT[which(soil_obj$profile$SLB %in% corr_lay)]
  }
  
  #return object
  return(soil_obj)
}
