#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#function to grab soil data
get_soils <- function(run_data, xy_main) {
  ### soil data
  soil_data <- list()
  soil_data$general <- data.frame(SITE=-99,COUNTRY="Generic",LAT=run_data$LAT,LON=run_data$LON,SCSFAM="Generic")
  soil_data$properties <- data.frame(SCOM="BN",SALB=0.13,SLU1=200,SLDR=xy_main$SLDR[which(xy_main$LOC == run_data$LOC)],SLRO=75,SLNF=1,SLPF=1,SMHB="SA012",SMPX="IB001",SMKE="IB001")
  soil_data$profile <- data.frame(SLB=c(4.5,9.1,16.6,28.9,49.3,82.9,138.3,229.6)) #depth
  soil_data$profile$SLMH <- -99
  soil_data$profile$SLLL <- as.numeric(xy_main[which(xy_main$LOC == run_data$LOC),paste("SLLL_",1:8,sep="")])
  soil_data$profile$SDUL <- as.numeric(xy_main[which(xy_main$LOC == run_data$LOC),paste("SDUL_",1:8,sep="")])
  soil_data$profile$SSAT <- as.numeric(xy_main[which(xy_main$LOC == run_data$LOC),paste("SSAT_",1:8,sep="")])
  soil_data$profile$SRGF <- 1.0
  soil_data$profile$SSKS <- as.numeric(xy_main[which(xy_main$LOC == run_data$LOC),paste("SSKS_",1:8,sep="")])
  soil_data$profile$SBDM <- as.numeric(xy_main[which(xy_main$LOC == run_data$LOC),paste("SBDM_",1:8,sep="")])
  soil_data$profile$SLOC <- as.numeric(xy_main[which(xy_main$LOC == run_data$LOC),paste("SLOC_",1:8,sep="")])
  soil_data$profile$SLCL <- -99; soil_data$profile$SLSI <- -99; soil_data$profile$SLCF <- -99 
  soil_data$profile$SLNI <- -99; soil_data$profile$SLHW <- -99; soil_data$profile$SLHB <- -99
  soil_data$profile$SCEC <- -99; soil_data$profile$SADC <- -99 
  
  #check that SDUL > SLLL
  corr_lay <- soil_data$profile$SLB[which(round(soil_data$profile$SDUL,3) <= round(soil_data$profile$SLLL,3))]
  if (length(corr_lay) > 0) {
    corr_fac <- mean((soil_data$profile$SDUL[-which(round(soil_data$profile$SDUL,3) <= round(soil_data$profile$SLLL,3))]-soil_data$profile$SLLL[-which(round(soil_data$profile$SDUL,3) <= round(soil_data$profile$SLLL,3))]),na.rm=T)
    soil_data$profile$SDUL[which(soil_data$profile$SLB %in% corr_lay)] <- soil_data$profile$SLLL[which(soil_data$profile$SLB %in% corr_lay)] + corr_fac
    soil_data$profile$SDUL[which(soil_data$profile$SLB %in% corr_lay)] <- soil_data$profile$SDUL[which(soil_data$profile$SLB %in% corr_lay)]
  }
  
  #check that SSAT > SDUL
  corr_lay <- soil_data$profile$SLB[which(round(soil_data$profile$SSAT,3) <= round(soil_data$profile$SDUL,3))]
  if (length(corr_lay) > 0) {
    corr_fac <- mean((soil_data$profile$SSAT[-which(round(soil_data$profile$SSAT,3) <= round(soil_data$profile$SDUL,3))]-soil_data$profile$SDUL[-which(round(soil_data$profile$SSAT,3) <= round(soil_data$profile$SDUL,3))]),na.rm=T)
    soil_data$profile$SSAT[which(soil_data$profile$SLB %in% corr_lay)] <- soil_data$profile$SDUL[which(soil_data$profile$SLB %in% corr_lay)] + corr_fac
    soil_data$profile$SSAT[which(soil_data$profile$SLB %in% corr_lay)] <- soil_data$profile$SSAT[which(soil_data$profile$SLB %in% corr_lay)]
  }
  
  #return object
  return(soil_data)
}
