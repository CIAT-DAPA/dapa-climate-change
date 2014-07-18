#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

##########################################################################################
########### write the soil file (SOIL.SOL), with a generic soil code to be used for a
########### given location. 
##########################################################################################

### for testing
#load the soil data from initial conditions
in_data <- list()
in_data$general <- data.frame(SITE=-99,COUNTRY="Generic",LAT=-9.562,LON=35.438,SCSFAM="Generic")
in_data$properties <- data.frame(SCOM="BN",SALB=0.13,SLU1=200,SLDR=0.4,SLRO=75,SLNF=1,SLPF=1,SMHB="IB001",SMPX="IB001",SMKE="IB001")
in_data$profile <- data.frame(SLB=c(4.5,9.1,16.6,49.3,82.9,138.3,229.6))
in_data$profile$SLMH <- -99
in_data$profile$SLLL <- c(0.113,0.234,0.221,0.216,0.216,0.216,0.216)
in_data$profile$SDUL <- c(0.246,0.310,0.324,0.316,0.316,0.316,0.316)
in_data$profile$SSAT <- c(0.392,0.391,0.426,0.419,0.419,0.419,0.419)
in_data$profile$SRGF <- 1.0
in_data$profile$SSKS <- c(1.44,0.34,0.39,0.43,0.43,0.43,0.43)
in_data$profile$SBDM <- c(1.54,1.55,1.48,1.50,1.50,1.50,1.50)
in_data$profile$SLOC <- c(0.60,0.38,0.29,0.23,0.23,0.23,0.23)
in_data$profile$SLCL <- -99; in_data$profile$SLSI <- -99; in_data$profile$SLCF <- -99 
in_data$profile$SLNI <- -99; in_data$profile$SLHW <- -99; in_data$profile$SLHB <- -99
in_data$profile$SCEC <- -99; in_data$profile$SADC <- -99 

#main function
make_soilfile <- function(in_data, outfile) {
  
  
}
