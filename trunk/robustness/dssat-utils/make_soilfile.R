#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

##########################################################################################
########### write the soil file (SOIL.SOL), with a generic soil code to be used for a
########### given location. 
##########################################################################################

### for testing
#load the soil data from initial conditions
#in_data <- list()
#in_data$general <- data.frame(SITE=-99,COUNTRY="Generic",LAT=-9.562,LON=35.438,SCSFAM="Generic")
#in_data$properties <- data.frame(SCOM="BN",SALB=0.13,SLU1=200,SLDR=0.4,SLRO=75,SLNF=1,SLPF=1,SMHB="IB001",SMPX="IB001",SMKE="IB001")
#in_data$profile <- data.frame(SLB=c(4.5,9.1,16.6,49.3,82.9,138.3,229.6))
#in_data$profile$SLMH <- -99
#in_data$profile$SLLL <- c(0.113,0.234,0.221,0.216,0.216,0.216,0.216)
#in_data$profile$SDUL <- c(0.246,0.310,0.324,0.316,0.316,0.316,0.316)
#in_data$profile$SSAT <- c(0.392,0.391,0.426,0.419,0.419,0.419,0.419)
#in_data$profile$SRGF <- 1.0
#in_data$profile$SSKS <- c(1.44,0.34,0.39,0.43,0.43,0.43,0.43)
#in_data$profile$SBDM <- c(1.54,1.55,1.48,1.50,1.50,1.50,1.50)
#in_data$profile$SLOC <- c(0.60,0.38,0.29,0.23,0.23,0.23,0.23)
#in_data$profile$SLCL <- -99; in_data$profile$SLSI <- -99; in_data$profile$SLCF <- -99 
#in_data$profile$SLNI <- -99; in_data$profile$SLHW <- -99; in_data$profile$SLHB <- -99
#in_data$profile$SCEC <- -99; in_data$profile$SADC <- -99 

#setwd("~/Leeds-work/quest-for-robustness/bin/dssat/csm45_1_23_bin_gfort")
#solfil <- make_soilfile(in_data, out_file="./SOIL.SOL")

#main function
make_soilfile <- function(in_data, out_file, overwrite=F) {
  #open file in write mode
  if (file.exists(out_file)) {
    if (overwrite) {
      pf <- file(out_file,open="w")
    } else {
      rnum <- round(runif(1,10000,20000),0)
      tmpvar <- unlist(strsplit(out_file,"/",fixed=T))
      pth_ref <- paste(tmpvar[1:(length(tmpvar)-1)],collapse="/")
      out_file <- paste(pth_ref,"/copy-",rnum,"_",tmpvar[length(tmpvar)],sep="")
      pf <- file(out_file,open="w")
    }
  } else {
    pf <- file(out_file,open="w")
  }
  
  #write header and stuff
  #pf <- file(out_file,open="w")
  cat("*SOILS: General DSSAT Soil Input File\n",file=pf)
  cat("\n",file=pf)
  cat("*IB00000001  WISE        SCL     140 GENERIC SOIL PROFILE\n",file=pf)
  cat("@SITE        COUNTRY          LAT     LONG SCS Family\n",file=pf)
  
  #general
  cat(paste(" ",sprintf("%1$-12s%2$-12s%3$8.3f%4$9.3f",
                    as.character(in_data$general$SITE),as.character(in_data$general$COUNTRY),
                    in_data$general$LAT, in_data$general$LON)," ",
            sprintf("%-12s",as.character(in_data$general$SCSFAM)),
            "\n",sep=""),file=pf)
  
  #properties
  cat("@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE\n",file=pf)
  cat(paste(sprintf("%1$6s%2$6.2f%3$6.1f%4$6.2f%5$6.2f%6$6.2f%7$6.2f%8$6s%9$6s%10$6s",
                    as.character(in_data$properties$SCOM),in_data$properties$SALB,
                    in_data$properties$SLU1, in_data$properties$SLDR, in_data$properties$SLRO,
                    in_data$properties$SLNF, in_data$properties$SLPF, in_data$properties$SMHB,
                    in_data$properties$SMPX, in_data$properties$SMKE),"\n",sep=""),file=pf)
  
  #profile
  cat("@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC\n",file=pf)
  for (i in 1:nrow(in_data$profile)) {
    cat(paste(sprintf("%1$6.1f%2$6s%3$6.3f%4$6.3f%5$6.3f%6$6.2f%7$6.2f%8$6.2f%9$6.2f%10$6.1f%11$6.1f",
                      in_data$profile$SLB[i],as.character(in_data$profile$SLMH[i]),
                      in_data$profile$SLLL[i], in_data$profile$SDUL[i], in_data$profile$SSAT[i],
                      in_data$profile$SRGF[i], in_data$profile$SSKS[i], in_data$profile$SBDM[i],
                      in_data$profile$SLOC[i], in_data$profile$SLCL[i], in_data$profile$SLSI[i]),
              sprintf("%1$6.1f%2$6.1f%3$6.1f%4$6.1f%5$6.1f%6$6.1f",
                      in_data$profile$SLCF[i], in_data$profile$SLNI[i], in_data$profile$SLHW[i],
                      in_data$profile$SLHB[i], in_data$profile$SCEC[i], in_data$profile$SADC[i]),
              "\n",sep=""),file=pf)
  }
  
  #close file
  close(pf)
  
  #output
  return(out_file)
}
