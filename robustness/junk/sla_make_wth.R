#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#open text file and create weather files for all years

#directory
wd <- "~/Leeds-work/quest-for-robustness"
srcDir <- "~/Repositories/dapa-climate-change/trunk/robustness"
slaDir <- paste(wd,"/SLA_analysis",sep="")

#source needed functions
source(paste(srcDir,"/glam-utils/make_wth.R",sep=""))

#read in weather for GLAM runs
wthdat <- read.table(paste(slaDir,"/wth_modifications.tab",sep=""),sep="\t",header=T)
s_details <- list(); s_details$INSI <- "AFRB"; s_details$LAT <- -9.562
s_details$LONG <- 35.438; s_details$ELEV <- -99; s_details$TAV <- -99
s_details$AMP <- -99; s_details$REFHT <- -99; s_details$WNDHT <- -99

for (yr in 1950:1970) {
  #yr <- 1950
  cat("...year",yr,"\n")
  wthfile <- paste(slaDir,"/wth_data/afrb001001",yr,".wth",sep="")
  wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=wthdat$SRAD,TMAX=wthdat$TMAX,TMIN=wthdat$TMIN,
                   RAIN=wthdat[,paste("RAIN_",yr,sep="")])
  wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
  wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
  wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
  
  wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)
}


