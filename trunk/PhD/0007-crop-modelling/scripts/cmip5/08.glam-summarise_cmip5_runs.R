#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

library(raster)

#source directories
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/cmip5/08.glam-summarise_cmip5_runs-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
maxiter <- 15 #to grab last optim values

#base and data directories
#bDir <- "W:/eejarv/PhD-work/crop-modelling"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
expList_his <- c("his_allin","his_norain","his_notemp","his_nosrad","his_bcrain")
expList_rcp <- c("rcp_allin","rcp_bcrain","rcp_del","rcp_sh")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")
sdList <- c(-7:7)

#load list of parameter sets
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#list of GCMs
gcmList <- list.files(paste(runsDir,"/exp-33_outputs",sep=""),pattern="_ENS_")

#list of variables of interest
varNames <- c("YGP","STG","DUR","TRADABS","TP_UP","T_TRANS","TP_TRANS","TOTPP",
              "TOTPP_HIT","TOTPP_WAT","LAI","HI","BMASS","YIELD")

i <- 11

#summarise everything in here
#maybe parallelise stuff
##########################################################################
######### BASELINE DATA
for (n in 1:length(expList_his)) {
  stat <- collate_his(cells,runsDir,gcm=gcmList[i],intype=expList_his[n],varNames,expSel)
}


##########################################################################
######### PROJECTION DATA
for (n in 1:length(expList_rcp)) {
  for (z in 1:length(CO2ExpList)) {
    stat <- collate_rcp(cells,runsDir,gcm=gcmList[i],intype=expList_rcp[n],co2=CO2ExpList[z],
                        varNames,expSel,sdList)
  }
}


##########################################################################
##########################################################################
##########################################################################
###
#do some testing / plotting / data analysis
###

# load(paste(runsDir,"/_outputs/HIS-his_bcrain-bcc_csm1_1_ENS_r1i1p1.RData",sep=""))
# his_data <- out_his_data; rm(out_his_data); g=gc(); rm(g)
# 
# load(paste(runsDir,"/_outputs/RCP-rcp_bcrain-CO2_p1-bcc_csm1_1_ENS_r1i1p1.RData",sep=""))
# rcp_c1 <- out_rcp_data; rm(out_rcp_data); g=gc(); rm(g)
# 
# load(paste(runsDir,"/_outputs/RCP-rcp_bcrain-CO2_p2-bcc_csm1_1_ENS_r1i1p1.RData",sep=""))
# rcp_c2 <- out_rcp_data; rm(out_rcp_data); g=gc(); rm(g)
# 
# load(paste(runsDir,"/_outputs/RCP-rcp_bcrain-CO2_p3-bcc_csm1_1_ENS_r1i1p1.RData",sep=""))
# rcp_c3 <- out_rcp_data; rm(out_rcp_data); g=gc(); rm(g)
# 
# load(paste(runsDir,"/_outputs/RCP-rcp_bcrain-CO2_p4-bcc_csm1_1_ENS_r1i1p1.RData",sep=""))
# rcp_c4 <- out_rcp_data; rm(out_rcp_data); g=gc(); rm(g)
# 
# #analyse rainfed
# #construct pdf for each GLAM member and for each CO2 param
# #1. baseline and future absolute yields
# #2. change in yields
# 
# #**use only those runs which did not run out of wth data #STG != 99
# #**use only rainfed runs
# #**average all planting dates
# #**only get yields
# 
# #get maximum of all data and make breaks
# maxval <- max(c(his_data[,,"RFD",,"YIELD"],rcp_c1[,,,"RFD",,"YIELD"]))
# maxval <- max(c(maxval,rcp_c2[,,,"RFD",,"YIELD"],rcp_c3[,,,"RFD",,"YIELD"]))
# maxval <- max(c(maxval,rcp_c4[,,,"RFD",,"YIELD"]))
# brks <- c(seq(0,maxval,by=50),maxval)
# 
# data_his <- list()
# data_rcp <- list()
# for (co2 in 1:4) {
#   #co2 <- 1
#   this_co2 <- get(paste("rcp_c",co2,sep=""))
#   
#   for (exp in expSel) {
#     #exp <- expSel[1]
#     exp <- paste(exp)
#     
#     cat("processing glam member",exp,"\n")
#     
#     his_vals <- as.numeric(his_data[,exp,"RFD",,"YIELD"])
#     his_stg <- as.numeric(his_data[,exp,"RFD",,"STG"])
#     his_vals <- his_vals[which(his_stg!=99)]
#     
#     #average planting dates in dataset
#     yrList <- dimnames(this_co2)$YEAR
#     rcp_vals <- c()
#     for (yr in yrList) {
#       #yr <- yrList[1]
#       yr <- paste(yr)
#       yrvec <- as.numeric(this_co2[,exp,,"RFD",yr,"YIELD"])
#       yrstg <- as.numeric(this_co2[,exp,,"RFD",yr,"STG"])
#       yrvec[which(yrstg==99)] <- NA #set NA anything that failed
#       
#       yrvals <- this_co2[,exp,,"RFD",yr,"YIELD"]
#       yrvals[] <- yrvec
#       
#       yrvals <- as.numeric(rowMeans(yrvals,na.rm=T))
#       rcp_vals <- c(rcp_vals,yrvals)
#     }
#     rcp_vals <- rcp_vals[which(!is.na(rcp_vals))]
#     
#     #make histograms
#     hg_his <- hist(his_vals,breaks=brks,plot=F)
#     hhis <- hg_his$counts/sum(hg_his$counts)*100
#     hhis <- data.frame(XVAL=hg_his$mids,VAL=hhis)
#     names(hhis)[2] <- paste("EXP.",exp,sep="")
#     
#     hg_rcp <- hist(rcp_vals,breaks=brks,plot=F)
#     hrcp <- hg_rcp$counts/sum(hg_rcp$counts)*100
#     hrcp <- data.frame(XVAL=hg_rcp$mids,VAL=hrcp)
#     names(hrcp)[2] <- paste("EXP.",exp,sep="")
#     
#     if (exp == expSel[1]) {
#       hhis_all <- hhis
#       hrcp_all <- hrcp
#     } else {
#       hhis_all <- merge(hhis_all,hhis,by="XVAL",sort=F)
#       hrcp_all <- merge(hrcp_all,hrcp,by="XVAL",sort=F)
#     }
#   }
#   
#   hhis_m <- apply(hhis_all[,paste("EXP.",expSel,sep="")],1,mean,na.rm=T)
#   hhis_sd <- apply(hhis_all[,paste("EXP.",expSel,sep="")],1,sd,na.rm=T)
#   hhis_all$MEAN <- hhis_m
#   hhis_all$SD <- hhis_sd
#   
#   hrcp_m <- apply(hrcp_all[,paste("EXP.",expSel,sep="")],1,mean,na.rm=T)
#   hrcp_sd <- apply(hrcp_all[,paste("EXP.",expSel,sep="")],1,sd,na.rm=T)
#   hrcp_all$MEAN <- hrcp_m
#   hrcp_all$SD <- hrcp_sd
#   
#   data_his[[paste("CO2_P",co2,sep="")]] <- hhis_all
#   data_rcp[[paste("CO2_P",co2,sep="")]] <- hrcp_all
# }
# 
# #plot
# #windows()
# par(mar=c(5,5,1,1))
# 
# plot(data_his$CO2_P1$XVAL,data_his$CO2_P1$MEAN,ty="l",main=NA,xlab="Yield (kg/ha)",ylab="pdf (%)",
#      xlim=c(0,2000),ylim=c(0,10))
# polup <- data_his$CO2_P1$MEAN+data_his$CO2_P1$SD
# poldw <- data_his$CO2_P1$MEAN-data_his$CO2_P1$SD
# poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
# polygon(x=c(data_his$CO2_P1$XVAL,rev(data_his$CO2_P1$XVAL)),y=c(polup,rev(poldw)),col="#FF000040",border=NA)
# lines(data_his$CO2_P1$XVAL,data_his$CO2_P1$MEAN,col="red")
# 
# #to set shading
# #rgb(red=0,green=255,blue=0,alpha=50,maxColorValue=255)
# 
# #co2 par 1
# polup <- data_rcp$CO2_P1$MEAN+data_rcp$CO2_P1$SD
# poldw <- data_rcp$CO2_P1$MEAN-data_rcp$CO2_P1$SD
# poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
# 
# polygon(x=c(data_rcp$CO2_P1$XVAL,rev(data_rcp$CO2_P1$XVAL)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
# lines(data_rcp$CO2_P1$XVAL,data_rcp$CO2_P1$MEAN,col="blue")
# 
# #co2 par 2
# polup <- data_rcp$CO2_P2$MEAN+data_rcp$CO2_P2$SD
# poldw <- data_rcp$CO2_P2$MEAN-data_rcp$CO2_P2$SD
# poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
# 
# polygon(x=c(data_rcp$CO2_P2$XVAL,rev(data_rcp$CO2_P2$XVAL)),y=c(polup,rev(poldw)),col="#00FF0032",border=NA)
# lines(data_rcp$CO2_P2$XVAL,data_rcp$CO2_P2$MEAN,col="green")
# 
# #co2 par 3
# polup <- data_rcp$CO2_P3$MEAN+data_rcp$CO2_P3$SD
# poldw <- data_rcp$CO2_P3$MEAN-data_rcp$CO2_P3$SD
# poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
# 
# polygon(x=c(data_rcp$CO2_P3$XVAL,rev(data_rcp$CO2_P3$XVAL)),y=c(polup,rev(poldw)),col="#FF323232",border=NA)
# lines(data_rcp$CO2_P3$XVAL,data_rcp$CO2_P3$MEAN,col="orange")
# 
# #co2 par 4
# polup <- data_rcp$CO2_P4$MEAN+data_rcp$CO2_P4$SD
# poldw <- data_rcp$CO2_P4$MEAN-data_rcp$CO2_P4$SD
# poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
# 
# polygon(x=c(data_rcp$CO2_P4$XVAL,rev(data_rcp$CO2_P4$XVAL)),y=c(polup,rev(poldw)),col="#32FA3232",border=NA)
# lines(data_rcp$CO2_P4$XVAL,data_rcp$CO2_P4$MEAN,col="purple")



