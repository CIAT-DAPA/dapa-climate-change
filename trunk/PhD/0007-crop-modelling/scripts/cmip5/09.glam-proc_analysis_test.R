#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

library(raster)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/cmip5/09.glam-proc_analysis_test-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
maxiter <- 15 #to grab last optim values

#base and data directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
#expList_his <- c("his_allin","his_bcrain")
#expList_rcp <- c("rcp_allin","rcp_bcrain")
inList <- c("allin","bcrain")
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

###########
### details of 
i <- 11 #1 #11
gcm <- gcmList[i]
htyp <- paste("his_",inList[2],sep="")
rtyp <- paste("rcp_",inList[2],sep="")
cpar <- CO2ExpList[1]
###


load(paste(runsDir,"/_outputs/HIS-",htyp,"-",gcm,".RData",sep=""))
his_data <- out_his_data; rm(out_his_data); g=gc(); rm(g)

load(paste(runsDir,"/_outputs/RCP-",rtyp,"-",cpar,"-",gcm,".RData",sep=""))
rcp_data <- out_rcp_data; rm(out_rcp_data); g=gc(); rm(g)


### put baseline, future and changes in yields with stg, duration, and TOTPP
data_all <- list()
for (exp in expSel[1]) {
  #exp <- expSel[1]
  exp <- paste(exp)
  
  data_all[[paste("EXP.",exp,sep="")]] <- data.frame()
  
  cat("processing glam member",exp,"\n")
  
  for (yr in 1:28) {
    #yr <- 1
    odf <- data.frame(YEAR=yr,CELL=dimnames(his_data)$CELL,
                      HIS.YIELD=as.numeric(his_data[,exp,"RFD",yr,"YIELD"]),
                      HIS.STG=as.numeric(his_data[,exp,"RFD",yr,"STG"]),
                      HIS.DUR=as.numeric(his_data[,exp,"RFD",yr,"DUR"]),
                      HIS.TRADABS=as.numeric(his_data[,exp,"RFD",yr,"TRADABS"]),
                      HIS.T_TRANS=as.numeric(his_data[,exp,"RFD",yr,"T_TRANS"]),
                      HIS.TP_UP=as.numeric(his_data[,exp,"RFD",yr,"TP_UP"]),
                      HIS.TOTPP_HIT=as.numeric(his_data[,exp,"RFD",yr,"TOTPP_HIT"]),
                      HIS.TOTPP_WAT=as.numeric(his_data[,exp,"RFD",yr,"TOTPP_WAT"]))
    
    for (pd in 1:length(sdList)) {
      #pd <- 1
      
      fdf <- data.frame(CELL=dimnames(rcp_data)$CELL,
                        YIELD=as.numeric(rcp_data[,exp,pd,"RFD",yr,"YIELD"]),
                        STG=as.numeric(rcp_data[,exp,pd,"RFD",yr,"STG"]),
                        DUR=as.numeric(rcp_data[,exp,pd,"RFD",yr,"DUR"]),
                        TRADABS=as.numeric(rcp_data[,exp,pd,"RFD",yr,"TRADABS"]),
                        T_TRANS=as.numeric(rcp_data[,exp,pd,"RFD",yr,"T_TRANS"]),
                        TP_UP=as.numeric(rcp_data[,exp,pd,"RFD",yr,"TP_UP"]),
                        TOTPP_HIT=as.numeric(rcp_data[,exp,pd,"RFD",yr,"TOTPP_HIT"]),
                        TOTPP_WAT=as.numeric(rcp_data[,exp,pd,"RFD",yr,"TOTPP_WAT"]))
      names(fdf) <- c("CELL",paste("PRJ.",c("YIELD","STG","DUR","TRADABS","T_TRANS","TP_UP","TOTPP_HIT","TOTPP_WAT"),".",pd,sep=""))
      odf <- merge(odf,fdf,by="CELL",sort=F)
    }
    
    data_all[[paste("EXP.",exp,sep="")]] <- rbind(data_all[[paste("EXP.",exp,sep="")]],odf)
  }
}


#########################################################
#########################################################
#check on the processes that are causing yield loss/increase in future scenarios
#output directory
outDir <- paste(runsDir,"/_outputs/processes/",gcm,"_",gsub("rcp_","",rtyp),"_",cpar,sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

########## matrix of processes
#T=future is better/ok/not affected
#F=future is worse/lower/affected
allproc <- expand.grid(DUR=c(NA,T,F),TDS=c(NA,T,F),HIT=c(NA,T,F),
                       WAT=c(NA,T,F),PUP=c(NA,T,F),RAD=c(NA,T,F))

#results
allres <- allproc

#loop the planting dates
for (i in 1:15) {
  #i <- 1
  tmpdf <- data.frame(CELL=data_all$EXP.33$CELL,YEAR=data_all$EXP.33$YEAR)
  tmpdf$HIS.YLD <- data_all$EXP.33$HIS.YIELD
  tmpdf$HIS.STG <- data_all$EXP.33$HIS.STG
  tmpdf$HIS.DUR <- data_all$EXP.33$HIS.DUR
  tmpdf$HIS.RAD <- data_all$EXP.33$HIS.TRADABS
  tmpdf$HIS.TRA <- data_all$EXP.33$HIS.T_TRANS
  tmpdf$HIS.PUP <- data_all$EXP.33$HIS.TP_UP
  tmpdf$HIS.HIT <- data_all$EXP.33$HIS.TOTPP_HIT
  tmpdf$HIS.WAT <- data_all$EXP.33$HIS.TOTPP_WAT
  tmpdf$PRJ.YLD <- data_all$EXP.33[,paste("PRJ.YIELD.",i,sep="")]
  tmpdf$PRJ.STG <- data_all$EXP.33[,paste("PRJ.STG.",i,sep="")]
  tmpdf$PRJ.DUR <- data_all$EXP.33[,paste("PRJ.DUR.",i,sep="")]
  tmpdf$PRJ.RAD <- data_all$EXP.33[,paste("PRJ.TRADABS.",i,sep="")]
  tmpdf$PRJ.TRA <- data_all$EXP.33[,paste("PRJ.T_TRANS.",i,sep="")]
  tmpdf$PRJ.PUP <- data_all$EXP.33[,paste("PRJ.TP_UP.",i,sep="")]
  tmpdf$PRJ.HIT <- data_all$EXP.33[,paste("PRJ.TOTPP_HIT.",i,sep="")]
  tmpdf$PRJ.WAT <- data_all$EXP.33[,paste("PRJ.TOTPP_WAT.",i,sep="")]
  
  #use only those where STG != 99
  tmpdf <- tmpdf[which(tmpdf$HIS.STG != 99 & tmpdf$PRJ.STG != 99),]
  tmpdf$PRJ.YCH <- (tmpdf$PRJ.YLD-tmpdf$HIS.YLD)/(tmpdf$HIS.YLD)*100
  tmpdf <- tmpdf[which(!is.na(tmpdf$PRJ.YCH)),] #remove NA (where baseline yield was zero)
  tmpdf <- tmpdf[which(!is.infinite(tmpdf$PRJ.YCH)),] #remove NA (where baseline yield was zero)
  
  ##########################################################################
  #lower yield
  yloss <- tmpdf[which(tmpdf$PRJ.YCH < 0),]
  this_res <- apply(allproc,1,FUN=filt_fun,yloss,nrow(tmpdf))
  allres$LOSS.MEAN <- this_res[1,]
  allres$LOSS.SD <- this_res[2,]
  allres$LOSS.COUNT <- this_res[3,]
  allres$LOSS.PER <- this_res[4,]
  allres$LOSS.PERLOSS <- this_res[5,]
  
  ##########################################################################
  #higher yield
  ygain <- tmpdf[which(tmpdf$PRJ.YCH > 0),]
  this_res <- apply(allproc,1,FUN=filt_fun,ygain,nrow(tmpdf))
  allres$GAIN.MEAN <- this_res[1,]
  allres$GAIN.SD <- this_res[2,]
  allres$GAIN.COUNT <- this_res[3,]
  allres$GAIN.PER <- this_res[4,]
  allres$GAIN.PERGAIN <- this_res[5,]
  
  write.csv(allres,paste(outDir,"/proc_exp-33_sowdate_",i,".csv",sep=""),quote=T,row.names=F)
}


#read preliminary thing
# explots <- read.table(paste(runsDir,"/_outputs/processes/explots.tab",sep=""),sep="\t",header=T)
# posit <- explots[which(explots$Dominant == "Positive"),]
# posit$Detail <- factor(posit$Detail)
# 
# negat <- explots[which(explots$Dominant == "Negative"),]
# negat$Detail <- factor(negat$Detail)
# 
# windows()
# par(mar=c(12,5,1,1),las=2)
# boxplot(posit$PERTOT.1~posit$Detail,at=seq(1,14,by=2),col="white",
#         ylim=c(0,60),xlim=c(0.5,14.5),ylab="Percent of simulations (%)")
# boxplot(posit$PERTOT~posit$Detail,at=seq(2,14,by=2),col="grey 80",add=T,names=rep(NA,7))
# grid()
# 
# 
# par(mar=c(12,5,1,1),las=2)
# boxplot(negat$PERTOT.1~negat$Detail,at=seq(1,14,by=2),col="white",
#         ylim=c(0,60),xlim=c(0.5,14.5),ylab="Percent of simulations (%)")
# boxplot(negat$PERTOT~negat$Detail,at=seq(2,14,by=2),col="grey 80",add=T,names=rep(NA,7))
# grid()










