#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster); library(rgdal)

bDir <- "F:/PhD-work/data-quality-study"
#bDir <- "~/PhD-work/data-quality-study"

#compDir <- paste(bDir,"/climate-comparison",sep="")
compDir <- "Z:/PhD-work/data-quality-study/climate-comparison"

oDir <- paste(compDir,"/EcoCrop-experiments",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#shuffled experiments
#read in the rasters and calculate the spatial average of each mont JUN to SEP

##################################################
### shuffled experiments#########################
##################################################
dataDir <- paste(bDir,"/EcoCrop-GNUT/shuffle-perturb/climate",sep="")
trList <- list.files(dataDir,pattern="_s_")

#loop the different trials
#for (trial in trList) {
doTrial <- function(trial) {
  cat("\nprocessing",trial,"\n")
  vn <- strsplit(trial,"_",fixed=T)[[1]][1] #"prec"
  sc <- strsplit(trial,"_",fixed=T)[[1]][3] #"all"
  
  outxDir <- paste(oDir,"/",vn,"_s_",sc,sep="")
  if (!file.exists(outxDir)) {dir.create(outxDir)}
  
  if (!file.exists(paste(outxDir,"/climate_",vn,"_s_",sc,".csv",sep=""))) {
    cdDir <- paste(dataDir,"/",vn,"_s_",sc,sep="")
    expList <- list.files(cdDir,pattern="s-")
    
    #prelim version with reduced number of re-sequenced sets
    expList <- sample(expList,size=100)
    
    #loop through experiments
    for (xp in expList) {
      cat("categorising experiment",xp,"which is",which(expList==xp),"\n")
      
      #xp <- expList[1]
      rs <- stack(paste(cdDir,"/",xp,"/",vn,"_",6:9,".asc",sep=""))
      out_all <- data.frame(EXP=xp,VN=vn,SC=sc,M6=mean(rs[[1]][],na.rm=T),
                            M7=mean(rs[[2]][],na.rm=T),
                            M8=mean(rs[[3]][],na.rm=T),
                            M9=mean(rs[[4]][],na.rm=T))
      
      if (xp == expList[1]) {
        xp_all <- out_all
      } else {
        xp_all <- rbind(out_all,xp_all)
      }
    }
    write.csv(xp_all,paste(outxDir,"/climate_",vn,"_s_",sc,".csv",sep=""),row.names=F,quote=F)
  }
}


##################################################
### perturbed experiments#########################
##################################################
dataDir <- paste(bDir,"/EcoCrop-GNUT/shuffle-perturb/climate",sep="")
trList <- list.files(dataDir,pattern="_p_")

oDir <- paste(compDir,"/EcoCrop-experiments",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#for (trial in trList) {
doTrial <- function(trial) {
  cat("\nprocessing",trial,"\n")
  vn <- strsplit(trial,"_",fixed=T)[[1]][1] #"prec"
  sc <- strsplit(trial,"_",fixed=T)[[1]][3] #"all"
  
  cdDir <- paste(dataDir,"/",trial,sep="")
  
  outxDir <- paste(oDir,"/",vn,"_p_",sc,sep="")
  if (!file.exists(outxDir)) {dir.create(outxDir)}
  
  if (!file.exists(paste(outxDir,"/climate_",vn,"_p_",sc,"_all.csv",sep=""))) {
    
    pvals <- c(seq(0,299,by=40),299)
    
    #loop through values of p
    for (p in pvals) {
      cat("categorising for value of p =",p,"\n")
      
      if (!file.exists(paste(outxDir,"/climate_",vn,"_p_",sc,"_",p,".csv",sep=""))) {
        #listing experiments
        expList <- list.files(cdDir,pattern=paste("p-",p,"_",sep=""))
        
        #loop through experiments
        for (xp in expList) {
          cat("categorising experiment",xp,"which is",which(expList==xp),"\n")
          
          #xp <- expList[1]
          #loop through months now
          #for (mth in 1:12) {
          rs <- stack(paste(cdDir,"/",xp,"/",vn,"_",6:9,".asc",sep=""))
          #val <- mean(rs[],na.rm=T)
          
          #out_row <- data.frame(MTH=mth,EXP=xp,VN=vn,SC=sc,VAL=val)
          out_all <- data.frame(EXP=xp,VN=vn,SC=sc,M6=mean(rs[[1]][],na.rm=T),
                                M7=mean(rs[[2]][],na.rm=T),
                                M8=mean(rs[[3]][],na.rm=T),
                                M9=mean(rs[[4]][],na.rm=T))
          
          #if (mth == 1) {
          #  out_all <- out_row
          #} else {
          #  out_all <- rbind(out_all,out_row)
          #}
          #}
          
          if (xp == expList[1]) {
            xp_all <- out_all
          } else {
            xp_all <- rbind(out_all,xp_all)
          }
        }
        write.csv(xp_all,paste(outxDir,"/climate_",vn,"_p_",sc,"_",p,".csv",sep=""),row.names=F,quote=F)
      } else {
        xp_all <- read.csv(paste(outxDir,"/climate_",vn,"_p_",sc,"_",p,".csv",sep=""))
      }
      
      if (p == 0) {
        out_ps <- xp_all
      } else {
        out_ps <- rbind(xp_all,out_ps)
      }
      
    }
    
    write.csv(out_ps,paste(outxDir,"/climate_",vn,"_p_",sc,"_all.csv",sep=""),row.names=F,quote=F)
  }
}

