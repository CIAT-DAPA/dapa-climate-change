#Mapping the metrics for the different datasets
#############################################################################
#############################################################################
#############################################################################

##########
#GHCN
##########
repoDir <- "D:/_tools"
src.dir <- paste(repoDir,"/dapa-climate-change/trunk/PhD/0002-BaselineComparison",sep=""); setwd(src.dir)
source("mapGHCNMetrics-TS.R")

work.dir <- "F:/PhD-work/climate-data-assessment/comparisons"
GCMDir <- paste(work.dir, "/input-data/gcm-data/20C3M/1961_1990", sep="")

#Defining GCM to work with
GCMList <- list.files(GCMDir)
for (model in GCMList) {
  for (prd in c("TTL","JJA","DJF")) {
    for (vr in c("rain","tmean")) {
      cat("Mashing up GCM",model,prd,vr,"\n")
      outRs <- mapMetric(wd=work.dir,dataset="ghcn",gcm=model,variable=vr,period=prd,metric="R2.FORCED")
      outRs <- mapMetric(wd=work.dir,dataset="ghcn",gcm=model,variable=vr,period=prd,metric="SLOPE.FORCED")
      outRs <- mapMetric(wd=work.dir,dataset="ghcn",gcm=model,variable=vr,period=prd,metric="ERROR")
    }
  }
}

##########
#GSOD
##########
repoDir <- "D:/_tools"
src.dir <- paste(repoDir,"/dapa-climate-change/trunk/PhD/0002-BaselineComparison",sep=""); setwd(src.dir)
source("mapGSODMetrics-TS.R")

work.dir <- "F:/PhD-work/climate-data-assessment/comparisons"
GCMDir <- paste(work.dir, "/input-data/gcm-data/20C3M/1961_1990", sep="")

#Defining GCM to work with
GCMList <- list.files(GCMDir)
for (model in GCMList) {
  for (prd in c("TTL","JJA","DJF")) {
    for (vr in c("rain","tmean")) {
      cat("Mashing up GCM",model,prd,vr,"\n")
      outRs <- mapMetric(wd=work.dir,dataset="gsod",gcm=model,variable=vr,period=prd,metric="R2.FORCED")
      outRs <- mapMetric(wd=work.dir,dataset="gsod",gcm=model,variable=vr,period=prd,metric="SLOPE.FORCED")
      outRs <- mapMetric(wd=work.dir,dataset="gsod",gcm=model,variable=vr,period=prd,metric="ERROR")
    }
  }
}

##########
#GHCN and GSOD but using the fitted cell mean this time
##########
repoDir <- "D:/_tools"
src.dir <- paste(repoDir,"/dapa-climate-change/trunk/PhD/0002-BaselineComparison",sep=""); setwd(src.dir)
source("mapMeanMetrics-TS.R")

mDataDir <- "F:/PhD-work"
work.dir <- paste(mDataDir,"/climate-data-assessment/comparisons", sep="")

gcmDir <- paste(work.dir, "/input-data/gcm-data/20C3M/1961_1990", sep="")
gcmList <- list.files(gcmDir)

#GHCN
for (model in gcmList) {
  for (prd in c("TTL","JJA","DJF")) {
    for (vr in c("rain","tmean")) {
      cat("Mashing up GCM",model,prd,vr,"\n")
      outRs <- mapMetricMean(wd=work.dir,dataset="ghcn",gcm=model,variable=vr,period=prd,metric="R2.FORCED")
      outRs <- mapMetricMean(wd=work.dir,dataset="ghcn",gcm=model,variable=vr,period=prd,metric="SLOPE.FORCED")
      outRs <- mapMetricMean(wd=work.dir,dataset="ghcn",gcm=model,variable=vr,period=prd,metric="ERROR")
    }
  }
}

#######################################################
repoDir <- "D:/_tools"
src.dir <- paste(repoDir,"/dapa-climate-change/trunk/PhD/0002-BaselineComparison",sep=""); setwd(src.dir)
source("mapMeanMetrics-TS.R")

mDataDir <- "F:/PhD-work"
work.dir <- paste(mDataDir,"/climate-data-assessment/comparisons", sep="")

gcmDir <- paste(work.dir, "/input-data/gcm-data/20C3M/1961_1990", sep="")
gcmList <- list.files(gcmDir)


#GSOD
for (model in gcmList) {
  for (prd in c("TTL","JJA","DJF")) {
    for (vr in c("rain","tmean")) {
      cat("Mashing up GCM",model,prd,vr,"\n")
      outRs <- mapMetricMean(wd=work.dir,dataset="gsod",gcm=model,variable=vr,period=prd,metric="R2.FORCED")
      outRs <- mapMetricMean(wd=work.dir,dataset="gsod",gcm=model,variable=vr,period=prd,metric="SLOPE.FORCED")
      outRs <- mapMetricMean(wd=work.dir,dataset="gsod",gcm=model,variable=vr,period=prd,metric="ERROR")
    }
  }
}

