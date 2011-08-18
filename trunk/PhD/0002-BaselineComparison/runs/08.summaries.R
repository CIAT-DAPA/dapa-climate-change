#Summaries

#Summarise all the comparisons for the 'climate normals' comparisons
source("summariseComparisons.R")

#WorldClim
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "wcl"
vb <- "dtr"
#sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
#sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
if (file.exists(ot.file)) {file.remove(ot.file)}
file.copy(in.file,ot.file)


#CRU
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "cru"
vb <- "dtr"
#sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
#sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
if (file.exists(ot.file)) {file.remove(ot.file)}
file.copy(in.file,ot.file)

#WCL-WS
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "wclst"
vb <- "dtr"
#sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
#sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
if (file.exists(ot.file)) {file.remove(ot.file)}
file.copy(in.file,ot.file)

#GHCN was not done because DTR data was unavailable for the regions.

#GSOD
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "gsod"
vb <- "dtr"
#sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
#sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
if (file.exists(ot.file)) {file.remove(ot.file)}
file.copy(in.file,ot.file)

######################################################
######################################################
#Generate boxplots

source("summariseComparisons.R")
f.dir <- "F:/PhD-work/climate-data-assessment/comparisons/results/_summaries"
generateBoxplots(fd=f.dir)
for (vn in c("prec","tmean","dtr")) {
  for (dset in c("wcl","cru","wclst","ghcn","gsod")) {
    for (prd in c("ANNUAL","DJF","JJA")) {
      if (file.exists(paste(f.dir,"/",vn,"-",dset,"-vs-gcm-summaryMetrics.csv",sep=""))) {
        createColoured(fDir=f.dir, variable=vn, dataset=dset, month="total", period=prd, metric="R2.FORCED")
      }
    }
  }
}




