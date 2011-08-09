#Summaries

source("summariseComparisons.R")

#WorldClim
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "wcl"
vb <- "dtr"
sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
file.copy(in.file,ot.file)


#CRU
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "cru"
vb <- "dtr"
sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
file.copy(in.file,ot.file)

#WCL-WS
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "wclst"
vb <- "dtr"
sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
file.copy(in.file,ot.file)

#GHCN
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "ghcn"
vb <- "dtr"
sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
file.copy(in.file,ot.file)

#GSOD
bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "gsod"
vb <- "dtr"
sdt <- metricsSummary(bDir=bd, dataset=ds, variable=vb)
sdt <- dataSummary(bDir=bd, dataset=ds, variable=vb)
#Copy the output file sumaryMetrics to the _summary folder
in.file <- paste(bd,"/",ds,"-vs-gcm/summary/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-summaryMetrics.csv",sep="")
file.copy(in.file,ot.file)
