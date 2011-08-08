#Summaries

source("summariseComparisons.R")

bd <- "F:/PhD-work/climate-data-assessment/comparisons/results"
ds <- "wcl"
vb <- "dtr"

metricsSummary(bDir=bd, dataset=ds, variable=vb)
dataSummary(bDir=bd, dataset=ds, variable=vb)

#Copy the output files to the _summary folder

in.file <- paste(bd,"/",ds,"-gcm/summary/",vb,"-",ds,"-vs-gcm-allPlotData.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-allPlotData.csv",sep="")
file.copy(in.file,ot.file)

in.file <- paste(bd,"/",ds,"-gcm/summary/",vb,"-",ds,"-vs-summaryMetrics.csv",sep="")
ot.file <- paste(bd,"/_summaries/",vb,"-",ds,"-vs-gcm-allPlotData.csv",sep="")
file.copy(in.file,ot.file)
