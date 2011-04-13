source("readGHCNData.R")

ghcndir <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/"

interpretGHCN(vn="prcp", outvn="rain", wd=ghcndir); baselineMean(outvn="rain", wd=ghcndir)
interpretGHCN(vn="prcp_adj", outvn="rain_adj", wd=ghcndir); baselineMean(outvn="rain_adj", wd=ghcndir)
interpretGHCN(vn="mean", outvn="tmean", wd=ghcndir); baselineMean(outvn="tmean", wd=ghcndir)
interpretGHCN(vn="mean_adj", outvn="tmean_adj", wd=ghcndir); baselineMean(outvn="tmean_adj", wd=ghcndir)
interpretGHCN(vn="max", outvn="tmax", wd=ghcndir); baselineMean(outvn="tmax", wd=ghcndir)
interpretGHCN(vn="max_adj", outvn="tmax_adj", wd=ghcndir); baselineMean(outvn="tmax_adj", wd=ghcndir)
interpretGHCN(vn="min", outvn="tmin", wd=ghcndir); baselineMean(outvn="tmin", wd=ghcndir)
interpretGHCN(vn="min_adj", outvn="tmin_adj", wd=ghcndir); baselineMean(outvn="tmin_adj", wd=ghcndir)

############################################################################
#Transform the NetCDF files into ascii files
source("processGCMNetCDF.R")

gd <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
pn <- processNCDF(gd, which=c(9:15)) #SoEE Core 1
pn <- processNCDF(gd, which=c(16:21)) #SoEE Core 2

gd <- "/mnt/GIS-HD716B/climate_change/IPCC_CMIP3/20C3M/original-data"
pn <- processNCDF(gd)


