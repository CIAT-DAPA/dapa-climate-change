#Load GGCMI fertilizer data
library('ncdf')
library('RColorBrewer')  #next option
library('lattice')
library('raster')

#static data
setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/10-GGCMI_fertilizers/")
crops = c('Maize','Potatoes','Rice','Soybeans','Wheat')
c=5  #set crop here

eval(parse(text=paste("fert = open.ncdf('agmip_",crops[c],"_apprate_fill_NPK_0.5.nc4')",sep=''))) #load entire netcdf file with 11 variables

print(fert)
summary(fert)
lat = get.var.ncdf(fert,"latitude")  #try extracting variables
lon = get.var.ncdf(fert,"longitude")
K2O.app = get.var.ncdf(fert,'K2Oapprate') 
N.app = get.var.ncdf(fert,'Napprate')  
P2O5.app = get.var.ncdf(fert,'P2O5apprate') 

#plot values
grid = expand.grid(lon=lon,lat=lat)
range(N.app,na.rm=T)
cutpts = seq(0,200,by=40)
levelplot(N.app~lon*lat,data=grid,at=cutpts,cuts = 10,pretty=T,col.regions=(brewer.pal(10,'RdBu')),xlim=c(-130,-20),ylim=c(-60,35))

#load as rasters to better extract data
eval(parse(text=paste("K2O.app2 = raster('agmip_",crops[c],"_apprate_fill_NPK_0.5.nc4',varname='K2Oapprate')",sep='')))
eval(parse(text=paste("N.app2 = raster('agmip_",crops[c],"_apprate_fill_NPK_0.5.nc4',varname='Napprate')",sep='')))
eval(parse(text=paste("P2O5.app2 = raster('agmip_",crops[c],"_apprate_fill_NPK_0.5.nc4',varname='P2O5apprate')",sep='')))

K2O.app3 = rasterToPoints(K2O.app2)
N.app3 = rasterToPoints(N.app2)
P2O5.app3 = rasterToPoints(P2O5.app2)

#match to cultivation points in Latin America
  eval(parse(text=paste("load('../08-Cells_toRun/",crops[c],".loc.cal.Rdat')",sep='')))
  eval(parse(text=paste("crop.loc.cal = ",crops[c],".loc.cal",sep='')))  #change to generic name
  
  ind.N = match(paste(round(crop.loc.cal$Longitude,2),round(crop.loc.cal$Latitude,2),sep=''),paste(round(N.app3[,'x'],2),round(N.app3[,'y'],2),sep=''))
  crop.loc.cal$N.app = N.app3[ind.N,3]
  
  ind.P = match(paste(crop.loc.cal$Longitude,crop.loc.cal$Latitude,sep=''),paste(P2O5.app3[,'x'],P2O5.app3[,'y'],sep=''))
  crop.loc.cal$P2O5.app = P2O5.app3[ind.P,3]
  
  ind.K = match(paste(crop.loc.cal$Longitude,crop.loc.cal$Latitude,sep=''),paste(K2O.app3[,'x'],K2O.app3[,'y'],sep=''))
  crop.loc.cal$K2O.app = K2O.app3[ind.K,3]
  
  eval(parse(text=paste(crops[c],".loc.cal = crop.loc.cal",sep='')))  #change back to crop-specific name
  eval(parse(text=paste("save(",crops[c],".loc.cal,file='../08-Cells_toRun/",crops[c],".loc.cal.Rdat')",sep='')))
  
