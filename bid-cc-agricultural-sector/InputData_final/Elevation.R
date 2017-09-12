#Load elevation data & map to matrices_cultivo
library(ncdf)
library(lattice)
library(raster)

file.nc=open.ncdf("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/elevation/WFDEI-elevation.nc") 
print(file.nc)
data <- get.var.ncdf(file.nc,"elevation")
lon = get.var.ncdf(file.nc,'lon')
lat = get.var.ncdf(file.nc,'lat')
levelplot(data,pretty=T)

lon.full = matrix(rep(lon,each=360), ncol=360, byrow=TRUE)
lat.full = matrix(rep(lat,each=720), nrow=720)

#cut out Latin America
ind.lon.LAM = which(lon>=(-118) & lon<=(-33))
ind.lat.LAM = which(lat>=(-56) & lat<=(34))

data.LAM = data[ind.lon.LAM,ind.lat.LAM]
sum(is.na(data.LAM)==F)  #number of land points
lon.LAM = lon.full[ind.lon.LAM,ind.lat.LAM]
lat.LAM = lat.full[ind.lon.LAM,ind.lat.LAM]

elev.full = array(NA,dim=c(170*180,3))
for (x in 1:170) {
  for (y in 1:180) {
    
    elev.p = data.LAM[x,y]
    lon.p = lon.LAM[x,y]
    lat.p = lat.LAM[x,y]
    elev.full[(x-1)*180+y,] = c(lon.p,lat.p,elev.p)
  }
}
colnames(elev.full) = c('lon','lat','elev')

#save elevation matrix
save(elev.full,file='Z:/01-climate-data/elevation/elevation_LAM.rdat')
