library(raster)
library('RColorBrewer')  #next option
library('lattice')
library('ncdf')

setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/05-Crop Calendar Sacks")

#crops
crops = c('Maize','Maize.2','Potatoes','Rice','Rice.2','Soybeans','Wheat','Wheat.Winter','Pulses')
crops2 = c('Maize','Maize','Potato','Rice','Rice','Soybean','Wheat','Wheat','Bean')

for (j in 1:length(crops))  {
  
  #load planting & harvest dates in raster
  eval(parse(text=paste('crop_plant<-raster("',crops[j],'.crop.calendar.nc",varname="plant")',sep='')))  #unfilled data
  eval(parse(text=paste('crop_harvest<-raster("',crops[j],'.crop.calendar.nc",varname="harvest")',sep='')))
#   eval(parse(text=paste('crop_plant<-raster("filled/',crops[j],'.crop.calendar.fill.nc",varname="plant")',sep='')))  #filled data
#   eval(parse(text=paste('crop_harvest<-raster("filled/',crops[j],'.crop.calendar.fill.nc",varname="harvest")',sep='')))
  
  #extract points from raster
  crop_plant2 = rasterToPoints(crop_plant)
  crop_harvest2 = rasterToPoints(crop_harvest)
  crop.cal = cbind(crop_plant2,crop_harvest2[,3])  #merge dates assuming same cells
  colnames(crop.cal) = c('Long','Lat','Planting','Harvest')
  
  #   #plot raster
  #   plot(crop_plant)
  
  #some netcdf functions
  eval(parse(text=paste('crop.full = open.ncdf("',crops[j],'.crop.calendar.nc")',sep='')))  #load entire netcdf file with 11 variables
  #eval(parse(text=paste('crop.full = open.ncdf("filled/',crops[j],'.crop.calendar.fill.nc")',sep='')))  #load entire netcdf file with 11 variables
  print(crop.full)
  summary(crop.full)
  lat = get.var.ncdf(crop.full,"latitude")  #try extracting variables
  lon = get.var.ncdf(crop.full,"longitude")
  plant = get.var.ncdf(crop.full,'plant') 
  harvest = get.var.ncdf(crop.full,'harvest')  
  
  #flip upright
  ord = order(lat)
  lat = lat[ord]
  plant = plant[,ord]
  harvest = harvest[,ord]
  
#   #try plotting variable from ncdf
#   image(lon,lat,plant,col=rev(brewer.pal(10,'RdBu')))  #1st option
#   filled.contour(lon,lat,plant,color=terrain.colors,asp=1)  #2nd option

  #Nice plot
  grid = expand.grid(lon=lon,lat=lat)
  cutpts = seq(0,365,by=50)
  levelplot(harvest~lon*lat,data=grid,at=cutpts,cuts = 8,pretty=T,col.regions=(rev(brewer.pal(8,'RdBu'))))
  
  #Try mapping crop calendars to production points for each crop
  eval(parse(text=paste('load("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/',crops2[j],'.loc.Rdat")',sep='')))
  eval(parse(text=paste('crop.loc = ',crops2[j],'.loc',sep='')))  

  #rounding lat/long to 2 decimal points (is this necessary for matching?!)
  crop.cal[,c('Long','Lat')] = round(crop.cal[,c('Long','Lat')],2)  #round to 2 decimals for matching purposes
  crop.loc[,c('Longitude','Latitude')] = round(crop.loc[,c('Longitude','Latitude')],2)  #round to 2 decimals for matching purposes

  #find matching rows and add planting & harvest dates to crop location matrix
  ind.cal = match(paste(crop.loc[,'Longitude'],crop.loc[,'Latitude'],sep=''),paste(crop.cal[,'Long'],crop.cal[,'Lat'],sep=''))
  crop.loc$plant = crop.cal[ind.cal,'Planting']
  crop.loc$harvest = crop.cal[ind.cal,'Harvest']
  
  #find locations included in each criteria that lack calendar info
  test = which(is.na(crop.loc$plant) & crop.loc$Prod.95==1) #95% production
  length(test)/dim(crop.loc)[1]*100  #about 10% for maize
  
  test = which(is.na(crop.loc$plant) & crop.loc$Area.95==1) #95% area
  length(test)/dim(crop.loc)[1]*100  #about 17% for maize
  
  test = which(is.na(crop.loc$plant) & crop.loc$Prod.99==1) #99% production
  length(test)/dim(crop.loc)[1]*100  #about 24% for maize
  
  test = which(is.na(crop.loc$plant) & crop.loc$Area.99==1) #99% area
  length(test)/dim(crop.loc)[1]*100  #about 24% for maize
}

#Load & analyze Mirca crop calendar data  (compare data coverage, but also consistency with Sacks)
mirca = read.table('//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/09-MIRCA2000/CELL_SPECIFIC_CROPPING_CALENDARS_30MN.txt',sep='',header=T)
#UPDATE CROP NUMBER HERE (2/28 maize, 10/36 potato, 3/29 rice, 8/34 soy, 1/27 wheat, 17/43 pulses)
crop.irr = which((mirca$crop==2|mirca$subcrop==2) & mirca$lat>=(-56) & mirca$lat<=33 & mirca$lon>=(-117) & mirca$lon<=(-34))  #cut out Latin America (& bit of US)
crop.rf = which((mirca$crop==28|mirca$subcrop==28) & mirca$lat>=(-56) & mirca$lat<=33 & mirca$lon>=(-117) & mirca$lon<=(-34))  #cut out Latin America (& bit of US)
crop.irr = mirca[crop.irr,]
crop.rf = mirca[crop.rf,]

#try matching these cells to criteria from Monfreda
ind_mirca.irr = match(paste(crop.loc$Latitude,crop.loc$Longitude,sep=''),paste(crop.irr$lat,crop.irr$lon,sep=''))
ind_mirca.rf = match(paste(crop.loc$Latitude,crop.loc$Longitude,sep=''),paste(crop.rf$lat,crop.rf$lon,sep=''))

mirca.crop.irr = crop.irr[ind_mirca.irr,]
mirca.crop.rf = crop.rf[ind_mirca.rf,]

# #check for double-cropping in Mirca??
# mirca.crop = rbind(mirca.crop.irr,mirca.crop.rf)

#Add MIRCA data to crop matrix
crop.loc$mirca.irr.start = mirca.crop.irr$start
crop.loc$mirca.irr.end = mirca.crop.irr$end
crop.loc$mirca.rf.start = mirca.crop.rf$start
crop.loc$mirca.rf.end = mirca.crop.rf$end

#convert months in MIRCA to mid-month DOY (for comparison with Sacks)
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
for (k in 1:dim(crop.loc)[1])  {
  if (is.na(crop.loc$mirca.irr.start[k])==F)  {
    crop.loc$mirca.irr.start[k] = sum(days[1:(crop.loc$mirca.irr.start[k]-1)])+15
    crop.loc$mirca.irr.end[k] = sum(days[1:(crop.loc$mirca.irr.end[k]-1)])+15
  }

  if (is.na(crop.loc$mirca.rf.start[k])==F)  {
    crop.loc$mirca.rf.start[k] = sum(days[1:(crop.loc$mirca.rf.start[k]-1)])+15
    crop.loc$mirca.rf.end[k] = sum(days[1:(crop.loc$mirca.rf.end[k]-1)])+15
  }
}

#save out revised crop location matrices with calendar information
eval(parse(text=paste(crops[j],'.loc.cal=crop.loc',sep='')))
# Rice.loc.cal$plant.2 = crop.loc$plant  #add 2nd season from Sacks
# Rice.loc.cal$harvest.2 = crop.loc$harvest
eval(parse(text=paste("save(",crops[j],".loc.cal,file='//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/",crops[j],".loc.cal.Rdat')",sep='')))

#Graph MIRCA and Sacks planting dates next to each other
plot(crop.loc$plant,ylim=c(0,365))
points(crop.loc$mirca.irr.start,pch=20,col='purple')
points(crop.loc$mirca.rf.start,pch=20,col='orange')

plot(crop.loc$harvest,ylim=c(0,365))
points(crop.loc$mirca.irr.end,pch=20,col='purple')
points(crop.loc$mirca.rf.end,pch=20,col='orange')

#calculate difference between 2 datasets
diff = abs(crop.loc$mirca.irr.start - crop.loc$plant)
diff2 = (365-diff)
diff3 = apply(cbind(diff,diff2),1,min)

diff.rf = abs(crop.loc$mirca.rf.start - crop.loc$plant)
diff2.rf = (365-diff.rf)
diff3.rf = apply(cbind(diff.rf,diff2.rf),1,min)

plot(diff3,ylim=c(0,365),cex=1.25)
points(diff3.rf,pch=20,col='purple')

#calculate % within various days of each other
sum(diff3<30,na.rm=T)/sum(is.na(diff3)==F)*100 
sum(diff3<45,na.rm=T)/sum(is.na(diff3)==F)*100  
sum(diff3<60,na.rm=T)/sum(is.na(diff3)==F)*100  #percent within 45 days (80% for maize!)
sum(diff3<90,na.rm=T)/sum(is.na(diff3)==F)*100

sum(diff3.rf<30,na.rm=T)/sum(is.na(diff3.rf)==F)*100
sum(diff3.rf<45,na.rm=T)/sum(is.na(diff3.rf)==F)*100 
sum(diff3.rf<60,na.rm=T)/sum(is.na(diff3.rf)==F)*100
sum(diff3.rf<90,na.rm=T)/sum(is.na(diff3.rf)==F)*100

#check data coverage for MIRCA
#find locations included in each criteria that lack calendar info
test = which(is.na(crop.loc$mirca.irr.start) & crop.loc$Prod.95==1) #95% production
length(test)/dim(crop.loc)[1]*100  #about 10% for maize

test = which(is.na(crop.loc$mirca.irr.start) & crop.loc$Area.95==1) #95% area
length(test)/dim(crop.loc)[1]*100  #about 17% for maize

test = which(is.na(crop.loc$mirca.irr.start) & crop.loc$Prod.99==1) #99% production
length(test)/dim(crop.loc)[1]*100  #about 24% for maize

test = which(is.na(crop.loc$mirca.irr.start) & crop.loc$Area.99==1) #99% area
length(test)/dim(crop.loc)[1]*100  #about 24% for maize

#Merge spring and winter wheat calendars 
load('//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/Wheat.Spring.loc.cal.Rdat')
load('//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/Wheat.Winter.loc.cal.Rdat')
spring = Wheat.loc.cal[is.na(Wheat.loc.cal$plant)==F,]  #pull out spring wheat specific data

ind_spring = match(paste(spring$Latitude,spring$Longitude,sep=''),paste(Wheat.Winter.loc.cal$Latitude,Wheat.Winter.loc.cal$Longitude,sep=''))
Wheat.Winter.loc.cal[ind_spring,] = spring

Wheat.loc.cal = Wheat.Winter.loc.cal
save(Wheat.loc.cal,file='//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/Wheat.loc.cal.Rdat')
