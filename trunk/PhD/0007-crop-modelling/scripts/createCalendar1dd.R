#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / University of Leeds

library(raster)

#wheat winter
rm(list=ls()); g=gc(); rm(g)

base_rs <- raster("F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/WHEAT/raster/gridded/fou/fou-66.asc")

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/WHEAT/calendar/wwin"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/WHEAT/calendar/wunk"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)


#gnut
rm(list=ls()); g=gc(); rm(g)

base_rs <- raster("F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/GNUT/raster/gridded/fou/fou-66.asc")

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/GNUT/calendar/gnut"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)


#sorghum -khariff
rm(list=ls()); g=gc(); rm(g)

base_rs <- raster("F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/SORG-KHARIFF/raster/gridded/fou/fou-66.asc")

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/SORG-KHARIFF/calendar/main"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(pday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/SORG-KHARIFF/calendar/second"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)


#sorghum -rabi
rm(list=ls()); g=gc(); rm(g)

base_rs <- raster("F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/SORG-RABI/raster/gridded/fou/fou-66.asc")

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/SORG-RABI/calendar/main"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/SORG-RABI/calendar/second"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)


#rice
rm(list=ls()); g=gc(); rm(g)

base_rs <- raster("F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/RICE/raster/gridded/fou/fou-1966.asc")
bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/RICE/calendar/main"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)

bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield/RICE/calendar/second"
setwd(bDir)
pday <- createCalendar(rname="plant.asc",rs=base_rs,wd=bDir)
pday <- writeRaster(pday,paste(bDir,"/plant_lr.asc",sep=""),format="ascii",overwrite=T)
hday <- createCalendar(rname="harvest.asc",rs=base_rs,wd=bDir)
hday <- writeRaster(hday,paste(bDir,"/harvest_lr.asc",sep=""),format="ascii",overwrite=T)


###############################################################
###############################################################
#function to aggregate the calendar data
###############################################################
###############################################################
createCalendar <- function(rname,rs,wd) {
  xt <- extent(rs)
  
  hr_rs <- raster(paste(wd,"/",rname,sep=""))
  hr_rs <- crop(hr_rs,xt)
  
  lr_rs <- aggregate(hr_rs,fact=(res(rs)/xres(hr_rs)),fun=mean)
  lr_rs <- resample(lr_rs,rs,"bilinear")
  
  lr_rs[which(is.na(rs[]))] <- NA
  return(lr_rs)
}

