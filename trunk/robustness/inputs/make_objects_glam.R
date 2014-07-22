#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014

stop("!")
#Make an .RData file with all data that is needed for maize GLAM runs. This is to avoid
#constant loading of individual raster data.

#A first file per cropping season with:
#1. grid cell list data frame
#2. planting and harvesting dates
#3. soil data
#4. ME in which located

#A second file per cropping season with:
#1. grid cell list data frame
#2. crop yields (1982-2005)

#note that the grid cells are defined as those having the 24 years of crop yield information

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
srcDir <- paste(wd,"/scripts",sep="")
metDir <- paste(wd,"/data/meteorology",sep="")
yiDir_mean <- paste(wd,"/data/yield_data_maize",sep="")
yiDir_main <- paste(wd,"/data/yield_data_major_maize",sep="")
yiDir_secd <- paste(wd,"/data/yield_data_second_maize",sep="")
sowDir <- paste(wd,"/data/crop_calendar_sacks",sep="")
solDir <- paste(wd,"/data/soils",sep="")
mesDir <- paste(wd,"/data/maize_MEs",sep="")

#output directory
mdataDir <- paste(wd,"/data/model_data",sep="")
if (!file.exists(mdataDir)) {dir.create(mdataDir)}

#dodgy points with unrealistic crop yield estimates
dodgypoints <- cbind(x=c(28.125,21.375,20.250),y=c(-23.0625,-26.4375,-25.3125))


###########################################################################################
###########################################################################################
#for main season first
#read in yield data for getting mask
yrs <- raster(paste(yiDir_main,"/rs_1995/gdhy_2013JAN29_maize_major_ModelYld500_1995.tif",sep=""))
af_extn <- extent(raster(paste(yiDir_mean,"/descriptive_stats/mean_ModelYld500.tif",sep="")))
yrs <- crop(yrs, af_extn)
yrs[cellFromXY(yrs,dodgypoints)] <- NA

#get xy from cell
xy_main <- as.data.frame(xyFromCell(yrs, which(!is.na(yrs[]))))

#load crop yield estimates for 1982-2005
yrs_stk <- stack(paste(yiDir_main,"/rs_",1982:2005,"/gdhy_2013JAN29_maize_major_ModelYld500_",1982:2005,".tif",sep=""))

#extract all years of data for all grid cells
xy_main <- cbind(xy_main, extract(yrs_stk, xy_main[,c("x","y")]))
names(xy_main)[3:ncol(xy_main)] <- paste("Y.",1982:2005,sep="")

#count number of NAs in each row
xy_main$NAs <- apply(xy_main[,paste("Y.",1982:2005,sep="")],1,FUN=function(x) {length(which(is.na(x)))})
xy_main <- xy_main[which(xy_main$NAs == 0),c("x","y")]
row.names(xy_main) <- 1:nrow(xy_main)

#add gridcell numbers to xy data.frame
xy_main <- cbind(LOC=cellFromXY(yrs,xy_main[,c("x","y")]), xy_main)
xy_main <- cbind(ID=1:nrow(xy_main), xy_main)

#extract ME data
me_rs <- raster(paste(mesDir,"/maizeMESglobal_lowres_me_final.tif",sep=""))
xy_main$ME <- extract(me_rs, xy_main[,c("x","y")])
xy_main <- xy_main[which(!is.na(xy_main$ME)),]

#extract ME data (N/S divisions of MEs)
me_rs_new <- raster(paste(mesDir,"/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
xy_main$ME_NEW <- extract(me_rs_new, xy_main[,c("x","y")])
xy_main <- xy_main[which(!is.na(xy_main$ME_NEW)),]

#load crop calendar data
sow_i <- raster(paste(sowDir,"/major_maize_plant.start.tif",sep=""))
sow_f <- raster(paste(sowDir,"/major_maize_plant.end.tif",sep=""))
har_i <- raster(paste(sowDir,"/major_maize_harvest.start.tif",sep=""))
har_f <- raster(paste(sowDir,"/major_maize_harvest.end.tif",sep=""))

#extract crop calendar data
xy_main <- cbind(xy_main, SOW_DATE1=extract(sow_i, xy_main[,c("x","y")]), 
                 SOW_DATE2=extract(sow_f, xy_main[,c("x","y")]),
                 HAR_DATE1=extract(har_i, xy_main[,c("x","y")]),
                 HAR_DATE2=extract(har_f, xy_main[,c("x","y")]))

#change crop calendar to integers
xy_main[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")] <- round(xy_main[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")],digits=0)

#load soil data
rll <- raster(paste(solDir,"/rll_lr_shangguan2014.tif",sep=""))
dul <- raster(paste(solDir,"/dul_lr_shangguan2014.tif",sep=""))
sat <- raster(paste(solDir,"/sat_lr_shangguan2014.tif",sep=""))
asw <- raster(paste(solDir,"/asw_lr_shangguan2014.tif",sep=""))

#extract soil data
xy_main <- cbind(xy_main, RLL=extract(rll, xy_main[,c("x","y")])*0.01, DUL=extract(dul, xy_main[,c("x","y")])*0.01,
                 SAT=extract(sat, xy_main[,c("x","y")])*0.01, ASW=extract(asw, xy_main[,c("x","y")])*0.01)

#clean up NAs: assign nearest neighbour to points with NA (for all variables in the dataset)
flds <- c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2","RLL","DUL","SAT","ASW")
xy_main$NAs <- apply(xy_main[,flds],1,FUN=function(x) {length(which(is.na(x)))})
nna <- which((xy_main$NAs == 0))
xy_main$NAs <- NULL
for (vn in flds) {
  #vn <- "SOW_DATE1"
  for (i in which(is.na(xy_main[,vn]))) {
    #i <- which(is.na(xy_main[,vn]))[1]
    #get xy coordinates from point id
    xy_i <- xy_main[i,c("x","y")]
    dis_i <- pointDistance(xy_i, xy_main[,c("x","y")],lonlat=T)
    dis_i <- dis_i[nna]
    dis_i <- dis_i[which(dis_i[] == min(dis_i))][1]
    nn_i <- as.numeric(names(dis_i))
    xy_main[i,vn] <- xy_main[nn_i,vn]
  }
}

#fix planting dates
xy_main$SOW_DATE1[which(xy_main$LOC == 1084)] <- 25; xy_main$SOW_DATE2[which(xy_main$LOC == 1084)] <- 50
xy_main$HAR_DATE1[which(xy_main$LOC == 1084)] <- 180; xy_main$HAR_DATE2[which(xy_main$LOC == 1084)] <- 251
xy_main$SOW_DATE1[which(xy_main$LOC == 1174)] <- 25; xy_main$SOW_DATE2[which(xy_main$LOC == 1174)] <- 50
xy_main$HAR_DATE1[which(xy_main$LOC == 1174)] <- 125; xy_main$HAR_DATE2[which(xy_main$LOC == 1174)] <- 150

tseq <- c(1233,1299,1300,1302,1303,1304,1305,1364,1365,1366,1367,1368,1369,1370,1371)
xy_main$SOW_DATE1[which(xy_main$LOC %in% tseq)] <- 1; xy_main$SOW_DATE2[which(xy_main$LOC %in% tseq)] <- 50
xy_main$HAR_DATE1[which(xy_main$LOC %in% tseq)] <- 120; xy_main$HAR_DATE2[which(xy_main$LOC %in% tseq)] <- 170

tseq <- c(1430,1431,1432,1433,1434,1435,1436,1437,1496,1497,1498,1499,1500,1501,1502,1503,1549,1550,1563,1564,1565,1566,1567,1568,1614,1615,1616,1620,1629,1630,1696)
xy_main$SOW_DATE1[which(xy_main$LOC %in% tseq)] <- 300; xy_main$SOW_DATE2[which(xy_main$LOC %in% tseq)] <- 360
xy_main$HAR_DATE1[which(xy_main$LOC %in% tseq)] <- 80; xy_main$HAR_DATE2[which(xy_main$LOC %in% tseq)] <- 130

xy_main$HAR_DATE1[which(xy_main$LOC == 1481)] <- 330
xy_main$HAR_DATE1[which(xy_main$LOC == 1684)] <- 98; xy_main$HAR_DATE2[which(xy_main$LOC == 1684)] <- 120

tseq <- c(1763,1897,1898,1899,1900,2281,2285,2346,2352,2419,2486,2616,2681)
a <- xy_main$SOW_DATE1[which(xy_main$LOC %in% tseq)]
b <- xy_main$SOW_DATE2[which(xy_main$LOC %in% tseq)]
xy_main$SOW_DATE1[which(xy_main$LOC %in% tseq)] <- b
xy_main$SOW_DATE2[which(xy_main$LOC %in% tseq)] <- a

#save this data frame in a RData file
save(list=c("xy_main"),file=paste(mdataDir,"/initial_conditions_major.RData",sep=""))

#extract crop yields
xy_main_yield <- xy_main[,c("x","y")]
xy_main_yield <- cbind(xy_main_yield, extract(yrs_stk, xy_main_yield[,c("x","y")]))
names(xy_main_yield)[3:ncol(xy_main_yield)] <- paste("Y.",1982:2005,sep="")

#ton/ha to kg/ha
xy_main_yield[,paste("Y.",1982:2005,sep="")] <- xy_main_yield[,paste("Y.",1982:2005,sep="")] * 1000

#save crop yields
save(list=c("xy_main_yield"),file=paste(mdataDir,"/yield_major.RData",sep=""))


###########################################################################################
###########################################################################################
### now for second season
#for main season first
#read in yield data for getting mask
yrs <- raster(paste(yiDir_secd,"/rs_1995/gdhy_2013JAN29_maize_second_ModelYld500_1995.tif",sep=""))
af_extn <- extent(raster(paste(yiDir_mean,"/descriptive_stats/mean_ModelYld500.tif",sep="")))
yrs <- crop(yrs, af_extn)
yrs[cellFromXY(yrs,dodgypoints)] <- NA

#get xy from cell
xy_second <- as.data.frame(xyFromCell(yrs, which(!is.na(yrs[]))))

#load crop yield estimates for 1982-2005
yrs_stk <- stack(paste(yiDir_secd,"/rs_",1982:2005,"/gdhy_2013JAN29_maize_second_ModelYld500_",1982:2005,".tif",sep=""))

#extract all years of data for all grid cells
xy_second <- cbind(xy_second, extract(yrs_stk, xy_second[,c("x","y")]))
names(xy_second)[3:ncol(xy_second)] <- paste("Y.",1982:2005,sep="")

#count number of NAs in each row
xy_second$NAs <- apply(xy_second[,paste("Y.",1982:2005,sep="")],1,FUN=function(x) {length(which(is.na(x)))})
xy_second <- xy_second[which(xy_second$NAs == 0),c("x","y")]
row.names(xy_second) <- 1:nrow(xy_second)

#add gridcell numbers to xy data.frame
xy_second <- cbind(LOC=cellFromXY(yrs,xy_second[,c("x","y")]), xy_second)
xy_second <- cbind(ID=1:nrow(xy_second), xy_second)

#extract ME data
me_rs <- raster(paste(mesDir,"/maizeMESglobal_lowres_me_final.tif",sep=""))
xy_second$ME <- extract(me_rs, xy_second[,c("x","y")])
xy_second <- xy_second[which(!is.na(xy_second$ME)),]

#extract splitted ME data
me_rs_new <- raster(paste(mesDir,"/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
xy_second$ME_NEW <- extract(me_rs_new, xy_second[,c("x","y")])
xy_second <- xy_second[which(!is.na(xy_second$ME_NEW)),]

#load crop calendar data
sow_i <- raster(paste(sowDir,"/second_maize_plant.start.tif",sep=""))
sow_f <- raster(paste(sowDir,"/second_maize_plant.end.tif",sep=""))
har_i <- raster(paste(sowDir,"/second_maize_harvest.start.tif",sep=""))
har_f <- raster(paste(sowDir,"/second_maize_harvest.end.tif",sep=""))

#extract crop calendar data
xy_second <- cbind(xy_second, SOW_DATE1=extract(sow_i, xy_second[,c("x","y")]), 
                 SOW_DATE2=extract(sow_f, xy_second[,c("x","y")]),
                 HAR_DATE1=extract(har_i, xy_second[,c("x","y")]),
                 HAR_DATE2=extract(har_f, xy_second[,c("x","y")]))

#change crop calendar to integers
xy_second[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")] <- round(xy_second[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")],digits=0)

#load soil data
rll <- raster(paste(solDir,"/rll_lr_shangguan2014.tif",sep=""))
dul <- raster(paste(solDir,"/dul_lr_shangguan2014.tif",sep=""))
sat <- raster(paste(solDir,"/sat_lr_shangguan2014.tif",sep=""))
asw <- raster(paste(solDir,"/asw_lr_shangguan2014.tif",sep=""))

#extract soil data
xy_second <- cbind(xy_second, RLL=extract(rll, xy_second[,c("x","y")])*0.01, DUL=extract(dul, xy_second[,c("x","y")])*0.01,
                 SAT=extract(sat, xy_second[,c("x","y")])*0.01, ASW=extract(asw, xy_second[,c("x","y")])*0.01)

#clean up NAs: assign nearest neighbour to points with NA (for all variables in the dataset)
flds <- c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2","RLL","DUL","SAT","ASW")
xy_second$NAs <- apply(xy_second[,flds],1,FUN=function(x) {length(which(is.na(x)))})
nna <- which((xy_second$NAs == 0))
xy_second$NAs <- NULL
for (vn in flds) {
  #vn <- "SOW_DATE1"
  for (i in which(is.na(xy_second[,vn]))) {
    #i <- which(is.na(xy_second[,vn]))[1]
    #get xy coordinates from point id
    xy_i <- xy_second[i,c("x","y")]
    dis_i <- pointDistance(xy_i, xy_second[,c("x","y")],lonlat=T)
    dis_i <- dis_i[nna]
    dis_i <- dis_i[which(dis_i[] == min(dis_i))][1]
    nn_i <- as.numeric(names(dis_i))
    xy_second[i,vn] <- xy_second[nn_i,vn]
  }
}

#save this data frame in a RData file
save(list=c("xy_second"),file=paste(mdataDir,"/initial_conditions_second.RData",sep=""))

#extract crop yields
xy_second_yield <- xy_second[,c("x","y")]
xy_second_yield <- cbind(xy_second_yield, extract(yrs_stk, xy_second_yield[,c("x","y")]))
names(xy_second_yield)[3:ncol(xy_second_yield)] <- paste("Y.",1982:2005,sep="")

#ton/ha to kg/ha
xy_second_yield[,paste("Y.",1982:2005,sep="")] <- xy_second_yield[,paste("Y.",1982:2005,sep="")] * 1000

#save crop yields
save(list=c("xy_second_yield"),file=paste(mdataDir,"/yield_second.RData",sep=""))


