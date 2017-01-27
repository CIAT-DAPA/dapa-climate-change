#Julian Ramirez-Villegas
#CCAFS / CIAT
#Jan 2016

#extract met data for crop seasons at 1x1 for Rojas et al. paper so that yield analyses can be done
#to investigate response of yield to climate.

#load needed libraries
library(raster); library(ncdf4); library(maptools)
data(wrld_simpl)

#name of crop to analyse
cropname <- "Rice"

#define seasons
#note that for wheat, GCCMI have merged planting dates, hence we use the average yield.
#this will introduce errors in areas where both winter and spring wheat are grown, but is not a 
#major problem since winter and spring wheat areas do not overlap significantly
if (tolower(cropname) %in% c("maize","rice")) {
  seas <- "_major_"
} else if (tolower(cropname) %in% c("soybean")) {
  seas <- c("_")
} else {
  seas <- c("_")
}

nfsDir <- "/nfs/a101/earjr"
#nfsDir <- "~/Leeds-work"
wd <- paste(nfsDir,"/ToE-global-crops-seasons",sep="")
yiDir <- paste(wd,"/yield_responses/yield_data/yield_data",seas,tolower(cropname),sep="")
metDir <- paste(wd,"/yield_responses/meteorology",sep="")
toeDir <- paste(wd,"/yield_responses/toe_data",sep="")
outDir <- paste(wd,"/yield_responses/analysis_data",sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

#other stuff
mth_list <- c("01","02","03","04","05","06","07","08","09","10","11","12")
source("~/Repositories/dapa-climate-change/toe-global-crops/leap_year.R")
source("~/Repositories/dapa-climate-change/toe-global-crops/gdd_calc.R")

#load planting and harvesting dates
if (tolower(cropname) == "soybean") {
  pdate_rs <- raster(paste(wd,"/data/growing_season_prod_countries_Soybeans.nc",sep=""),varname="planting_day")
  hdate_rs <- raster(paste(wd,"/data/growing_season_prod_countries_Soybeans.nc",sep=""),varname="harvest_day")
} else {
  pdate_rs <- raster(paste(wd,"/data/growing_season_prod_countries_",cropname,".nc",sep=""),varname="planting_day")
  hdate_rs <- raster(paste(wd,"/data/growing_season_prod_countries_",cropname,".nc",sep=""),varname="harvest_day")
}

#load yield data to get mask
yield_stk <- stack(paste(yiDir,"/gdhy_2013JAN29_",tolower(cropname),seas,"ModelYld500_",1986:2005,".tif",sep=""))
yield_stk <- shift(yield_stk, x=-0.5, y=-0.25)
ymean_rs <- mean(yield_stk)
msk_rs <- ymean_rs
msk_rs[which(!is.na(msk_rs[]))] <- 1

#keep constructing mask using Maisa's output (these are the pixels we're interested in)
#fix what Maisa sent
if (tolower(cropname) == "soybean") {
  toe_rs <- stack(paste(toeDir,"/pp_change_atTOE_RCP",c(26,45,60,85),"_Soy.nc",sep=""),varname="pr_change")
} else {
  toe_rs <- stack(paste(toeDir,"/pp_change_atTOE_RCP",c(26,45,60,85),"_",cropname,".nc",sep=""),varname="pr_change")
}
toe_rs1 <- t(toe_rs)
toe_rs1 <- flip(toe_rs1, direction="y")
toe_rs1 <- flip(toe_rs1, direction="x")
extent(toe_rs1) <- c(-180,180,-90,90)
toe_msk <- mean(toe_rs1, na.rm=T)
toe_msk[which(!is.na(toe_msk[]))] <- 1

#verify the geographic consistency of these data (and where there is no consistency then just do a "shift")
#commented out since i found out that:
#1. yld_msk (and data) must be shifted by 0.5 deg to West (left) and 0.25 deg to South (down)
#2. met_msk (and data) must be shifted by 0.5 deg to West (left) and 0.25 deg to North (up)
met_msk <- raster(paste(metDir,"/Rainf_daily_WFDEI_GPCC/toe_Rainf_daily_WFDEI_GPCC_198501.nc",sep=""),varname="Rainf")
met_msk <- rotate(met_msk); met_msk <- shift(met_msk, x="-0.5", y="0.25")
met_msk[which(!is.na(met_msk[]))] <- 1
met_msk[which(is.na(met_msk[]))] <- 0
#writeRaster(toe_msk, paste(wd,"/yield_responses/toe_msk.tif",sep=""),format="GTiff")
#writeRaster(ymean_rs, paste(wd,"/yield_responses/yld_msk.tif",sep=""),format="GTiff")
#writeRaster(met_msk, paste(wd,"/yield_responses/met_msk.tif",sep=""),format="GTiff")

#create data.frame with cell_id, x, y, is_present, year, and yield_ton_ha
in_data <- data.frame(cell_id=1:ncell(msk_rs), x=xFromCell(msk_rs, 1:ncell(msk_rs)), y=yFromCell(msk_rs, 1:ncell(msk_rs)))
in_data$is_present <- extract(msk_rs, in_data[,c("x","y")])
in_data$has_toe <- extract(toe_msk, in_data[,c("x","y")])
in_data$has_met <- extract(met_msk, in_data[,c("x","y")])
in_data$pdate <- extract(pdate_rs, in_data[,c("x","y")])
in_data$hdate <- extract(hdate_rs, in_data[,c("x","y")])
in_data <- in_data[complete.cases(in_data),]
all_data <- data.frame()
for (year in 1986:2005) {
  #year <- 1986
  cat("extracting yield for year:",year,"which is position=",grep(paste(year),names(yield_stk)),"\n")
  tdata <- in_data
  tdata$year <- year
  tdata$yield_ton_ha <- extract(yield_stk[[grep(paste(year),names(yield_stk))]], in_data[,c("x","y")])
  tdata <- tdata[complete.cases(tdata),]
  all_data <- rbind(all_data, tdata)
}
row.names(all_data) <- 1:nrow(all_data)

#create meteorology array. Start from year before to account for those years where
#planted in year before and harvested this year
if (!file.exists(paste(metDir,"/meteorology_",tolower(cropname),".RData",sep=""))) {
  met_array <- array(data=NA,dim=c(nrow(in_data),3,length(1985:2005),366),
                     dimnames=list(CELL=1:nrow(in_data),VARNAME=c("prec","tmin","tmax"),YEAR=1985:2005,DAY=1:366))
  
  #fill in array
  for (year in 1985:2005) {
    #year <- 1985
    #load monthly meteorology for this year
    prec_year <- tmin_year <- tmax_year <- c()
    for (mth in mth_list) {
      #mth <- mth_list[1]
      cat("loading met. data for month=",mth,"for year=",year,"\n")
      
      #precip
      prec_rs <- stack(paste(metDir,"/Rainf_daily_WFDEI_GPCC/toe_Rainf_daily_WFDEI_GPCC_",year,mth,".nc",sep=""),varname="Rainf")
      prec_rs <- rotate(prec_rs)
      prec_year <- c(prec_year,prec_rs)
      rm(prec_rs)
      
      #tmin
      tmin_rs <- stack(paste(metDir,"/Tmin_daily_WFDEI/toe_Tmin_daily_WFDEI_",year,mth,".nc",sep=""),varname="Tmin")
      tmin_rs <- rotate(tmin_rs)
      tmin_year <- c(tmin_year,tmin_rs)
      rm(tmin_rs)
      
      #tmax
      tmax_rs <- stack(paste(metDir,"/Tmax_daily_WFDEI/toe_Tmax_daily_WFDEI_",year,mth,".nc",sep=""),varname="Tmax")
      tmax_rs <- rotate(tmax_rs)
      tmax_year <- c(tmax_year,tmax_rs)
      rm(tmax_rs)
    }
    prec_year <- stack(prec_year); prec_year <- shift(prec_year, x="-0.5", y="0.25") #shift to match toe data
    tmin_year <- stack(tmin_year); tmin_year <- shift(tmin_year, x="-0.5", y="0.25") #shift to match toe data
    tmax_year <- stack(tmax_year); tmax_year <- shift(tmax_year, x="-0.5", y="0.25") #shift to match toe data
    
    if (is.leapyear(year)) {ndays <- 366} else {ndays <- 365}
    met_array[,"prec",paste(year),1:ndays] <- as.numeric(extract(prec_year, in_data[, c("x","y")]))
    met_array[,"tmin",paste(year),1:ndays] <- as.numeric(extract(tmin_year, in_data[, c("x","y")]))
    met_array[,"tmax",paste(year),1:ndays] <- as.numeric(extract(tmax_year, in_data[, c("x","y")]))
  }
  save(met_array, file=paste(metDir,"/meteorology_",tolower(cropname),".RData",sep=""))
} else {
  load(file=paste(metDir,"/meteorology_",tolower(cropname),".RData",sep=""))
}


###
#for particular pixel extract the full series of weather data for year2 (if hdate > pdate) or for
#both year1 and year2 (if pdate > hdate)
season_vars <- function(p_i, year) {
  #p_i <- 550; year <- 1986 #1 969 463
  #cat("i=",p_i,"\n")
  pdate_i <- yld_year$pdate[p_i]
  hdate_i <- yld_year$hdate[p_i]
  has_met <- yld_year$has_met[p_i]
  
  #if this pixel does not have met. data then take the closest neighbour's data (it'd usually be a coastal pixel)
  if (has_met == 0) {
    cleanmet <- yld_year[which(yld_year$has_met == 1),]
    pdist <- pointDistance(c(yld_year$x[p_i],yld_year$y[p_i]), cbind(x=cleanmet$x, y=cleanmet$y), lonlat=T)
    new_pi <- cleanmet$cell_id[which(pdist == min(pdist, na.rm=T))[1]]
    new_pi <- which(yld_year$cell_id == new_pi)
    old_pi <- p_i
    p_i <- new_pi
  }
  
  #get cardinal temperatures (for GDD calculation)
  if (tolower(cropname) == "maize") {tb <- 8; to <- 30; tm <- 38} #from ERL 2013 paper
  if (tolower(cropname) == "wheat") {tb <- 0; to <- 21; tm <- 35} #from ERL 2013 paper
  if (tolower(cropname) == "rice") {tb <- 8; to <- 30; tm <- 42} #from Oryza2000
  if (tolower(cropname) == "soybean") {tb <- 7; to <- 32; tm <- 45} #from ERL 2013 paper
  
  #compute climate parameters for growing season
  if (pdate_i < hdate_i) {
    if (is.leapyear(year)) {ndays <- 366} else {ndays <- 365}
    met_data <- data.frame(doy=1:ndays, prec=met_array[paste(p_i),"prec",paste(year),paste(1:ndays)], 
                           tmin=met_array[paste(p_i),"tmin",paste(year),paste(1:ndays)],
                           tmax=met_array[paste(p_i),"tmax",paste(year),paste(1:ndays)])
    met_data <- met_data[which(met_data$doy >= pdate_i & met_data$doy <= hdate_i),]
  } else if (pdate_i >= hdate_i) {
    #previous year
    if (is.leapyear(year-1)) {ndays1 <- 366} else {ndays1 <- 365}
    met_data1 <- data.frame(doy=1:ndays1, prec=met_array[paste(p_i),"prec",paste(year-1),paste(1:ndays1)], 
                            tmin=met_array[paste(p_i),"tmin",paste(year-1),paste(1:ndays1)],
                            tmax=met_array[paste(p_i),"tmax",paste(year-1),paste(1:ndays1)])
    
    #this year
    if (is.leapyear(year)) {ndays2 <- 366} else {ndays2 <- 365}
    met_data2 <- data.frame(doy=1:ndays2, prec=met_array[paste(p_i),"prec",paste(year),paste(1:ndays2)], 
                            tmin=met_array[paste(p_i),"tmin",paste(year),paste(1:ndays2)],
                            tmax=met_array[paste(p_i),"tmax",paste(year),paste(1:ndays2)])
    
    
    #merge both matrices
    met_data <- rbind(met_data1[pdate_i:ndays1,], met_data1[1:hdate_i,])
  }
  
  #unit conversions (because this is WFDEI data)
  met_data$prec <- met_data$prec * 3600 * 24 #kg m-2 s-1 to mm/day
  met_data$prec[which(met_data$prec < 0)] <- 0
  met_data$tmax <- met_data$tmax - 273.15 #K to C
  met_data$tmin <- met_data$tmin - 273.15 #K to C
  met_data$tmean <- (met_data$tmax + met_data$tmin) * 0.5
  
  #compute indices
  prec_seas <- sum(met_data$prec, na.rm=T) #seasonal total precip
  tmean_seas <- mean(met_data$tmean, na.rm=T) #seasonal mean T
  gdd_seas <- calc_att(met_data,tb,to) #seasonal total cumulative GDD using capped-top function
  
  return(c(prec_seas,tmean_seas,gdd_seas))
}

#test function
#season_vars(1, 1986); season_vars(969, 1986); season_vars(463, 1986); season_vars(550, 1986)

#compute for all years
if (!file.exists(paste(outDir,"/analysis_data_",tolower(cropname),".RData",sep=""))) {
  out_data <- data.frame()
  for (year in 1986:2005) {
    #year <- 1986
    cat("processing year=",year,"\n")
    yld_year <- all_data[which(all_data$year == year),]
    seas_out <- as.data.frame(t(sapply(1:nrow(in_data), season_vars, year)))
    names(seas_out) <- c("prec","tmean","gdd")
    yld_year <- cbind(yld_year, seas_out)
    out_data <- rbind(out_data, yld_year)
  }
  
  #calculate logyield
  out_data$logyield <- log(out_data$yield_ton_ha)
  
  #decade and 5-year technology periods
  out_data$dec <- NA; out_data$period <- NA
  out_data$dec[which(out_data$year <= 1990)] <- 1980
  out_data$dec[which(out_data$year > 1990 & out_data$year <= 2000)] <- 1990
  out_data$dec[which(out_data$year > 2000)] <- 2000
  out_data$period[which(out_data$year <= 1985)] <- 1980
  out_data$period[which(out_data$year > 1985 & out_data$year <= 1990)] <- 1985
  out_data$period[which(out_data$year > 1990 & out_data$year <= 1995)] <- 1990
  out_data$period[which(out_data$year > 1995 & out_data$year <= 2000)] <- 1995
  out_data$period[which(out_data$year > 2000 & out_data$year <= 2005)] <- 2000
  
  #save output file
  save(out_data, file=paste(outDir,"/analysis_data_",tolower(cropname),".RData",sep=""))
} else {
  load(file=paste(outDir,"/analysis_data_",tolower(cropname),".RData",sep=""))
}

