#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")
#Create shuffled and perturbed gridded climate data for EcoCrop

#get seeds from Jim's data
#use set.seed(s) to get the seed correctly
#calculate sd and modified sd (range 0 to 299)
#use rnorm to get the new value

#Shuffle subseasonal: monthly rainfall and temperature values are shuffled within a season, preserving
#                     interannual variability and climatology.
#Perturb seasonal: a single adjustment (d) was applied to all input values across the entire growing 
#                  season. The value d was chosen by substracting the seasonal (June-Sept) mean from
#                  the value v* selected from a normal distribution with mean equal to the seasonal
#                  mean, and sd equal to p% of the seasonal sd.
#
#Perturb spatial: a single adjustment (d) was applied chosen by substracting the spatio-seasonal mean
#                 from a value chosen from the normal distribution with mean equal to the whole India
#                 mean, and sd equal to %p of the whole India sd
#

#Seasonal configuration
library(raster)
percent_std_dev <- seq(0,299,by=1)
rstDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse"
outDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/shuffle-perturb/climate/p-seasonal"
wthDir <- "D:/CIAT_work/GLAM/PNAS-paper/GJ-weather/shuf-pert/dqs_data/mtemp/season"
sList <- list.files(wthDir,pattern="tmax_p-0_s-") #List of unique seeds
sList <- gsub("tmax_p-0_s-","",sList); sList <- as.numeric(gsub(".dat","",sList))

#Loop through variables to perform the seasonal modifications
for (v in c("prec","tmean","tmin")) {
  stk <- stack(paste(rstDir,"/",v,"_",c(1:12),".asc",sep=""))
  orig_values <- extract(stk,1:ncell(stk)) #Extract monthly values all cells
  for (seed in sList) {
    for (pval in percent_std_dev) {
      cat("Processing:",v,"/ s =",seed,"/ p =",pval,"%\n")
      outPth <- paste(outDir,"/",v,"_p-",pval,"_s-",seed,sep="") #create folder
      if (!file.exists(outPth)) {dir.create(outPth)} #create folder
      new_values <- apply(orig_values,1,apply_modif_seas,seed,pval) #get new rasters
      
      for (m in 1:12) { #loop to write monthly files
        rs <- raster(stk)
        rs[] <- new_values[m,]
        writeRaster(rs,paste(outPth,"/",v,"_",m,".asc",sep=""),format='ascii',overwrite=T)
        rm(rs); g=gc(); rm(g)
      }
      rm(new_values); g=gc(); rm(g)
    }
  }
  rm(stk); rm(orig_values); g=gc(); rm(g)
}

#function to perform the modification
apply_modif_seas <- function(orig_value,s,p) {
  nas <- length(which(is.na(orig_value))) #do not deal with NAs in the calculation
  if (nas > 0) {
    return(rep(NA,times=length(orig_value)))
  } else {
    seasonal_mean <- mean(orig_value[6:9]) #change for whole year if needed
    seasonal_sd <- sd(orig_value[6:9]) #change for whole year if needed
    out_std_dev <- p / 100 * seasonal_sd #modified standard deviation
    set.seed(s) #setting seed
    diff_val <- rnorm(1,seasonal_mean,out_std_dev) - seasonal_mean #calculate modifier
    new_value <- orig_value + diff_val #add modifier to observed
    new_value[which(new_value<0)] <- 0; new_value[which(new_value>1000)] <- 999.9 #check range
    return(new_value)
  }
}

################################################################################
################################################################################
#Spatial perturbation configuration
library(raster)
percent_std_dev <- seq(0,299,by=1)
rstDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse"
outDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/shuffle-perturb/climate/p-spatial"
wthDir <- "D:/CIAT_work/GLAM/PNAS-paper/GJ-weather/shuf-pert/dqs_data/mtemp/season"
sList <- list.files(wthDir,pattern="tmax_p-0_s-") #List of unique seeds
sList <- gsub("tmax_p-0_s-","",sList); sList <- as.numeric(gsub(".dat","",sList))

#Loop through variables to perform the spatial modifications
for (v in c("prec","tmean","tmin")) {
  stk <- paste(rstDir,"/",v,"_",c(1:12),".asc",sep="") #vector of filenames
  stk <- sapply(stk,list) #list of filenames
  stk <- lapply(stk,raster) #list of rasters
  for (seed in sList) {
    for (pval in percent_std_dev) {
      cat("Processing:",v,"/ s =",seed,"/ p =",pval,"%\n")
      stk_mod <- lapply(stk,apply_modif_spat,seed,pval) #list of results
      outPth <- paste(outDir,"/",v,"_p-",pval,"_s-",seed,sep="") #create folder
      if (!file.exists(outPth)) {dir.create(outPth)} #create folder
      for (m in 1:12) { #write the results
        rs <- stk_mod[[m]] #make month
        rs <- writeRaster(rs,paste(outPth,"/",v,"_",m,".asc",sep=""),format='ascii',overwrite=T)
        rm(rs);g=gc();rm(g)
      }
      rm(stk_mod);g=gc();rm(g)
    }
  }
  rm(stk);g=gc();rm(g)
}

#function to perform the spatial modification
apply_modif_spat <- function(rs,s,p) {
  orig_values <- rs[which(!is.na(rs[]))] #get values
  spatial_mean <- mean(orig_values) #calc. spat. mean
  spatial_sd <- sd(orig_values) #calc. spat. stdev
  out_std_dev <- p / 100 * spatial_sd #calc output std depending upon p%
  set.seed(s) #set the seed
  diff_val <- rnorm(1,spatial_mean,out_std_dev) - spatial_mean #calc modification
  new_values <- orig_values + diff_val #apply modification to original values
  new_values[which(new_values<0)] <- 0; new_values[which(new_values>1000)] <- 999.9 #check range
  rs[which(!is.na(rs[]))] <- new_values #return values to raster file
  return(rs)
}


################################################################################
################################################################################
#Seasonal shuffling configuration
library(raster)
rstDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse"
outDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/shuffle-perturb/climate/s-seasonal"

#Loop through variables to perform the seasonal shuffling
for (v in c("prec","tmean","tmin")) {
  stk <- stack(paste(rstDir,"/",v,"_",c(1:12),".asc",sep=""))
  orig_values <- extract(stk,1:ncell(stk)) #Extract monthly values all cells
  if (v == "tmin" | v == "tmean") {
    wthDir <- "D:/CIAT_work/GLAM/PNAS-paper/GJ-weather/shuf-pert/dqs_data_shuffled/temp/wyear"
    sList <- list.files(wthDir,pattern="tmax_s-") #List of unique seeds from Jim data
    sList <- gsub("tmax_s-","",sList); sList <- as.numeric(gsub(".dat","",sList))
  } else {
    wthDir <- "D:/CIAT_work/GLAM/PNAS-paper/GJ-weather/shuf-pert/dqs_data_shuffled/prec/wyear"
    sList <- list.files(wthDir,pattern="s-") #List of unique seeds from Jim data
    sList <- gsub("s-","",sList); sList <- as.numeric(gsub(".dat","",sList))
  }
  for (seed in sList) {
    cat("Processing:",v,"/ s =",seed,"\n")
    outPth <- paste(outDir,"/",v,"_s-",seed,sep="") #create folder
    if (!file.exists(outPth)) {dir.create(outPth)} #create folder
    if (!file.exists(paste(outPth,"/",v,"_12.asc",sep=""))) {
      new_values <- apply(orig_values,1,apply_shuff_seas,seed) #get new rasters
      
      for (m in 1:12) { #loop to write monthly files
        rs <- raster(stk)
        rs[] <- new_values[m,]
        writeRaster(rs,paste(outPth,"/",v,"_",m,".asc",sep=""),format='ascii',overwrite=T)
        rm(rs); g=gc(); rm(g)
      }
      rm(new_values); g=gc(); rm(g)
    }
  }
  rm(stk); rm(orig_values); g=gc(); rm(g)
}

#shuffling function
apply_shuff_seas <- function(orig_value,s) {
  nas <- length(which(is.na(orig_value))) #do not deal with NAs in the calculation
  if (nas > 0) {
    return(rep(NA,times=length(orig_value)))
  } else {
    gs_value <- orig_value[6:9] #get values for growing season
    set.seed(s) #set the seed
    gs_value <- sample(gs_value) #shuffle data
    new_value <- orig_value; new_value[6:9] <- gs_value #get data in
    return(new_value)
  }
}
