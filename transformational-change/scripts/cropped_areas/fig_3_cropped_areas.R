#Julian Ramirez-Villegas and Ulrike Rippke
#CIAT / CCAFS / UoL
#May 2014
stop("!")

#load needed libraries
library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

####
#maps: of time of crossing
#1. loop running decades to calculate frequency of crossing each threshold
#2. from these calculate:
#   *time1: 0-2 years below threshold
#   *time2: 3-5 years below threshold
#   *time3: more than 5 years below threshold
#3. use these to determine date of crossing
#4. calculate minimum and maximum crossing amongst all GCMs
#5. plot minimum, maximum and ensemble mean

#run options
dataset <- "cru"
rcp <- "rcp60" #rcp60 rcp85

#i/o directories
#b_dir <- "/nfs/a101/earjr/cul-de-sacs"
b_dir <- "~/Leeds-work/cul-de-sacs"
base_run <- paste(b_dir,"/model_runs/",dataset,"_hist",sep="")
base_run_y <- paste(b_dir,"/model_runs/",dataset,"_hist_yearly",sep="")

#output dirs
out_dir <- paste(b_dir,"/analysis_outputs_cropped_areas",sep="")
fig_dir <- paste(out_dir,"/figures_",dataset,"_crosscheck",sep="")
dfil_dir <- paste(out_dir,"/data_files_",dataset,sep="")
if (!file.exists(dfil_dir)) {dir.create(dfil_dir,recursive=T)}
if (!file.exists(fig_dir)) {dir.create(fig_dir,recursive=T)}

#rcp input dir
#rcp_run <- paste(b_dir,"/model_runs/",dataset,"_futclim_bc/",rcp,sep="")
rcp_run <- paste("/nfs/a101/earjr/cul-de-sacs/model_runs/",dataset,"_futclim_bc/",rcp,sep="")

#read in thresholds and crop names
thresh_val <- read.csv(paste(b_dir,"/model_data/thresholds.csv", sep=""))
thresh_val <- thresh_val[which(thresh_val$dataset == dataset),]
thresh_val <- thresh_val[c(1:3,10:13,15:16),]
thresh_val$dataset <- NULL; row.names(thresh_val) <- 1:nrow(thresh_val)
thresh_val$AUC <- thresh_val$MinROCdist <- thresh_val$MaxKappa <- NULL
names(thresh_val)[2] <- "value"
thresh_val$value <- thresh_val$value * 100
thresh_val$crop[which(thresh_val$crop == "fmillet_EAF_SAF")] <- "fmillet"
thresh_val$crop[which(thresh_val$crop == "yam_WAF")] <- "yam"
thresh_val$crop <- paste(thresh_val$crop)

#load baseline suitability rasters
base_stk <- stack(paste(base_run,"/",thresh_val$crop,"_suit.tif",sep=""))
names(base_stk) <- paste(thresh_val$crop)

#list of GCMs
if (!file.exists(paste(b_dir,"/scratch/gcm_list.RData",sep=""))) {
  gcm_list <- list.files(rcp_run)
  #save(gcm_list,file=paste(b_dir,"/scratch/gcm_list.RData",sep=""))
} else {
  load(file=paste(b_dir,"/scratch/gcm_list.RData",sep=""))
}

#list of years and decades
yr_list <- c(2006:2098)
dc_list <- c((min(yr_list)+10):(max(yr_list)-9))

#### load crossing time data (for transformational i.e. ctime==2), for cropped areas
#loop through crops
ctime <- 2
cross_2_all <- list(earliest=c(),mean=c(),latest=c())
for (i in 1:nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crop[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #folder of dfil_dir per crop
  dfil_crop <- paste(dfil_dir,"/",crop_name,sep="")
  
  if (exists("cross_stk_con")) rm(list=c("cross_stk_con"))
  cross_all <- list(cross1=c(),cross2=c())
  for (gcm in gcm_list) {
    #gcm <- gcm_list[1]
    
    #load processed output
    load(file=paste(dfil_crop,"/crossing_crosscheck_",rcp,"_",gcm,".RData",sep=""))
    
    #put into raster stack
    cross_all$cross2 <- c(cross_all$cross2, cross_stk$layer.2)
    
    #remove any previous objects and load data for this GCM
    rm(list=c("cross_stk_con"))
  }
  cross_all$cross2 <- stack(cross_all$cross2)
  
  calcmean <- function(x) {
    if (length(which(is.na(x))) >= round(length(x)/2)) {
      y <- NA
    } else if (length(which(x == 2090)) >= round(length(x)/2)) {
      y <- 2095
    } else if (length(which(x == 2015)) >= round(length(x)/2)) {
      y <- 2014
    } else if (length(which(x < 0)) >= round(length(x)/2)) {
      y <- NA
    } else {
      x <- x[which(x > 2015 & x < 2090)]; y <- mean(x, na.rm=T)
    }
    return(y)
  }
  
  calcmin <- function(x) {
    if (length(which(is.na(x))) >= round(length(x)/2)) {
      y <- NA
    } else if (length(which(x == 2090)) >= round(length(x)/2)) {
      y <- 2095
    } else if (length(which(x == 2015)) >= round(length(x)/2)) {
      y <- 2014
    } else if (length(which(x < 0)) >= round(length(x)/2)) {
      y <- NA
    } else {
      x <- x[which(x > 2015 & x < 2090)]; y <- min(x, na.rm=T)
    }
    return(y)
  }
  
  calcmax <- function(x) {
    if (length(which(is.na(x))) >= round(length(x)/2)) {
      y <- NA
    } else if (length(which(x >= 2089)) >= round(length(x)/2)) {
      y <- 2095
    } else if (length(which(x == 2015)) >= round(length(x)/2)) {
      y <- 2014
    } else if (length(which(x < 0)) >= round(length(x)/2)) {
      y <- NA
    } else {
      x <- x[which(x > 2015 & x < 2090)]; y <- max(x, na.rm=T)
    }
    return(y)
  }
  
  crosstime <- c(calc(cross_all[[paste("cross",ctime,sep="")]], fun=calcmin),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=calcmean),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=calcmax))
  crosstime <- stack(crosstime); names(crosstime) <- c("Earliest","Mean","Latest")
  
  #append into multi-crop list
  cross_2_all$earliest <- c(cross_2_all$earliest, crosstime[["Earliest"]])
  cross_2_all$mean <- c(cross_2_all$mean, crosstime[["Mean"]])
  cross_2_all$latest <- c(cross_2_all$latest, crosstime[["Latest"]])
}
cross_2_all$earliest <- stack(cross_2_all$earliest)
cross_2_all$mean <- stack(cross_2_all$mean)
cross_2_all$latest <- stack(cross_2_all$latest)

names(cross_2_all$earliest) <- paste(thresh_val$crop)
names(cross_2_all$mean) <- paste(thresh_val$crop)
names(cross_2_all$latest) <- paste(thresh_val$crop)


#### load crossing time data (for transformational i.e. ctime==2), for all areas (ie. no non-cropped)
#loop through crops
ctime <- 2
cross_2_all2 <- list(earliest=c(),mean=c(),latest=c())
for (i in 1:nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crop[i])
  thr <- thresh_val$value[i]
  cat("\n...processing crop=",crop_name,"\n")
  
  #extract data from baseline raster
  xy_allb <- as.data.frame(xyFromCell(base_stk[[crop_name]], 1:ncell(base_stk[[crop_name]])))
  xy_allb$base <- extract(base_stk[[crop_name]], xy_allb[,c("x","y")])
  
  #load yearly his stack and extract data
  yr_stk <- stack(paste(base_run_y,"/",1986:2005,"/",crop_name,"_suit.tif",sep=""))
  dc_stk <- yr_stk
  xy_all <- cbind(xy_allb, as.data.frame(extract(dc_stk, xy_allb[,c("x","y")])))
  
  #function to calculate frequency of crossing
  calc_freq <- function(x, thr) {
    x_b <- x[1]
    x_d <- x[2:length(x)]
    if (is.na(x_b)) {
      y <- NA
    } else if (x_b < thr) {
      y <- NA
    } else {
      y <- length(which(x_d < thr))# / length(x_d)
    }
    return(y)
  }
  
  #run calculation for all grid cells
  xy_all$freq <- apply(xy_all[,3:ncol(xy_all)], 1, FUN=calc_freq, thr)
  
  #put into raster
  rs_freq_his <- raster(dc_stk)
  rs_freq_his[cellFromXY(rs_freq_his, xy_all[,c("x","y")])] <- xy_all$freq
  
  #folder of dfil_dir per crop
  dfil_crop <- paste(b_dir,"/analysis_outputs/data_files_",dataset,"/",crop_name,sep="")
  
  if (exists("cross_stk")) rm(list=c("cross_stk","dc_out_stk"))
  cross_all <- list(cross1=c(),cross2=c())
  for (gcm in gcm_list) {
    #gcm <- gcm_list[1]
    cat("...processing gcm=",gcm,"\n")
    
    #load processed output
    load(file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
    lastdec_rs <- dc_out_stk[[nlayers(dc_out_stk)]]
    rm(cross_stk)
    
    #calculate cross time again
    #function to calculate decade
    calc_ctime <- function(x, adap=1) {
      #x <- as.numeric(xy_all[12540,3:ncol(xy_all)])
      #plot(as.numeric(xy_all[9427,3:ncol(xy_all)]),ty="l")
      #plot(as.numeric(xy_all[12540,3:ncol(xy_all)]),ty="l")
      if (is.na(x[1])) { #pixel is orignally NA
        y <- NA
      } else if (length(which(x == 0)) == length(x)) { #pixel equals zero (meaning it never crosses threshold)
        y <- 99 #length(dc_list)+1
      } else {
        if (adap==1) { #preparatory phase when >5 and < 10 bad years occur
          #1. if all years are <= 5 then we return length(dc_list)+1 (pixel never damaged)
          #2. if some years are > 5 but no years are above 10 then we return length(dc_list)+1 
          #   (pixel never suffering transformation, thus no need for anticipation)
          #3. if some years are > 5 AND some years are above 10 then we return minimum date of cross
          #   as the date of preparatory phase
          if (length(which(x > 5)) == 0) { #if frequency > 5 never occurs then never crosses
            y <- 99 #length(dc_list)+1
          } else if (length(which(x > 5)) > 0) {
            if (length(which(x > 10)) == 0) { #no trans. hence no prep. but perhaps systemic
              if (length(which(x <= 5)) == 0) { #no years below 5 (i.e. all years between 5 and 10), meaning min is needed
                y <- min(which(x > 5)) #systemic occurring at beginning of period
              } else {
                y <- max(which(x <= 5))+1 #when crossed and stays below
                #systemic change pixels can be identified by knowing areas of no trans (i.e. where eq. length(dc_list)+1)
                #but there is a lower value for the prep. (in this case systemic)
              }
            } else if (length(which(x > 10)) > 0) {
              if (length(which(x > 10)) == length(x)) {
                y <- 0 #preparatory is now
              } else {
                y <- min(which(x > 5 & x <= 10)) #first time of cross is preparatory
              }
            }
          }
        } else if (adap==2) { #transformation phase when > 10 bad years occur
          #1. if all years are < 10 then we return length(dc_list)+1
          #2. else we return first date of cross
          if (length(which(x > 10)) == 0) {
            y <- 99 #length(dc_list)+1 #doesn't cross in analysis period
          } else if (length(which(x > 10)) > 0) {
            y <- min(which(x > 10)) #minimum possible date of crossing
          }
        } else {
          stop("error in value of adap")
        }
      }
      return(y)
    }
    
    #value NA is NA, 0 or < threshold in baseline
    #value >= 0 is actual frequency
    xy_all <- as.data.frame(xyFromCell(dc_out_stk, 1:ncell(dc_out_stk)))
    xy_all <- cbind(xy_all, as.data.frame(extract(dc_out_stk, xy_all[,c("x","y")])))
    
    #run calculation for all grid cells
    cross_stk <- c()
    for (adaptype in 1:2) {
      #adaptype <- 2
      cat("...calculating adaptation time=",adaptype,"\n")
      xy_all$crossval <- apply(xy_all[,grep("dec.",names(xy_all))], 1, FUN=calc_ctime, adaptype)
      crosstime_val <- data.frame(crossval=1:length(dc_list), crosstime=dc_list)
      crosstime_val <- rbind(crosstime_val,c(0,2015)) #2015 is "now" (only relevant for systemic, i.e. adap=1)
      crosstime_val <- rbind(crosstime_val,c(75,2090)) #2090 is no crossing
      crosstime_val <- rbind(crosstime_val,c(99,(max(dc_list)+1))) #2090 is no crossing
      crosstime_val <- rbind(crosstime_val,c(NA,NA))
      xy_all <- merge(xy_all, crosstime_val, by="crossval", all.x=T, all.y=F)
      names(xy_all)[ncol(xy_all)] <- paste("crosstime",adaptype,sep="")
      
      #create raster
      rs_cross <- raster(dc_out_stk)
      rs_cross[cellFromXY(rs_cross, xy_all[,c("x","y")])] <- xy_all[,paste("crosstime",adaptype,sep="")]
      
      #append into rasterStack
      cross_stk <- c(cross_stk, rs_cross)
    }
    cross_stk <- stack(cross_stk)
    
    #cross-check results
    crosscheck <- function(x,adap=1) {
      hisval <- x[1]; prepval <- x[2]; taval <- x[3]; lastval <- x[4]
      if (adap == 1) {
        if (is.na(prepval) | is.na(taval) | is.na(hisval) | is.na(lastval)) {
          y <- NA
        } else if (prepval <= taval) {
          y <- prepval
        } else if (prepval > taval) {
          if (hisval > 10 | lastval <= 5) {
            y <- NA
          } else {
            y <- taval
          }
        }
      } else if (adap == 2) {
        if (is.na(taval) | is.na(prepval) | is.na(hisval) | is.na(lastval)) {
          y <- NA
        } else if (prepval <= taval) {
          y <- taval
        } else if (prepval > taval) {
          if (hisval > 10 | lastval <= 5) {
            y <- NA
          } else {
            y <- taval
          }
        }
      }
      return(y)
    }
    
    xy_all <- as.data.frame(xyFromCell(stack(rs_freq_his,cross_stk[[1]],cross_stk[[2]],lastdec_rs), 1:ncell(cross_stk)))
    xy_all <- cbind(xy_all, as.data.frame(extract(stack(rs_freq_his,cross_stk[[1]],cross_stk[[2]],lastdec_rs), xy_all[,c("x","y")])))
    
    #run calculation for all grid cells
    cross_stk_con2 <- c()
    for (adaptype in 1:2) {
      xy_all$crossval <- apply(xy_all[,c(3:6)], 1, FUN=crosscheck, adaptype)
      names(xy_all)[ncol(xy_all)] <- paste("crosstime",adaptype,sep="")
      rs_cross <- raster(cross_stk)
      rs_cross[cellFromXY(rs_cross, xy_all[,c("x","y")])] <- xy_all[,paste("crosstime",adaptype,sep="")]
      cross_stk_con2 <- c(cross_stk_con2, rs_cross)
    }
    cross_stk_con2 <- stack(cross_stk_con2)
    
    #put into raster stack
    cross_all$cross2 <- c(cross_all$cross2, cross_stk_con2$layer.2)
    
    #remove any previous objects and load data for this GCM
    rm(list=c("cross_stk","dc_out_stk","cross_stk_con2"))
  }
  cross_all$cross2 <- stack(cross_all$cross2)
  
  calcmean <- function(x) {
    if (length(which(is.na(x))) >= round(length(x)/2)) {
      y <- NA
    } else if (length(which(x == 2090)) >= round(length(x)/2)) {
      y <- 2095
    } else if (length(which(x == 2015)) >= round(length(x)/2)) {
      y <- 2014
    } else if (length(which(x < 0)) >= round(length(x)/2)) {
      y <- NA
    } else {
      x <- x[which(x > 2015 & x < 2090)]; y <- mean(x, na.rm=T)
    }
    return(y)
  }
  
  calcmin <- function(x) {
    if (length(which(is.na(x))) >= round(length(x)/2)) {
      y <- NA
    } else if (length(which(x == 2090)) >= round(length(x)/2)) {
      y <- 2095
    } else if (length(which(x == 2015)) >= round(length(x)/2)) {
      y <- 2014
    } else if (length(which(x < 0)) >= round(length(x)/2)) {
      y <- NA
    } else {
      x <- x[which(x > 2015 & x < 2090)]; y <- min(x, na.rm=T)
    }
    return(y)
  }
  
  calcmax <- function(x) {
    if (length(which(is.na(x))) >= round(length(x)/2)) {
      y <- NA
    } else if (length(which(x >= 2089)) >= round(length(x)/2)) {
      y <- 2095
    } else if (length(which(x == 2015)) >= round(length(x)/2)) {
      y <- 2014
    } else if (length(which(x < 0)) >= round(length(x)/2)) {
      y <- NA
    } else {
      x <- x[which(x > 2015 & x < 2090)]; y <- max(x, na.rm=T)
    }
    return(y)
  }
  
  crosstime <- c(calc(cross_all[[paste("cross",ctime,sep="")]], fun=calcmin),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=calcmean),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=calcmax))
  crosstime <- stack(crosstime); names(crosstime) <- c("Earliest","Mean","Latest")
  
  #append into multi-crop list
  cross_2_all2$earliest <- c(cross_2_all2$earliest, crosstime[["Earliest"]])
  cross_2_all2$mean <- c(cross_2_all2$mean, crosstime[["Mean"]])
  cross_2_all2$latest <- c(cross_2_all2$latest, crosstime[["Latest"]])
}
cross_2_all2$earliest <- stack(cross_2_all2$earliest)
cross_2_all2$mean <- stack(cross_2_all2$mean)
cross_2_all2$latest <- stack(cross_2_all2$latest)

names(cross_2_all2$earliest) <- paste(thresh_val$crop)
names(cross_2_all2$mean) <- paste(thresh_val$crop)
names(cross_2_all2$latest) <- paste(thresh_val$crop)


### calculate substitutes for each crop and three times
#looping through ensemble values
cross_new <- list(earliest=list(), mean=list(), latest=list())
legends <- list(earliest=list(), mean=list(), latest=list())
for (uval in c("earliest","mean","latest")) {
  #uval <- "mean"
  cat("...processing",uval,"\n")
  
  #extract xy data for the calculation), non-cropped
  rs_stk <- cross_2_all[[uval]]
  xy_all <- as.data.frame(xyFromCell(cross_2_all[[uval]], 1:ncell(cross_2_all[[uval]])))
  xy_all <- cbind(xy_all, extract(cross_2_all[[uval]], xy_all[,c("x","y")]))
  
  #from no non-cropped
  rs_stk2 <- cross_2_all2[[uval]]
  xy_all2 <- as.data.frame(xyFromCell(cross_2_all2[[uval]], 1:ncell(cross_2_all2[[uval]])))
  xy_all2 <- cbind(xy_all2, extract(cross_2_all2[[uval]], xy_all2[,c("x","y")]))
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_alltemp[10301,3:ncol(xy_alltemp)])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
      x[which(x <= x_ref)] <- NA
      if (length(which(is.na(x))) == length(x)) {
        xsub <- c()
      } else {
        xsub <- which(x == max(x,na.rm=T)) #which value(s) is(are) associated with maximum
        xsub <- xsub[which(xsub != posit)] #remove its own
      }
      if (length(xsub) == 0) { #there is no substitute (all are NA, or all cross <= this time of cross)
        y <- -1
      } else if (length(xsub) == 1) { #there is only one substitute
        y <- xsub
      } else { #there is > 1 substitute
        y <- as.numeric(paste(xsub,collapse=""))
      }
    }
    return(y)
  }
  
  #need to generate a sensible categorisation of this
  crop_list <- c("BA","CA","BE","FM","GN","PM","SO","YM","MZ")
  
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #create temporary xy_all object with this crop being only-cropped, others being all-areas
    xy_alltemp <- xy_all2
    xy_alltemp[,(cid+2)] <- xy_all[,(cid+2)]
    
    #calculate substitutes
    xy_alltemp$subs <- apply(xy_alltemp[,3:ncol(xy_alltemp)], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_alltemp[,c("x","y")])] <- xy_alltemp$subs
    
    new_list <- data.frame()
    i <- 1
    for (val in sort(unique(rsx[]))) {
      #val <- unique(rsx[])[3]
      if (is.na(val)) {
        new_val <- NA
        trow <- data.frame(orig_val=val, new_val=new_val, leg=NA, ncrops=NA)
      } else if (val == -1) {
        trow <- data.frame(orig_val=val, new_val=i, leg="No Avail", ncrops=0)
        i <- i+1
      } else if (val == -2) {
        trow <- data.frame(orig_val=val, new_val=i, leg="No Adap.", ncrops=-1)
        i <- i+1
      } else {
        nc <- nchar(paste(val))
        if (nc == 1) {
          tstr <- crop_list[val]
        } else {
          tstr <- c()
          for (j in 1:nc) {
            tchar <- as.numeric(substr(paste(val),j,j))
            tstr <- c(tstr, crop_list[tchar])
          }
        }
        trow <- data.frame(orig_val=val, new_val=i, leg=paste(tstr,collapse="_"), ncrops=nc)
        i <- i+1
      }
      new_list <- rbind(new_list, trow)
    }
    
    #update values in raster
    new_rs <- rsx
    xi <- nrow(new_list[which(new_list$ncrops <= 1),])
    for (i in 1:nrow(new_list)) {
      #i <- 1
      orval <- new_list$orig_val[i]
      nwval <- new_list$new_val[i]
      ncrop <- new_list$ncrops[i]
      if (ncrop <= 1) {new_rs[which(rsx[] == orval)] <- i-xi; lastval <- nwval} else {new_rs[which(rsx[] == orval)] <- ncrop}
      if (ncrop <= 1) {cat(paste(new_list$leg[i]),"-",orval,"/",nwval,"/",i-xi,"\n")} else {cat(paste(new_list$leg[i]),"-",orval,"/",nwval,"/",ncrop,"\n")}
    }
    
    #put in output list
    cross_new[[uval]][[cid]] <- new_rs
    legends[[uval]][[cid]] <- new_list
    xy_alltemp$subs <- NULL
  }
}

cross_new$earliest <- stack(cross_new$earliest)
cross_new$mean <- stack(cross_new$mean)
cross_new$latest <- stack(cross_new$latest)
names(cross_new$earliest) <- names(cross_new$mean) <- names(cross_new$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","P millet","Sorghum","Yam","Maize")
names(legends$earliest) <- names(legends$mean) <- names(legends$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","P millet","Sorghum","Yam","Maize")
#print(legends$earliest[[1]])
#plot(cross_new$earliest[[1]])

osubs_dir <- paste(fig_dir,"/fig_substitution_",rcp,sep="")
if (!file.exists(osubs_dir)) {dir.create(osubs_dir)}
for (cname in names(cross_new$mean)) {
  if (!file.exists(paste(osubs_dir,"/mean_substitution_",cname,".tif",sep=""))) writeRaster(cross_new$mean[[cname]], paste(osubs_dir,"/mean_substitution_",cname,".tif",sep=""),format="GTiff")
  write.csv(legends$earliest[[cname]],paste(osubs_dir,"/mean_substitution_",cname,".csv",sep=""),row.names=F)
}

##### calculate areas corresponding to each crop
#looping through ensemble values
cross_tab <- list(earliest=list(), mean=list(), latest=list())
for (uval in c("earliest","mean","latest")) {
  #uval <- "mean"
  cat("...processing",uval,"\n")
  
  #extract xy data for the calculation), non-cropped
  rs_stk <- cross_2_all[[uval]]
  xy_all <- as.data.frame(xyFromCell(cross_2_all[[uval]], 1:ncell(cross_2_all[[uval]])))
  xy_all <- cbind(xy_all, extract(cross_2_all[[uval]], xy_all[,c("x","y")]))
  
  #from no non-cropped
  rs_stk2 <- cross_2_all2[[uval]]
  xy_all2 <- as.data.frame(xyFromCell(cross_2_all2[[uval]], 1:ncell(cross_2_all2[[uval]])))
  xy_all2 <- cbind(xy_all2, extract(cross_2_all2[[uval]], xy_all2[,c("x","y")]))
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_alltemp[10301,3:ncol(xy_alltemp)])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
      x[which(x <= x_ref)] <- NA
      if (length(which(is.na(x))) == length(x)) {
        xsub <- c()
      } else {
        xsub <- which(x == max(x,na.rm=T)) #which value(s) is(are) associated with maximum
        xsub <- xsub[which(xsub != posit)] #remove its own
      }
      if (length(xsub) == 0) { #there is no substitute (all are NA, or all cross <= this time of cross)
        y <- -1
      } else if (length(xsub) == 1) { #there is only one substitute
        y <- xsub
      } else { #there is > 1 substitute
        y <- as.numeric(paste(xsub,collapse=""))
      }
    }
    return(y)
  }
  
  #need to generate a sensible categorisation of this
  crop_list <- c("BA","CA","BE","FM","GN","PM","SO","YM","MZ")
  
  crop_all <- data.frame()
  for (cid in 1:length(crop_list)) {
    #cid <- 3
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #create temporary xy_all object with this crop being only-cropped, others being all-areas
    xy_alltemp <- xy_all2
    xy_alltemp[,(cid+2)] <- xy_all[,(cid+2)]
    
    #calculate substitutes
    xy_alltemp$subs <- apply(xy_alltemp[,3:ncol(xy_alltemp)], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_alltemp[,c("x","y")])] <- xy_alltemp$subs
    ntot <- length(which(!is.na(rsx[]))) - length(which(rsx[] == -2))
    
    new_list <- data.frame()
    i <- 1
    for (val in sort(unique(rsx[]))) {
      #val <- sort(unique(rsx[]))[1]
      if (is.na(val)) {
        new_val <- NA
        trow <- data.frame(orig_val=val, new_val=new_val, leg=NA, ncrops=NA, size=NA)
      } else if (val == -1) {
        nbar <- length(which(rsx[] == val)) / ntot * 100
        trow <- data.frame(orig_val=val, new_val=i, leg="No Avail.", ncrops=0, size=nbar)
        new_list <- rbind(new_list, trow)
        i <- i+1
      } else if (val == -2) {
        nbar <- length(which(rsx[] == val)) / length(which(!is.na(rsx[]))) * 100
        trow <- data.frame(orig_val=val, new_val=i, leg="No Adap.", ncrops=-1, size=nbar)
        new_list <- rbind(new_list, trow)
        i <- i+1
      } else {
        nc <- nchar(paste(val))
        if (nc == 1) {
          nbar <- length(which(rsx[] == val)) / ntot * 100
          tstr <- crop_list[val]
          trow <- data.frame(orig_val=val, new_val=i, leg=crop_list[val], ncrops=nc, size=nbar)
          new_list <- rbind(new_list, trow)
        } else {
          tstr <- c()
          for (j in 1:nc) {
            tchar <- as.numeric(substr(paste(val),j,j))
            tstr <- c(tstr, crop_list[tchar])
            nbar <- length(which(rsx[] == val)) / ntot * 100
            trow <- data.frame(orig_val=val, new_val=i, leg=crop_list[tchar], ncrops=nc, size=nbar)
            new_list <- rbind(new_list, trow)
          }
        }
        #trow <- data.frame(orig_val=val, new_val=i, leg=paste(tstr,collapse="_"), ncrops=nc, size=nbar)
        i <- i+1
      }
      #new_list <- rbind(new_list, trow)
    }
    tcrop <- cbind(crop=crop_list[cid], new_list)
    crop_all <- rbind(crop_all, tcrop)
    xy_alltemp$subs <- NULL
  }
  crop_agg <- aggregate(crop_all[,"size"], by=list(leg=crop_all$leg, crop=crop_all$crop), FUN=function(x) {sum(x, na.rm=T)})
  cross_tab[[uval]] <- crop_agg
}

###
#barplots
library(ggplot2)

crop_list <- c("BA","CA","BE","FM","GN","PM","SO","YM","MZ")
for (cid in 1:length(crop_list)) {
  #cid <- 1
  cat("   ...processing crop",crop_list[cid],"\n")
  
  #earliest / mean / latest for this crop
  earliest <- cross_tab$earliest[which(cross_tab$earliest$crop == crop_list[cid]),]
  meanval <- cross_tab$mean[which(cross_tab$mean$crop == crop_list[cid]),]
  latest <- cross_tab$latest[which(cross_tab$latest$crop == crop_list[cid]),]
  
  #get values per crop
  all_df <- data.frame()
  clist_red <- c("No Avail.", crop_list[-which(crop_list == crop_list[cid])])
  for (crp in clist_red) {
    #crp <- clist_red[1]
    tval_e <- earliest$x[which(earliest$leg == crp)]
    if (length(tval_e) == 0) tval_e <- 0
    
    tval_m <- meanval$x[which(meanval$leg == crp)]
    if (length(tval_m) == 0) tval_m <- 0
    
    tval_l <- latest$x[which(latest$leg == crp)]
    if (length(tval_l) == 0) tval_l <- 0
    
    tdf <- data.frame(crop_lost=crop_list[cid], crop_rep=crp, earliest=tval_e, mean=tval_m, latest=tval_l)
    all_df <- rbind(all_df, tdf)
  }
  
  #print figure
  tlev <- all_df$crop_rep[which(all_df$crop_rep != "No Avail.")][order(all_df$mean[which(all_df$crop_rep != "No Avail.")],decreasing=T)]
  tlev <- factor(c(paste(tlev), "No Avail."), levels=c(paste(tlev), "No Avail."))
  all_df$crop_rep <- factor(all_df$crop_rep, levels=tlev)
  all_df$std <- as.numeric(apply(all_df[,c("earliest","mean","latest")],1,FUN=function(x) {sd(x)}))
  all_df$minval <- all_df$mean - all_df$std
  all_df$maxval <- all_df$mean + all_df$std
  
  p <- ggplot(all_df, aes(x = crop_rep, y = mean)) + 
    geom_bar(width=0.99,stat="identity",size=0.5, fill="grey 70", colour="black") + 
    geom_errorbar(aes(x=crop_rep, ymin = minval, ymax = maxval), width=0.1,size=0.5) +
    scale_x_discrete("Crop replacement") + 
    scale_y_continuous("Percent area (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
    theme_bw() +
    theme(axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          axis.title.x=element_text(size=18),
          axis.title.y=element_text(size=18),
          legend.position="right",
          legend.title = element_text(size=15),
          legend.text = element_text(size=15),
          legend.key = element_rect(color="white"))
  #print(p)
  
  pdf(paste(fig_dir,"/crop_replacement_",crop_list[cid],"_",rcp,".pdf",sep=""),height=6,width=8,pointsize=16)
  print(p)
  dev.off()
  setwd(fig_dir)
  system(paste("convert -verbose -density 300 crop_replacement_",crop_list[cid],"_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crop_replacement_",crop_list[cid],"_",rcp,".png",sep=""))
  setwd("~")
}



########################################################################################
########################################################################################
########################################################################################
### calculate the same as above only for crops whose crossing time == 2095

#looping through ensemble values
cross_new <- list(earliest=list(), mean=list(), latest=list())
legends <- list(earliest=list(), mean=list(), latest=list())
for (uval in c("earliest","mean","latest")) {
  #uval <- "earliest"
  cat("...processing",uval,"\n")
  
  #extract xy data for the calculation)
  rs_stk <- cross_2_all[[uval]]
  xy_all <- as.data.frame(xyFromCell(cross_2_all[[uval]], 1:ncell(cross_2_all[[uval]])))
  xy_all <- cbind(xy_all, extract(cross_2_all[[uval]], xy_all[,c("x","y")]))
  
  #from no non-cropped
  rs_stk2 <- cross_2_all2[[uval]]
  xy_all2 <- as.data.frame(xyFromCell(cross_2_all2[[uval]], 1:ncell(cross_2_all2[[uval]])))
  xy_all2 <- cbind(xy_all2, extract(cross_2_all2[[uval]], xy_all2[,c("x","y")]))
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_all[9941,grep("\\.tif",names(xy_all))])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
      x[which(x <= x_ref)] <- NA
      xsub <- which(x == 2095) #which value(s) is(are) associated with 2095
      xsub <- xsub[which(xsub != posit)] #remove its own
      if (length(xsub) == 0) { #there is no substitute (all are NA, or all cross <= this time of cross)
        y <- -1
      } else if (length(xsub) == 1) { #there is only one substitute
        y <- xsub
      } else { #there is > 1 substitute
        y <- as.numeric(paste(xsub,collapse=""))
      }
    }
    return(y)
  }
  
  #need to generate a sensible categorisation of this
  crop_list <- c("BA","CA","BE","FM","GN","PM","SO","YM","MZ")
  
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #create temporary xy_all object with this crop being only-cropped, others being all-areas
    xy_alltemp <- xy_all2
    xy_alltemp[,(cid+2)] <- xy_all[,(cid+2)]
    
    #calculate substitutes
    xy_alltemp$subs <- apply(xy_alltemp[,3:ncol(xy_alltemp)], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_alltemp[,c("x","y")])] <- xy_alltemp$subs
    
    new_list <- data.frame()
    i <- 1
    for (val in sort(unique(rsx[]))) {
      #val <- unique(rsx[])[3]
      if (is.na(val)) {
        new_val <- NA
        trow <- data.frame(orig_val=val, new_val=new_val, leg=NA, ncrops=NA)
      } else if (val == -1) {
        trow <- data.frame(orig_val=val, new_val=i, leg="No Avail", ncrops=0)
        i <- i+1
      } else if (val == -2) {
        trow <- data.frame(orig_val=val, new_val=i, leg="No Adap.", ncrops=-1)
        i <- i+1
      } else {
        nc <- nchar(paste(val))
        if (nc == 1) {
          tstr <- crop_list[val]
        } else {
          tstr <- c()
          for (j in 1:nc) {
            tchar <- as.numeric(substr(paste(val),j,j))
            tstr <- c(tstr, crop_list[tchar])
          }
        }
        trow <- data.frame(orig_val=val, new_val=i, leg=paste(tstr,collapse="_"), ncrops=nc)
        i <- i+1
      }
      new_list <- rbind(new_list, trow)
    }
    
    #update values in raster
    new_rs <- rsx
    xi <- nrow(new_list[which(new_list$ncrops <= 1),])
    for (i in 1:nrow(new_list)) {
      #i <- 1
      orval <- new_list$orig_val[i]
      nwval <- new_list$new_val[i]
      ncrop <- new_list$ncrops[i]
      if (ncrop <= 1) {new_rs[which(rsx[] == orval)] <- i-xi; lastval <- nwval} else {new_rs[which(rsx[] == orval)] <- ncrop}
      if (ncrop <= 1) {cat(paste(new_list$leg[i]),"-",orval,"/",nwval,"/",i-xi,"\n")} else {cat(paste(new_list$leg[i]),"-",orval,"/",nwval,"/",ncrop,"\n")}
    }
    
    #put in output list
    cross_new[[uval]][[cid]] <- new_rs
    legends[[uval]][[cid]] <- new_list
    xy_alltemp$subs <- NULL
  }
}

cross_new$earliest <- stack(cross_new$earliest)
cross_new$mean <- stack(cross_new$mean)
cross_new$latest <- stack(cross_new$latest)
names(cross_new$earliest) <- names(cross_new$mean) <- names(cross_new$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","P millet","Sorghum","Yam","Maize")
names(legends$earliest) <- names(legends$mean) <- names(legends$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","P millet","Sorghum","Yam","Maize")
#print(legends$earliest[[1]])
#plot(cross_new$earliest[[1]])

osubs_dir <- paste(fig_dir,"/fig_substitution_nofail_",rcp,sep="")
if (!file.exists(osubs_dir)) {dir.create(osubs_dir)}
for (cname in names(cross_new$mean)) {
  if (!file.exists(paste(osubs_dir,"/mean_substitution_",cname,".tif",sep=""))) writeRaster(cross_new$mean[[cname]], paste(osubs_dir,"/mean_substitution_",cname,".tif",sep=""),format="GTiff")
  write.csv(legends$earliest[[cname]],paste(osubs_dir,"/mean_substitution_",cname,".csv",sep=""),row.names=F)
}

##### calculate areas corresponding to each crop
#looping through ensemble values
cross_tab <- list(earliest=list(), mean=list(), latest=list())
for (uval in c("earliest","mean","latest")) {
  #uval <- "earliest"
  cat("...processing",uval,"\n")
  
  #extract xy data for the calculation)
  rs_stk <- cross_2_all[[uval]]
  xy_all <- as.data.frame(xyFromCell(cross_2_all[[uval]], 1:ncell(cross_2_all[[uval]])))
  xy_all <- cbind(xy_all, extract(cross_2_all[[uval]], xy_all[,c("x","y")]))
  
  #from no non-cropped
  rs_stk2 <- cross_2_all2[[uval]]
  xy_all2 <- as.data.frame(xyFromCell(cross_2_all2[[uval]], 1:ncell(cross_2_all2[[uval]])))
  xy_all2 <- cbind(xy_all2, extract(cross_2_all2[[uval]], xy_all2[,c("x","y")]))
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_all[9941,grep("\\.tif",names(xy_all))])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
      x[which(x <= x_ref)] <- NA
      xsub <- which(x == 2095) #which value(s) is(are) associated with 2095
      xsub <- xsub[which(xsub != posit)] #remove its own
      if (length(xsub) == 0) { #there is no substitute (all are NA, or all cross <= this time of cross)
        y <- -1
      } else if (length(xsub) == 1) { #there is only one substitute
        y <- xsub
      } else { #there is > 1 substitute
        y <- as.numeric(paste(xsub,collapse=""))
      }
    }
    return(y)
  }
  
  #need to generate a sensible categorisation of this
  crop_list <- c("BA","CA","BE","FM","GN","PM","SO","YM","MZ")
  
  crop_all <- data.frame()
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #create temporary xy_all object with this crop being only-cropped, others being all-areas
    xy_alltemp <- xy_all2
    xy_alltemp[,(cid+2)] <- xy_all[,(cid+2)]
    
    #calculate substitutes
    xy_alltemp$subs <- apply(xy_alltemp[,3:ncol(xy_alltemp)], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_alltemp[,c("x","y")])] <- xy_alltemp$subs
    ntot <- length(which(!is.na(rsx[]))) - length(which(rsx[] == -2))
    
    new_list <- data.frame()
    i <- 1
    for (val in sort(unique(rsx[]))) {
      #val <- sort(unique(rsx[]))[1]
      if (is.na(val)) {
        new_val <- NA
        trow <- data.frame(orig_val=val, new_val=new_val, leg=NA, ncrops=NA, size=NA)
      } else if (val == -1) {
        nbar <- length(which(rsx[] == val)) / ntot * 100
        trow <- data.frame(orig_val=val, new_val=i, leg="No Avail.", ncrops=0, size=nbar)
        new_list <- rbind(new_list, trow)
        i <- i+1
      } else if (val == -2) {
        nbar <- length(which(rsx[] == val)) / length(which(!is.na(rsx[]))) * 100
        trow <- data.frame(orig_val=val, new_val=i, leg="No Adap.", ncrops=-1, size=nbar)
        new_list <- rbind(new_list, trow)
        i <- i+1
      } else {
        nc <- nchar(paste(val))
        if (nc == 1) {
          nbar <- length(which(rsx[] == val)) / ntot * 100
          tstr <- crop_list[val]
          trow <- data.frame(orig_val=val, new_val=i, leg=crop_list[val], ncrops=nc, size=nbar)
          new_list <- rbind(new_list, trow)
        } else {
          tstr <- c()
          for (j in 1:nc) {
            tchar <- as.numeric(substr(paste(val),j,j))
            tstr <- c(tstr, crop_list[tchar])
            nbar <- length(which(rsx[] == val)) / ntot * 100
            trow <- data.frame(orig_val=val, new_val=i, leg=crop_list[tchar], ncrops=nc, size=nbar)
            new_list <- rbind(new_list, trow)
          }
        }
        #trow <- data.frame(orig_val=val, new_val=i, leg=paste(tstr,collapse="_"), ncrops=nc, size=nbar)
        i <- i+1
      }
      #new_list <- rbind(new_list, trow)
    }
    tcrop <- cbind(crop=crop_list[cid], new_list)
    crop_all <- rbind(crop_all, tcrop)
    xy_alltemp$subs <- NULL
  }
  crop_agg <- aggregate(crop_all[,"size"], by=list(leg=crop_all$leg, crop=crop_all$crop), FUN=function(x) {sum(x, na.rm=T)})
  cross_tab[[uval]] <- crop_agg
}

###
#barplots
library(ggplot2)

crop_list <- c("BA","CA","BE","FM","GN","PM","SO","YM","MZ")
for (cid in 1:length(crop_list)) {
  #cid <- 1
  cat("   ...processing crop",crop_list[cid],"\n")
  
  #earliest / mean / latest for this crop
  earliest <- cross_tab$earliest[which(cross_tab$earliest$crop == crop_list[cid]),]
  meanval <- cross_tab$mean[which(cross_tab$mean$crop == crop_list[cid]),]
  latest <- cross_tab$latest[which(cross_tab$latest$crop == crop_list[cid]),]
  
  #get values per crop
  all_df <- data.frame()
  clist_red <- c("No Avail.", crop_list[-which(crop_list == crop_list[cid])])
  for (crp in clist_red) {
    #crp <- clist_red[1]
    tval_e <- earliest$x[which(earliest$leg == crp)]
    if (length(tval_e) == 0) tval_e <- 0
    
    tval_m <- meanval$x[which(meanval$leg == crp)]
    if (length(tval_m) == 0) tval_m <- 0
    
    tval_l <- latest$x[which(latest$leg == crp)]
    if (length(tval_l) == 0) tval_l <- 0
    
    tdf <- data.frame(crop_lost=crop_list[cid], crop_rep=crp, earliest=tval_e, mean=tval_m, latest=tval_l)
    all_df <- rbind(all_df, tdf)
  }
  
  #print figure
  tlev <- all_df$crop_rep[which(all_df$crop_rep != "No Avail.")][order(all_df$mean[which(all_df$crop_rep != "No Avail.")],decreasing=T)]
  tlev <- factor(c(paste(tlev), "No Avail."), levels=c(paste(tlev), "No Avail."))
  all_df$crop_rep <- factor(all_df$crop_rep, levels=tlev)
  all_df$std <- as.numeric(apply(all_df[,c("earliest","mean","latest")],1,FUN=function(x) {sd(x)}))
  all_df$minval <- all_df$mean - all_df$std
  all_df$maxval <- all_df$mean + all_df$std
  
  p <- ggplot(all_df, aes(x = crop_rep, y = mean)) + 
    geom_bar(width=0.99,stat="identity",size=0.5, fill="grey 70", colour="black") + 
    geom_errorbar(aes(x=crop_rep, ymin = minval, ymax = maxval), width=0.1,size=0.5) +
    scale_x_discrete("Crop replacement") + 
    scale_y_continuous("Percent area (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
    theme_bw() +
    theme(axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          axis.title.x=element_text(size=18),
          axis.title.y=element_text(size=18),
          legend.position="right",
          legend.title = element_text(size=15),
          legend.text = element_text(size=15),
          legend.key = element_rect(color="white"))
  #print(p)
  
  pdf(paste(fig_dir,"/crop_replacement_",crop_list[cid],"_",rcp,"_nofail.pdf",sep=""),height=6,width=8,pointsize=16)
  print(p)
  dev.off()
  setwd(fig_dir)
  system(paste("convert -verbose -density 300 crop_replacement_",crop_list[cid],"_",rcp,"_nofail.pdf -quality 100 -sharpen 0x1.0 -alpha off crop_replacement_",crop_list[cid],"_",rcp,"_nofail.png",sep=""))
  setwd("~")
}

