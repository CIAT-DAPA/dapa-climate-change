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

#i/o directories
#mbp at CIAT
#b_dir <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_modelling"
#base_run <- paste(b_dir,"/CRU_30min_1971-2000_af/analyses/cru_select_corNames",sep="")

#mbp at UoL
b_dir <- "/nfs/a101/earjr/cul-de-sacs"
base_run <- paste(b_dir,"/cru_select_corNames",sep="")

#output dirs
out_dir <- paste("~/Google Drive/papers/transformational-adaptation") #mbp
fig_dir <- paste(out_dir,"/figures",sep="")
dfil_dir <- paste(out_dir,"/data_files_new",sep="")
if (!file.exists(dfil_dir)) {dir.create(dfil_dir)}

#rcp input dir
rcp <- "RCP_85" #RCP_60_new RCP_85
rcp_run <- paste(b_dir,"/FUTURE_af/",rcp,"/analyses/runs-future",sep="")

#read in thresholds and crop names
if (!file.exists(paste(dfil_dir,"/thres_compl.csv",sep=""))) {
  thresh_val <- read.csv(paste(b_dir,"/thres_compl.csv",sep=""))
  write.csv(thresh_val, paste(dfil_dir,"/thres_compl.csv",sep=""))
} else {
  thresh_val <- read.csv(paste(dfil_dir,"/thres_compl.csv",sep=""))
}


#load baseline suitability rasters
base_stk <- stack(paste(base_run,"/",thresh_val$crops,sep=""))
names(base_stk) <- paste(thresh_val$crops)

#list of GCMs
gcm_list <- list.files(rcp_run)
#gcm_list <- list.files(paste(dfil_dir,"/Maize_suit",sep=""),pattern=rcp)
#gcm_list <- gsub(paste("crossing_",rcp,"_",sep=""),"",gcm_list)
#gcm_list <- gsub(".RData","",gcm_list)

#list of years and decades
yr_list <- c(2006:2098)
dc_list <- c((min(yr_list)+10):(max(yr_list)-9))

#### load crossing time data (for transformational i.e. ctime==2)
#loop through crops
ctime <- 2
cross_2_all <- list(earliest=c(),mean=c(),latest=c())
for (i in 1:nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #folder of dfil_dir per crop
  dfil_crop <- paste(dfil_dir,"/",gsub("\\.tif","",crop_name),sep="")
  
  if (exists("cross_stk")) rm(list=c("cross_stk","dc_out_stk"))
  cross_all <- list(cross1=c(),cross2=c())
  for (gcm in gcm_list[-grep("eco_ensemble",gcm_list)]) {
    #gcm <- gcm_list[1]
    
    #load processed output
    load(file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
    
    #put into raster stack
    cross_all$cross2 <- c(cross_all$cross2, cross_stk$layer.2)
    
    #remove any previous objects and load data for this GCM
    rm(list=c("cross_stk","dc_out_stk"))
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

names(cross_2_all$earliest) <- paste(thresh_val$crops)
names(cross_2_all$mean) <- paste(thresh_val$crops)
names(cross_2_all$latest) <- paste(thresh_val$crops)

### calculate substitutes for each crop and three times
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
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_all[9941,])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    x[which(x <= x_ref)] <- NA
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
      xsub <- which(x == max(x,na.rm=T)) #which value(s) is(are) associated with maximum
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
  crop_list <- c("BA","CA","BE","FM","GN","MZ","PM","SO","YM")
  
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #calculate substitutes
    xy_all$subs <- apply(xy_all[,grep("\\.tif",names(xy_all))], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_all[,c("x","y")])] <- xy_all$subs
    
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
  }
}

cross_new$earliest <- stack(cross_new$earliest)
cross_new$mean <- stack(cross_new$mean)
cross_new$latest <- stack(cross_new$latest)
names(cross_new$earliest) <- names(cross_new$mean) <- names(cross_new$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
names(legends$earliest) <- names(legends$mean) <- names(legends$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
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
  #uval <- "earliest"
  cat("...processing",uval,"\n")
  
  #extract xy data for the calculation)
  rs_stk <- cross_2_all[[uval]]
  xy_all <- as.data.frame(xyFromCell(cross_2_all[[uval]], 1:ncell(cross_2_all[[uval]])))
  xy_all <- cbind(xy_all, extract(cross_2_all[[uval]], xy_all[,c("x","y")]))
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_all[9941,])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    x[which(x <= x_ref)] <- NA
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
      xsub <- which(x == max(x,na.rm=T)) #which value(s) is(are) associated with maximum
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
  crop_list <- c("BA","CA","BE","FM","GN","MZ","PM","SO","YM")
  
  crop_all <- data.frame()
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #calculate substitutes
    xy_all$subs <- apply(xy_all[,grep("\\.tif",names(xy_all))], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_all[,c("x","y")])] <- xy_all$subs
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
          trow <- data.frame(orig_val=val, new_val=i, leg=crop_list[tchar], ncrops=nc, size=nbar)
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
  }
  crop_agg <- aggregate(crop_all[,"size"], by=list(leg=crop_all$leg, crop=crop_all$crop), FUN=function(x) {sum(x, na.rm=T)})
  cross_tab[[uval]] <- crop_agg
}

###
#barplots
library(ggplot2)

crop_list <- c("BA","CA","BE","FM","GN","MZ","PM","SO","YM")
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
  
  p <- ggplot(all_df, aes(x = crop_rep, y = mean)) + 
    geom_bar(width=0.99,stat="identity",size=0.5, fill="grey 70", colour="black") + 
    #geom_errorbar(aes(x=crop_rep, ymin = latest, ymax = earliest), width=0.1,size=0.5) +
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
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_all[9941,grep("\\.tif",names(xy_all))])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    x[which(x <= x_ref)] <- NA
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
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
  crop_list <- c("BA","CA","BE","FM","GN","MZ","PM","SO","YM")
  
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #calculate substitutes
    xy_all$subs <- apply(xy_all[,grep("\\.tif",names(xy_all))], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_all[,c("x","y")])] <- xy_all$subs
    
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
  }
}

cross_new$earliest <- stack(cross_new$earliest)
cross_new$mean <- stack(cross_new$mean)
cross_new$latest <- stack(cross_new$latest)
names(cross_new$earliest) <- names(cross_new$mean) <- names(cross_new$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
names(legends$earliest) <- names(legends$mean) <- names(legends$latest) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
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
  
  #function to calculate substitute for each crop
  calc_subs <- function(x, posit=1) {
    #x <- as.numeric(xy_all[9941,])
    x <- round(x,0) #[3:length(x)]
    x_ref <- x[posit]
    x[which(x <= x_ref)] <- NA
    if (is.na(x_ref)) { #pixel is unsuitable current time
      y <- NA
    } else if (x_ref == 2095) {
      y <- -2 #no substitution needed, as the crop never crosses threshold
    } else {
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
  crop_list <- c("BA","CA","BE","FM","GN","MZ","PM","SO","YM")
  
  crop_all <- data.frame()
  for (cid in 1:length(crop_list)) {
    #cid <- 1
    cat("   ...processing crop",crop_list[cid],"\n")
    
    #calculate substitutes
    xy_all$subs <- apply(xy_all[,grep("\\.tif",names(xy_all))], 1, calc_subs, cid)
    rsx <- raster(rs_stk)
    rsx[cellFromXY(rsx, xy_all[,c("x","y")])] <- xy_all$subs
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
          trow <- data.frame(orig_val=val, new_val=i, leg=crop_list[tchar], ncrops=nc, size=nbar)
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
  }
  crop_agg <- aggregate(crop_all[,"size"], by=list(leg=crop_all$leg, crop=crop_all$crop), FUN=function(x) {sum(x, na.rm=T)})
  cross_tab[[uval]] <- crop_agg
}

###
#barplots
library(ggplot2)

crop_list <- c("BA","CA","BE","FM","GN","MZ","PM","SO","YM")
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
  
  p <- ggplot(all_df, aes(x = crop_rep, y = mean)) + 
    geom_bar(width=0.99,stat="identity",size=0.5, fill="grey 70", colour="black") + 
    #geom_errorbar(aes(x=crop_rep, ymin = latest, ymax = earliest), width=0.1,size=0.5) +
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

