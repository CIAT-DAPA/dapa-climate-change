#Julian Ramirez-Villegas, Ulrike Rippke and Louis Parker
#CIAT / CCAFS / UoL
#May 2014
stop("!")

### directories where model output is
#Baseline suitability simulation (using CRU)
#\\DAPADFS\workspace_cluster_6\VULNERABILITY_ANALYSIS_CC_SAM\ECOCROP_DEVELOPMENT_CC_SAM\ULI\Uli_modelling\CRU_30min_1971-2000_af\analyses\cru_select_corNames

#RCP6.0 runs
#\\DAPADFS\workspace_cluster_6\VULNERABILITY_ANALYSIS_CC_SAM\ECOCROP_DEVELOPMENT_CC_SAM\ULI\Uli_modelling\FUTURE_af\RCP_60_new\analyses\runs-future

#RCP8.5 runs
#\\DAPADFS\workspace_cluster_6\VULNERABILITY_ANALYSIS_CC_SAM\ECOCROP_DEVELOPMENT_CC_SAM\ULI\Uli_modelling\FUTURE_af\RCP_85\analyses\runs-future

#thresholds
#\\DAPADFS\workspace_cluster_6\VULNERABILITY_ANALYSIS_CC_SAM\ECOCROP_DEVELOPMENT_CC_SAM\ULI\Uli_modelling\thres_ov_short.csv

#Results of cumulated transformation
#\\DAPADFS\workspace_cluster_6\VULNERABILITY_ANALYSIS_CC_SAM\ECOCROP_DEVELOPMENT_CC_SAM\ULI\Uli_modelling\results\cumulated_transf

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

#loop through crops
for (i in 1:nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  thr <- thresh_val$threshold[i]
  cat("\n...processing crop=",crop_name,"\n")
  
  #folder of dfil_dir per crop
  dfil_crop <- paste(dfil_dir,"/",gsub("\\.tif","",crop_name),sep="")
  if (!file.exists(dfil_crop)) {dir.create(dfil_crop)}
  
  #extract data from baseline raster
  xy_allb <- as.data.frame(xyFromCell(base_stk[[crop_name]], 1:ncell(base_stk[[crop_name]])))
  xy_allb$base <- extract(base_stk[[crop_name]], xy_allb[,c("x","y")])
  
  #loop through GCMs
  for (gcm in gcm_list) {
    #gcm <- gcm_list[1]
    cat("...processing gcm=",gcm,"\n")
    
    if (!file.exists(paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))) {
      #raster stack with all years
      yr_stk <- stack(paste(rcp_run,"/",gcm,"/",yr_list,"/",crop_name,sep=""))
      
      #loop decades
      dc_out_stk <- c()
      for (dc in dc_list) {
        #dc <- dc_list[1]
        cat("...processing decade=",dc,"\n")
        
        dc_stk <- yr_stk[[(dc-10-2005):(dc+10-2006)]]
        
        #extract decade data
        xy_all <- cbind(xy_allb, as.data.frame(extract(dc_stk, xy_allb[,c("x","y")])))
        
        #function to calculate frequency of crossing
        calc_freq <- function(x, thr) {
          x_b <- x[1]
          x_d <- x[2:length(x)]
          if (is.na(x_b)) {
            y <- NA
          } else if (x_b == 0) {
            y <- -2
          } else if (x_b < thr) {
            y <- -1
          } else {
            y <- length(which(x_d < thr))# / length(x_d)
          }
          return(y)
        }
        
        #run calculation for all grid cells
        xy_all$freq <- apply(xy_all[,3:ncol(xy_all)], 1, FUN=calc_freq, thr)
        
        #put into raster
        rs_freq <- raster(dc_stk)
        rs_freq[cellFromXY(rs_freq, xy_all[,c("x","y")])] <- xy_all$freq
        
        #append to raster stack
        dc_out_stk <- c(dc_out_stk, rs_freq)
      }
      dc_out_stk <- stack(dc_out_stk)
      names(dc_out_stk) <- paste("dec.",dc_list,sep="")
      
      ###
      #calculate the two decades of interest:
      #*time1: 5-10 years below threshold
      #*time2: more than 10 years below threshold
      
      #function to calculate decade
      calc_ctime <- function(x, adap=1) {
        #x <- as.numeric(xy_all[12540,3:ncol(xy_all)])
        #plot(as.numeric(xy_all[9427,3:ncol(xy_all)]),ty="l")
        #plot(as.numeric(xy_all[12540,3:ncol(xy_all)]),ty="l")
        if (is.na(x[1])) { #pixel is orignally NA
          y <- NA
        } else if (length(which(x < 0)) > 0) { #pixel is below zero (meaning either was 0 suit, or below threshold)
          y <- -2
        } else if (length(which(x == 0)) == length(x)) { #pixel equals zero (meaning it never crosses any threshold)
          y <- length(dc_list)+1
        } else {
          if (adap==1) { #preparatory phase when >=5 and < 10 bad years occur
            #1. if all years are < 5 then we return length(dc_list)+1 (pixel never damaged)
            #2. if some years are >= 5 but no years are above 10 then we return length(dc_list)+1 
            #   (pixel never suffering transformation, thus no need for anticipation)
            #3. if some years are >= 5 AND some years are above 10 then we return minimum date of cross
            #   as the date of preparatory phase
            if (length(which(x >= 5)) == 0) {
              y <- length(dc_list)+1
            } else if (length(which(x >= 5)) > 0) {
              if (length(which(x > 10)) == 0) { #no trans. hence no prep. but perhaps systemic
                if (length(which(x < 5)) == 0) { #no years below 5 (i.e. all years between 5 and 10), meaning min is needed
                  y <- min(which(x > 5))+1 #systemic occurring at beginning of period
                } else {
                  y <- max(which(x < 5))+1 #when crossed and stays below
                  #systemic change pixels can be identified by knowing areas of no trans (i.e. where eq. length(dc_list)+1)
                  #but there is a lower value for the prep. (in this case systemic)
                }
              } else if (length(which(x > 10)) > 0) {
                if (length(which(x > 10)) == length(x)) {
                  y <- 0 #preparatory is now
                } else {
                  y <- min(which(x >= 5 & x <= 10)) #first time of cross is preparatory
                }
              }
            }
          } else if (adap==2) { #transformation phase when > 10 bad years occur
            #1. if all years are < 10 then we return length(dc_list)+1
            #2. else we return first date of cross
            if (length(which(x > 10)) == 0) {
              y <- length(dc_list)+1 #doesn't cross in analysis period
            } else if (length(which(x > 10)) > 0) {
              y <- min(which(x > 10)) #minimum possible date of crossing
            }
          } else {
            stop("error in value of adap")
          }
        }
        return(y)
      }
      
      #value NA is NA in baseline
      #value -1 is baseline = 0
      #value -2 is baseline < thr
      #value >= 0 is actual frequency
      xy_all <- as.data.frame(xyFromCell(dc_out_stk, 1:ncell(dc_out_stk)))
      xy_all <- cbind(xy_all, as.data.frame(extract(dc_out_stk, xy_all[,c("x","y")])))
      
      #run calculation for all grid cells
      cross_stk <- c()
      for (adaptype in 1:2) {
        #adaptype <- 1
        cat("...calculating adaptation time=",adaptype,"\n")
        xy_all$crossval <- apply(xy_all[,grep("dec.",names(xy_all))], 1, FUN=calc_ctime, adaptype)
        crosstime_val <- data.frame(crossval=1:length(dc_list), crosstime=dc_list)
        crosstime_val <- rbind(crosstime_val,c(-1,-1))
        crosstime_val <- rbind(crosstime_val,c(0,2015))
        crosstime_val <- rbind(crosstime_val,c((length(dc_list)+1),(max(dc_list)+1)))
        xy_all <- merge(xy_all, crosstime_val, by="crossval", all.x=T, all.y=F)
        names(xy_all)[ncol(xy_all)] <- paste("crosstime",adaptype,sep="")
        
        #create raster
        rs_cross <- raster(dc_out_stk)
        rs_cross[cellFromXY(rs_cross, xy_all[,c("x","y")])] <- xy_all[,paste("crosstime",adaptype,sep="")]
        
        #append into rasterStack
        cross_stk <- c(cross_stk, rs_cross)
      }
      cross_stk <- stack(cross_stk)
      
      #save this model output
      save(list=c("dc_out_stk","cross_stk"), file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
    } else {
      load(file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
    }
  }
}


####
#load all GCMs for each crop and:
#4. calculate minimum and maximum crossing amongst all GCMs
#5. plot minimum, maximum and ensemble mean

###
#function to plot maps
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T,colours=NA) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
    if (!is.na(colours[1])) {pal <- colours}
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
    if (!is.na(colours[1])) {pal <- colours}
  }
  if (rev) {pal <- rev(pal)}
  pal <- c("grey 30",pal,"grey 80")
  
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  
  #set theme
  this_theme <- custom.theme(fill = pal, region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}

#figure details
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=20), norths=seq(-90,90,by=20))

#loop through crops
cross_1_all <- list(earliest=c(),mean=c(),latest=c())
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
    cross_all$cross1 <- c(cross_all$cross1, cross_stk$layer.1)
    cross_all$cross2 <- c(cross_all$cross2, cross_stk$layer.2)
    
    #remove any previous objects and load data for this GCM
    rm(list=c("cross_stk","dc_out_stk"))
  }
  
  cross_all$cross1 <- stack(cross_all$cross1)
  cross_all$cross2 <- stack(cross_all$cross2)
  
  #put together the three rasters i need for each
  load(file=paste(dfil_crop,"/crossing_",rcp,"_eco_ensemble.RData",sep=""))
  
  for (ctime in 1:2) {
    #ctime <- 2
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
    
    #plot figure
    tplot <- rs_levplot2(crosstime,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                         leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
    pdf(paste(fig_dir,"/crossing_time_",ctime,"_",gsub("\\.tif","",crop_name),"_",rcp,".pdf",sep=""), height=6,width=12,pointsize=16)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 crossing_time_",ctime,"_",gsub("\\.tif","",crop_name),"_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_",ctime,"_",gsub("\\.tif","",crop_name),"_",rcp,".png",sep=""))
    setwd("~")
    
    #append into multi-crop list
    if (ctime == 1) {
      cross_1_all$earliest <- c(cross_1_all$earliest, crosstime[["Earliest"]])
      cross_1_all$mean <- c(cross_1_all$mean, crosstime[["Mean"]])
      cross_1_all$latest <- c(cross_1_all$latest, crosstime[["Latest"]])
    } else {
      cross_2_all$earliest <- c(cross_2_all$earliest, crosstime[["Earliest"]])
      cross_2_all$mean <- c(cross_2_all$mean, crosstime[["Mean"]])
      cross_2_all$latest <- c(cross_2_all$latest, crosstime[["Latest"]])
    }
  }
}

### ground adapt
#plot earliest for all crops
plot_rs <- stack(cross_1_all$earliest)
names(plot_rs) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
tplot <- rs_levplot2(plot_rs,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                     leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
pdf(paste(fig_dir,"/crossing_time_1_all-crops_earliest_",rcp,".pdf",sep=""), height=8,width=8,pointsize=16)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 crossing_time_1_all-crops_earliest_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_1_all-crops_earliest_",rcp,".png",sep=""))
setwd("~")

#plot mean for all crops
plot_rs <- stack(cross_1_all$mean)
names(plot_rs) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
tplot <- rs_levplot2(plot_rs,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                     leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
pdf(paste(fig_dir,"/crossing_time_1_all-crops_mean_",rcp,".pdf",sep=""), height=8,width=8,pointsize=16)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 crossing_time_1_all-crops_mean_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_1_all-crops_mean_",rcp,".png",sep=""))
setwd("~")


#plot latest for all crops
plot_rs <- stack(cross_1_all$latest)
names(plot_rs) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
tplot <- rs_levplot2(plot_rs,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                     leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
pdf(paste(fig_dir,"/crossing_time_1_all-crops_latest_",rcp,".pdf",sep=""), height=8,width=8,pointsize=16)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 crossing_time_1_all-crops_latest_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_1_all-crops_latest_",rcp,".png",sep=""))
setwd("~")


### transformation phase
#plot earliest for all crops
plot_rs <- stack(cross_2_all$earliest)
names(plot_rs) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
tplot <- rs_levplot2(plot_rs,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                     leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
pdf(paste(fig_dir,"/crossing_time_2_all-crops_earliest_",rcp,".pdf",sep=""), height=8,width=8,pointsize=16)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 crossing_time_2_all-crops_earliest_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_2_all-crops_earliest_",rcp,".png",sep=""))
setwd("~")

#plot mean for all crops
plot_rs <- stack(cross_2_all$mean)
names(plot_rs) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
tplot <- rs_levplot2(plot_rs,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                     leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
pdf(paste(fig_dir,"/crossing_time_2_all-crops_mean_",rcp,".pdf",sep=""), height=8,width=8,pointsize=16)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 crossing_time_2_all-crops_mean_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_2_all-crops_mean_",rcp,".png",sep=""))
setwd("~")

#plot latest for all crops
plot_rs <- stack(cross_2_all$latest)
names(plot_rs) <- c("Banana","Cassava","Bean","F millet","Groundnut","Maize","P millet","Sorghum","Yam")
tplot <- rs_levplot2(plot_rs,zn=NA,zx=NA,nb=NA,brks=c(seq(2010,2090,by=5),2095),scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,
                     leg=list(at=c(seq(2010,2090,by=5),2095),labels=c("Now",paste(seq(2015,2090,by=5)),"No Adap.")))
pdf(paste(fig_dir,"/crossing_time_2_all-crops_latest_",rcp,".pdf",sep=""), height=8,width=8,pointsize=16)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 crossing_time_2_all-crops_latest_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off crossing_time_2_all-crops_latest_",rcp,".png",sep=""))
setwd("~")


####
#for each GCM and crop need to loop through decades and calculate:
#1. percentage of pixels that go transformed, for each of the limits
#2. amount of area (projection defined using http://people.oregonstate.edu/~savricb/selectiontool/#)

crosstime_val <- data.frame(crossval=1:length(dc_list), crosstime=dc_list)
crosstime_val <- rbind(crosstime_val,c(-1,-1))
crosstime_val <- rbind(crosstime_val,c(0,2015))
crosstime_val <- rbind(crosstime_val,c((length(dc_list)+1),(max(dc_list)+1)))

if (!file.exists(paste(dfil_dir,"/cumulative_transformation_",rcp,"_with_maize.RData",sep=""))) {
  cum_chg_m <- list(); cum_chg_u <- list(); cum_chg_l <- list()
  for (i in 1:nrow(thresh_val)) {
    #i <- 1
    crop_name <- paste(thresh_val$crops[i])
    cat("\n...processing crop=",crop_name,"\n")
    
    #folder of dfil_dir per crop
    dfil_crop <- paste(dfil_dir,"/",gsub("\\.tif","",crop_name),sep="")
    
    #decadal output
    dc_out <- data.frame()
    
    if (exists("cross_stk")) rm(list=c("cross_stk","dc_out_stk"))
    for (gcm in gcm_list[-grep("eco_ensemble",gcm_list)]) {
      #gcm <- gcm_list[1]
      
      #load processed output
      load(file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
      rm(dc_out_stk)
      
      #total number of pixels in area
      crosstime <- cross_stk$layer.2
      crosstime[which(crosstime[] < 0)] <- NA
      ntotal <- length(which(!is.na(crosstime[])))
      
      #project (use projection definition from http://people.oregonstate.edu/~savricb/selectiontool/#)
      crosstime_prj <- projectRaster(crosstime, crs="+proj=aea +lon_0=22.67578125", method="ngb")
      
      #loop decades
      for (dc in 1:length(dc_list)) {
        #dc <- 1
        tdec <- crosstime_val$crosstime[crosstime_val$crossval == dc]
        ndc <- length(which(crosstime[] <= tdec)) / ntotal * 100 #use <= for cumulative areas with change
        trha <- length(which(crosstime_prj[] <= tdec)) * xres(crosstime_prj) * yres(crosstime_prj) #in m2
        trha <- trha / (100*100) #in ha
        out_row <- data.frame(GCM=gcm, DEC_ID=dc, DEC=tdec, PER.TRANS=ndc, HA.TRANS=trha)
        dc_out <- rbind(dc_out, out_row)
      }
      rm(cross_stk)
    }
    dc_out$MHA.TRANS <- dc_out$HA.TRANS / 1000000
    
    #calculate mean and s.d. for each decade
    dc_out_m <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {median(x,na.rm=T)})
    dc_out_u <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
    dc_out_l <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
    #dc_out_v <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {sd(x,na.rm=T)})
    #dc_out_x <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {max(x,na.rm=T)})
    #dc_out_n <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {min(x,na.rm=T)})
    
    #put into lists
    cum_chg_m[[crop_name]] <- dc_out_m; cum_chg_u[[crop_name]] <- dc_out_u; cum_chg_l[[crop_name]] <- dc_out_l
  }
  save(list=c("cum_chg_m","cum_chg_u","cum_chg_l"),file=paste(dfil_dir,"/cumulative_transformation_",rcp,"_with_maize.RData",sep=""))
} else {
  load(file=paste(dfil_dir,"/cumulative_transformation_",rcp,"_with_maize.RData",sep=""))
}


################################################################################################
# plot into pdfs

if (rcp == "RCP_60_new") {yx1 <- 40; yx2 <- 550; stp2 <- 50}
if (rcp == "RCP_85") {yx1 <- 70; yx2 <- 1000; stp2 <- 100}

##### roots and bananas
#relative change
pdf(paste(fig_dir,"/cumulative_transformation_roots-banana_per_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
for (i in c(1,2,9)) { #nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #get data
  dc_out_m <- cum_chg_m[[crop_name]]; dc_out_u <- cum_chg_u[[crop_name]]; dc_out_l <- cum_chg_l[[crop_name]]
  
  #plot line and polygon
  if (i == 1) {
    par(mar=c(5,5,1,1),las=1,lwd=1.5)
    plot(dc_out_m$DEC, dc_out_m$PER.TRANS, ty="l", xlim=c(2000,2095), ylim=c(0,yx1),
         xlab="Year", ylab="Extent of transformation (%)", axes=F, 
         col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
    axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
    axis(2, at=seq(0,yx1,by=5),labels=paste(seq(0,yx1,by=5)),lwd=1.5)
    box(lwd=1.5)
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 2) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 9) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
  }
}
grid(lwd=1.5)
legend(x=2000,y=yx1,legend=c("Banana","Cassava","Yams"),lty=c(1,1,1),lwd=c(2,2,2),cex=0.9,box.lwd=1.5,bg="white",
       col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
             rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255),
             rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255)))
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 cumulative_transformation_roots-banana_per_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off cumulative_transformation_roots-banana_per_",rcp,".png",sep=""))
setwd("~")


#absolute change
pdf(paste(fig_dir,"/cumulative_transformation_roots-banana_abs_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
for (i in c(1,2,9)) { #nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #get data
  dc_out_m <- cum_chg_m[[crop_name]]; dc_out_u <- cum_chg_u[[crop_name]]; dc_out_l <- cum_chg_l[[crop_name]]
  
  #plot line and polygon
  if (i == 1) {
    par(mar=c(5,5,1,1),las=1,lwd=1.5)
    plot(dc_out_m$DEC, dc_out_m$MHA.TRANS, ty="l", xlim=c(2000,2095), ylim=c(0,yx2),
         xlab="Year", ylab="Extent of transformation (Million ha)", axes=F, 
         col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
    axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
    axis(2, at=seq(0,yx2,by=stp2),labels=paste(seq(0,yx2,by=stp2)),lwd=1.5)
    box(lwd=1.5)
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 2) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 9) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
  }
}
grid(lwd=1.5)
legend(x=2000,y=yx2,legend=c("Banana","Cassava","Yams"),lty=c(1,1,1),lwd=c(2,2,2),cex=0.9,box.lwd=1.5,bg="white",
       col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
             rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255),
             rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255)))
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 cumulative_transformation_roots-banana_abs_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off cumulative_transformation_roots-banana_abs_",rcp,".png",sep=""))
setwd("~")



##### grain legumes
#relative change
pdf(paste(fig_dir,"/cumulative_transformation_legumes_per_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
for (i in c(3,5)) { #nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #get data
  dc_out_m <- cum_chg_m[[crop_name]]; dc_out_u <- cum_chg_u[[crop_name]]; dc_out_l <- cum_chg_l[[crop_name]]
  
  #plot line and polygon
  if (i == 3) {
    par(mar=c(5,5,1,1),las=1,lwd=1.5)
    plot(dc_out_m$DEC, dc_out_m$PER.TRANS*.5, ty="l", xlim=c(2000,2095), ylim=c(0,yx1),
         xlab="Year", ylab="Extent of transformation (%)", axes=F, 
         col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
    axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
    axis(2, at=seq(0,yx1,by=5),labels=paste(seq(0,yx1,by=5)),lwd=1.5)
    box(lwd=1.5)
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS*.5,rev(dc_out_u$PER.TRANS*.5),0),
            col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS*0.5, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]*0.5), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 5) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
  }
}
grid(lwd=1.5)
legend(x=2000,y=yx1,legend=c("Bean (*0.5)","Groundnut"),lty=c(1,1),lwd=c(2,2),cex=0.9,box.lwd=1.5,bg="white",
       col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
             rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255)))
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 cumulative_transformation_legumes_per_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off cumulative_transformation_legumes_per_",rcp,".png",sep=""))
setwd("~")


#absolute change
pdf(paste(fig_dir,"/cumulative_transformation_legumes_abs_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
for (i in c(3,5)) { #nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #get data
  dc_out_m <- cum_chg_m[[crop_name]]; dc_out_u <- cum_chg_u[[crop_name]]; dc_out_l <- cum_chg_l[[crop_name]]
  
  #plot line and polygon
  if (i == 3) {
    par(mar=c(5,5,1,1),las=1,lwd=1.5)
    if (rcp == "RCP_85") {
      plot(dc_out_m$DEC, dc_out_m$MHA.TRANS, ty="l", xlim=c(2000,2095), ylim=c(0,yx2),
           xlab="Year", ylab="Extent of transformation (Million ha)", axes=F, 
           col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
      axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
      axis(2, at=seq(0,yx2,by=stp2),labels=paste(seq(0,yx2,by=stp2)),lwd=1.5)
      box(lwd=1.5)
      polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
              col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
              border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
      lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    } else {
      plot(dc_out_m$DEC, dc_out_m$MHA.TRANS*.5, ty="l", xlim=c(2000,2095), ylim=c(0,yx2),
           xlab="Year", ylab="Extent of transformation (Million ha)", axes=F, 
           col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
      axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
      axis(2, at=seq(0,yx2,by=stp2),labels=paste(seq(0,yx2,by=stp2)),lwd=1.5)
      box(lwd=1.5)
      polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS*.5,rev(dc_out_u$MHA.TRANS*.5),0),
              col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
              border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
      lines(dc_out_m$DEC, dc_out_m$MHA.TRANS*.5, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]*.5), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    }
  } else if (i == 5) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
  }
}
grid(lwd=1.5)
if (rcp == "RCP_85") {
  legend(x=2000,y=yx2,legend=c("Bean","Groundnut"),lty=c(1),lwd=c(2),cex=0.9,box.lwd=1.5,bg="white",
         col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
               rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255)))
} else {
  legend(x=2000,y=yx2,legend=c("Bean (*0.5)","Groundnut"),lty=c(1),lwd=c(2),cex=0.9,box.lwd=1.5,bg="white",
         col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
               rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255)))
}
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 cumulative_transformation_legumes_abs_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off cumulative_transformation_legumes_abs_",rcp,".png",sep=""))
setwd("~")


##### cereals
#relative change
pdf(paste(fig_dir,"/cumulative_transformation_cereals_per_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
for (i in c(4,6,7,8)) { #nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #get data
  dc_out_m <- cum_chg_m[[crop_name]]; dc_out_u <- cum_chg_u[[crop_name]]; dc_out_l <- cum_chg_l[[crop_name]]
  
  #plot line and polygon
  if (i == 4) {
    par(mar=c(5,5,1,1),las=1,lwd=1.5)
    plot(dc_out_m$DEC, dc_out_m$PER.TRANS, ty="l", xlim=c(2000,2095), ylim=c(0,yx1),
         xlab="Year", ylab="Extent of transformation (%)", axes=F, 
         col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
    axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
    axis(2, at=seq(0,yx1,by=5),labels=paste(seq(0,yx1,by=5)),lwd=1.5)
    box(lwd=1.5)
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 6) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 7) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 8) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$PER.TRANS,rev(dc_out_u$PER.TRANS),0),
            col=rgb(red=255,green=165,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=165,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$PER.TRANS, col=rgb(red=255,green=165,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$PER.TRANS[1]), lty=2, col=rgb(red=255,green=165,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  }
}
grid(lwd=1.5)
legend(x=2000,y=yx1,legend=c("F. millet","Maize","P. millet","Sorghum"),lty=c(1,1,1,1),lwd=c(2,2,2,2),cex=0.9,box.lwd=1.5,bg="white",
       col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
             rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255),
             rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255),
             rgb(red=255,green=165,blue=0,alpha=255,maxColorValue=255)))
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 cumulative_transformation_cereals_per_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off cumulative_transformation_cereals_per_",rcp,".png",sep=""))
setwd("~")


#absolute change
pdf(paste(fig_dir,"/cumulative_transformation_cereals_abs_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
for (i in c(4,6,7,8)) { #nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #get data
  dc_out_m <- cum_chg_m[[crop_name]]; dc_out_u <- cum_chg_u[[crop_name]]; dc_out_l <- cum_chg_l[[crop_name]]
  
  #plot line and polygon
  if (i == 4) {
    par(mar=c(5,5,1,1),las=1,lwd=1.5)
    plot(dc_out_m$DEC, dc_out_m$MHA.TRANS, ty="l", xlim=c(2000,2095), ylim=c(0,yx2),
         xlab="Year", ylab="Extent of transformation (Million ha)", axes=F, 
         col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
    axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
    axis(2, at=seq(0,yx2,by=stp2),labels=paste(seq(0,yx2,by=stp2)),lwd=1.5)
    box(lwd=1.5)
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 6) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 7) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255),
            border=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
  } else if (i == 8) {
    polygon(x=c(c(2000,dc_out_m$DEC),rev(c(2000,dc_out_m$DEC))),y=c(0,dc_out_l$MHA.TRANS,rev(dc_out_u$MHA.TRANS),0),
            col=rgb(red=255,green=165,blue=0,alpha=50,maxColorValue=255),
            border=rgb(red=255,green=165,blue=0,alpha=50,maxColorValue=255))
    lines(dc_out_m$DEC, dc_out_m$MHA.TRANS, col=rgb(red=255,green=165,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    lines(c(2000,dc_out_m$DEC[1]), c(0,dc_out_m$MHA.TRANS[1]), lty=2, col=rgb(red=255,green=165,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
  }
}
grid(lwd=1.5)
legend(x=2000,y=yx2,legend=c("F. millet","Maize","P. millet","Sorghum"),lty=c(1,1,1),lwd=c(2,2,2),cex=0.9,box.lwd=1.5,bg="white",
       col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
             rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255),
             rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255),
             rgb(red=255,green=165,blue=0,alpha=255,maxColorValue=255)))
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 cumulative_transformation_cereals_abs_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off cumulative_transformation_cereals_abs_",rcp,".png",sep=""))
setwd("~")


##########################################################################################
##########################################################################################
### for each crop cumulated transformation and spread for top producing countries

#table of countries and raster of world
ctry_list <- wrld_simpl@data
wrld_rs <- rasterize(wrld_simpl, base_stk)
wrld_rs_prj <- projectRaster(wrld_rs, crs="+proj=aea +lon_0=22.67578125", method="ngb")

#read in table of top-5 per crop
top5_list <- read.table(paste(out_dir,"/top_5_producers.tab",sep=""),sep="\t",header=T)

#reset figure configuration
if (rcp == "RCP_60_new") {yx1 <- 50}
if (rcp == "RCP_85") {yx1 <- 70}

cum_chg_m <- list(); cum_chg_u <- list(); cum_chg_l <- list()
for (i in 1:nrow(thresh_val)) {
  #i <- 4
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #folder of dfil_dir per crop
  dfil_crop <- paste(dfil_dir,"/",gsub("\\.tif","",crop_name),sep="")
  
  #list of countries for crop
  c_list <- paste(t(top5_list[which(top5_list$Crop_name == crop_name),paste("Top_",1:5,sep="")]))
  
  #decadal output
  dc_out <- data.frame()
  
  if (exists("cross_stk")) rm(list=c("cross_stk","dc_out_stk"))
  for (gcm in gcm_list[-grep("eco_ensemble",gcm_list)]) {
    #gcm <- gcm_list[1]
    
    #load processed output
    load(file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
    rm(dc_out_stk)
    
    #total number of pixels in area
    crosstime <- cross_stk$layer.2
    crosstime[which(crosstime[] < 0)] <- NA
    
    #project (use projection definition from http://people.oregonstate.edu/~savricb/selectiontool/#)
    crosstime_prj <- projectRaster(crosstime, crs="+proj=aea +lon_0=22.67578125", method="ngb")
    
    #loop countries
    for (ctry in c_list) {
      #ctry <- c_list[1]
      ctry_id <- which(ctry_list$ISO3 == ctry)
      c_crosstime <- raster(crosstime); c_crosstime_prj <- raster(crosstime_prj)
      
      xy_ctry <- xyFromCell(wrld_rs, which(wrld_rs[] == ctry_id))
      c_crosstime[cellFromXY(c_crosstime, xy_ctry)] <- crosstime[cellFromXY(crosstime, xy_ctry)]
      
      xy_ctry_prj <- xyFromCell(wrld_rs_prj, which(wrld_rs_prj[] == ctry_id))
      c_crosstime_prj[cellFromXY(c_crosstime_prj, xy_ctry_prj)] <- crosstime_prj[cellFromXY(crosstime_prj, xy_ctry_prj)]
      
      ntotal <- max(c(0,length(which(!is.na(c_crosstime[])))))
      #loop decades
      for (dc in 1:length(dc_list)) {
        #dc <- 1
        tdec <- crosstime_val$crosstime[crosstime_val$crossval == dc]
        ndc <- length(which(c_crosstime[] <= tdec)) / ntotal * 100 #use <= for cumulative areas with change
        trha <- length(which(c_crosstime_prj[] <= tdec)) * xres(c_crosstime_prj) * yres(c_crosstime_prj) #in m2
        trha <- trha / (100*100) #in ha
        out_row <- data.frame(COUNTRY=ctry,GCM=gcm, DEC_ID=dc, DEC=tdec, PER.TRANS=ndc, HA.TRANS=trha)
        dc_out <- rbind(dc_out, out_row)
      }
    }
    rm(cross_stk)
  }
  dc_out$MHA.TRANS <- dc_out$HA.TRANS / 1000000
  
  #calculate mean and s.d. for each decade
  dc_out_m <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC,COUNTRY=dc_out$COUNTRY), FUN=function(x) {median(x,na.rm=T)})
  dc_out_u <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC,COUNTRY=dc_out$COUNTRY), FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
  dc_out_l <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC,COUNTRY=dc_out$COUNTRY), FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
  #dc_out_v <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {sd(x,na.rm=T)})
  #dc_out_x <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {max(x,na.rm=T)})
  #dc_out_n <- aggregate(dc_out[,c("PER.TRANS","HA.TRANS","MHA.TRANS")],by=list(DEC=dc_out$DEC), FUN=function(x) {min(x,na.rm=T)})
  
  #put into lists
  cum_chg_m[[crop_name]] <- dc_out_m; cum_chg_u[[crop_name]] <- dc_out_u; cum_chg_l[[crop_name]] <- dc_out_l
  
  #plots here
  pdf(paste(fig_dir,"/top5_cumulative_transformation_",gsub("\\.tif","",crop_name),"_rel_",rcp,".pdf",sep=""), height=7.5,width=9,pointsize=18)
  for (ctry in c_list) {
    #ctry <- c_list[1]
    ctry_out_m <- dc_out_m[which(dc_out_m$COUNTRY == ctry),]
    ctry_out_l <- dc_out_l[which(dc_out_l$COUNTRY == ctry),]
    ctry_out_u <- dc_out_u[which(dc_out_u$COUNTRY == ctry),]
    
    if (i == 3 | i == 4) {
      ctry_out_m$PER.TRANS <- ctry_out_m$PER.TRANS * 0.5
      ctry_out_l$PER.TRANS <- ctry_out_l$PER.TRANS * 0.5
      ctry_out_u$PER.TRANS <- ctry_out_u$PER.TRANS * 0.5
    }
    
    
    if (ctry == c_list[1]) {
      par(mar=c(5,5,1,1),las=1,lwd=1.5)
      plot(ctry_out_m$DEC, ctry_out_m$PER.TRANS, ty="l", xlim=c(2000,2095), ylim=c(0,yx1),
           xlab="Year", ylab="Extent of transformation (%)", axes=F, 
           col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255))
      axis(1, at=seq(2000,2090,by=10),labels=paste(seq(2000,2090,by=10)),lwd=1.5)
      axis(2, at=seq(0,yx1,by=5),labels=paste(seq(0,yx1,by=5)),lwd=1.5)
      box(lwd=1.5)
      polygon(x=c(c(2000,ctry_out_m$DEC),rev(c(2000,ctry_out_m$DEC))),y=c(0,ctry_out_l$PER.TRANS,rev(ctry_out_u$PER.TRANS),0),
              col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),
              border=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
      lines(ctry_out_m$DEC, ctry_out_m$PER.TRANS, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,ctry_out_m$DEC[1]), c(0,ctry_out_m$PER.TRANS[1]), lty=2, col=rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    } else if (ctry == c_list[2]) {
      polygon(x=c(c(2000,ctry_out_m$DEC),rev(c(2000,ctry_out_m$DEC))),y=c(0,ctry_out_l$PER.TRANS,rev(ctry_out_u$PER.TRANS),0),
              col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),
              border=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255))
      lines(ctry_out_m$DEC, ctry_out_m$PER.TRANS, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,ctry_out_m$DEC[1]), c(0,ctry_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    } else if (ctry == c_list[3]) {
      polygon(x=c(c(2000,ctry_out_m$DEC),rev(c(2000,ctry_out_m$DEC))),y=c(0,ctry_out_l$PER.TRANS,rev(ctry_out_u$PER.TRANS),0),
              col=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255),
              border=rgb(red=0,green=150,blue=150,alpha=50,maxColorValue=255))
      lines(ctry_out_m$DEC, ctry_out_m$PER.TRANS, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,ctry_out_m$DEC[1]), c(0,ctry_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255), lwd=1.5)
    } else if (ctry == c_list[4]) {
      polygon(x=c(c(2000,ctry_out_m$DEC),rev(c(2000,ctry_out_m$DEC))),y=c(0,ctry_out_l$PER.TRANS,rev(ctry_out_u$PER.TRANS),0),
              col=rgb(red=255,green=150,blue=0,alpha=50,maxColorValue=255),
              border=rgb(red=255,green=150,blue=0,alpha=50,maxColorValue=255))
      lines(ctry_out_m$DEC, ctry_out_m$PER.TRANS, col=rgb(red=255,green=150,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,ctry_out_m$DEC[1]), c(0,ctry_out_m$PER.TRANS[1]), lty=2, col=rgb(red=255,green=150,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    } else if (ctry == c_list[4]) {
      polygon(x=c(c(2000,ctry_out_m$DEC),rev(c(2000,ctry_out_m$DEC))),y=c(0,ctry_out_l$PER.TRANS,rev(ctry_out_u$PER.TRANS),0),
              col=rgb(red=255,green=150,blue=0,alpha=50,maxColorValue=255),
              border=rgb(red=255,green=150,blue=0,alpha=50,maxColorValue=255))
      lines(ctry_out_m$DEC, ctry_out_m$PER.TRANS, col=rgb(red=255,green=150,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,ctry_out_m$DEC[1]), c(0,ctry_out_m$PER.TRANS[1]), lty=2, col=rgb(red=255,green=150,blue=0,alpha=255,maxColorValue=255), lwd=1.5)
    } else if (ctry == c_list[5]) {
      polygon(x=c(c(2000,ctry_out_m$DEC),rev(c(2000,ctry_out_m$DEC))),y=c(0,ctry_out_l$PER.TRANS,rev(ctry_out_u$PER.TRANS),0),
              col=rgb(red=0,green=150,blue=255,alpha=50,maxColorValue=255),
              border=rgb(red=0,green=150,blue=255,alpha=50,maxColorValue=255))
      lines(ctry_out_m$DEC,ctry_out_m$PER.TRANS, col=rgb(red=0,green=150,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
      lines(c(2000,ctry_out_m$DEC[1]), c(0,ctry_out_m$PER.TRANS[1]), lty=2, col=rgb(red=0,green=150,blue=255,alpha=255,maxColorValue=255), lwd=1.5)
    }
  }
  grid(lwd=1.5)
  legend(x=2000,y=yx1,legend=c_list,lty=rep(1,5),lwd=rep(3,5),cex=1,box.lwd=2,bg="white",ncol=2,
         col=c(rgb(red=255,green=0,blue=0,alpha=255,maxColorValue=255),
               rgb(red=0,green=0,blue=255,alpha=255,maxColorValue=255),
               rgb(red=0,green=150,blue=150,alpha=255,maxColorValue=255),
               rgb(red=255,green=150,blue=0,alpha=255,maxColorValue=255),
               rgb(red=0,green=150,blue=255,alpha=255,maxColorValue=255)))
  dev.off()
  setwd(fig_dir)
  system(paste("convert -verbose -density 300 top5_cumulative_transformation_",gsub("\\.tif","",crop_name),"_rel_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off top5_cumulative_transformation_",gsub("\\.tif","",crop_name),"_rel_",rcp,".png",sep=""))
  setwd("~")
}




