#Julian Ramirez-Villegas and Ulrike Rippke
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
b_dir <- "Y:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_modelling"
base_run <- paste(b_dir,"/CRU_30min_1971-2000_af/analyses/cru_select_corNames",sep="")
out_dir <- paste("D:/transformational-adaptation")
fig_dir <- paste(out_dir,"/figures",sep="")
dfil_dir <- paste(out_dir,"/data_files.old",sep="")
if (!file.exists(dfil_dir)) {dir.create(dfil_dir)}

#rcp input dir
rcp <- "RCP_60_new" #RCP_85_new
rcp_run <- paste(b_dir,"/FUTURE_af/",rcp,"/analyses/runs-future",sep="")

#read in thresholds and crop names
thresh_val <- read.csv(paste(b_dir,"/thres_ov_short.csv",sep=""))

#load baseline suitability rasters
base_stk <- stack(paste(base_run,"/",thresh_val$crops,sep=""))
names(base_stk) <- paste(thresh_val$crops)

#list of GCMs
gcm_list <- list.files(rcp_run)

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
              y <- min(which(x > 10))
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
        crosstime_val <- rbind(crosstime_val,c(-1,-1)); crosstime_val <- rbind(crosstime_val,c((length(dc_list)+1),(max(dc_list)+1)))
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
    pal <- c("grey 50",colorRampPalette(c(col_i,col_f))(ncol))
    if (!is.na(colours[1])) {pal <- colours}
  }
  if (rev) {pal <- rev(pal)}
  
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

i <- 1
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
  #ctime <- 1
  crosstime <- c(calc(cross_all[[paste("cross",ctime,sep="")]], fun=function(x){if (length(which(is.na(x))) == length(x)) {return(NA)} else {return(min(x,na.rm=T))}}),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=function(x){if (length(which(is.na(x))) == length(x)) {return(NA)} else {return(round(mean(x,na.rm=T)))}}),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=function(x){if (length(which(is.na(x))) == length(x)) {return(NA)} else {return(max(x,na.rm=T))}}))
  crosstime <- stack(crosstime); names(crosstime) <- c("Earliest","Mean","Latest")
  crosstime[which(crosstime[] < 0)] <- NA
  
  #plot figure
  tplot <- rs_levplot2(crosstime,zn=2010,zx=2090,nb=16,brks=NA,scale="RdYlGn",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
  pdf(paste(fig_dir,"/crossing_time_",ctime,"_",crop_name,".pdf",sep=""), height=5,width=10,pointsize=16)
  print(tplot)
  dev.off()
}



####
#for each GCM and crop need to loop through decades and calculate:
#1. percentage of pixels that go transformed, for each of the limits
#2. amount of area (need to define projection)





