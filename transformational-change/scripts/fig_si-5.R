#Julian Ramirez-Villegas, Ulrike Rippke and Louis Parker
#CIAT / CCAFS / UoL
#May 2014
stop("!")

#gains in suitable area

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
#b_dir <- "Y:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_modelling"
b_dir <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_modelling"
base_run <- paste(b_dir,"/CRU_30min_1971-2000_af/analyses/cru_select_corNames",sep="")
out_dir <- paste("~/Google Drive/papers/transformational-adaptation") #mbp
fig_dir <- paste(out_dir,"/figures",sep="")
dfil_dir <- paste(out_dir,"/data_files_new",sep="")
if (!file.exists(dfil_dir)) {dir.create(dfil_dir)}

#rcp input dir
rcp <- "RCP_60_new" #RCP_60_new RCP_85
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
    
    if (!file.exists(paste(dfil_crop,"/crossinverse_",rcp,"_",gcm,".RData",sep=""))) {
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
          #x <- as.numeric(xy_all[9941,3:ncol(xy_all)])
          x_b <- x[1]
          x_d <- x[2:length(x)]
          if (is.na(x_b)) { #if pixel value is NA in baseline then NA (outside land areas)
            y <- NA
          } else if (x_b == 0 | x_b < thr) { #if baseline == 0 (unsuitable) then perform calculation
            y <- length(which(x_d < thr))
          } else if (x_b < thr) { #if baseline < threshold then perform calculation
            y <- length(which(x_d < thr))
          } else { #if not below threshold then return -1
            y <- -1 #length(which(x_d < thr))
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
      load(file=paste(dfil_crop,"/crossinverse_",rcp,"_",gcm,".RData",sep=""))
    }
  }
}
