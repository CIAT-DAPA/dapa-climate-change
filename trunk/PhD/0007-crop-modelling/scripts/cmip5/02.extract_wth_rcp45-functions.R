#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

########################################################
#wrapper function for parallel processing
########################################################
wrapper_RCP45_extract <- function(i) {
  #libraries
  library(raster); library(ncdf)
  
  #sourcing needed functions
  source(paste(src.dir,"/scripts/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #output gridcell data dir
  cDataDir <- paste(base_dir,"/climate-data/gridcell-data",sep="")
  outDir <- paste(cDataDir,"/IND_RCP45",sep="")
  if (!file.exists(outDir)) {dir.create(outDir)}
  
  #load GCM characteristics
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
  
  #load cell details
  cropName <- "gnut"
  all_cells <- read.csv(paste(base_dir,"/GLAM/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
  
  #get the indian extent
  drs <- raster(paste(src.dir2,"/data/mask.tif",sep=""))
  drs[which(!is.na(drs[]))] <- 1
  
  #extract the data for a given GCM
  od <- CMIP5_extract(cells=all_cells,cChars=gcmChars,dum_rs=drs,i=i,yi=ys,yf=ye,oDir=outDir)
}



# #loop through ensembles
# for (ens in ens_list) {
#   #ens <- ens_list[1]
#   cat("\nprocessing ensemble",paste(ens),"of model",paste(gcm),"\n")
#   this_ens <- this_gcm[which(this_gcm$Ensemble == ens),]
#   
#   #create directory of ensemble
#   odir_ens <- paste(odir_gcm,"/",ens,sep="")
#   if (!file.exists(odir_ens)) {dir.create(odir_ens)}
#   
#   #list variables
#   vn_list <- c("pr","tasmin","tasmax","rsds")
#   
#   #loop through variables
#   for (vn in vn_list) {
#     #vn <- vn_list[1]
#     cat("variable:",vn,"\n")
#     
#     #output variable directory
#     odir_var <- paste(odir_ens,"/",vn,sep="")
#     if (!file.exists(odir_var)) {dir.create(odir_var)}
#     
#     flist <- list.files(odir_var,pattern="\\.csv")
#     if (length(flist) != nrow(cells)) {
#       #loop through years
#       yrc <- 1
#       for (year in yi:yf) {
#         #year <- 2020
#         cat("\nyear:",year,"\n")
#         
#         #input directory
#         yr_dir <- paste(gcm_dir,"/",gcm,"/",ens,"/",vn,"_",year,sep="")
#         #list of nc files in the year folder (could be monthly e.g. srad, or daily)
#         nc_list <- list.files(yr_dir,pattern="\\.nc")
#         
#         #check if there is data for that year
#         if (!file.exists(yr_dir) | length(nc_list) == 0) {
#           #there is either no data or no folder for that year and variable, 
#           #so need to generate dummy monthly files with all values being NA
#           #this would be done only if the object does not exist (i.e. if it was not
#           #created by a previous year)
#           if (!exists("odf_all")) {
#             odf_all <- list()
#             for (cell in cells$CELL) {
#               #create a matrix where to put all data, in the form that i need
#               odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=13))
#               names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste("MONTH",1:12,sep=""))
#             }
#           }
#           
#           #put the year to a value to the cell values
#           for (cell in cells$CELL) {
#             odf_all[[paste("c",cell,sep="")]][yrc,"YEAR"] <- year
#           }
#         } else {
#           #there is data, either monthly or daily, so it will be extracted
#           if (length(nc_list) == 12) {
#             #loop cells in here and create bits of a list for each if this object did not exist
#             if (!exists("odf_all")) {
#               odf_all <- list()
#               for (cell in cells$CELL) {
#                 #create a matrix where to put all data, in the form that i need
#                 odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=13))
#                 names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste("MONTH",1:12,sep=""))
#               }
#             }
#             
#             #we're dealing with monthly files, hence the daily date.grid seems not necessary
#             dg <- data.frame(MONTH=1:12)
#             dg$MTH.STR <- 1:12
#             dg$MTH.STR[which(dg$MONTH < 10)] <- paste("0",dg$MONTH[which(dg$MONTH < 10)],sep="")
#             dg$YRDATA <- NA
#             dg$VARDATA <- NA
#             names(dg)[length(names(dg))] <- paste(vn)
#             
#             #organise the file list just to make sure that it will load in the
#             #proper order
#             cat("sorting file list\n")
#             for (mFile in ncList) {
#               mth <- gsub(gcm,"",mFile)
#               mth <- gsub(paste("_",ens,"_",sep=""),"",mth)
#               mth <- gsub(paste(year,"_",sep=""),"",mth)
#               mth <- gsub("\\.nc","",mth)
#               mth <- unlist(strsplit(mth,"_",fixed=T))[2]
#               dg$YRDATA[which(dg$MTH.STR == mth)] <- mFile
#             }
#           } else {
#             #we're dealing with daily data
#             #loop cells in here and create bits of a list for each
#             if (!exists("odf_all")) {
#               odf_all <- list()
#               for (cell in cells$CELL) {
#                 #create a matrix where to put all data, in the form that i need
#                 odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=367))
#                 names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste(1:366))
#               }
#             }
#             
#             #list of daily nc files in the year folder
#             day_list <- list.files(yr_dir,pattern="\\.nc")
#             
#             #this bit is to fix something weird going on with the GFDL datasets
#             #those were adding an additional 2 to the month name and the extension
#             #hence a file for month 01 would be named as *_mth_012_day_*.nc2, for
#             #some unknown reason
#             day_list2 <- list.files(yr_dir,pattern="\\.nc2")
#             if (length(day_list2) != 0) {
#               setwd(yr_dir)
#               system("rename .nc2 .nc *.nc2")
#               setwd(odir_gcm)
#               day_list <- list.files(yr_dir,pattern="\\.nc")
#             }
#             
#             #check which calendar is used
#             wlp <- this_ens$has_leap[1]
#             
#             #calendar to fit data into
#             dg <- createDateGridCMIP5(year,whatLeap=wlp)
#             dg$YRDATA <- NA
#             dg$VARDATA <- NA
#             names(dg)[length(names(dg))] <- paste(vn)
#             
#             #organise the file list just to make sure that it will load in the
#             #proper order
#             cat("sorting file list\n")
#             for (day_file in day_list) {
#               mth <- gsub(gcm,"",day_file)
#               mth <- gsub(paste("_",ens,"_",sep=""),"",mth)
#               mth <- gsub(paste(year,"_",sep=""),"",mth)
#               mth <- gsub("\\.nc","",mth)
#               day <- unlist(strsplit(mth,"_",fixed=T))[4]
#               mth <- unlist(strsplit(mth,"_",fixed=T))[2]
#               if (nchar(mth) == 3) {mth <- substr(mth,1,2)}
#               dg$YRDATA[which(dg$MTH.STR == mth & dg$DAY.STR == day)] <- day_file
#             }
#           }
#           
#           cat("loading and sorting out data\n")
#           
#           oday_list <- dg$YRDATA
#           
#           #count if there are NAs in the list of files
#           wna <- which(is.na(oday_list))
#           if (length(wna) > 0) {
#             #there are some NAs, so the days need to be looped, so to ensure a proper extraction
#             #this needs to produce a data frame that is similar to the output of the extract
#             #command in raster
#             
#             all_vals <- matrix(nrow=nrow(cells),ncol=nrow(dg))
#             for (day in 1:nrow(dg)) {
#               dfil <- dg$YRDATA[day]
#               #if the file for that day is not NA then load the raster
#               if (!is.na(dfil)) {
#                 rsd <- raster(paste(yrDir,"/",dfil,sep=""),varname=vn)
#                 rsd <- rotate(rsd)
#                 rsd <- crop(rsd,dum_rs)
#                 rsd <- resample(rsd,dum_rs,method="ngb")
#                 
#                 #flux to mm | K to C | w/m2 to MJ/m2
#                 if (vn == "pr") {
#                   rsd <- rsd*3600*24
#                 } else if (vn == "tasmin") {
#                   rsd <- rsd - 273.15
#                 } else if (vn == "tasmax") {
#                   rsd <- rsd - 273.15
#                 } else if (vn == "rsds") {
#                   #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
#                   rsd <- rsd * 24 * 3600 / 1000000
#                 }
#                 dvals <- extract(rsd,cbind(X=cells$X,Y=cells$Y))
#                 all_vals[,day] <- as.numeric(dvals)
#               }
#             }
#             
#           } else {
#             #load whole year as a raster stack
#             rstk <- stack(paste(yr_dir,"/",oday_list,sep=""),varname=vn)
#             
#             #rotate and crop whole raster stack
#             rstk <- rotate(rstk)
#             rstk <- crop(rstk,dum_rs)
#             
#             #here i need to resample this raster file to 1x1d resolution 
#             #so that the data can be nicely extracted
#             
#             #i use nearest neighbour in order to maintain the original GCM spatial
#             #variation (i.e. coarse cells), but still make it comparable to my original
#             #GLAM runs
#             rstk <- resample(rstk,dum_rs,method="ngb")
#             
#             #flux to mm | K to C | w/m2 to MJ/m2
#             if (vn == "pr") {
#               rstk <- rstk*3600*24
#             } else if (vn == "tasmin") {
#               rstk <- rstk - 273.15
#             } else if (vn == "tasmax") {
#               rstk <- rstk - 273.15
#             } else if (vn == "rsds") {
#               #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
#               rstk <- rstk * 24 * 3600 / 1000000
#             }
#             
#             cat("extracting data for gridcells\n")
#             #extract value of all cells
#             all_vals <- extract(rstk,cbind(X=cells$X,Y=cells$Y))
#           }
#           
#           cat("formatting output data.frame\n")
#           ccnt <- 1
#           for (cell in cells$CELL) {
#             #put this into the year's matrix
#             dg[,paste(vn)] <- as.numeric(all_vals[ccnt,])
#             
#             out_row <- c(year,dg[,paste(vn)])
#             if (length(nc_list) == 12) {
#               out_row <- c(out_row)
#             } else {
#               if (length(out_row) < 367) {
#                 out_row <- c(out_row,rep(NA,times=(367-length(out_row))))
#               }
#             }
#             
#             odf_all[[paste("c",cell,sep="")]][yrc,] <- out_row
#             ccnt <- ccnt+1
#           }
#         }
#         yrc <- yrc+1
#       }
#       
#       #here loop again to write the output
#       cat("writing output files\n")
#       for (cell in cells$CELL) {
#         write.csv(odf_all[[paste("c",cell,sep="")]],paste(odir_var,"/cell-",cell,".csv",sep=""),row.names=F,quote=F)
#       }
#     } else {
#       cat("variable",vn,"was already processed\n")
#     }
#     if (exists("odf_all")) {rm(odf_all); g=gc(); rm(g)}
#   } #end loop of variables
# } #end loop of ensembles








