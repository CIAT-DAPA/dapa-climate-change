#Julian Ramirez-Villegas
#June 2012
#CIAT / CCAFS / UoL


#function to write data that were gathered from a GLAM run
write_all_data <- function(gdata,cells,run_setup,varnames,msk,vid=8) {
  require(raster)
  
  #output directories
  out_gdir <- run_setup$OUT_GDIR
  out_xdir <- paste(out_gdir,"/exp-",run_setup$EXPID,sep="")
  if (!file.exists(out_xdir)) {dir.create(out_xdir)}
  
  gded_dir <- paste(out_xdir,"/gridded",sep="")
  if (!file.exists(gded_dir)) {dir.create(gded_dir)}
  
  tbed_dir <- paste(out_xdir,"/tables",sep="")
  if (!file.exists(tbed_dir)) {dir.create(tbed_dir)}
  
  #cells that are not to be used because of little harvested area
  cellNo <- cells$CELL[which(cells$AHRATIO<.2)]
  vname <- paste(varnames$EOS[vid]) #get variable name
  
  cat("writing tables...\n")
  #write table data
  if (run_setup$POT) {
    write.csv(gdata$IRR,paste(tbed_dir,"/pot_",tolower(vname),"_irr.csv",sep=""),row.names=F,quote=T)
    write.csv(gdata$RFD,paste(tbed_dir,"/pot_",tolower(vname),"_rfd.csv",sep=""),row.names=F,quote=T)
    write.csv(gdata$BTH,paste(tbed_dir,"/pot_",tolower(vname),"_bth.csv",sep=""),row.names=F,quote=T)
  } else {
    write.csv(gdata$IRR,paste(tbed_dir,"/frm_",tolower(vname),"_irr.csv",sep=""),row.names=F,quote=T)
    write.csv(gdata$RFD,paste(tbed_dir,"/frm_",tolower(vname),"_rfd.csv",sep=""),row.names=F,quote=T)
    write.csv(gdata$BTH,paste(tbed_dir,"/frm_",tolower(vname),"_bth.csv",sep=""),row.names=F,quote=T)
  }
  
  #write yearly gridded data
  cat("writing gridded data...\n")
  for (yr in 1966:1993) {
    #names of output raster
    if (run_setup$POT) {
      irr_name <- paste(gded_dir,"/pot_",yr,"_",tolower(vname),"_irr.tif",sep="")
      rfd_name <- paste(gded_dir,"/pot_",yr,"_",tolower(vname),"_rfd.tif",sep="")
      bth_name <- paste(gded_dir,"/pot_",yr,"_",tolower(vname),"_bth.tif",sep="")
    } else {
      irr_name <- paste(gded_dir,"/frm_",yr,"_",tolower(vname),"_irr.tif",sep="")
      rfd_name <- paste(gded_dir,"/frm_",yr,"_",tolower(vname),"_rfd.tif",sep="")
      bth_name <- paste(gded_dir,"/frm_",yr,"_",tolower(vname),"_bth.tif",sep="")
    }
    
    #write irrigation data
    if (!file.exists(irr_name)) {
      irr <- raster(msk)
      irr[gdata$IRR$CELL] <- gdata$IRR[,paste("Y",yr,sep="")]
      irr[cellNo] <- NA
      irr <- writeRaster(irr,irr_name,format="GTiff",overwrite=F)
    }
    
    #write rainfed data
    if (!file.exists(rfd_name)) {
      rfd <- raster(msk)
      rfd[gdata$RFD$CELL] <- gdata$RFD[,paste("Y",yr,sep="")]
      rfd[cellNo] <- NA
      rfd <- writeRaster(rfd,rfd_name,format="GTiff",overwrite=F)
    }
    
    #write combined data
    if (!file.exists(bth_name)) {
      bth <- raster(msk)
      bth[gdata$RFD$CELL] <- gdata$RFD[,paste("Y",yr,sep="")]
      bth[cellNo] <- NA
      bth <- writeRaster(bth,bth_name,format="GTiff",overwrite=F)
    }
  }
  return(out_xdir)
}

#get whole grid data fro a given variable (set by vid)
get_grid_data <- function(run_setup,cells,irr_dir,varnames,vid=8) {
  
  #get irrigation data
  cat("extracting irrigation data...\n")
  require(raster)
  irr_stk <- stack(paste(irr_dir,"/raw-",1966:1993,".asc",sep=""))
  irr_frc <- as.data.frame(extract(irr_stk,cbind(X=cells$X,Y=cells$Y)))
  names(irr_frc) <- paste("Y",1966:1993,sep="")
  irr_frc <- cbind(CELL=cells$CELL,irr_frc)
  
  #loop gridcells
  cat("retrieving gridcell data...\n")
  for (i in 1:nrow(cells)) {
    #i <- 1
    cll <- cells$CELL[i]
    x <- cells$X[i]
    y <- cells$Y[i]
    
    #get cell data
    cell_data <- get_cell_data(cell=cll,vid=vid,cells=cells,irr_data=irr_frc,
                               varnames=varnames,run_setup=setup)
    
    #transpose
    irr <- data.frame(cll,x,y,t(as.numeric(cell_data$VALUE_IRR)))
    names(irr) <- c("CELL","X","Y",paste("Y",1966:1993,sep=""))
    
    rfd <- data.frame(cll,x,y,t(as.numeric(cell_data$VALUE_RFD)))
    names(rfd) <- c("CELL","X","Y",paste("Y",1966:1993,sep=""))
    
    bth <- data.frame(cll,x,y,t(as.numeric(cell_data$VALUE)))
    names(bth) <- c("CELL","X","Y",paste("Y",1966:1993,sep=""))
    
    if (i == 1) {
      irr_all <- irr
      rfd_all <- rfd
      bth_all <- bth
    } else {
      irr_all <- rbind(irr_all,irr)
      rfd_all <- rbind(rfd_all,rfd)
      bth_all <- rbind(bth_all,bth)
    }
  }
  
  #return only one object
  data_list <- list(IRR=irr_all,RFD=rfd_all,BTH=bth_all)
  return(data_list)
}



#function to get data for a given cell
get_cell_data <- function(cell,vid,cells,irr_data,varnames,run_setup) {
  exp_dir <- run_setup$EXP_DIR
  crop_long <- run_setup$CROP_LONG
  pot <- run_setup$POT
  
  vname <- paste(varnames$EOS[vid])
  #cat("cell",cell,"-- retrieving data for variable",vname,"\n")
  
  #extract irrigation data
  irr_vls <- irr_data[which(irr_data$CELL == cell),paste("Y",1966:1993,sep="")]
  irr_vls <- as.numeric(irr_vls)
  irr_vls <- data.frame(YEAR=1966:1993,IRATIO=irr_vls)
  irr_vls$IRATIO[which(is.na(irr_vls$IRATIO))] <- 0
  irr_vls$IRATIO[which(irr_vls$IRATIO > 1)] <- 1
  
  #set out files (rfd, irr)
  if (!pot) {
    rfd_fil <- paste(exp_dir,"/gridcells/fcal_",cell,"/",tolower(crop_long),"_RFD.out",sep="")
    irr_fil <- paste(exp_dir,"/gridcells/fcal_",cell,"/",tolower(crop_long),"_IRR.out",sep="")
  } else {
    rfd_fil <- paste(exp_dir,"/gridcells/fcal_",cell,"/ygp_1/",tolower(crop_long),"_RFD.out",sep="")
    irr_fil <- paste(exp_dir,"/gridcells/fcal_",cell,"/ygp_1/",tolower(crop_long),"_IRR.out",sep="")
  }
  
  #read out files (rfd, irr), and set names
  if (sum(irr_vls$IRATIO,na.rm=T)>0) {
    irr <- read.table(irr_fil,sep="\t"); names(irr) <- varnames$EOS
    rfd <- read.table(rfd_fil,sep="\t"); names(rfd) <- varnames$EOS
    
    #capture values for all years (rfd and irr) and calculate weighted average
    all_vals <- data.frame(YEAR=irr$YEAR,VALUE_IRR=irr[,vname],VALUE_RFD=rfd[,vname],IRATIO=irr_vls$IRATIO)
  } else {
    rfd <- read.table(rfd_fil,sep="\t"); names(rfd) <- varnames$EOS
    
    #capture values for all years (rfd and irr) and calculate weighted average
    all_vals <- data.frame(YEAR=rfd$YEAR,VALUE_IRR=0,VALUE_RFD=rfd[,vname],IRATIO=irr_vls$IRATIO)
  }
  
  #calculate weighted average
  all_vals$VALUE <- all_vals$VALUE_IRR * all_vals$IRATIO + all_vals$VALUE_RFD*(1-all_vals$IRATIO)
  return(all_vals)
}


