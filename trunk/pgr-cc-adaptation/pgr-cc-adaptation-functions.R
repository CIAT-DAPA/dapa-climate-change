#Julian Ramirez-Villegas
#Jan 2012
#functions for PGR paper


#function to get data from RData of a given grid cell
get_loc_adapt <- function(loc) {
  cell <- gCells$LOC[loc]
  lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
  rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
  wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
  sorg <- gCells$SORG[loc]
  
  #cat("loc",loc,"\n")
  
  #location of file in chunks of 10k files. File number limitation
  if (loc <= 10000) {
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  
  if (file.exists(oDatFile)) {
    load(oDatFile)
    if (rice == 1) {
      rice1_buf <- output$RICE$ADAPTATION_2035$BUF
      rice1_ctr <- output$RICE$ADAPTATION_2035$CTR
      rice1_glo <- output$RICE$ADAPTATION_2035$GLO
      rice2_buf <- output$RICE$ADAPTATION_2075$BUF
      rice2_ctr <- output$RICE$ADAPTATION_2075$CTR
      rice2_glo <- output$RICE$ADAPTATION_2075$GLO
    } else {
      rice1_buf <- NA; rice1_ctr <- NA; rice1_glo <- NA
      rice2_buf <- NA; rice2_ctr <- NA; rice2_glo <- NA
    }
    
    if (wspr == 1) {
      wspr1_buf <- output$WSPR$ADAPTATION_2035$BUF
      wspr1_ctr <- output$WSPR$ADAPTATION_2035$CTR
      wspr1_glo <- output$WSPR$ADAPTATION_2035$GLO
      wspr2_buf <- output$WSPR$ADAPTATION_2075$BUF
      wspr2_ctr <- output$WSPR$ADAPTATION_2075$CTR
      wspr2_glo <- output$WSPR$ADAPTATION_2075$GLO
    } else {
      wspr1_buf <- NA; wspr1_ctr <- NA; wspr1_glo <- NA
      wspr2_buf <- NA; wspr2_ctr <- NA; wspr2_glo <- NA
    }
    
    if (wwin == 1) {
      wwin1_buf <- output$WWIN$ADAPTATION_2035$BUF
      wwin1_ctr <- output$WWIN$ADAPTATION_2035$CTR
      wwin1_glo <- output$WWIN$ADAPTATION_2035$GLO
      wwin2_buf <- output$WWIN$ADAPTATION_2075$BUF
      wwin2_ctr <- output$WWIN$ADAPTATION_2075$CTR
      wwin2_glo <- output$WWIN$ADAPTATION_2075$GLO
    } else {
      wwin1_buf <- NA; wwin1_ctr <- NA; wwin1_glo <- NA
      wwin2_buf <- NA; wwin2_ctr <- NA; wwin2_glo <- NA
    }
    
    if (mill == 1) {
      mill1_buf <- output$MILL$ADAPTATION_2035$BUF
      mill1_ctr <- output$MILL$ADAPTATION_2035$CTR
      mill1_glo <- output$MILL$ADAPTATION_2035$GLO
      mill2_buf <- output$MILL$ADAPTATION_2075$BUF
      mill2_ctr <- output$MILL$ADAPTATION_2075$CTR
      mill2_glo <- output$MILL$ADAPTATION_2075$GLO
    } else {
      mill1_buf <- NA; mill1_ctr <- NA; mill1_glo <- NA
      mill2_buf <- NA; mill2_ctr <- NA; mill2_glo <- NA
    }
    
    if (sorg == 1) {
      sorg1_buf <- output$SORG$ADAPTATION_2035$BUF
      sorg1_ctr <- output$SORG$ADAPTATION_2035$CTR
      sorg1_glo <- output$SORG$ADAPTATION_2035$GLO
      sorg2_buf <- output$SORG$ADAPTATION_2075$BUF
      sorg2_ctr <- output$SORG$ADAPTATION_2075$CTR
      sorg2_glo <- output$SORG$ADAPTATION_2075$GLO
    } else {
      sorg1_buf <- NA; sorg1_ctr <- NA; sorg1_glo <- NA
      sorg2_buf <- NA; sorg2_ctr <- NA; sorg2_glo <- NA
    }
    rm(output)
    
    #outputs
    out_res <- data.frame(LOC=cell,RICE1_BUF=rice1_buf,RICE1_CTR=rice1_ctr,RICE1_GLO=rice1_glo,
                          RICE2_BUF=rice2_buf,RICE2_CTR=rice2_ctr,RICE2_GLO=rice2_glo, 
                          WSPR1_BUF=wspr1_buf,WSPR1_CTR=wspr1_ctr,WSPR1_GLO=wspr1_glo,
                          WSPR2_BUF=wspr2_buf,WSPR2_CTR=wspr2_ctr,WSPR2_GLO=wspr2_glo,
                          WWIN1_BUF=wwin1_buf,WWIN1_CTR=wwin1_ctr,WWIN1_GLO=wwin1_glo,
                          WWIN2_BUF=wwin2_buf,WWIN2_CTR=wwin2_ctr,WWIN2_GLO=wwin2_glo,
                          MILL1_BUF=mill1_buf,MILL1_CTR=mill1_ctr,MILL1_GLO=mill1_glo,
                          MILL2_BUF=mill2_buf,MILL2_CTR=mill2_ctr,MILL2_GLO=mill2_glo,
                          SORG1_BUF=sorg1_buf,SORG1_CTR=sorg1_ctr,SORG1_GLO=sorg1_glo,
                          SORG2_BUF=sorg2_buf,SORG2_CTR=sorg2_ctr,SORG2_GLO=sorg2_glo)
  } else {
    out_res <- data.frame(LOC=cell,RICE1_BUF=NA,RICE1_CTR=NA,RICE1_GLO=NA,
                          RICE2_BUF=NA,RICE2_CTR=NA,RICE2_GLO=NA, 
                          WSPR1_BUF=NA,WSPR1_CTR=NA,WSPR1_GLO=NA,
                          WSPR2_BUF=NA,WSPR2_CTR=NA,WSPR2_GLO=NA,
                          WWIN1_BUF=NA,WWIN1_CTR=NA,WWIN1_GLO=NA,
                          WWIN2_BUF=NA,WWIN2_CTR=NA,WWIN2_GLO=NA,
                          MILL1_BUF=NA,MILL1_CTR=NA,MILL1_GLO=NA,
                          MILL2_BUF=NA,MILL2_CTR=NA,MILL2_GLO=NA,
                          SORG1_BUF=NA,SORG1_CTR=NA,SORG1_GLO=NA,
                          SORG2_BUF=NA,SORG2_CTR=NA,SORG2_GLO=NA)
  }
  return(out_res)
}



#calculate adaptation ranges for a grid cell
calc_adapt_gridcell <- function(loc) {
  library(raster); library(sfsmisc); library(maptools)
  source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))
  
  #dump everything to scratch
  gcmScratch <- paste(scratch,"/",gcm,"_ENS_",ens,sep="")
  dumpCheck <- paste(gcmScratch,"/dump.tmp",sep="")
  if (!file.exists(dumpCheck)) {
    cat("dumping outputs to scratch\n")
    setwd(scratch)
    system(paste("cp -rf ",gcm_outDir," .",sep=""))
    ff <- file(dumpCheck,"w")
    cat("dumped\n",file=ff)
    close(ff)
  }
  
  #get details
  allCells <- gCells
  cell <- gCells$LOC[loc]; cid <- gCells$CID[loc]
  lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
  rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
  wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
  sorg <- gCells$SORG[loc]
  
  cat("processing loc",loc,"cell=",cell,"(gcm_i=",gcm_i,")","\n")
  
  #update RData file
  if (loc <= 10000) {
    datFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    datFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    datFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  load(datFile)
  
  if (is.null(output$ADAPTATION)) {
    rm(output)
    if (wspr == 1) {
      out_wspr1 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="WSPR",2035,gcm_outDir,gcmScratch)
      out_wspr2 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="WSPR",2075,gcm_outDir,gcmScratch)
    } else {
      out_wspr1 <- NA
      out_wspr2 <- NA
    }
    
    if (wwin == 1) {
      out_wwin1 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="WWIN",2035,gcm_outDir,gcmScratch)
      out_wwin2 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="WWIN",2075,gcm_outDir,gcmScratch)
    } else {
      out_wwin1 <- NA
      out_wwin2 <- NA
    }
    
    if (rice == 1) {
      out_rice1 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="RICE",2035,gcm_outDir,gcmScratch)
      out_rice2 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="RICE",2075,gcm_outDir,gcmScratch)
    } else {
      out_rice1 <- NA
      out_rice2 <- NA
    }
    
    if (sorg == 1) {
      out_sorg1 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="SORG",2035,gcm_outDir,gcmScratch)
      out_sorg2 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="SORG",2075,gcm_outDir,gcmScratch)
    } else {
      out_sorg1 <- NA
      out_sorg2 <- NA
    }
    
    if (mill == 1) {
      out_mill1 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="MILL",2035,gcm_outDir,gcmScratch)
      out_mill2 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="MILL",2075,gcm_outDir,gcmScratch)
    } else {
      out_mill1 <- NA
      out_mill2 <- NA
    }
    
    #load previous output
    load(datFile)
    
    #append adaptation results to previous output
    output$RICE$ADAPTATION_2035 <- out_rice1
    output$RICE$ADAPTATION_2075 <- out_rice2
    output$WSPR$ADAPTATION_2035 <- out_wspr1
    output$WSPR$ADAPTATION_2075 <- out_wspr2
    output$WWIN$ADAPTATION_2035 <- out_wwin1
    output$WWIN$ADAPTATION_2075 <- out_wwin2
    output$MILL$ADAPTATION_2035 <- out_mill1
    output$MILL$ADAPTATION_2075 <- out_mill2
    output$SORG$ADAPTATION_2035 <- out_sorg1
    output$SORG$ADAPTATION_2075 <- out_sorg2
    
    #switch adaptation is done
    output$ADAPTATION <- T
    
    #update RData file
    save(output,file=datFile)
    rm(output)
  } else if (is.null(output$ADAPTATION_v2)) {
    rm(output)
    if (sorg == 1) {
      out_sorg1 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="SORG",2035,gcm_outDir,gcmScratch)
      out_sorg2 <- search_overlaps_novel(loc,lon,lat,cid,allCells,crop_name="SORG",2075,gcm_outDir,gcmScratch)
    } else {
      out_sorg1 <- NA
      out_sorg2 <- NA
    }
    
    #load previous output
    load(datFile)
    
    #put data into list
    output$SORG$ADAPTATION_2035 <- out_sorg1
    output$SORG$ADAPTATION_2075 <- out_sorg2
    
    #switch adaptation is done
    output$ADAPTATION_v2 <- T
    
    #update RData file
    save(output,file=datFile)
    rm(output)
  } else {
    rm(output)
  }
}


#search within the three neighborhoods for similarity in climates
search_overlaps_novel <- function(loc,lon,lat,cid,allCells,crop_name,period,gcm_outDir,gcmScratch) {
  #location of file in chunks of 10k files. File number limitation
  if (loc <= 10000) {
    datFile <- paste(gcmScratch,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    datFile <- paste(gcmScratch,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    datFile <- paste(gcmScratch,"/part_3/loc_",loc,".RData",sep="")
  }
  
  #load the future non-overlapped distribution
  load(datFile)
  gsData <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$GS_DATA
  qlims <- as.numeric(quantile(gsData$CRU,probs=c(0.01,0.99)))
  cru_in <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$PDF_CRU
  cru_in <- cru_in[which(cru_in$x >= qlims[1] & cru_in$x <= qlims[2]),]
  
  del_ot <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$PDF_DEL
  del_ot <- del_ot[which(del_ot$x > max(cru_in$x)),]
  rm(output)
  
  #test plotting
  #plot(cru_in$x,cru_in$y,ty="l",xlim=c(0,35))
  #lines(del_ot$x,del_ot$y,ty="l",col="blue")
  #abline(v=min(del_ot$x))
  #lines(t_cru_in$x,t_cru_in$y,col="red")
  
  if (nrow(del_ot) > 1) {
    #remove testing location
    allCells <- allCells[-loc,]
    
    #choose locations within 250 km
    allCells$DIST <- pointDistance(c(lon,lat),cbind(x=allCells$LON,y=allCells$LAT),longlat=T)/1000
    this_cells <- allCells[which(allCells$DIST <= 250 & allCells[,crop_name] == 1),]
    if (nrow(this_cells) == 0) {
      xval_buf <- 0
    } else {
      #here test for location how much overlap there is
      a_loc <- as.numeric(row.names(this_cells))
      xval_buf <- sapply(a_loc,calc_novel_overlap,del_ot,gcm_outDir,crop_name,period,gcmScratch)
      xval_buf <- max(xval_buf,na.rm=T)
    }
    
    #choose locations within country
    this_cells <- allCells[which(allCells$CID == cid & allCells[,crop_name] == 1),]
    if (nrow(this_cells) == 0) {
      xval_ctr <- 0
    } else {
      #here test for location how much overlap there is
      a_loc <- as.numeric(row.names(this_cells))
      if (length(a_loc)>100) {a_loc <- sample(a_loc,100)}
      xval_ctr <- sapply(a_loc,calc_novel_overlap,del_ot,gcm_outDir,crop_name,period,gcmScratch)
      xval_ctr <- max(xval_ctr,na.rm=T)
    }
    
    #choose locations within globe for crop
    this_cells <- allCells[which(allCells[,crop_name] == 1),]
    if (nrow(this_cells) == 0) {
      xval_glo <- 0
    } else {
      #here test for location how much overlap there is
      a_loc <- as.numeric(row.names(this_cells))
      #test use random sample to reduce computing time
      a_loc <- sample(a_loc,round(length(a_loc)*.005,0))
      
      xval_glo <- sapply(a_loc,calc_novel_overlap,del_ot,gcm_outDir,crop_name,period,gcmScratch)
      xval_glo <- max(xval_glo,na.rm=T)
    }
  } else {
    xval_buf <- 1
    xval_ctr <- 1
    xval_glo <- 1
  }
  r_df <- data.frame(BUF=xval_buf,CTR=xval_ctr,GLO=xval_glo)
  return(r_df)
}


#calculate overlap between novel part of a distribution and another distribution
calc_novel_overlap <- function(this_loc,novel_pdf,gcm_outDir,crop_name,period,gcmScratch) {
  #1. load output of location
  #location of file in chunks of 10k files. File number limitation
  if (this_loc <= 10000) {
    tl_datFile <- paste(gcmScratch,"/part_1/loc_",this_loc,".RData",sep="")
  } else if (this_loc > 10000 & this_loc <= 20000) {
    tl_datFile <- paste(gcmScratch,"/part_2/loc_",this_loc,".RData",sep="")
  } else if (this_loc > 20000) {
    tl_datFile <- paste(gcmScratch,"/part_3/loc_",this_loc,".RData",sep="")
  }
  load(tl_datFile)
  
  #2. calculate 1-99% CRU PDF
  t_gsData <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$GS_DATA
  t_qlims <- as.numeric(quantile(t_gsData$CRU,probs=c(0.01,0.99)))
  t_cru_in <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$PDF_CRU
  t_cru_in <- t_cru_in[which(t_cru_in$x >= t_qlims[1] & t_cru_in$x <= t_qlims[2]),]
  rm(output)
  
  #3. overlap with novel_pdf
  t_ovr <- novel_pdf[which(novel_pdf$x <= max(t_cru_in$x) & novel_pdf$x >= min(t_cru_in$x)),]
  
  #4. return overlap value
  if (nrow(t_ovr) > 1) {
    t_oa <- integrate.xy(t_ovr$x,t_ovr$y)
  } else {
    t_oa <- 0
  }
  novel_a <- integrate.xy(novel_pdf$x,novel_pdf$y)
  t_frac <- t_oa/novel_a
  return(t_frac)
}


###plotting
plot_overlap <- function(cropname, period, xt=extent(-130,160,-45,75), fig_dir) {
  #load world simpl
  require(maptools); data(wrld_simpl)
  
  #general plot characteristics
  rs <- out_rs[[toupper(cropname)]][[toupper(period)]]
  rs <- crop(rs,xt)
  
  ht <- 1000
  fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
  wt <- ht*(fct+.1)
  wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
  grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=30), norths=seq(-90,90,by=30))
  grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)
  
  
  ############################################
  brks <- seq(0,1,by=0.025)
  brks.lab <- round(brks,2)
  cols <- c(colorRampPalette(c("dark red","red","orange","yellow","light blue","blue","dark blue"))(length(brks)))
  layt <- list(wld,grli)
  
  #do the plot
  figName <- paste(tolower(cropname),"_",tolower(period),sep="")
  tiffName <- paste(fig_dir,"/",figName,".tif",sep="")
  tiff(tiffName,res=300,compression="lzw",height=ht,width=wt)
  x <- spplot(rs,sp.layout=layt,col.regions=cols,
             par.settings=list(fontsize=list(text=8)),
             at=brks,pretty=brks)
  print(x)
  dev.off()
}



#function to get data from RData of a given grid cell
get_loc_fraction <- function(loc) {
  cell <- gCells$LOC[loc]
  lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
  rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
  wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
  sorg <- gCells$SORG[loc]
  
  #location of file in chunks of 10k files. File number limitation
  if (loc <= 10000) {
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  
  if (file.exists(oDatFile)) {
    load(oDatFile)
    if (rice == 1) {
      rice1 <- output$RICE$OVERLAP_2035$FRACTION
      rice2 <- output$RICE$OVERLAP_2075$FRACTION
    } else {
      rice1 <- NA
      rice2 <- NA
    }
    
    if (wspr == 1) {
      wspr1 <- output$WSPR$OVERLAP_2035$FRACTION
      wspr2 <- output$WSPR$OVERLAP_2075$FRACTION
    } else {
      wspr1 <- NA
      wspr2 <- NA
    }
    
    if (wwin == 1) {
      wwin1 <- output$WWIN$OVERLAP_2035$FRACTION
      wwin2 <- output$WWIN$OVERLAP_2075$FRACTION
    } else {
      wwin1 <- NA
      wwin2 <- NA
    }
    
    if (mill == 1) {
      mill1 <- output$MILL$OVERLAP_2035$FRACTION
      mill2 <- output$MILL$OVERLAP_2075$FRACTION
    } else {
      mill1 <- NA
      mill2 <- NA
    }
    
    if (sorg == 1) {
      sorg1 <- output$SORG$OVERLAP_2035$FRACTION
      sorg2 <- output$SORG$OVERLAP_2075$FRACTION
    } else {
      sorg1 <- NA
      sorg2 <- NA
    }
    rm(output)
    
    #outputs
    out_res <- data.frame(LOC=cell,RICE1=rice1,RICE2=rice2,WSPR1=wspr1,WSPR2=wspr2,
                          WWIN1=wwin1,WWIN2=wwin2,MILL1=mill1,MILL2=mill2,SORG1=sorg1,
                          SORG2=sorg2)
  } else {
    out_res <- data.frame(LOC=cell,RICE1=NA,RICE2=NA,WSPR1=NA,WSPR2=NA,
                          WWIN1=NA,WWIN2=NA,MILL1=NA,MILL2=NA,SORG1=NA,SORG2=NA)
  }
  return(out_res)
}


#check if adaptation processes are done
check_done_adapt <- function(loc) {
  #location of file in chunks of 10k files. File number limitation
  cell <- gCells$LOC[loc]
  if (loc <= 10000) {
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  #add to list if not exists
  load(oDatFile)
  if (is.null(output$ADAPTATION)) {
    fStatus <- F
  } else {
    fStatus <- output$ADAPTATION
  }
  return(fStatus)
}


#check if processes are done
check_done <- function(loc) {
  #location of file in chunks of 10k files. File number limitation
  cell <- gCells$LOC[loc]
  if (loc <= 10000) {
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  #add to list if not exists
  fStatus <- file.exists(oDatFile)
  return(fStatus)
}


#function to analyse a given grid cell
analyse_gridcell <- function(loc) {
  library(raster); library(sfsmisc); library(rgdal); library(sp)
  source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))
  #loc <- 1
  #details
  cell <- gCells$LOC[loc]
  lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
  rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
  wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
  sorg <- gCells$SORG[loc]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("processing grid cell",cell,"(",loc,"of",nrow(gCells),")\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  #location of file in chunks of 10k files. File number limitation
  if (loc <= 10000) {
    if (!file.exists(paste(gcm_outDir,"/part_1",sep=""))) {dir.create(paste(gcm_outDir,"/part_1",sep=""))}
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
    bDatFile <- paste(out_bDir,"/bcc-csm1-1-m_ENS_r1i1p1/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    if (!file.exists(paste(gcm_outDir,"/part_2",sep=""))) {dir.create(paste(gcm_outDir,"/part_2",sep=""))}
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
    bDatFile <- paste(out_bDir,"/bcc-csm1-1-m_ENS_r1i1p1/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    if (!file.exists(paste(gcm_outDir,"/part_3",sep=""))) {dir.create(paste(gcm_outDir,"/part_3",sep=""))}
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
    bDatFile <- paste(out_bDir,"/bcc-csm1-1-m_ENS_r1i1p1/part_3/loc_",loc,".RData",sep="")
  }
  
  
  if (!file.exists(oDatFile)) {
    #extract GCM data for this loc
    hisData <- get_loc_data(lon,lat,hisDir,gcm,ens,vn="tasmax",scratch,yi_h,yf_h,sce="historical")
    rcpData1 <- get_loc_data(lon,lat,rcpDir,gcm,ens,vn="tasmax",scratch,yi_f1,yf_f1,sce="rcp45")
    rcpData2 <- get_loc_data(lon,lat,rcpDir,gcm,ens,vn="tasmax",scratch,yi_f2,yf_f2,sce="rcp45")
    chgData1 <- merge(hisData,rcpData1,by="MONTH",sort=F)
    chgData2 <- merge(hisData,rcpData2,by="MONTH",sort=F)
    names(chgData1) <- c("MONTH","HIS","RCP")
    names(chgData2) <- c("MONTH","HIS","RCP")
    chgData1$CHG <- chgData1$RCP - chgData1$HIS
    chgData2$CHG <- chgData2$RCP - chgData2$HIS
    
    #extract CRU data for this loc
    if (file.exists(bDatFile)) {
      load(bDatFile)
      cruData <- output$DATA_CRU
      rm(output); g=gc(); rm(g)
    } else {
      cruData <- get_loc_data_cru(lon,lat,cruDir,vn="tmx",scratch,yi_h,yf_h)
      names(cruData)[5] <- "CRU"
    }
    
    
    #calculate overlap for rice
    out_rice <- list()
    if (rice == 1) {
      pl <- extract(ca_rs$RICE$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$RICE$HR,cbind(x=lon,y=lat))
      out_rice$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_rice$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_rice$OVERLAP_2035 <- NA
      out_rice$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for wspr
    out_wspr <- list()
    if (wspr == 1) {
      pl <- extract(ca_rs$WSPR$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$WSPR$HR,cbind(x=lon,y=lat))
      out_wspr$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_wspr$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_wspr$OVERLAP_2035 <- NA
      out_wspr$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for wwin
    out_wwin <- list()
    if (wwin == 1) {
      pl <- extract(ca_rs$WWIN$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$WWIN$HR,cbind(x=lon,y=lat))
      out_wwin$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_wwin$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_wwin$OVERLAP_2035 <- NA
      out_wwin$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for mill
    out_mill <- list()
    if (mill == 1) {
      pl <- extract(ca_rs$MILL$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$MILL$HR,cbind(x=lon,y=lat))
      out_mill$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_mill$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_mill$OVERLAP_2035 <- NA
      out_mill$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for mill
    out_sorg <- list()
    if (sorg == 1) {
      pl <- extract(ca_rs$SORG$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$SORG$HR,cbind(x=lon,y=lat))
      out_sorg$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_sorg$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_sorg$OVERLAP_2035 <- NA
      out_sorg$OVERLAP_2075 <- NA
    }
    
    #write RData as a test
    output <- list(DATA_CRU=cruData,DATA_2035=chgData1,DATA_2075=chgData2,
                   RICE=out_rice,WSPR=out_wspr,WWIN=out_wwin,MILL=out_mill,
                   SORG=out_sorg)
    save(list=c("output"),file=oDatFile)
  }
}


#function to calculate overlap between historical and future climates
calc_overlap <- function(cell,lon,lat,cruData,chgData,pl,hr) {
  #merge cruData with chgData
  allData <- merge(cruData,chgData,by="MONTH",sort=F)
  allData$DEL <- allData$CRU+allData$CHG
  
  #filter to growing season
  if (hr > pl) {gs <- pl:hr}
  if (hr < pl) {gs <- c(pl:12,1:hr)}
  if (hr == pl) {gs <- pl}
  allData <- allData[which(allData$MONTH %in% gs),]
  
  #calculate 5-95% pdf of current climate
  qlims <- as.numeric(quantile(allData$CRU,probs=c(0.01,0.99)))
  dp1 <- density(allData$CRU)
  xy1 <- data.frame(x=dp1$x,y=dp1$y)
  xy1_in <- xy1[which(xy1$x >= qlims[1] & xy1$x <= qlims[2]),]
  
  #calculate 5-95% pdf of future climate
  #qlims <- as.numeric(quantile(allData$DEL,probs=c(0.05,0.95)))
  dp2 <- density(allData$DEL)
  xy2 <- data.frame(x=dp2$x,y=dp2$y)
  ovr <- xy2[which(xy2$x <= max(xy1_in$x)),]
  #xy2 <- xy2[which(xy2$x >= qlims[1] & xy2$x <= qlims[2]),]
  
  #plots
  #plot(xy1$x,xy1$y,ty="l",xlim=c(-30,40),ylim=c(0,.05))
  #lines(xy2$x,xy2$y,col="red")
  #abline(v=min(xy1_in$x),lty=2); abline(v=max(xy1_in$x),lty=2)
  #lines(ovr$x,ovr$y,col="blue")
  
  #areas
  a1 <- integrate.xy(xy1$x,xy1$y)
  a2 <- integrate.xy(xy2$x,xy2$y)
  if (nrow(ovr) < 2) {
    ao <- 0
  } else {
    ao <- integrate.xy(ovr$x,ovr$y)
  }
  frac <- ao/a2
  
  #output data for this grid cell
  out_data <- list()
  out_data$LON <- lon
  out_data$LAT <- lat
  out_data$LOC <- cell
  out_data$PL <- pl
  out_data$HR <- hr
  out_data$GS_DATA <- allData
  out_data$PDF_CRU <- xy1
  out_data$PDF_DEL <- xy2
  out_data$INT_CRU <- a1
  out_data$INT_DEL <- a2
  out_data$INT_OVR <- ao
  out_data$FRACTION <- frac
  return(out_data)
}



#extract data from netcdf files using cdo
get_loc_data_cru <- function(lon,lat,in_dataDir,vn="tmx",scratch,yi,yf) {
  #load historical data of tasmax
  fil <- paste("cru_ts_3_10.1966.2005.",vn,".dat.nc",sep="")
  
  #inner scratch dir
  inScratch <- paste(scratch,"/",gsub(".dat.nc","",fil),sep="")
  if (!file.exists(inScratch)) {dir.create(inScratch)}
  setwd(inScratch)
  
  #copy files to temporary scratch folder
  inFil <- paste(in_dataDir,"/",fil,sep="")
  if (!file.exists(paste(inScratch,"/data.nc",sep=""))) {
    system(paste("cp -f ",inFil," data.nc",sep=""))
  }
  
  odat <- paste("data_lon-",lon,"_lat-",lat,".tab",sep="")
  if (!file.exists(odat)) {
    #extract values using CDO
    system(paste("cdo -outputtab,year,month,lon,lat,value -remapnn,lon=",lon,"_lat=",lat," data.nc > ",odat,sep=""))
  }
  
  #organise table
  loc_data <- read.table(odat,header=F,sep="")
  names(loc_data) <- c("YEAR","MONTH","LON","LAT","VALUE")
  loc_data$VALUE <- loc_data$VALUE
  loc_data <- loc_data[which(loc_data$YEAR %in% yi:yf),]
  
  #remove file
  if (file.exists(odat)) {system(paste("rm -f ",odat,sep=""))}
  
  #return object
  return(loc_data)
}



#extract data from netcdf files using cdo
get_loc_data <- function(lon,lat,in_dataDir,gcm,ens,vn="tasmax",scratch,yi,yf,sce="historical") {
  #load historical data of tasmax
  gcm_dataDir <- paste(in_dataDir,"/",vn,"/",gcm,"/",ens,sep="")
  gcmFiles <- list.files(gcm_dataDir,pattern=paste(gcm,"_",sce,"_",ens,sep=""))
  
  #loop through files
  loc_hdata <- data.frame()
  for (fil in gcmFiles) {
    #inner scratch dir
    inScratch <- paste(scratch,"/",gsub(".nc","",fil),sep="")
    if (!file.exists(inScratch)) {dir.create(inScratch)}
    setwd(inScratch)
    
    #copy files to temporary scratch folder
    inFil <- paste(gcm_dataDir,"/",fil,sep="")
    if (!file.exists(paste(inScratch,"/data.nc",sep=""))) {
      system(paste("cp -f ",inFil," data.nc",sep=""))
    }
    
    #output tabular data file
    odat <- paste("data_lon-",lon,"_lat-",lat,".tab",sep="")
    if (file.exists(odat)) {system(paste("rm -f ",odat,sep=""))}
    
    #extract values using CDO
    system(paste("cdo -outputtab,year,month,lon,lat,value -remapnn,lon=",lon,"_lat=",lat," data.nc > ",odat,sep=""))
    
    #organise table
    loc_data <- read.table(odat,header=F,sep="")
    names(loc_data) <- c("YEAR","MONTH","LON","LAT","VALUE")
    loc_data$VALUE <- loc_data$VALUE - 273.15
    loc_hdata <- rbind(loc_hdata,loc_data)
    
    #remove file
    if (file.exists(odat)) {system(paste("rm -f ",odat,sep=""))}
  }
  
  #remove extra years and comput climatology
  loc_hdata <- loc_hdata[which(loc_hdata$YEAR %in% yi:yf),]
  loc_data_cl <- data.frame(VALUE=tapply(loc_hdata$VALUE,loc_hdata$MONTH,FUN=mean))
  loc_data_cl <- cbind(MONTH=row.names(loc_data_cl),loc_data_cl)
  
  #return object
  return(loc_data_cl)
}



#find month given a Julian day 
find_month <- function(x) {
  #x <- vals[1]
  x <- round(x,0)
  if (x==0) {x <- 1}
  if (x>365) {x <- 365}
  dg <- createDateGrid(2000)
  dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
  dg$DOM <- as.numeric(substr(dg$MTH.DAY,5,6))
  m <- dg$MTH[which(dg$JD == x)]
  return(m)
}


#get only first listed ensemble member for each gcm
get_single_ens <- function(x,list1,list2) {
  gcm <- paste(x)
  ens <- paste(list2$ENS[which(list2$GCM == gcm)][1])
  yri_h <- list1$YI[which(list1$GCM == gcm & list1$ENS == ens)]
  yrf_h <- list1$YF[which(list1$GCM == gcm & list1$ENS == ens)]
  yri_f <- list2$YI[which(list2$GCM == gcm & list2$ENS == ens)]
  yrf_f <- list2$YF[which(list2$GCM == gcm & list2$ENS == ens)]
  yret <- c(gcm,ens,yri_h,yrf_h,yri_f,yrf_f)
  return(yret)
}


#get list of GCM and ensemble members
get_gcmList <- function(vn,data_dir,yi,yf) {
  rawList <- list.files(paste(data_dir,"/",vn,sep=""),recursive=T,include.dirs=F)
  mList <- lapply(rawList,FUN=get_details)
  mList <- as.data.frame(do.call("rbind",mList))
  names(mList) <- c("GCM","ENS","YI","YF")
  mList <- cbind(GCM_ENS=paste(mList$GCM,"_ENS_",mList$ENS,sep=""),mList)
  mList$YI <- as.numeric(paste(mList$YI))
  mList$YF <- as.numeric(paste(mList$YF))
  
  #eliminate repetitions (due to files being splitted in groups of years)
  uList <- paste(unique(mList$GCM_ENS))
  uList <- lapply(uList,FUN=get_final_details,mList)
  uList <- as.data.frame(do.call("rbind",uList))
  names(uList) <- c("GCM","ENS","YI","YF")
  uList <- cbind(GCM_ENS=paste(uList$GCM,"_ENS_",uList$ENS,sep=""),uList)
  uList$YI <- as.numeric(paste(uList$YI))
  uList$YF <- as.numeric(paste(uList$YF))
  
  #check if period includes yi (1965) and yf (2005)
  uList$PERIOD <- F
  uList$PERIOD[which(uList$YI <= yi & uList$YF >= yf)] <- T
  uList <- uList[which(uList$PERIOD),]
  uList$PERIOD <- NULL
  return(uList)
}


#get final details (without repetition)
get_final_details <- function(x,list2) {
  gcm <- unlist(strsplit(x,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(x,"_ENS_",fixed=T))[2]
  yri <- min(list2$YI[which(list2$GCM_ENS==x)])
  yrf <- max(list2$YF[which(list2$GCM_ENS==x)])
  yret <- c(gcm,ens,yri,yrf)
  return(yret)
}


#get details of listed files
get_details <- function(x) {
  y <- unlist(strsplit(x,"/",fixed=T))
  gcm <- y[1]
  ens <- y[2]
  fil <- y[3]
  yrs <- unlist(strsplit(fil,"_",fixed=T))
  yrs <- yrs[length(yrs)]
  yrs <- gsub(".nc","",yrs)
  yri <- as.numeric(substr(unlist(strsplit(yrs,"-",fixed=T))[1],1,4))
  yrf <- as.numeric(substr(unlist(strsplit(yrs,"-",fixed=T))[2],1,4))
  yret <- c(gcm,ens,yri,yrf)
  return(yret)
}

