#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL


### wrapper to combine ecocrop and glam runs based on various methods
### this basically shows that there is an agreement between the two models
### and that the disagreement can be attributed to sub-seasonal weather
### variations
wrapper_eg_combine <- function(i) {
  exp <- expid_list[i]
  if (exp < 10) {exp <- paste("0",exp,sep="")} else {exp <- paste(exp)}
  cat("\ncombination for experiment",exp,"...\n")
  
  #write results dir
  res_dir <- paste(crop_dir,"/ecg_analyses/results/exp-",exp,sep="")
  if (!file.exists(res_dir)) {dir.create(res_dir,recursive=T)}
  
  #control file for process checking
  con_file <- paste(res_dir,"/combine.run",sep="")
  if (!file.exists(con_file)) {
    #####################################################################
    ##### data loading
    cat("loading data...")
    #load glam potential yields
    glam_pot_rfd <- stack(paste(glam_dir,"/exp-",exp,"/gridded/pot_",1966:1993,"_yield_rfd.tif",sep=""))
    glam_pot_irr <- stack(paste(glam_dir,"/exp-",exp,"/gridded/pot_",1966:1993,"_yield_irr.tif",sep=""))
    glam_pot_bth <- stack(paste(glam_dir,"/exp-",exp,"/gridded/pot_",1966:1993,"_yield_bth.tif",sep=""))
    
    #load glam farmers (ygp-limited) yields
    glam_frm_rfd <- stack(paste(glam_dir,"/exp-",exp,"/gridded/frm_",1966:1993,"_yield_rfd.tif",sep=""))
    glam_frm_irr <- stack(paste(glam_dir,"/exp-",exp,"/gridded/frm_",1966:1993,"_yield_irr.tif",sep=""))
    glam_frm_bth <- stack(paste(glam_dir,"/exp-",exp,"/gridded/frm_",1966:1993,"_yield_bth.tif",sep=""))
    
    #load ecocrop yearly and climatological predictions
    ecrp_yr <- stack(paste(ec_dir,"/analyses/runs_eg/yearly/",1966:1993,"/1-",crop_name,"-tmean_suitability.asc",sep=""))
    ecrp_cl <- raster(paste(ec_dir,"/analyses/runs_eg/clm_1966_1993/1-",crop_name,"-tmean_suitability.asc",sep=""))
    
    #load zones grid
    zones <- raster(paste(crop_dir,"/gnut-zones/zones_lr.asc",sep=""))
    
    #preliminary calculations
    cat("\npreliminary calculations...")
    #calculate mean glam potential yields
    glam_pot_rfd_m <- calc(glam_pot_rfd,fun=function(x) {mean(x,na.rm=T)})
    glam_pot_irr_m <- calc(glam_pot_irr,fun=function(x) {mean(x,na.rm=T)})
    glam_pot_bth_m <- calc(glam_pot_bth,fun=function(x) {mean(x,na.rm=T)})
    
    #calculate mean glam farmers yields
    glam_frm_rfd_m <- calc(glam_frm_rfd,fun=function(x) {mean(x,na.rm=T)})
    glam_frm_irr_m <- calc(glam_frm_irr,fun=function(x) {mean(x,na.rm=T)})
    glam_frm_bth_m <- calc(glam_frm_bth,fun=function(x) {mean(x,na.rm=T)})
    
    #####################################################################
    ###extract data for gridcells of interest
    cat("\nextracting data...")
    #extract coordinates in GLAM, and then extract values of years and other stuff
    xy <- as.data.frame(xyFromCell(glam_pot_rfd[[1]],which(!is.na(glam_pot_rfd[[1]][]))))
    xy$CELL <- cellFromXY(glam_pot_rfd[[1]],cbind(x=xy$x,y=xy$y))
    xy$ZONE <- extract(zones,cbind(x=xy$x,y=xy$y))
    xy$GLAM.POT.RFD <- extract(glam_pot_rfd_m,cbind(x=xy$x,y=xy$y))
    xy$GLAM.POT.IRR <- extract(glam_pot_irr_m,cbind(x=xy$x,y=xy$y))
    xy$GLAM.POT.BTH <- extract(glam_pot_bth_m,cbind(x=xy$x,y=xy$y))
    
    xy$GLAM.FRM.RFD <- extract(glam_frm_rfd_m,cbind(x=xy$x,y=xy$y))
    xy$GLAM.FRM.IRR <- extract(glam_frm_irr_m,cbind(x=xy$x,y=xy$y))
    xy$GLAM.FRM.BTH <- extract(glam_frm_bth_m,cbind(x=xy$x,y=xy$y))
    
    xy$ECROP <- extract(ecrp_cl,cbind(x=xy$x,y=xy$y))
    xy <- xy[which(!is.na(xy$ECROP)),]
    
    #plot of historical mean comparisons
    if (!file.exists(paste(res_dir,"/yield_vs_suit_climatology.tiff",sep=""))) {
      tiff(paste(res_dir,"/yield_vs_suit_climatology.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1,1),cex=1)
      plot(xy$GLAM.POT.RFD,xy$ECROP,pch=20,
           xlab="Rainfed GLAM yield, no YGP-limited (kg/ha)",
           ylab="Crop suitability (%)")
      grid()
      dev.off()
    }
    
    ########################################################################
    ########################################################################
    #extract data for all years and gridcells for all further
    #analyses. Get data of all gridcells
    cat("\ngetting gridcell yield/suit data...")
    glam_data <- extract(glam_pot_rfd,cbind(x=xy$x,y=xy$y))
    eco_data <- extract(ecrp_yr,cbind(x=xy$x,y=xy$y))
    
    if (!file.exists(paste(res_dir,"/cell_data_yield_suit.csv",sep=""))) {
      cell_data <- lapply(as.numeric(xy$CELL),FUN=get_cell_data,xy,glam_data,eco_data)
      cell_data <- do.call("rbind",cell_data)
      write.csv(cell_data,paste(res_dir,"/cell_data_yield_suit.csv",sep=""),quote=F,row.names=F)
    } else {
      cell_data <- read.csv(paste(res_dir,"/cell_data_yield_suit.csv",sep=""))
    }
    
    #plot of yearly data
    if (!file.exists(paste(res_dir,"/yield_vs_suit_yearly.tiff",sep=""))) {
      tiff(paste(res_dir,"/yield_vs_suit_yearly.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1,1),cex=1)
      plot(cell_data$YIELD, cell_data$SUIT,pch=20,cex=0.75,
           xlab="Rainfed GLAM yield, no YGP-limited (kg/ha)",
           ylab="Crop suitability (%)")
      grid()
      dev.off()
    }
    
    #plot 3-D kernel density
    if (!file.exists(paste(res_dir,"/yield_vs_suit_density.tiff",sep=""))) {
      f2 <- kde2d(cell_data$YIELD, cell_data$SUIT, 
                  n = 100, lims = c(-50, 5000, -10, 150),
                  h = c(1000,75))
      cols <- colorRampPalette(c("grey 80","grey 10"))(25)
      tiff(paste(res_dir,"/yield_vs_suit_density.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1.5,1.5),cex=1)
      image(f2,ylim=c(0,100),xlim=c(0,4000),col=cols,useRaster=T,
            xlab="GLAM yield, no YGP-limited (kg/ha)",
            ylab="Crop suitability (%)")
      #points(cell_data$YIELD, cell_data$SUIT, pch=20,cex=0.5)
      grid(col="white")
      dev.off()
    }
    
    ########################################################################
    ########################################################################
    ### analysis of suitability residuals
    cat("\nextracting weather data...")
    #get cell weather data
    if (!file.exists(paste(res_dir,"/cell_data_wth.RData",sep=""))) {
      cell_wth_data <- lapply(as.numeric(unique(cell_data$CELL)),
                              FUN=get_wth_data,cell_data,crop_dir,wth_dir,exp)
      save(list=c("cell_wth_data"),file=paste(res_dir,"/cell_data_wth.RData",sep=""))
    } else {
      load(paste(res_dir,"/cell_data_wth.RData",sep=""))
    }
    if (!file.exists(paste(res_dir,"/cell_data_wth.csv",sep=""))) {
      cell_wth_data.df <- do.call("rbind",cell_wth_data)
      write.csv(cell_wth_data.df,paste(res_dir,"/cell_data_wth.csv",sep=""),quote=T,row.names=F)
    } else {
      cell_wth_data.df <- read.csv(paste(res_dir,"/cell_data_wth.csv",sep=""))
    }
    
    #regressions on residuals
    cat("\ninitial regressions...")
    if (!file.exists(paste(res_dir,"/regression_residuals.RData",sep=""))) {
      out_all <- lapply(cell_wth_data,FUN=regress_cell,fit_par="ALL")
      save(list=c("out_all"),file=paste(res_dir,"/regression_residuals.RData",sep=""))
    } else {
      load(paste(res_dir,"/regression_residuals.RData",sep=""))
    }
    if (!file.exists(paste(res_dir,"/regression_residuals.csv",sep=""))) {
      oall_reg <- do.call("rbind",out_all)
      nparam <- apply(oall_reg[,3:(ncol(oall_reg)-1)],1,FUN=function(x) {length(which(x!=0))})
      oall_reg$NPAR <- nparam
      oall_reg$NPAR_PER <- oall_reg$NPAR / length(3:(ncol(oall_reg)-2)) * 100
      write.csv(oall_reg,paste(res_dir,"/regression_residuals.csv",sep=""),quote=T,row.names=F)
    } else {
      oall_reg <- read.csv(paste(res_dir,"/regression_residuals.csv",sep=""))
    }
    
    #make rasters of correlations, and of number of regression terms
    if (!file.exists(paste(res_dir,"/ccoef_residuals.asc",sep=""))) {
      ccoef_rs <- raster(glam_pot_rfd)
      ccoef_rs[oall_reg$CELL] <- oall_reg$CCOEF
      ccoef_rs <- writeRaster(ccoef_rs,paste(res_dir,"/ccoef_residuals.asc",sep=""),format="ascii")
    } else {
      ccoef_rs <- raster(paste(res_dir,"/ccoef_residuals.asc",sep=""))
    }
    
    if (!file.exists(paste(res_dir,"/npars_residuals.asc",sep=""))) {
      npars_rs <- raster(glam_pot_rfd)
      npars_rs[oall_reg$CELL] <- oall_reg$NPAR
      npars_rs <- writeRaster(npars_rs,paste(res_dir,"/npars_residuals.asc",sep=""),format="ascii")
    } else {
      npars_rs <- raster(paste(res_dir,"/npars_residuals.asc",sep=""))
    }
    
    if (!file.exists(paste(res_dir,"/ppars_residuals.asc",sep=""))) {
      ppars_rs <- raster(glam_pot_rfd)
      ppars_rs[oall_reg$CELL] <- oall_reg$NPAR_PER
      ppars_rs <- writeRaster(ppars_rs,paste(res_dir,"/ppars_residuals.asc",sep=""),format="ascii")
    } else {
      ppars_rs <- raster(paste(res_dir,"/ppars_residuals.asc",sep=""))
    }
    
    #count number of times each term is included in a regression
    if (!file.exists(paste(res_dir,"/parameter_importance.csv",sep=""))) {
      pfreq <- as.numeric(apply(oall_reg,2,FUN=function(x) {length(which(x!=0))}))
      pfreq <- data.frame(PAR=names(oall_reg),FREQ=pfreq)
      pfreq$RFREQ <- pfreq$FREQ / nrow(oall_reg) * 100
      pfreq <- pfreq[which(pfreq$PAR %in% names(oall_reg[,3:(ncol(oall_reg)-3)])),]
      pfreq <- pfreq[order(pfreq$RFREQ,decreasing=T),]
      write.csv(pfreq,paste(res_dir,"/parameter_importance.csv",sep=""),quote=T,row.names=F)
    } else {
      pfreq <- read.csv(paste(res_dir,"/parameter_importance.csv",sep=""))
    }
    
    #spatial importance of parameters
    if (!file.exists(paste(res_dir,"/parameter_importance.tiff",sep=""))) {
      tiff(paste(res_dir,"/parameter_importance.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(6,5,1,1),cex=1)
      barplot(height=pfreq$RFREQ,names.arg=pfreq$PAR,las=2,
              ylab="Percent gridcells where significant (%)",
              xlab=NA,ylim=c(0,100))
      grid()
      abline(h=50,col="red")
      abline(h=30,col="red",lty=2)
      dev.off()
    }
    
    
    ###second iteration with fewer regression terms
    #re_run the fitting algorithm for all gridcells but only 
    #with variables that were found to be important
    cat("\nfinal regressions...")
    #fit_par <- paste(pfreq$PAR[which(pfreq$RFREQ >= 50)])
    fit_par <- paste(pfreq$PAR[order(pfreq$RFREQ,decreasing=T)][1:3])
    
    ########### do the cross-validation just here
    if (!file.exists(paste(res_dir,"/bootstrapping_residuals_i2.RData",sep=""))) {
      out_bts <- lapply(cell_wth_data,FUN=regress_cell,fit_par,100,7) #28 row * 25% = 7
      save(list=c("out_bts"),file=paste(res_dir,"/bootstrapping_residuals_i2.RData",sep=""))
    } else {
      load(paste(res_dir,"/bootstrapping_residuals_i2.RData",sep=""))
    }
    if (!file.exists(paste(res_dir,"/bootstrapping_residuals_i2.csv",sep=""))) {
      obts_all <- do.call("rbind",out_bts)
      bts_nparam <- apply(obts_all[,4:(ncol(obts_all)-1)],1,FUN=function(x) {length(which(x!=0))})
      obts_all$NPAR <- bts_nparam
      obts_all$NPAR_PER <- obts_all$NPAR / length(4:(ncol(obts_all)-2)) * 100
      write.csv(obts_all,paste(res_dir,"/bootstrapping_residuals_i2.csv",sep=""),quote=T,row.names=F)
    } else {
      obts_all <- read.csv(paste(res_dir,"/bootstrapping_residuals_i2.csv",sep=""))
    }
    
    #perform the actual regressions
    if (!file.exists(paste(res_dir,"/regression_residuals_i2.RData",sep=""))) {
      out_all2 <- lapply(cell_wth_data,FUN=regress_cell,fit_par)
      save(list=c("out_all2"),file=paste(res_dir,"/regression_residuals_i2.RData",sep=""))
    } else {
      load(paste(res_dir,"/regression_residuals_i2.RData",sep=""))
    }
    if (!file.exists(paste(res_dir,"/regression_residuals_i2.csv",sep=""))) {
      oall_reg2 <- do.call("rbind",out_all2)
      nparam <- apply(oall_reg2[,3:(ncol(oall_reg2)-1)],1,FUN=function(x) {length(which(x!=0))})
      oall_reg2$NPAR <- nparam
      oall_reg2$NPAR_PER <- oall_reg2$NPAR / length(3:(ncol(oall_reg2)-2)) * 100
      write.csv(oall_reg2,paste(res_dir,"/regression_residuals_i2.csv",sep=""),quote=T,row.names=F)
    } else {
      oall_reg2 <- read.csv(paste(res_dir,"/regression_residuals_i2.csv",sep=""))
    }
    
    #make rasters of correlations, and of number of regression terms
    if (!file.exists(paste(res_dir,"/ccoef_residuals_i2.asc",sep=""))) {
      ccoef_rs2 <- raster(glam_pot_rfd)
      ccoef_rs2[oall_reg2$CELL] <- oall_reg2$CCOEF
      ccoef_rs2 <- writeRaster(ccoef_rs2,paste(res_dir,"/ccoef_residuals_i2.asc",sep=""),format="ascii")
    } else {
      ccoef_rs2 <- raster(paste(res_dir,"/ccoef_residuals_i2.asc",sep=""))
    }
    
    if (!file.exists(paste(res_dir,"/npars_residuals_i2.asc",sep=""))) {
      npars_rs2 <- raster(glam_pot_rfd)
      npars_rs2[oall_reg2$CELL] <- oall_reg2$NPAR
      npars_rs2 <- writeRaster(npars_rs2,paste(res_dir,"/npars_residuals_i2.asc",sep=""),format="ascii")
    } else {
      npars_rs2 <- raster(paste(res_dir,"/npars_residuals_i2.asc",sep=""))
    }
    
    if (!file.exists(paste(res_dir,"/ppars_residuals_i2.asc",sep=""))) {
      ppars_rs2 <- raster(glam_pot_rfd)
      ppars_rs2[oall_reg2$CELL] <- oall_reg2$NPAR_PER
      ppars_rs2 <- writeRaster(ppars_rs2,paste(res_dir,"/ppars_residuals_i2.asc",sep=""),format="ascii")
    } else {
      ppars_rs2 <- raster(paste(res_dir,"/ppars_residuals_i2.asc",sep=""))
    }
    
    #count number of times each term is included in a regression
    if (!file.exists(paste(res_dir,"/parameter_importance_i2.csv",sep=""))) {
      pfreq2 <- as.numeric(apply(oall_reg2,2,FUN=function(x) {length(which(x!=0))}))
      pfreq2 <- data.frame(PAR=names(oall_reg2),FREQ=pfreq2)
      pfreq2$RFREQ <- pfreq2$FREQ / nrow(oall_reg2) * 100
      pfreq2 <- pfreq2[which(pfreq2$PAR %in% names(oall_reg2[,3:(ncol(oall_reg2)-3)])),]
      pfreq2 <- pfreq2[order(pfreq2$RFREQ,decreasing=T),]
      pfreq2 <- pfreq2[which(pfreq2$RFREQ > 0),]
      write.csv(pfreq2,paste(res_dir,"/parameter_importance_i2.csv",sep=""),quote=T,row.names=F)
    } else {
      pfreq2 <- read.csv(paste(res_dir,"/parameter_importance_i2.csv",sep=""))
    }
    
    #spatial importance of parameters (second iteration)
    if (!file.exists(paste(res_dir,"/parameter_importance_i2.tiff",sep=""))) {
      tiff(paste(res_dir,"/parameter_importance_i2.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(6,5,1,1),cex=1)
      barplot(height=pfreq2$RFREQ,names.arg=pfreq2$PAR,las=2,
              ylab="Percent gridcells where significant (%)",
              xlab=NA,ylim=c(0,100))
      grid()
      abline(h=50,col="red")
      abline(h=30,col="red",lty=2)
      dev.off()
    }
    
    
    ########################################################################
    ########################################################################
    ###analysis of average suitability per yield quantile
    cat("\nyield quantiles vs. suitability...")
    y_quan <- quantile(cell_data$YIELD,probs=seq(0,1,by=0.1))
    q_mat <- data.frame(QUANTILE=seq(0,1,by=0.1),Y_VALUE=as.numeric(y_quan),YIELD=NA,SUIT=NA)
    cell_data$Y_QUANT <- NA
    for (i in 1:nrow(q_mat)) {
      if (i == 1) {
        cell_data$Y_QUANT[which(cell_data$YIELD<=q_mat$Y_VALUE[i])] <- i
        
        q_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$YIELD<=q_mat$Y_VALUE[i])],na.rm=T)
        q_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$YIELD<=q_mat$Y_VALUE[i])],na.rm=T)
      } else {
        cell_data$Y_QUANT[which(cell_data$YIELD<=q_mat$Y_VALUE[i] & cell_data$YIELD>q_mat$Y_VALUE[i-1])] <- i
        
        q_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$YIELD<=q_mat$Y_VALUE[i] & cell_data$YIELD>q_mat$Y_VALUE[i-1])],na.rm=T)
        q_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$YIELD<=q_mat$Y_VALUE[i] & cell_data$YIELD>q_mat$Y_VALUE[i-1])],na.rm=T)
      }
    }
    write.csv(q_mat,paste(res_dir,"/yield_quant.csv",sep=""),quote=F,row.names=F)
    
    #plot
    if (!file.exists(paste(res_dir,"/yield_quantiles_xy.tiff",sep=""))) {
      tiff(paste(res_dir,"/yield_quantiles_xy.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1,1),cex=1)
      plot(q_mat$YIELD,q_mat$SUIT,ty="l",
           ylab="Mean suitability (%)",xlab="Mean GLAM yield (kg/ha)")
      points(q_mat$YIELD,q_mat$SUIT,pch=20)
      grid()
      dev.off()
    }
    
    if (!file.exists(paste(res_dir,"/yield_quantiles_boxplot.tiff",sep=""))) {
      tiff(paste(res_dir,"/yield_quantiles_boxplot.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1,1),cex=1)
      boxplot(cell_data$SUIT~cell_data$Y_QUANT,pch=NA,cex=0.75,col="grey 80",
              ylab="Mean suitability (%)",xlab="GLAM yield quantile")
      grid()
      dev.off()
    }
    
    ########################################################################
    ########################################################################
    ###analysis of average yield per suitability class
    cat("\nsuitability classes vs. yield...")
    c_mat <- data.frame(CLASS=seq(1,11,by=1),S_VALUE=seq(0,100,by=10),YIELD=NA,SUIT=NA)
    cell_data$S_CLASS <- NA
    for (i in 1:nrow(c_mat)) {
      if (i == 1) {
        cell_data$S_CLASS[which(cell_data$SUIT<=c_mat$S_VALUE[i])] <- i
        
        c_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$SUIT<=c_mat$S_VALUE[i])],na.rm=T)
        c_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$SUIT<=c_mat$S_VALUE[i])],na.rm=T)
      } else {
        cell_data$S_CLASS[which(cell_data$SUIT<=c_mat$S_VALUE[i] & cell_data$SUIT>c_mat$S_VALUE[i-1])] <- i
        
        c_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$SUIT<=c_mat$S_VALUE[i] & cell_data$SUIT>c_mat$S_VALUE[i-1])],na.rm=T)
        c_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$SUIT<=c_mat$S_VALUE[i] & cell_data$SUIT>c_mat$S_VALUE[i-1])],na.rm=T)
      }
    }
    write.csv(c_mat,paste(res_dir,"/suit_class.csv",sep=""),quote=F,row.names=F)
    
    #update cell_data
    write.csv(cell_data,paste(res_dir,"/cell_data_yield_suit.csv",sep=""),quote=F,row.names=F)
    
    #plot
    if (!file.exists(paste(res_dir,"/suit_classes_xy.tiff",sep=""))) {
      tiff(paste(res_dir,"/suit_classes_xy.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1,1),cex=1)
      plot(c_mat$SUIT,c_mat$YIELD,ty="p",pch=20,
           ylab="Mean suitability (%)",xlab="Mean GLAM yield (kg/ha)")
      abline(400,(1200-400)/100)
      grid()
      dev.off()
    }
    if (!file.exists(paste(res_dir,"/suit_classes_boxplot.tiff",sep=""))) {
      tiff(paste(res_dir,"/suit_classes_boxplot.tiff",sep=""),res=300,pointsize=10,
           width=1500,height=1300,units="px",compression="lzw")
      par(mar=c(5,5,1,1),cex=1)
      boxplot(cell_data$YIELD~cell_data$S_CLASS,pch=NA,cex=0.75,col="grey 80",
              ylab="Mean GLAM yield (kg/ha)",xlab="Suitability class",ylim=c(0,3000))
      grid()
      dev.off()
    }
    
    
    ########################################################################################
    ########################################################################################
    #make the above plot using the most water or temperature stressed years
    #(5% or 95% quantiles)
    cat("\nanalysis of extremes...")
    if (!file.exists(paste(res_dir,"/extreme_values.csv",sep=""))) {
      #most temperature stressed years
      #top 95% and low 5% of HTS1
      hts1_top <- quantile(cell_wth_data.df$HTS1,probs=c(0.95))
      hts1_bot <- quantile(cell_wth_data.df$HTS1,probs=c(0.05))
      
      #top 95% and low 5% of TSTD (much more extremes)
      tstd_top <- quantile(cell_wth_data.df$TSTD,probs=c(0.95))
      tstd_bot <- quantile(cell_wth_data.df$TSTD,probs=c(0.05))
      
      #top 95% and low 5% of TCOV (much more extremes)
      tcov_top <- quantile(cell_wth_data.df$TCOV,probs=c(0.95))
      tcov_bot <- quantile(cell_wth_data.df$TCOV,probs=c(0.05))
      
      #most drought stress years
      #5/95% of RAIN
      rain_top <- quantile(cell_wth_data.df$RAIN,probs=c(0.95))
      rain_bot <- quantile(cell_wth_data.df$RAIN,probs=c(0.05))
      
      #5/95% of RD.0
      rd.0_top <- quantile(cell_wth_data.df$RD.0,probs=c(0.95))
      rd.0_bot <- quantile(cell_wth_data.df$RD.0,probs=c(0.05))
      
      #5/95% of RSTD (much more extremes)
      rstd_top <- quantile(cell_wth_data.df$RSTD,probs=c(0.95))
      rstd_bot <- quantile(cell_wth_data.df$RSTD,probs=c(0.05))
      
      #5/95% of RCOV (much more extremes)
      rcov_top <- quantile(cell_wth_data.df$RCOV,probs=c(0.95))
      rcov_bot <- quantile(cell_wth_data.df$RCOV,probs=c(0.05))
      
      #5/95% of ERATIO.50 (water stress)
      ea50_top <- quantile(cell_wth_data.df$ERATIO.50,probs=c(0.95))
      ea50_bot <- quantile(cell_wth_data.df$ERATIO.50,probs=c(0.05))
      
      #5/95% of ERATIO.25 (water stress)
      ea25_top <- quantile(cell_wth_data.df$ERATIO.25,probs=c(0.95))
      ea25_bot <- quantile(cell_wth_data.df$ERATIO.25,probs=c(0.05))
      
      #best and worst years overall
      #5/95% of EFF.GD
      efgd_top <- quantile(cell_wth_data.df$EFF.GD,probs=c(0.95))
      efgd_bot <- quantile(cell_wth_data.df$EFF.GD,probs=c(0.05))
      
      #construct table with the above information
      extr_val <- data.frame(PAR=c("HTS1","TSTD","TCOV","RAIN","RD.0","RSTD","RCOV","ERATIO.25","ERATIO.50","EFF.GD"),
                             TOP95=c(hts1_top,tstd_top,tcov_top,rain_top,rd.0_top,rstd_top,rcov_top,ea25_top,ea50_top,efgd_top),
                             BOT05=c(hts1_bot,tstd_bot,tcov_bot,rain_bot,rd.0_bot,rstd_bot,rcov_bot,ea25_bot,ea50_bot,efgd_bot))
      write.csv(extr_val,paste(res_dir,"/extreme_values.csv",sep=""),quote=F,row.names=F)
    } else {
      extr_val <- read.csv(paste(res_dir,"/extreme_values.csv",sep=""))
    }
    
    #make table for heatmap
    if (!file.exists(paste(res_dir,"/heatmap_extremes.csv",sep=""))) {
      cell_merg_wth <- cell_data
      cell_merg_wth$YIELD <- NULL; cell_merg_wth$SUIT <- NULL
      cell_merg_wth <- merge(cell_wth_data.df,cell_merg_wth,by=c("CELL","YEAR"),sort=F,all=T)
      for (i in 1:nrow(c_mat)) {
        subs_merg <- cell_merg_wth[which(cell_merg_wth$S_CLASS == i),]
        
        ymo_df <- as.data.frame(matrix(nrow=3,ncol=1))
        sdo_df <- as.data.frame(matrix(nrow=3,ncol=1))
        #loop through accounting variables
        for (j in 1:nrow(extr_val)) {
          tpar <- paste(extr_val$PAR[j])
          subs_merg$QUANT <- "MID"
          subs_merg$QUANT[which(subs_merg[,tpar] <= extr_val$BOT05[j])] <- "B05"
          subs_merg$QUANT[which(subs_merg[,tpar] >= extr_val$TOP95[j])] <- "T95"
          
          #mean values
          ym1 <- mean(subs_merg$YIELD[which(subs_merg$QUANT == "B05")],na.rm=T)
          sd1 <- sd(subs_merg$YIELD[which(subs_merg$QUANT == "B05")],na.rm=T)
          ym2 <- mean(subs_merg$YIELD[which(subs_merg$QUANT == "MID")],na.rm=T)
          sd2 <- sd(subs_merg$YIELD[which(subs_merg$QUANT == "MID")],na.rm=T)
          ym3 <- mean(subs_merg$YIELD[which(subs_merg$QUANT == "T95")],na.rm=T)
          sd3 <- sd(subs_merg$YIELD[which(subs_merg$QUANT == "T95")],na.rm=T)
          yma <- mean(subs_merg$YIELD,na.rm=T)
          sda <- sd(subs_merg$YIELD,na.rm=T)
          
          #output
          ymo <- c(ym1/yma,ym2/yma,ym3/yma)
          ymo_df <- cbind(ymo_df,ymo)
          
          sdo <- c(sd1/sda,sd2/sda,sd3/sda)
          sdo_df <- cbind(sdo_df,sdo)
        }
        
        names(ymo_df) <- c("QUANT",paste("MEAN.",extr_val$PAR,sep=""))
        sdo_df$V1 <- NULL; names(sdo_df) <- paste("SD.",extr_val$PAR,sep="")
        ymo_df$QUANT <- c("B05","MID","T95")
        ymo_df <- cbind(CLASS=i,ymo_df)
        
        if (i==1) {
          o_class <- cbind(ymo_df,sdo_df)
        } else {
          o_class <- rbind(o_class,cbind(ymo_df,sdo_df))
        }
      }
      write.csv(o_class,paste(res_dir,"/heatmap_extremes.csv",sep=""),quote=F,row.names=F)
    } else {
      o_class <- read.csv(paste(res_dir,"/heatmap_extremes.csv",sep=""))
    }
    
    ########################################################################
    ########################################################################
    ### plot rasters here
    cat("\nfinal raster plots...\n")
    ht <- 1000
    fct <- (ccoef_rs@extent@xmin-ccoef_rs@extent@xmax)/(ccoef_rs@extent@ymin-ccoef_rs@extent@ymax)
    wt <- ht*(fct+.1)
    
    ##plot correlation coefficient
    brks <- seq(0,1,by=0.1)
    brks.lab <- round(brks,2)
    cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))
    
    tiffName <- paste(res_dir,"/ccoef.tiff",sep="")
    if (!file.exists(tiffName)) {
      tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
      par(mar=c(3,3,1,3.5))
      plot(ccoef_rs,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    tiffName <- paste(res_dir,"/ccoef_i2.tiff",sep="")
    if (!file.exists(tiffName)) {
      tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
      par(mar=c(3,3,1,3.5))
      plot(ccoef_rs2,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    ##plot of number of explanatory terms
    brks <- seq(1,max(npars_rs[],na.rm=T),by=1)
    brks.lab <- round(brks,0)
    cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))
    
    tiffName <- paste(res_dir,"/num_params.tiff",sep="")
    if (!file.exists(tiffName)) {
      tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
      par(mar=c(3,3,1,3.5))
      plot(npars_rs,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    ##plot of number of explanatory terms
    brks <- seq(1,max(npars_rs2[],na.rm=T),by=1)
    brks.lab <- round(brks,0)
    cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))
    
    tiffName <- paste(res_dir,"/num_params_i2.tiff",sep="")
    if (!file.exists(tiffName)) {
      tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
      par(mar=c(3,3,1,3.5))
      plot(npars_rs2,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    ##plot of percent of explanatory terms
    brks <- seq(0,100,by=10)
    brks.lab <- round(brks,0)
    cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))
    
    tiffName <- paste(res_dir,"/per_params.tiff",sep="")
    if (!file.exists(tiffName)) {
      tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
      par(mar=c(3,3,1,3.5))
      plot(ppars_rs,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    tiffName <- paste(res_dir,"/per_params_i2.tiff",sep="")
    if (!file.exists(tiffName)) {
      tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
      par(mar=c(3,3,1,3.5))
      plot(ppars_rs2,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    ff <- file(con_file,"w")
    cat("processed on",date(),"\n",file=ff)
    close(ff)
  } else {
    cat("\nexperiment already done...")
  }
  
  return(res_dir)
}




#function to regress the data (remainder of perfect fit)
#of a given gridcell, against key growing season weather variables
get_wth_data <- function(cell,cell_data,crop_dir,wth_dir,exp) {
  cat("\ngricell",cell,"...\n")
  
  #extract yield and suitability values
  yield <- as.numeric(cell_data$YIELD[which(cell_data$CELL==cell)])
  suit <- as.numeric(cell_data$SUIT[which(cell_data$CELL==cell)])
  
  #preliminary plots of historical yield and suitability, commented
  # plot(1966:1993,yield/max(yield),ty="l",col="red")
  # lines(1966:1993,suit/100,ty="l",col="blue")
  # plot(yield/max(yield),suit/100,pch=20,ylim=c(0,1),xlim=c(0,1))
  # abline(0,1,col="red")
  
  #make data frame with data
  data_cell <- data.frame(YEAR=1966:1993,YIELD=yield,SUIT=suit)
  data_cell$SUIT_NORM <- data_cell$SUIT/100
  data_cell$YIELD_NORM <- data_cell$YIELD/max(data_cell$YIELD)
  
  #calculate the difference between each point and the perfect fit (0,1)
  x <- c(0,1)
  y <- c(0,1)
  p_fit <- lm(y~x)
  data_cell$PFIT <- as.numeric(predict(p_fit,newdata=data.frame(x=data_cell$YIELD_NORM)))
  data_cell$DIFF <- data_cell$SUIT_NORM-data_cell$PFIT
  
  #some additional plots, commented
  # plot(data_cell$YIELD_NORM,data_cell$SUIT_NORM,pch=20,ylim=c(0,1),xlim=c(0,1))
  # abline(0,1,col="red")
  # points(data_cell$YIELD_NORM,data_cell$PFIT,col="red",pch=20)
  # points(data_cell$YIELD_NORM,data_cell$DIFF,col="red",pch=20)
  
  #calculate growing season variables
  #loop through years
  cat("calculating yearly data...\n")
  out_df <- data.frame()
  for (yr in 1966:1993) {
    #yr <- 1966 #year i want to get
    #cat("year",yr,"\n")
    #read weather data from *.wth file
    wth_dir <- paste(crop_dir,"/inputs/ascii/wth/rfd_",cell,sep="")
    wth_fil <- paste(wth_dir,"/ingc001001",yr,".wth",sep="")
    wth <- read.fortran(wth_fil,format=c("I5","F6","3F7"),skip=4)
    names(wth) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
    wth$YEAR <- as.numeric(substr(wth$DATE,1,2))
    wth$JDAY <- as.numeric(substr(wth$DATE,3,5))
    
    #get planting, idur, harvest date date from glam run
    run_dir <- paste(crop_dir,"/calib/exp-",exp,"_outputs/gridcells/fcal_",cell,sep="")
    
    #get harvest date (from ygp=1 run, rainfed)
    glam_file <- paste(run_dir,"/ygp_1/groundnut_RFD.out",sep="")
    glam_run <- read.table(glam_file,sep="\t",header=F)
    names(glam_run) <- vnames$EOS
    
    sow <- glam_run$IPDATE[which(glam_run$YEAR == yr)]
    har <- sow + glam_run$IDUR[which(glam_run$YEAR == yr)]
    
    #Calculate the water balance
    wth_out <- wth
    wth_out$ETMAX <- NA; wth_out$AVAIL <- NA; wth_out$ERATIO <- NA
    wth_out$CUM_RAIN <- NA; wth_out$RUNOFF <- NA; wth_out$DEMAND <- NA
    wth_out <- watbal_wrapper(wth_out)
    
    #calculate mean temperature
    wth_out$TMEAN <- (wth_out$TMAX + wth_out$TMIN)/2
    
    #remove not-needed dates in the wth file
    wth_out <- wth_out[which(wth_out$JDAY >= sow & wth_out$JDAY <= har),]
    
    #here try to correlate that difference with some metrics of the growing season, such as:
    #number of days with rain > 0mm, 2mm, 5mm, 10mm, 15mm, 20mm
    rain <- sum(wth_out$RAIN)
    rstd <- sd(wth_out$RAIN)
    if (mean(wth_out$RAIN) == 0) {
      rcov <- rstd/1
    } else {
      rcov <- rstd/mean(wth_out$RAIN)
    }
    rd_0 <- length(which(wth_out$RAIN>0))
    rd_2 <- length(which(wth_out$RAIN>2))
    rd_5 <- length(which(wth_out$RAIN>5))
    rd_10 <- length(which(wth_out$RAIN>10))
    rd_15 <- length(which(wth_out$RAIN>15))
    rd_20 <- length(which(wth_out$RAIN>20))
    
    #days exceeding thresholds of HTS and TETRS
    hts_34 <- length(which(wth_out$TMAX>34))
    hts_40 <- length(which(wth_out$TMAX>40))
    
    tetr_35 <- length(which(wth_out$TMEAN>35))
    tetr_47 <- length(which(wth_out$TMEAN>47))
    
    #water stress days, calculated from simple WATBAL of PJones
    #number of days with Ea/Ep ratio < 0.25, 0.5, 0.75
    eratio_25 <- length(which(wth_out$ERATIO<0.25))
    eratio_50 <- length(which(wth_out$ERATIO<0.5))
    eratio_75 <- length(which(wth_out$ERATIO<0.75))
    
    #standard deviation of temperature
    tstd <- sd(wth_out$TMEAN)
    tmen <- mean(wth_out$TMEAN)
    tcov <- tstd/tmen
    
    #from Trnka et al. (2011) GCB
    #sum of effective global radiation
    #sum of global radiation of days with daily mean temperature >8
    #daily minimum temperature >0, and ETRATIO>0.5
    effsrad <- sum(wth_out$SRAD[which(wth_out$TMEAN > 8 & wth_out$TMIN > 0 & wth_out$ERATIO > 0.5)])
    
    #sum of effective growing days
    #number of days with daily mean temperature >8, daily minimum
    #temperature >0 and ERATIO>0.5
    effgd <- length(which(wth_out$TMEAN > 8 & wth_out$TMIN > 0 & wth_out$ERATIO > 0.5))
    
    #output row
    orow <- data.frame(YEAR=yr,SOW=sow,HAR=har,RAIN=rain,RSTD=rstd,RCOV=rcov,RD.0=rd_0,
                       RD.2=rd_2,RD.5=rd_5,RD.10=rd_10,RD.15=rd_15,RD.20=rd_20,
                       HTS1=hts_34,HTS2=hts_40,TETR1=tetr_35,TETR2=tetr_47,
                       ERATIO.25=eratio_25,ERATIO.50=eratio_50,ERATIO.75=eratio_75,
                       TSTD=tstd,TMEN=tmen,TCOV=tcov,EFF.SRAD=effsrad,EFF.GD=effgd)
    out_df <- rbind(out_df,orow)
  }
  
  #calculate water use efficiency
  data_cell$WUE <- data_cell$YIELD/out_df$RAIN
  dcell <- data_cell
  dcell <- merge(dcell,out_df,by=c("YEAR"),sort=F)
  dcell <- cbind(CELL=cell,dcell)
  
  return(dcell)
}


#function to regress the data (remainder of perfect fit)
#of a given gridcell, against key growing season weather variables
regress_cell <- function(dcell,fit_par="ALL",nboots=0,nsize=1) {
  #cat("\ngricell",cell,"...\n")
  
  #correlation matrix to explore potential explanatory power for that difference
  cell <- dcell$CELL[1]
  dcell$CELL <- NULL
  cor_mx <- as.data.frame(cor(dcell))
  
  #remove HTS thresholds if required
  if (is.na(cor_mx$HTS1[1])) {
    if (length(fit_par) == 1) {
      if (fit_par == "ALL") {
        fit_par <- fit_par
      } else {
        if (length(which("TETR1" %in% fit_par)) > 0) {
          stop("regression cannot be fitted with no variables TETR1")
        }
      }
    } else {
      if (length(which("HTS1" %in% fit_par)) > 0) {
        fit_par <- fit_par[which(!fit_par %in% "HTS1")]
      }
    }
    cor_mx$HTS1 <- NULL
    w_rem <- which(row.names(cor_mx) == "HTS1")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  if (is.na(cor_mx$HTS2[1])) {
    if (length(fit_par) == 1) {
      if (fit_par == "ALL") {
        fit_par <- fit_par
      } else {
        if (length(which("TETR1" %in% fit_par)) > 0) {
          stop("regression cannot be fitted with no variables TETR1")
        }
      }
    } else {
      if (length(which("HTS2" %in% fit_par)) > 0) {
        fit_par <- fit_par[which(!fit_par %in% "HTS2")]
      }
    }
    cor_mx$HTS2 <- NULL
    w_rem <- which(row.names(cor_mx) == "HTS2")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  
  #remove TETR thresholds if required
  if (is.na(cor_mx$TETR1[1])) {
    if (length(fit_par) == 1) {
      if (fit_par == "ALL") {
        fit_par <- fit_par
      } else {
        if (length(which("TETR1" %in% fit_par)) > 0) {
          stop("regression cannot be fitted with no variables TETR1")
        }
      }
    } else {
      if (length(which("TETR1" %in% fit_par)) > 0) {
        fit_par <- fit_par[which(!fit_par %in% "TETR1")]
      }
    }
    cor_mx$TETR1 <- NULL
    w_rem <- which(row.names(cor_mx) == "TETR1")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  if (is.na(cor_mx$TETR2[1])) {
    if (length(fit_par) == 1) {
      if (fit_par == "ALL") {
        fit_par <- fit_par
      } else {
        if (length(which("TETR1" %in% fit_par)) > 0) {
          stop("regression cannot be fitted with no variables TETR1")
        }
      }
    } else {
      if (length(which("TETR2" %in% fit_par)) > 0) {
        fit_par <- fit_par[which(!fit_par %in% "TETR2")]
      }
    }
    cor_mx$TETR2 <- NULL
    w_rem <- which(row.names(cor_mx) == "TETR2")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  
  #remove useless fields
  cor_mx$YEAR <- NULL; cor_mx$YIELD <- NULL; cor_mx$SUIT <- NULL
  cor_mx$SUIT_NORM <- NULL; cor_mx$YIELD_NORM <- NULL; cor_mx$PFIT <- NULL
  cor_mx$WUE <- NULL; cor_mx$SOW <- NULL; cor_mx$HAR <- NULL; cor_mx$RAIN <- NULL
  cor_mx$TMEN <- NULL; cor_mx$RSTD <- NULL; cor_mx$TSTD <- NULL
  
  #data for fitting
  if (length(fit_par) == 1) {
    if (fit_par == "ALL") {
      fit_data <- dcell[,names(cor_mx)]
    } else {
      fit_par <- c("DIFF",fit_par)
      fit_data <- dcell[,fit_par]
    }
  } else {
    fit_par <- c("DIFF",fit_par)
    fit_data <- dcell[,fit_par]
  }
  
  #here do the bootstrapping
  if (nboots == 0) {
    #cat("fitting multiple regression\n")
    #fit the multiple regression
    reg_fit <- lm(DIFF ~ .,data=fit_data)
    
    #stepwise the regression based on AIC
    reg_stp <- stepAIC(reg_fit,direction="both",trace=F)
    anv_stp <- reg_stp$anova
    sum_stp <- summary(reg_stp)
    
    #calculate the response and do some plots (commented)
    diffp <- predict(reg_stp,dcell)
    #plot(diffp,dcell$DIFF)
    ccoef <- cor(diffp,dcell$DIFF)
    #length(which(dcell$DIFF<0)) #number of years with diff < 0
    
    #put this all into a matrix with the coefficients
    reg_df <- data.frame(CELL=cell,INT=0,RCOV=0,RD.0=0,RD.2=0,RD.5=0,RD.10=0,
                         RD.15=0,RD.20=0,HTS1=0,HTS2=0,TETR1=0,TETR2=0,ERATIO.25=0,
                         ERATIO.50=0,ERATIO.75=0,TCOV=0,EFF.SRAD=0,EFF.GD=0,CCOEF=ccoef)
    
    coef_mx <- as.data.frame(sum_stp$coefficients)
    row.names(coef_mx)[1] <- "INT"
    rnames <- row.names(coef_mx)
    for (rn in rnames) {
      reg_df[which(reg_df$CELL == cell),rn] <- coef_mx$Estimate[which(rnames==rn)]
    }
    #out_all <- rbind(out_all,reg_df)
  } else {
    reg_df <- data.frame()
    for (br in 1:nboots) {
      boots_data <- sample(1:nrow(fit_data),size=nsize)
      boots_data <- fit_data[-boots_data,]
      reg_fit <- lm(DIFF ~ .,data=boots_data)
      
      #stepwise the regression based on AIC
      reg_stp <- stepAIC(reg_fit,direction="both",trace=F)
      anv_stp <- reg_stp$anova
      sum_stp <- summary(reg_stp)
      
      #calculate the response and do some plots (commented)
      diffp <- predict(reg_stp,dcell)
      #plot(diffp,dcell$DIFF)
      ccoef <- cor(diffp,dcell$DIFF)
      #length(which(dcell$DIFF<0)) #number of years with diff < 0
      
      #put this all into a matrix with the coefficients
      bts_df <- data.frame(CELL=cell,INT=0,RCOV=0,RD.0=0,RD.2=0,RD.5=0,RD.10=0,
                           RD.15=0,RD.20=0,HTS1=0,HTS2=0,TETR1=0,TETR2=0,ERATIO.25=0,
                           ERATIO.50=0,ERATIO.75=0,TCOV=0,EFF.SRAD=0,EFF.GD=0,CCOEF=ccoef)
      
      coef_mx <- as.data.frame(sum_stp$coefficients)
      row.names(coef_mx)[1] <- "INT"
      rnames <- row.names(coef_mx)
      for (rn in rnames) {
        bts_df[which(bts_df$CELL == cell),rn] <- coef_mx$Estimate[which(rnames==rn)]
      }
      bts_df <- cbind(REP=br,bts_df)
      reg_df <- rbind(reg_df,bts_df)
    }
  }
  return(reg_df)
}



###get data for a given cell
get_cell_data <- function(cell,xy,glam_data,eco_data) {
  #coordinates of gridcell
  x_coord <- xy$x[which(xy$CELL == cell)]
  y_coord <- xy$y[which(xy$CELL == cell)]
  posit <- which(xy$CELL == cell)
  
  #yield and suitability data
  yield <- as.numeric(glam_data[posit,])
  suit <- as.numeric(eco_data[posit,])
  dfp <- data.frame(CELL=cell,YEAR=1966:1993,YIELD=yield,SUIT=suit)
  
  #dfo <- rbind(dfo,dfp)
  #plot(1966:1993,(yield-mean(yield))/sd(yield),ty="l",col="red")
  #lines(1966:1993,(suit-mean(suit))/sd(suit),ty="l",col="blue")
  #plot(yield,suit,pch=20,ylim=c(0,100),xlim=c(0,5000))
  return(dfp)
}


#assess the accuracy of EcoCrop's spatial prediction using a gridded dataset
#of presence and absence
eval_ecocrop <- function(rsl,eval_rs) {
  pa_rsl <- rsl; pa_rsl[which(rsl[]>0)] <- 1 #bin the prediction
  
  met <- xyFromCell(eval_rs,1:ncell(eval_rs))
  met <- cbind(met,PRE=extract(pa_rsl,met))
  met <- cbind(met,OBS=extract(eval_rs,1:ncell(eval_rs))); met <- as.data.frame(met)
  met <- met[which(!is.na(met$PRE)),]; met <- met[which(!is.na(met$OBS)),] #get rid of NAs
  
  #get the values *1 is observed and 2 is prediction
  ntp <- length(which(met$PRE > 0 & met$OBS == 1))
  tpr <- ntp/length(which(met$OBS == 1))
  #false negative rate
  nfp <- length(which(met$PRE == 0 & met$OBS == 1))
  fpr <- nfp/length(which(met$OBS == 1))
  #true negative rate (if absences are available)
  if (length(which(met$OBS == 0)) != 0) {
    ntn <- length(which(met$PRE > 0 & met$OBS == 0))
    tnr <- ntn / length(which(met$OBS == 0))
  } else {tnr <- NA}
  
  rm(met); g=gc(); rm(g)
  met.final <- data.frame(TPR=tpr, FPR=fpr, TNR=tnr)
  return(met.final)
}



#function to copy the relevant cru and iitm data for an ecocrop run
#everything will be in GeoTiff
copy_clim_data <- function(clm_type,iitm_dir,cru_dir,oclm_dir,cru_prefix=NA) {
  #create output folder if it does not exist
  oclm_dir <- paste(oclm_dir,"/",clm_type,sep="")
  if (!file.exists(oclm_dir)) {dir.create(oclm_dir,recursive=T)}
  
  if (!is.na(cru_prefix)) {cru_prefix <- paste(cru_prefix,"_",sep="")} else {cru_prefix <- ""}
  
  #loop through months
  for (i in 1:12) {
    #copying rainfall
    rf <- raster(paste(iitm_dir,"/rain-",i,".tif",sep=""))
    rf <- writeRaster(rf,paste(oclm_dir,"/prec_",i,".tif",sep=""),format="GTiff",overwrite=T)
    
    #copying min. temperature
    tn <- raster(paste(cru_dir,"/tmn_1dd/tmn_",cru_prefix,i,".asc",sep=""))
    tn <- crop(tn,rf)
    tn <- resample(tn,rf,method="ngb")
    tn[which(is.na(rf[]))] <- NA
    tn <- writeRaster(tn,paste(oclm_dir,"/tmin_",i,".tif",sep=""),format="GTiff",overwrite=T)
    rm(tn)
    
    #copying max. temperature
    tx <- raster(paste(cru_dir,"/tmx_1dd/tmx_",cru_prefix,i,".asc",sep=""))
    tx <- crop(tx,rf)
    tx <- resample(tx,rf,method="ngb")
    tx[which(is.na(rf[]))] <- NA
    tx <- writeRaster(tx,paste(oclm_dir,"/tmax_",i,".tif",sep=""),format="GTiff",overwrite=T)
    rm(tx)
    
    #copying min. temperature
    tm <- raster(paste(cru_dir,"/tmp_1dd/tmp_",cru_prefix,i,".asc",sep=""))
    tm <- crop(tm,rf)
    tm <- resample(tm,rf,method="ngb")
    tm[which(is.na(rf[]))] <- NA
    tm <- writeRaster(tm,paste(oclm_dir,"/tmean_",i,".tif",sep=""),format="GTiff",overwrite=T)
    rm(tm)
  }
  return(oclm_dir)
}


#produce growing season parameters based on sowing date and duration
get_gs_data <- function(i,rs) {
  x <- rs[i,] #get that row
  x2 <- calibrationParameters(x, gs=x$DUR, verbose=F) #get growing season data for that duration
  this_gs <- x$SOW_MTH #which month the crop was planted
  fields <- c(paste("GS",this_gs,"_P",sep=""),paste("GS",this_gs,"_T",sep=""),
              paste("GS",this_gs,"_N",sep=""),paste("GS",this_gs,"_X",sep=""))
  gs_data <- x2[,fields] #get only data for the growing season when crop was planted
  names(gs_data) <- c("GS1_P","GS1_T","GS1_N","GS1_X") #assign new names to fields for final merging
  x <- cbind(x,gs_data) #merge with original input data
  return(x)
}


#function to find corresponding fraction of month for a given Julian day
find_month <- function(jd,dg) {
  mth <- dg$MTH[which(dg$JD==jd)]
  day <- dg$DAY[which(dg$JD==jd)] / length(dg$DAY[which(dg$MTH==mth)])
  mth <- mth+day
  return(mth)
}
