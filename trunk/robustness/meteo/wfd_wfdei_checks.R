#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014

#############################################################################################
####### WFD/WFDEI daily data looked dodgy in places so i decided to compare these to CRU and WCL
#############################################################################################

#packages
library(raster); library(rgdal); library(ncdf); library(rasterVis); library(maptools)
data(wrld_simpl)

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
yi_dir <- paste(wd,"/data/yield_data_maize",sep="")

#load objects
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

#define dataset, period, variable
dataset <- "WFDEI" #WFD, WFDEI
years <- 1982:2005 #1982:2001 for WFD, 1982:2005 for WFDEI

#i/o directories for cru
cru_idir <- "~/Leeds-work/datasets/meteorology/cru-ts-v3-21"
cru_odir <- paste(met_dir,"/cru-ts-v3-21",sep="")
if (!file.exists(cru_odir)) {dir.create(cru_odir)}

#i/o directories for worldclim
wcl_idir <- "~/Leeds-work/datasets/meteorology/worldclim_global_5min"
wcl_odir <- paste(met_dir,"/worldclim",sep="")
if (!file.exists(wcl_odir)) {dir.create(wcl_odir)}

#/o directory for figs
fig_dir <- paste(wd,"/text/wfd_wfdei_checks/",tolower(dataset),sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir,recursive=T)}

#determine extent to cut the resampled netcdfs (of CRU and WorldClim)
yrs <- raster(paste(yi_dir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
bbox <- extent(yrs)

#loop variables
for (vname in c("Rainf","SWdown","Tmax","Tmin")) {
  #vname <- "Rainf"
  if (vname == "Rainf") {suffix <- "_GPCC"} else {suffix <- ""}
  
  cat("\n...processing",vname,"\n")
  
  #i/o data dirs
  data_dir <- paste(met_dir,"/baseline_climate/",vname,"_daily_",dataset,suffix,sep="")
  mdata_odir <- paste(met_dir,"/baseline_climate/",vname,"_monthly_",dataset,suffix,sep="")
  if (!file.exists(mdata_odir)) {dir.create(mdata_odir)}
  ydata_odir <- paste(met_dir,"/baseline_climate/",vname,"_yearly_",dataset,suffix,sep="")
  if (!file.exists(ydata_odir)) {dir.create(ydata_odir)}
  cdata_odir <- paste(met_dir,"/baseline_climate/",vname,"_climatology_",dataset,suffix,sep="")
  if (!file.exists(cdata_odir)) {dir.create(cdata_odir)}
  
  #mean climate output file
  clm_fname <- paste(cdata_odir,"/afr_",vname,"_meanclim_",dataset,suffix,"_",min(years),"-",max(years),".tif",sep="")
  
  #variability (sigma) output file
  var_fname <- paste(cdata_odir,"/afr_",vname,"_stdclim_",dataset,suffix,"_",min(years),"-",max(years),".tif",sep="")
  
  #variability (c.v.) output file
  cv_fname <- paste(cdata_odir,"/afr_",vname,"_cvclim_",dataset,suffix,"_",min(years),"-",max(years),".tif",sep="")
  
  #loop years
  for (yr in years) {
    #yr <- 1982
    cat("\n...processing year=",yr,"\n")
    
    #name of yearly file
    y_fname <- paste(ydata_odir,"/afr_",vname,"_yearly_",dataset,suffix,"_",yr,".tif",sep="")
    
    if (!file.exists(y_fname)) {
      #loop months
      for (m in 1:12) {
        #m <- 1
        cat("...processing month=",m,"\n")
        
        #file names
        i_fname <- paste(data_dir,"/afr_",vname,"_daily_",dataset,suffix,"_",yr,sprintf("%1$02d",m),".nc",sep="")
        m_fname <- paste(mdata_odir,"/afr_",vname,"_monthly_",dataset,suffix,"_",yr,sprintf("%1$02d",m),".tif",sep="")
        
        if (!file.exists(m_fname)) {
          #read raster
          rs <- stack(i_fname)
          
          #convert units
          if (vname == "Rainf") {
            rs <- rs * 3600 * 24 #kg m-2 s-1 to mm/day
          } else if (vname == "SWdown") {
            rs <- rs * 24 * 3600 / 1000000 #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
          } else {
            rs <- rs - 273.15 #K to C
          }
          
          #sum or mean
          if (vname == "Rainf") {
            rs_m <- calc(rs, function(x) {sum(x,na.rm=T)})
          } else {
            rs_m <- calc(rs, function(x) {mean(x,na.rm=T)})
          }
          
          #write raster
          rs_m <- writeRaster(rs_m,m_fname,format="GTiff")
        } else {
          rs_m <- raster(m_fname)
        }
        
        #put everything into a list/stack
        if (m == 1) {rs_y <- rs_m} else {rs_y <- c(rs_y,rs_m)}
      }
      
      #calculate mean / total for year
      rs_y <- stack(rs_y)
      #sum or mean
      if (vname == "Rainf") {
        rs_y <- calc(rs_y, function(x) {sum(x,na.rm=T)})
      } else {
        rs_y <- calc(rs_y, function(x) {mean(x,na.rm=T)})
      }
      
      #write raster
      rs_y <- writeRaster(rs_y,y_fname,format="GTiff")
    } else {
      rs_y <- raster(y_fname)
    }
    
    if (yr == years[1]) {
      rs_clm <- rs_y
    } else {
      rs_clm <- c(rs_clm,rs_y)
    }
  }
  
  rs_clm <- stack(rs_clm)
  
  #mean
  if (!file.exists(clm_fname)) {
    rs_mean <- calc(rs_clm, function(x) {mean(x,na.rm=T)})
    rs_mean <- writeRaster(rs_mean,clm_fname,format="GTiff")
  } else {
    rs_mean <- raster(clm_fname)
  }
  
  #s.d.
  if (!file.exists(var_fname)) {
    rs_var <- calc(rs_clm, function(x) {sd(x,na.rm=T)})
    rs_var <- writeRaster(rs_var,var_fname,format="GTiff")
  } else {
    rs_var <- raster(var_fname)
  }
  
  #c.v.
  if (!file.exists(cv_fname)) {
    if (vname == "Rainf") {
      rs_cv <- rs_var / (rs_mean+0.1) * 100
    } else {
      rs_cv <- rs_var / (rs_mean) * 100
    }
    rs_cv <- writeRaster(rs_cv,cv_fname,format="GTiff")
  }
  
  #calculate climatological mean, s.d., c.v. for all months
  cat("\n... calculating monthly means\n")
  for (m in 1:12) {
    #m <- 1
    cat("...processing month=",m,"\n")
    
    cmth_fname_clm <- paste(cdata_odir,"/afr_",vname,"_meanclim_",dataset,suffix,"_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep="")
    cmth_fname_var <- paste(cdata_odir,"/afr_",vname,"_stdclim_",dataset,suffix,"_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep="")
    cmth_fname_cv <- paste(cdata_odir,"/afr_",vname,"_cvclim_",dataset,suffix,"_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep="")
    
    #load all years rasters
    rs_stk <- stack(paste(mdata_odir,"/afr_",vname,"_monthly_",dataset,suffix,"_",years,sprintf("%1$02d",m),".tif",sep=""))
    
    #mean
    if (!file.exists(cmth_fname_clm)) {
      rs_mmean <- calc(rs_stk, function(x) {mean(x,na.rm=T)})
      rs_mmean <- writeRaster(rs_mmean,cmth_fname_clm,format="GTiff")
    } else {
      rs_mmean <- raster(cmth_fname_clm)
    }
    
    #s.d.
    if (!file.exists(cmth_fname_var)) {
      rs_mvar <- calc(rs_stk, function(x) {sd(x,na.rm=T)})
      rs_mvar <- writeRaster(rs_mvar,cmth_fname_var,format="GTiff")
    } else {
      rs_mvar <- raster(cmth_fname_var)
    }
    
    #c.v.
    if (!file.exists(cmth_fname_cv)) {
      if (vname == "Rainf") {
        rs_mcv <- rs_mvar / (rs_mmean+0.1) * 100
      } else {
        rs_mcv <- rs_mvar / (rs_mmean) * 100
      }
      rs_mcv <- writeRaster(rs_mcv,cmth_fname_cv,format="GTiff")
    }
  }
}


################################################################################
################################################################################
#### calculate mean, s.d. and c.v. for cru, for africa

##first process the CRU time series 1950-2012 (change resolution and cut to Africa)
#loop variables
for (vname in c("pre","tmn","tmx","tmp")) {
  #vname <- "pre"
  cat("\n...processing variable=",vname,"\n")
  
  cru_ovdir <- paste(cru_odir,"/",vname,sep="")
  if (!file.exists(cru_ovdir)) {dir.create(cru_ovdir)}
  
  #loop years
  for (yr in 1950:2012) {
    #yr <- 1950
    cat("...processing cru for year=",yr,"\n")
    
    ifil <- paste(cru_idir,"/",vname,"/",vname,"_",yr,".nc",sep="")
    ofil <- paste(cru_ovdir,"/afr_",vname,"_",yr,".nc",sep="")
    
    if (!file.exists(ofil)) {
      #first remapcon2
      system(paste("cdo remapcon2,r320x160 ",ifil," ",cru_ovdir,"/",vname,"_remapped.nc",sep=""))
      
      #second cut to africa
      system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",cru_ovdir,"/",vname,"_remapped.nc ",ofil,sep=""))
      
      #remove junk
      system(paste("rm -f ",cru_ovdir,"/",vname,"_remapped.nc",sep=""))
    }
  }
}


###
#calculate sum / mean of year, climatology for year and months
for (vname in c("pre","tmn","tmx","tmp")) {
  #vname <- "pre"
  cru_ovdir <- paste(cru_odir,"/",vname,sep="")
  yr_cru_odir <- paste(cru_odir,"/",vname,"_yearly",sep="")
  if (!file.exists(yr_cru_odir)) {dir.create(yr_cru_odir)}
  clm_cru_odir <- paste(cru_odir,"/",vname,"_climatology",sep="")
  if (!file.exists(clm_cru_odir)) {dir.create(clm_cru_odir)}
  
  #loop years
  for (yr in years) {
    #yr <- years[1]
    cat("...processing year=",yr,"\n")
    
    y_fname <- paste(yr_cru_odir,"/afr_",vname,"_",yr,".tif",sep="")
    
    if (!file.exists(y_fname)) {
      rs_y <- stack(paste(cru_ovdir,"/afr_",vname,"_",yr,".nc",sep=""))
      if (vname == "pre") {
        rs_y <- calc(rs_y, function(x) {sum(x,na.rm=T)})
      } else {
        rs_y <- calc(rs_y, function(x) {mean(x,na.rm=T)})
      }
      rs_y <- writeRaster(rs_y,y_fname,format="GTiff")
    } else {
      rs_y <- raster(y_fname)
    }
    
    if (yr == years[1]) {
      rs_clm <- rs_y
    } else {
      rs_clm <- c(rs_clm,rs_y)
    }
  }
  
  rs_clm <- stack(rs_clm)
  
  clm_fname <- paste(clm_cru_odir,"/afr_",vname,"_meanclim_",min(years),"-",max(years),".tif",sep="")
  var_fname <- paste(clm_cru_odir,"/afr_",vname,"_stdclim_",min(years),"-",max(years),".tif",sep="")
  cv_fname <- paste(clm_cru_odir,"/afr_",vname,"_cvclim_",min(years),"-",max(years),".tif",sep="")
  
  #mean
  if (!file.exists(clm_fname)) {
    rs_mean <- calc(rs_clm, function(x) {mean(x,na.rm=T)})
    rs_mean <- writeRaster(rs_mean,clm_fname,format="GTiff")
  } else {
    rs_mean <- raster(clm_fname)
  }
  
  #s.d.
  if (!file.exists(var_fname)) {
    rs_var <- calc(rs_clm, function(x) {sd(x,na.rm=T)})
    rs_var <- writeRaster(rs_var,var_fname,format="GTiff")
  } else {
    rs_var <- raster(var_fname)
  }
  
  #c.v.
  if (!file.exists(cv_fname)) {
    if (vname == "pre") {
      rs_cv <- rs_var / (rs_mean+0.1) * 100
    } else {
      rs_cv <- rs_var / (rs_mean) * 100
    }
    rs_cv <- writeRaster(rs_cv,cv_fname,format="GTiff")
  }
  
  #calculate climatological mean, s.d., c.v. for all months
  cat("\n... calculating monthly means\n")
  for (m in 1:12) {
    #m <- 1
    cat("...processing month=",m,"\n")
    
    cmth_fname_clm <- paste(clm_cru_odir,"/afr_",vname,"_meanclim_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep="")
    cmth_fname_var <- paste(clm_cru_odir,"/afr_",vname,"_stdclim_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep="")
    cmth_fname_cv <- paste(clm_cru_odir,"/afr_",vname,"_cvclim_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep="")
    
    #load all years rasters
    for (yr in years) {
      #yr <- years[1]
      rs_tstk <- stack(paste(cru_ovdir,"/afr_",vname,"_",yr,".nc",sep=""))
      if (yr == years[1]) {rs_stk <- rs_tstk[[m]]} else {rs_stk <- c(rs_stk, rs_tstk[[m]])}
    }
    rs_stk <- stack(rs_stk)
    
    #mean
    if (!file.exists(cmth_fname_clm)) {
      rs_mmean <- calc(rs_stk, function(x) {mean(x,na.rm=T)})
      rs_mmean <- writeRaster(rs_mmean,cmth_fname_clm,format="GTiff")
    } else {
      rs_mmean <- raster(cmth_fname_clm)
    }
    
    #s.d.
    if (!file.exists(cmth_fname_var)) {
      rs_mvar <- calc(rs_stk, function(x) {sd(x,na.rm=T)})
      rs_mvar <- writeRaster(rs_mvar,cmth_fname_var,format="GTiff")
    } else {
      rs_mvar <- raster(cmth_fname_var)
    }
    
    #c.v.
    if (!file.exists(cmth_fname_cv)) {
      if (vname == "pre") {
        rs_mcv <- rs_mvar / (rs_mmean+0.1) * 100
      } else {
        rs_mcv <- rs_mvar / (rs_mmean) * 100
      }
      rs_mcv <- writeRaster(rs_mcv,cmth_fname_cv,format="GTiff")
    }
  }
}


################################################################################
################################################################################
#aggregate worldclim to 0.5 degree, then remapnn, then cut to Africa

for (vname in c("prec","tmax","tmin","tmean")) {
  #vname <- "prec"
  
  for (m in 1:12) {
    #m <- 1
    cat("...processing month",m,"\n")
    
    #file names
    ifil <- paste(wcl_idir,"/",vname,"_",m,sep="")
    ofil <- paste(wcl_odir,"/afr_", vname,"_",m,".nc",sep="")
    
    if (!file.exists(ofil)) {
      #load raster
      rs <- raster(ifil)
      
      #aggregate to 0.5 degree
      rs <- aggregate(rs, fact=6, fun=mean, expand=T, na.rm=T)
      rsx <- raster(xmn=-180,xmx=180,ymn=-90,ymx=90,ncols=720,nrows=360)
      rs <- merge(rs,rsx)
      rs <- rotate(rs)
      
      #write .nc
      rs <- writeRaster(rs, paste(wcl_odir,"/", vname,"_",m,".nc",sep=""), format="CDF", varname="pr", varunit="mm", 
                        longname="Total monthly precipitation in mm", xname="lon", yname="lat",
                        zname="time", zunit="day")
      
      #remapcon2
      system(paste("cdo remapnn,r320x160 ",wcl_odir,"/", vname,"_",m,".nc"," ",wcl_odir,"/",vname,"_remapped.nc",sep=""))
      
      #cut to Africa
      system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",wcl_odir,"/",vname,"_remapped.nc ",ofil,sep=""))
      
      #remove junk
      system(paste("rm -f ",wcl_odir,"/",vname,"_",m,".nc",sep=""))
      system(paste("rm -f ",wcl_odir,"/",vname,"_remapped.nc",sep=""))
      
      #load raster
      rso <- raster(ofil)
    } else {
      rso <- raster(ofil)
    }
    
    #append into single object
    if (m == 1) {rs_all <- rso} else {rs_all <- c(rs_all, rso)}
  }
  rs_all <- stack(rs_all)
  
  clm_fname <- paste(wcl_odir,"/afr_",vname,"_meanclim.tif",sep="")
  
  #mean
  if (!file.exists(clm_fname)) {
    if (vname == "prec") {
      rs_mean <- calc(rs_all, function(x) {sum(x,na.rm=T)})
    } else {
      rs_mean <- calc(rs_all, function(x) {mean(x,na.rm=T)})
    }
    rs_mean <- writeRaster(rs_mean,clm_fname,format="GTiff")
  } else {
    rs_mean <- raster(clm_fname)
  }
}


################################################################################
################################################################################
###
#plot maps of CRU, worldclim, WFD or WFDEI
###

### functions
#functions
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
  }
  if (rev) {pal <- rev(pal)}
  
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  
  #set theme
  this_theme <- custom.theme(fill = pal,region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}

#figure details
ht <- 6
rs <- yrs
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=15), norths=seq(-90,90,by=15))

#loop variable
for (vname in c("Rainf","Tmax","Tmin")) {
  #vname <- "Rainf"
  cat("\n...processing for",vname,"\n")
  
  if (vname == "Rainf") {vname_cru <- "pre"; vname_wcl <- "prec"}
  if (vname == "Tmax") {vname_cru <- "tmx"; vname_wcl <- "tmax"}
  if (vname == "Tmin") {vname_cru <- "tmn"; vname_wcl <- "tmin"}
  if (vname == "Rainf") {suffix <- "_GPCC"} else {suffix <- ""}
  
  #define minval, maxval (for yearly)
  if (vname == "Rainf") {minval <- 1e-5; maxval <- 5000}
  if (vname == "Tmax") {minval <- 10; maxval <- 45}
  if (vname == "Tmin") {minval <- 0; maxval <- 35}
  
  #1. mean climate (all datasets)
  #a. year
  rs_wcl <- raster(paste(wcl_odir,"/afr_",vname_wcl,"_meanclim.tif",sep=""))
  if (vname != "Rainf") {rs_wcl <- rs_wcl * 0.1}
  rs_cru <- raster(paste(cru_odir,"/",vname_cru,"_climatology/afr_",vname_cru,"_meanclim_",min(years),"-",max(years),".tif",sep=""))
  rs_dse <- raster(paste(met_dir,"/baseline_climate/",vname,"_climatology_",dataset,suffix,"/afr_",vname,"_meanclim_",dataset,suffix,"_",min(years),"-",max(years),".tif",sep=""))
  rs_dse <- crop(rs_dse,rs_wcl)
  
  trs <- stack(rs_wcl,rs_cru,rs_dse)
  names(trs) <- c("WorldClim","CRU-TS v3.21",dataset)
  #print(min(trs[],na.rm=T)); print(max(trs[],na.rm=T))
  if (vname == "Rainf") {
    tplot <- rs_levplot2(trs,zn=minval,zx=maxval,nb=20,brks=NA,scale="Spectral",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
  } else {
    tplot <- rs_levplot2(trs,zn=minval,zx=maxval,nb=20,brks=NA,scale="YlOrRd",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
  }
  
  #plot into a three-panel figure
  pdf(paste(fig_dir,"/meanclim_",vname,"_",dataset,".pdf",sep=""), height=3.5,width=10,pointsize=14)
  print(tplot)
  dev.off()
  
  ##
  #2. c.v. (only CRU and WF* datasets)
  rs_cru <- raster(paste(cru_odir,"/",vname_cru,"_climatology/afr_",vname_cru,"_cvclim_",min(years),"-",max(years),".tif",sep=""))
  rs_dse <- raster(paste(met_dir,"/baseline_climate/",vname,"_climatology_",dataset,suffix,"/afr_",vname,"_cvclim_",dataset,suffix,"_",min(years),"-",max(years),".tif",sep=""))
  rs_dse <- crop(rs_dse,rs_cru)
  rs_cru[which(rs_cru[] > 100)] <- 100
  rs_dse[which(rs_dse[] > 100)] <- 100
  trs <- stack(rs_cru,rs_dse)
  names(trs) <- c("CRU-TS v3.21",dataset)
  tplot <- rs_levplot2(trs,zn=0,zx=100,nb=20,brks=NA,scale="Blues",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
  pdf(paste(fig_dir,"/cvclim_",vname,"_",dataset,".pdf",sep=""), height=3.5,width=7.5,pointsize=14)
  print(tplot)
  dev.off()
  
  #define minval, maxval (for monthly)
  if (vname == "Rainf") {minval <- 1e-5; maxval <- 1000}
  if (vname == "Tmax") {minval <- 10; maxval <- 45}
  if (vname == "Tmin") {minval <- 0; maxval <- 35}
  
  #b. each month
  for (m in 1:12) {
    #m <- 1
    cat("...plotting for month",m,"\n")
    
    #load rasters
    rs_wcl <- raster(paste(wcl_odir,"/afr_",vname_wcl,"_",m,".nc",sep=""))
    if (vname != "Rainf") {rs_wcl <- rs_wcl * 0.1}
    rs_cru <- raster(paste(cru_odir,"/",vname_cru,"_climatology/afr_",vname_cru,"_meanclim_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep=""))
    rs_dse <- raster(paste(met_dir,"/baseline_climate/",vname,"_climatology_",dataset,suffix,"/afr_",vname,"_meanclim_",dataset,suffix,"_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep=""))
    rs_dse <- crop(rs_dse,rs_wcl)
    
    #plot into a three-panel figure
    trs <- stack(rs_wcl,rs_cru,rs_dse)
    names(trs) <- c("WorldClim","CRU-TS v3.21",dataset)
    #maxval <- max(trs[],na.rm=T)
    if (vname == "Rainf") {
      tplot <- rs_levplot2(trs,zn=minval,zx=maxval,nb=20,brks=NA,scale="Spectral",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
    } else {
      tplot <- rs_levplot2(trs,zn=minval,zx=maxval,nb=20,brks=NA,scale="YlOrRd",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
    }
    pdf(paste(fig_dir,"/meanclim_",vname,"_",dataset,"_",sprintf("%1$02d",m),".pdf",sep=""), height=3.5,width=10,pointsize=14)
    print(tplot)
    dev.off()
    
    #2. c.v. (only CRU and WF* datasets)
    rs_cru <- raster(paste(cru_odir,"/",vname_cru,"_climatology/afr_",vname_cru,"_cvclim_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep=""))
    rs_dse <- raster(paste(met_dir,"/baseline_climate/",vname,"_climatology_",dataset,suffix,"/afr_",vname,"_cvclim_",dataset,suffix,"_",min(years),"-",max(years),"_",sprintf("%1$02d",m),".tif",sep=""))
    rs_dse <- crop(rs_dse,rs_cru)
    rs_cru[which(rs_cru[] > 100)] <- 100
    rs_dse[which(rs_dse[] > 100)] <- 100
    trs <- stack(rs_cru,rs_dse)
    names(trs) <- c("CRU-TS v3.21",dataset)
    tplot <- rs_levplot2(trs,zn=0,zx=100,nb=20,brks=NA,scale="Blues",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
    pdf(paste(fig_dir,"/cvclim_",vname,"_",dataset,"_",sprintf("%1$02d",m),".pdf",sep=""), height=3.5,width=7.5,pointsize=14)
    print(tplot)
    dev.off()
  }
}


################################################################################
################################################################################
##### calculate yield / mm (or / kg rain fall)

rs_wcl <- raster(paste(wcl_odir,"/afr_prec_meanclim.tif",sep=""))
rs_cru <- raster(paste(cru_odir,"/pre_climatology/afr_pre_meanclim_",min(years),"-",max(years),".tif",sep=""))
rs_dse <- raster(paste(met_dir,"/baseline_climate/Rainf_climatology_",dataset,"_GPCC/afr_Rainf_meanclim_",dataset,"_GPCC_",min(years),"-",max(years),".tif",sep=""))
rs_dse <- crop(rs_dse,rs_wcl)

#set grid cells not in yield dataset as NA
true_cells <- cellFromXY(rs_wcl,xy_main_yield[,c("x","y")])
rs_wcl[!(1:ncell(rs_wcl)) %in% true_cells] <- NA
true_cells <- cellFromXY(rs_cru,xy_main_yield[,c("x","y")])
rs_cru[!(1:ncell(rs_cru)) %in% true_cells] <- NA
true_cells <- cellFromXY(rs_dse,xy_main_yield[,c("x","y")])
rs_dse[!(1:ncell(rs_dse)) %in% true_cells] <- NA

#create raster with yield
rs_yield <- raster(rs_dse)
rs_yield[cellFromXY(rs_yield,xy_main_yield[,c("x","y")])] <- rowMeans(xy_main_yield[,paste("Y.",years,sep="")])

#calculate g grain / kg water ratio
#mm * 10 [m3 / ha] * 1000 [kg / m3] = kg / ha water
ratio_wcl <- (rs_yield * 1000) / (rs_wcl * 10 * 1000)
ratio_cru <- (rs_yield * 1000) / (rs_cru * 10 * 1000)
ratio_dse <- (rs_yield * 1000) / (rs_dse * 10 * 1000)

trs <- stack(ratio_wcl,ratio_cru,ratio_dse)
names(trs) <- c("WorldClim","CRU-TS v3.21",dataset)

tplot <- rs_levplot2(trs,zn=0,zx=0.75,nb=15,brks=NA,scale="YlGnBu",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/yield_ratio_Rainf_",dataset,".pdf",sep=""), height=3.5,width=10,pointsize=14)
print(tplot)
dev.off()


