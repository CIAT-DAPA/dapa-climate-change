#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015
#stop("!")

#load libraries
library(raster); library(rgdal); library(RMAWGEN); library(sp); library(maptools)
library(lhs); library(MASS); library(RColorBrewer)

#directories
wd <- "~/Leeds-work/p4s-csa/hh-analyses"
agmerra_dir <- paste(wd,"/AgMERRA_data",sep="")
chirps_dir <- paste(wd,"/CHIRPS_data/chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05",sep="")
cru_dir <- paste(wd,"/CRU_data",sep="")
adm_dir <- paste(wd,"/adm_data",sep="")
isric_dir <- paste(wd,"/ISRIC_soil",sep="")
ifprisoil_dir <- paste(wd,"/IFPRI_soil_profiles",sep="")
dssat_dir <- paste(wd,"/dssat_bin/dssat-csm-4.6.1.0_bin",sep="")
mdata_outdir <- paste(wd,"/model_data",sep="")
if (!file.exists(mdata_outdir)) {dir.create(mdata_outdir)}
runs_dir <- paste(mdata_outdir,"/model_runs_mill",sep="")
if (!file.exists(runs_dir)) {dir.create(runs_dir)}

#source all functions
source(paste(wd,"/scripts/extract_CHIRPS.R",sep=""))
source(paste(wd,"/scripts/extract_CRU.R",sep=""))
source(paste(wd,"/scripts/extract_AgMERRA.R",sep=""))
source(paste(wd,"/scripts/wgen_srad.R",sep=""))
source(paste(wd,"/scripts/extract_weather_loc.R",sep=""))
source(paste(wd,"/scripts/calc_phdate.R",sep=""))
source(paste(wd,"/scripts/calc_risk_indices.R",sep=""))
source(paste(wd,"/scripts/get_soils_obj.R",sep=""))
source(paste(wd,"/scripts/get_xfile_obj.R",sep=""))
source(paste(wd,"/scripts/update_soil_xfile.R",sep=""))
source(paste(wd,"/scripts/make_soilfile.R",sep=""))
source(paste(wd,"/scripts/make_xfile.R",sep=""))
source(paste(wd,"/scripts/make_wthfile.R",sep=""))

#load Niger shp
shp <- readRDS(paste(adm_dir,"/NER_adm1.rds",sep=""))

#load hh data
hh_data <- read.csv(paste(wd,"/hh_data/LSMS_Niger_ResultsforRobertII.csv",sep=""))
hh_data$contrib_foodcrops_total <- hh_data$contrib_foodcrops_cons + hh_data$contrib_foodcrops_sold

#select millet-based households
hh_mill <- hh_data[which(hh_data$contrib_millet == 1 & hh_data$contrib_foodcrops_total == 1),]
hh_mill <- hh_mill[which(hh_mill$area.acres > 0),]
hh_mill_xy <- data.frame(hhid=hh_mill$hhid,lon=hh_mill$longitude,lat=hh_mill$latitude)

#extract weather for locations
if (!file.exists(paste(mdata_outdir,"/millet_hh_wth.RData",sep=""))) {
  #extract data
  loc_wth <- extract_weather_loc(year=2011,chirps_dir,agmerra_dir,cru_dir,xy=hh_mill_xy)
  
  #save object
  save(list=c("loc_wth","hh_mill_xy"), file=paste(mdata_outdir,"/millet_hh_wth.RData",sep=""))
} else {
  load(file=paste(mdata_outdir,"/millet_hh_wth.RData",sep=""))
}

#extract soil data, and calculate soil water holding capacity for selected sites
#soilcap = sum(af_AWCh2__M_sd[1-x]_1km) * x
if (!file.exists(paste(mdata_outdir,"/soil_mill.RData",sep=""))) {
  root_depth <- raster(paste(isric_dir,"/af_ERDICM__M_1km.tif",sep=""))
  shp_prj <- spTransform(shp, root_depth@crs)
  root_depth <- crop(root_depth, extent(shp_prj))
  root_depth <- projectRaster(root_depth, crs=shp@proj4string)
  xy_soil <- hh_mill_xy
  xy_soil$rdepth <- extract(root_depth, data.frame(x=xy_soil$lon, y=xy_soil$lat))
  
  depths <- c(25,100,225,450,800,1500)
  for (s_i in 1:6) {
    #s_i <- 1
    tdepth <- depths[s_i]
    cat("...extracting depth=",tdepth,"mm\n")
    rs <- raster(paste(isric_dir,"/af_AWCh2__M_sd",s_i,"_1km.tif",sep=""))
    rs <- crop(rs, extent(shp_prj))
    rs <- projectRaster(rs, crs=shp@proj4string)
    xy_soil$value <- extract(rs, data.frame(x=xy_soil$lon, y=xy_soil$lat))
    names(xy_soil)[ncol(xy_soil)] <- paste("d.",tdepth,sep="")
  }
  #calculate soil water holding capacity in mm, minval and maxval taken from
  #Fatondji et al. (2012) --in: Kihara, J. et al. Improving soil fert. recommendation using DSSAT
  xy_soil$soilcp <- apply(xy_soil,1,FUN=soilcap_calc,minval=45,maxval=100)
  
  #put names of AWCh2 raster in here
  names(xy_soil)[grep("d.",names(xy_soil),fixed=T)] <- paste("AWCh2.",names(xy_soil)[grep("d.",names(xy_soil),fixed=T)],sep="")
  
  #extract the rest of the soil data
  soilgrid_list <- c("BLD_T","CEC_T","CLYPPT_T","CRFVOL_T","LRI_T","ORCDRC_T","SLTPPT_T","SNDPPT_T","WWP","tetaS")
  soilgrid_list <- "PHIHOX_T"
  for (soilgrid in soilgrid_list) {
    #soilgrid <- soilgrid_list[1]
    for (s_i in 1:6) {
      #s_i <- 1
      tdepth <- depths[s_i]
      cat("...extracting soilgrid=",soilgrid,"depth=",tdepth,"mm\n")
      rs <- raster(paste(isric_dir,"/af_",soilgrid,"__M_sd",s_i,"_1km.tif",sep=""))
      rs <- crop(rs, extent(shp_prj))
      rs <- projectRaster(rs, crs=shp@proj4string)
      xy_soil$value <- extract(rs, data.frame(x=xy_soil$lon, y=xy_soil$lat))
      names(xy_soil)[ncol(xy_soil)] <- paste(soilgrid,".d.",tdepth,sep="")
      rm(rs); g=gc(); rm(g)
    }
  }
  
  save(xy_soil, file=paste(mdata_outdir,"/soil_mill.RData",sep=""))
} else {
  load(file=paste(mdata_outdir,"/soil_mill.RData",sep=""))
}

#year
year <- 2011

#calculate watbal
if (!file.exists(paste(mdata_outdir,"/millet_hh_wth_watbal.RData",sep=""))) {
  for (x_i in 1:length(loc_wth)) {
    loc_wth[[x_i]] <- watbal_wrapper(out_all=loc_wth[[x_i]], soilcp=xy_soil$soilcp[x_i])
    #plot(loc_wth[[x_i]]$ERATIO*100,ty="l"); lines(loc_wth[[x_i]]$ETMAX,col="red"); lines(loc_wth[[x_i]]$RAIN,col="blue")
  }
  save(loc_wth, file=paste(mdata_outdir,"/millet_hh_wth_watbal.RData",sep=""))
} else {
  load(file=paste(mdata_outdir,"/millet_hh_wth_watbal.RData",sep=""))
}

#determine likely sowing dates for each site, report also estimated harvest date
#taken from Marteau et al. (2011)
min_ini <- as.numeric(format(as.Date(paste0(year,"/4/15")) ,"%j"))
max_ini <- as.numeric(format(as.Date(paste0(year,"/6/30")) ,"%j")) + 20
max_end <- as.numeric(format(as.Date(paste0(year,"/10/27")) ,"%j")) + 20

if (!file.exists(paste(mdata_outdir,"/phdates_mill.RData",sep=""))) {
  out_seas <- data.frame()
  for (x_i in 1:length(loc_wth)) {
    #x_i <- 1
    sdate <- pdate_marteau(loc_wth[[x_i]],mindate=min_ini,maxdate=max_ini,p_thresh=1)
    min_end <- sdate + 50
    hdate1 <- hdate_mohamed(loc_wth[[x_i]],mindate=min_end,maxdate=max_end,p_thresh=1)
    hdate2 <- hdate_jones(loc_wth[[x_i]],mindate=min_end,maxdate=max_end,e_thresh=0.1)
    hdate <- max(c(hdate1,hdate2),na.rm=T)
    if (is.na(hdate)) {hdate <- max_end}
    #plot(loc_wth[[x_i]]$ERATIO*100,ty="l"); lines(loc_wth[[x_i]]$RAIN,col="blue"); abline(v=c(sdate,hdate))
    
    #calculate indices and store
    totrain <- calc_totrain(loc_wth[[x_i]],season_ini=sdate,season_end=hdate)
    raindays <- calc_raindays(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,p_thresh=1)
    cdd_max <- calc_max_cdd(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,p_thresh=1)
    cdd_mean <- calc_mean_cdd(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,p_thresh=0.1)
    txx_pn <- calc_txxdays(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,t_thresh=40)
    txx_dev <- calc_txxdays(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,t_thresh=34)
    tnn_pn <- calc_tnndays(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,t_thresh=10)
    wsdays <- calc_wsdays(loc_wth[[x_i]],season_ini=sdate,season_end=hdate,e_thresh=0.3)
    
    #output row and append to data.frame
    out_row <- data.frame(hhid=hh_mill_xy$hhid[x_i],lon=hh_mill_xy$lon[x_i],lat=hh_mill_xy$lat[x_i],
                          SDATE=sdate,HDATE1=hdate1,HDATE2=hdate2,HDATE=hdate,
                          SEASDUR=(hdate-sdate),TOTRAIN=totrain,RAINDAYS=raindays,CDD_MAX=cdd_max,
                          CDD_MEAN=cdd_mean,TXX_PN=txx_pn,TXX_DEV=txx_dev,TNN_PN=tnn_pn,
                          WSDAYS=wsdays)
    out_seas <- rbind(out_seas,out_row)
  }
  save(out_seas, file=paste(mdata_outdir,"/phdates_mill.RData",sep=""))
} else {
  load(file=paste(mdata_outdir,"/phdates_mill.RData",sep=""))
}


### create maximin latin hypercube matrix (same for all households)
param_list <- read.table(paste(mdata_outdir,"/perturb_parameter_list.tab",sep=""),header=T)
nparam <- nrow(param_list)
nrep <- 300
set.seed(2739); lhyp1 <- maximinLHS(n=nrep,k=nparam,dup=1)

### perform runs for all selected households
for (hh_i in 1:nrow(hh_mill_xy)) {
  #hh_i <- 2
  if (!file.exists(paste(runs_dir,"/model_output_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))) {
    #create run folder and copy relevant files (inc. executable) in there
    trun_dir <- paste(runs_dir,"/hhid-",hh_mill_xy$hhid[hh_i],sep="")
    if (!file.exists(trun_dir)) {dir.create(trun_dir)}
    
    setwd(trun_dir)
    system(paste("cp -f ",dssat_dir,"/MLCER046.* .",sep=""))
    system(paste("cp -f ",dssat_dir,"/*.LST .",sep=""))
    system(paste("cp -f ",dssat_dir,"/*.CDE .",sep=""))
    system(paste("cp -f ",dssat_dir,"/*.SDA .",sep=""))
    system(paste("cp -f ",dssat_dir,"/*.WDA .",sep=""))
    system(paste("cp -f ",dssat_dir,"/DSCSM046.* .",sep=""))
    system(paste("cp -f ",dssat_dir,"/DSS* .",sep=""))
    
    #first run hypercube simulations for household
    summary_all <- data.frame()
    for (hy_i in 1:nrow(lhyp1)) {
      #hy_i <- 1
      cat("\n...running simulation number=",hy_i,"for hhid=",hh_mill_xy$hhid[hh_i],"\n")
      
      #get initial soil profile and xfile objects
      loc_soil <- get_soils_obj(soil_data=xy_soil, xy_loc=hh_mill_xy[hh_i,], soilgen_file=paste(ifprisoil_dir,"/NE.RData",sep=""))
      loc_xfil <- get_xfile(years=year,basename="P4NE",xy_loc=out_seas[hh_i,])
      
      #update soil and xfile objects
      tempobj <- update_soil_xfile(hyp_line=lhyp1[hy_i,], xfil_data=loc_xfil, soil_data=loc_soil, param_list)
      loc_soil <- tempobj$SOIL; loc_xfil <- tempobj$XFILE; rm(tempobj)
      
      #get weather
      wth_site <- loc_wth[[hh_i]]
      wth_head <- retrieve_header(x=wth_site, xy_loc=hh_mill_xy[hh_i,],years=year)
      wth_site$DATE <- paste(format(as.Date(wth_site$DATE), "%y"),format(as.Date(wth_site$DATE), "%j"),sep="")
      
      #second, write files in runs folder
      wthfil <- write_wth(in_data=wth_site, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
      soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
      xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
      
      #third, run model
      system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
      
      #fourth, load the seasonal output (yield, biomass, pdate, duration, NUCM, CNAM, ONTAM, OCTAM)
      summ_out <- read.table("Summary.OUT",skip=4)
      names(summ_out) <- c("RUNNO","TRNO","R#","O#","C#","CR","MODEL","TNAM1","TNAM2","FNAM","WSTA","SOIL_ID",
                           "SDAT","PDAT","EDAT","ADAT","MDAT","HDAT","DWAP","CWAM","HWAM","HWAH",
                           "BWAH","PWAM","HWUM","H#AM","H#UM","HIAM","LAIX","IR#M","IRCM","PRCM",
                           "ETCM","EPCM","ESCM","ROCM","DRCM","SWXM","NI#M","NICM","NFXM","NUCM",
                           "NLCM","NIAM","CNAM","GNAM","PI#M","PICM","PUPC","SPAM","KI#M","KICM",
                           "KUPC","SKAM","RECM","ONTAM","ONAM","OPTAM","OPAM","OCTAM","OCAM","DMPPM",
                           "DMPEM","DMPTM","DMPIM","YPPM","YPEM","YPTM","YPIM","DPNAM","DPNUM",
                           "YPNAM","YPNUM","NDCH","TMAXA","TMINA","SRADA","DAYLA","CO2A","PRCP",
                           "ETCP","ESCP","EPCP")
      summ_out <- summ_out[,c("RUNNO","TNAM1","TNAM2","PDAT","ADAT","MDAT","NDCH","CWAM","HWAM",
                              "CNAM","ONTAM","OCTAM")]
      summ_out$TNAM <- paste(summ_out$TNAM1, summ_out$TNAM2, sep=" ")
      summ_out$TNAM1 <- summ_out$TNAM2 <- NULL
      
      #read OVERVIEW.OUT line by line, to get stresses
      cat("......reading overview file\n")
      ifil <- file("OVERVIEW.OUT", open="r")
      all_lines <- readLines(ifil,n=-1)
      close(ifil); rm(ifil)
      runno <- 1
      allstress <- data.frame()
      for (l_i in 1:length(all_lines)) {
        #l_i <- 5
        tline <- all_lines[l_i]
        tcon <- textConnection(tline)
        tline <- try(read.table(tcon),silent=T)
        close(tcon)
        
        if (class(tline) != "try-error") {
          if (length(grep("\\*ENVIRONMENTAL",tline$V1)) > 0) {
            #cat("found a run, reading all the stuff line=",l_i,"\n")
            #phases as follows:
            #EM-EJ: Emergence-End Juvenile
            #EJ-FI: End Juvenil-Floral Init
            #FI-LX: Floral Init-End Lf Grow
            #LX-GF: End Lf Grth-Beg Grn Fil
            #GFILL: Grain Filling Phase
            phases <- c("EM-EJ","EJ-FI","FI-LX","LX-GF","GFILL")
            iphase <- 1
            for (j in (l_i+8):(l_i+12)) {
              #j <- l_i+8
              trow <- all_lines[j]
              rowcon <- textConnection(trow)
              trow <- read.table(rowcon)
              close(rowcon)
              trow <- trow[,(ncol(trow)-5):(ncol(trow)-2)]
              names(trow) <- paste(phases[iphase],"_",c("WPHOTO","WGWTH","NPHOTO","NGWTH"),sep="")
              if (iphase==1) {stressdata <- trow} else {stressdata <- cbind(stressdata,trow)}
              iphase <- iphase+1
              closeAllConnections()
            }
            stressdata <- cbind(RUNNO=runno,stressdata)
            allstress <- rbind(allstress, stressdata)
            runno <- runno+1
          }
        }
      }
      rm(all_lines)
      
      #merge with summ_out
      summ_out <- merge(summ_out, allstress, by="RUNNO")
      
      #fifth, reading done, delete wth, soil, and xfile files
      system("rm -f *.WTH")
      system("rm -f P4.SOL")
      system("rm -f P4NE1101.MLX")
      
      #finally, append to global data.frame for this household, and remove objects
      summary_all <- rbind(summary_all, cbind(HYP_RUN=hy_i,summ_out))
      rm(list=c("summ_out","allstress"))
    }
    
    #save output object
    save(summary_all, file=paste(runs_dir,"/model_output_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))
    
    #bail out and delete folder
    setwd("~")
    system(paste("rm -rf ",trun_dir,sep=""))
  } else {
    load(file=paste(runs_dir,"/model_output_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))
  }
  
  #for checking results
  #hh_i <- 9; load(file=paste(runs_dir,"/model_output_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))
  #plot(loc_wth[[hh_i]]$ERATIO*100,ty="l"); lines(loc_wth[[hh_i]]$RAIN,col="blue"); abline(v=c(out_seas$SDATE[hh_i],out_seas$HDATE[hh_i]))
  
  #second, calculate obs. yield, and get estimated growing season duration
  farm_area <- hh_mill$area.acres[hh_i] * 0.404686 #acre to ha
  tot_ener <- hh_mill$HHTotalEnergy[hh_i] #assume this is in kcal
  tot_prod <- tot_ener / 3650 #3650 is factor for millet, assume factor is kcal kg-1
  yield <- tot_prod / farm_area #in kg ha-1
  seasdur <- out_seas$SEASDUR[hh_i]
  
  #third, select simulations within some range of yield and duraton values (25th percentile diff.)
  kdata <- summary_all[,c("HYP_RUN","RUNNO","TNAM","HWAM","NDCH")]
  kdata$YDIFF <- abs((yield - kdata$HWAM)) / yield * 100
  kdata$DDIFF <- abs((seasdur - kdata$NDCH)) / seasdur * 100
  
  #joint selection (may yield no simulations if runs are too far off the obs./est. values)
  #kdata$WHICH_Y <- 0; kdata$WHICH_Y[which(kdata$YDIFF <= quantile(kdata$YDIFF, probs=0.25))] <- 1
  #kdata$WHICH_D <- 0; kdata$WHICH_D[which(kdata$DDIFF <= quantile(kdata$DDIFF, probs=0.25))] <- 1
  #kdata$WHICH <- kdata$WHICH_Y + kdata$WHICH_D
  #sel_runs <- kdata[which(kdata$WHICH == 2),]
  
  #nested selection of simulations (safer way of selecting the runs)
  sel_runs <- kdata[which(kdata$DDIFF <= quantile(kdata$DDIFF, probs=0.25)),]
  sel_runs <- sel_runs[which(sel_runs$YDIFF <= quantile(sel_runs$YDIFF, probs=0.25)),]
  
  #2-d density plot to pick ideal space
  rf_palette <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r_col <- rf_palette(32)
  k_dens <- kde2d(kdata$HWAM, kdata$NDCH, n=100)
  
  if (!file.exists(paste(runs_dir,"/model_output_plot_hhid_",hh_mill_xy$hhid[hh_i],".pdf",sep=""))) {
    pdf(paste(runs_dir,"/model_output_plot_hhid_",hh_mill_xy$hhid[hh_i],".pdf",sep=""),height=5,width=6)
    par(mar=c(5,5,1,1),las=1)
    image(k_dens, col=r_col, xlab="Yield (kg/ha)", ylab="Crop duration (days)")
    points(yield, seasdur,pch=20,col="black",cex=1.5)
    abline(v=yield,h=seasdur)
    polygon(x=c(min(sel_runs$HWAM),max(sel_runs$HWAM),max(sel_runs$HWAM),min(sel_runs$HWAM)),
            y=c(min(sel_runs$NDCH),min(sel_runs$NDCH),max(sel_runs$NDCH),max(sel_runs$NDCH)), col=NA, border="black")
    #points(kdata$HWAM, kdata$NDCH, pch=20, cex=0.3,col="black")
    #points(sel_runs$HWAM, sel_runs$NDCH, pch=20, cex=0.5,col="red")
    dev.off()
  }
  
  #create a final output object
  if (!file.exists(paste(runs_dir,"/model_selected_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))) {
    hh_output <- list(ALL_RUNS=kdata, SEL_RUNS=sel_runs, DENS_ALL=k_dens, OBS_YIELD=yield, 
                      EST_DUR=seasdur)
    save(hh_output, file=paste(runs_dir,"/model_selected_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))
  } else {
    load(file=paste(runs_dir,"/model_selected_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))
  }
  
  #clean up
  rm(list=c("hh_output","kdata","summary_all","yield","seasdur","k_dens"))
}

