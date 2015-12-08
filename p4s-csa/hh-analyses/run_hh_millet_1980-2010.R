#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015
#stop("!")

#load libraries
library(raster); library(rgdal); library(RMAWGEN); library(sp); library(maptools)
library(lhs); library(MASS); library(RColorBrewer); library(ncdf4)

#directories
src_dir <- "~/Repositories/dapa-climate-change/p4s-csa/hh-analyses"
wd <- "~/Leeds-work/p4s-csa/hh-analyses"
agmerra_dir <- paste(wd,"/AgMERRA_data",sep="")
chirps_dir <- paste(wd,"/CHIRPS_data/chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05",sep="")
cru_dir <- paste(wd,"/CRU_data",sep="")
adm_dir <- paste(wd,"/adm_data",sep="")
isric_dir <- paste(wd,"/ISRIC_soil",sep="")
ifprisoil_dir <- paste(wd,"/IFPRI_soil_profiles",sep="")
dssat_dir <- paste(wd,"/dssat_bin/dssat-csm-4.6.1.0_bin",sep="")
mdata_outdir <- paste(wd,"/model_data",sep="")
runs_dir <- paste(mdata_outdir,"/model_runs_mill_1980_2010",sep="")
if (!file.exists(runs_dir)) {dir.create(runs_dir)}

#source all functions
source(paste(src_dir,"/extract_CHIRPS.R",sep=""))
source(paste(src_dir,"/extract_CRU.R",sep=""))
source(paste(src_dir,"/extract_AgMERRA.R",sep=""))
source(paste(src_dir,"/wgen_srad.R",sep=""))
source(paste(src_dir,"/extract_weather_loc.R",sep=""))
source(paste(src_dir,"/calc_phdate.R",sep=""))
source(paste(src_dir,"/calc_risk_indices.R",sep=""))
source(paste(src_dir,"/get_soils_obj.R",sep=""))
source(paste(src_dir,"/get_xfile_obj.R",sep=""))
source(paste(src_dir,"/update_soil_xfile.R",sep=""))
source(paste(src_dir,"/make_soilfile.R",sep=""))
source(paste(src_dir,"/make_xfile.R",sep=""))
source(paste(src_dir,"/make_wthfile.R",sep=""))

#load Niger shp
shp <- readRDS(paste(adm_dir,"/NER_adm1.rds",sep=""))

#load hh data
hh_data <- read.csv(paste(wd,"/hh_data/LSMS_Niger_ResultsforRobertII.csv",sep=""))
hh_data$contrib_foodcrops_total <- hh_data$contrib_foodcrops_cons + hh_data$contrib_foodcrops_sold

#select millet-based households
hh_mill <- hh_data[which(hh_data$contrib_millet == 1 & hh_data$contrib_foodcrops_total == 1),]
hh_mill <- hh_mill[which(hh_mill$area.acres > 0),]
hh_mill_xy <- data.frame(hhid=hh_mill$hhid,lon=hh_mill$longitude,lat=hh_mill$latitude)

#load soil data
load(file=paste(mdata_outdir,"/soil_mill.RData",sep=""))

#list of years
yr_list <- 1981:2010

#extract weather for locations
for (year in yr_list) {
  #year <- yr_list[1]
  cat("\n...extracting data for year=",year,"\n")
  if (!file.exists(paste(runs_dir,"/millet_hh_wth_",year,".RData",sep=""))) {
    #extract data
    loc_wth <- extract_weather_loc(year=year,chirps_dir,agmerra_dir,cru_dir,xy=hh_mill_xy)
    
    #save object
    save(list=c("loc_wth","hh_mill_xy"), file=paste(runs_dir,"/millet_hh_wth_",year,".RData",sep=""))
  }
}


#calculate watbal
for (year in yr_list) {
  #year <- yr_list[1]
  cat("\n...calculating watbal for year=",year,"\n")
  if (!file.exists(paste(runs_dir,"/millet_hh_wth_watbal_",year,".RData",sep=""))) {
    if (exists("loc_wth")) {rm("loc_wth")}
    load(file=paste(runs_dir,"/millet_hh_wth_",year,".RData",sep=""))
    for (x_i in 1:length(loc_wth)) {
      loc_wth[[x_i]] <- watbal_wrapper(out_all=loc_wth[[x_i]], soilcp=xy_soil$soilcp[x_i])
      #plot(loc_wth[[x_i]]$ERATIO*100,ty="l"); lines(loc_wth[[x_i]]$ETMAX,col="red"); lines(loc_wth[[x_i]]$RAIN,col="blue")
    }
    save(loc_wth, file=paste(runs_dir,"/millet_hh_wth_watbal_",year,".RData",sep=""))
  }
}


#determine likely sowing dates for each site, report also estimated harvest date
#taken from Marteau et al. (2011)
for (year in yr_list) {
  #year <- yr_list[1]
  cat("\n...calculating watbal for year=",year,"\n")
  
  #determine max. and min. sowing date
  min_ini <- as.numeric(format(as.Date(paste0(year,"/4/15")) ,"%j"))
  max_ini <- as.numeric(format(as.Date(paste0(year,"/6/30")) ,"%j")) + 20
  max_end <- as.numeric(format(as.Date(paste0(year,"/10/27")) ,"%j")) + 20
  
  if (!file.exists(paste(runs_dir,"/phdates_mill_",year,".RData",sep=""))) {
    if (exists("loc_wth")) {rm("loc_wth")}
    load(file=paste(runs_dir,"/millet_hh_wth_watbal_",year,".RData",sep=""))
    
    out_seas <- data.frame()
    for (x_i in 1:length(loc_wth)) {
      #x_i <- 1
      sdate <- pdate_marteau(loc_wth[[x_i]],mindate=min_ini,maxdate=max_ini,p_thresh=1)
      if (is.na(sdate)) {sdate <- max_ini}
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
    save(out_seas, file=paste(runs_dir,"/phdates_mill_",year,".RData",sep=""))
  }
}

### create maximin latin hypercube matrix (same for all households)
param_list <- read.table(paste(mdata_outdir,"/perturb_parameter_list.tab",sep=""),header=T)
nparam <- nrow(param_list)
nrep <- 300
set.seed(2739); lhyp1 <- maximinLHS(n=nrep,k=nparam,dup=1)

### perform runs for all selected households, run models for period 1980-2010, for:
# 0. standard setup (with parametric variability)
# 1. setup without water stress, per phase, and whole cycle
# 2. setup without N stress
# 3. setup without water and N stress, per phase, and whole cycle (i.e. watbal=off)
# 4. no temperature stress on grain filling (copy SPE file from ./model_data)
# 5. SLPF=0.9 (for other non-modelled factors)
nostress_df <- data.frame(ns_run=c(1:15),runtype=c("nows",paste("nows_p",1:5,sep=""),"nons","nonsws",paste("nonsws_p",1:5,sep=""),"nohts","noother"))

for (year in yr_list) {
  #year <- yr_list[1]
  if (exists("out_seas")) {rm("out_seas")}; if (exists("loc_wth")) {rm("loc_wth")}
  load(file=paste(runs_dir,"/phdates_mill_",year,".RData",sep=""))
  load(file=paste(runs_dir,"/millet_hh_wth_watbal_",year,".RData",sep=""))
  
  for (hh_i in 1:nrow(hh_mill_xy)) {
    #hh_i <- 1
    if (!file.exists(paste(runs_dir,"/model_output_hhid_",hh_mill_xy$hhid[hh_i],"_",year,".RData",sep=""))) {
      #create run folder and copy relevant files (inc. executable) in there
      trun_dir <- paste(runs_dir,"/hhid-",hh_mill_xy$hhid[hh_i],"_",year,sep="")
      if (!file.exists(trun_dir)) {dir.create(trun_dir)}
      
      #load selected runs
      load(file=paste(mdata_outdir,"/model_runs_mill_calib/model_selected_hhid_",hh_mill_xy$hhid[hh_i],".RData",sep=""))
      hh_selruns <- hh_output$SEL_RUNS; rm(hh_output)
      
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
      for (hy_i in unique(hh_selruns$HYP_RUN)) {
        #hy_i <- unique(hh_selruns$HYP_RUN)[1]
        cat("\n...running hypercube simulations no.=",hy_i,"for hhid=",hh_mill_xy$hhid[hh_i],"\n")
        runnos <- hh_selruns$RUNNO[which(hh_selruns$HYP_RUN == hy_i)]
        
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
        get_summary_data <- function() {
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
          summ_out <- summ_out[,c("RUNNO","TNAM1","TNAM2","PDAT","EDAT","ADAT","MDAT","NDCH","CWAM","HWAM",
                                  "CNAM","ONTAM","OCTAM")]
          summ_out$TNAM <- paste(summ_out$TNAM1, summ_out$TNAM2, sep=" ")
          summ_out$TNAM1 <- summ_out$TNAM2 <- NULL
          return(summ_out)
        }
        
        #read OVERVIEW.OUT line by line, to get stresses
        get_overview_data <- function() {
          cat("......reading overview file\n")
          ifil <- file("OVERVIEW.OUT", open="r")
          all_lines <- readLines(ifil,n=-1)
          close(ifil); rm(ifil)
          runno <- 1
          allstress <- data.frame()
          for (l_i in 1:length(all_lines)) {
            #l_i <- 102
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
                  trow <- trow[,c((ncol(trow)-21),(ncol(trow)-13),(ncol(trow)-12),(ncol(trow)-5):(ncol(trow)-2))]
                  names(trow) <- paste(phases[iphase],"_",c("TSPAN","ET","PET","WPHOTO","WGWTH","NPHOTO","NGWTH"),sep="")
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
          return(allstress)
        }
        
        #merge with summ_out
        summ_out <- get_summary_data()
        allstress <- get_overview_data()
        summ_out <- merge(summ_out, allstress, by="RUNNO")
        summ_out <- summ_out[which(summ_out$RUNNO %in% runnos),]
        summ_out <- cbind(RUNTYPE="std",summ_out)
        
        #fifth, reading done, delete wth, soil, and xfile files
        system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
        
        ### now run the "no-stress" scenarios, to find out which stresses are most key
        ### phase transitions are calculated from PDAT, EDAT, and TSPAN columns in summ_out
        ns_data_all <- data.frame()
        for (trunno in runnos) {
          #trunno <- runnos[1]
          for (s_i in 1:nrow(nostress_df)) {
            #s_i <- 1
            ns_run <- nostress_df$ns_run[s_i]
            runtype <- paste(nostress_df$runtype[s_i])
            cat("\n...running no-stress run=",runtype,"for Run No.=",trunno,", hypercube run=",hy_i,"and hh=",hh_mill_xy$hhid[hh_i],"\n")
            
            if (runtype == "nows" | runtype == "nonsws") {
              pet_cycle <- sum(as.numeric(summ_out[which(summ_out$RUNNO == trunno),grep("_PET",names(summ_out))]))
              aet_cycle <- sum(as.numeric(summ_out[which(summ_out$RUNNO == trunno),grep("_ET",names(summ_out))]))
              phase_ini <- summ_out$EDAT[which(summ_out$RUNNO == trunno)]
              phase_end <- summ_out$MDAT[which(summ_out$RUNNO == trunno)]
              wat_amt <- pet_cycle - aet_cycle
              wat_add <- wat_amt / (phase_end - phase_ini + 1) * 2.0 #100% more for drainage / perc. loss
              
              #update weather
              wth_updated <- wth_site
              phase_seq <- substr(paste(phase_ini:phase_end),3,7)
              wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] <- wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] + wat_add
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_updated, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              if (runtype == "nonsws") {
                xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
                xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              } else {
                xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              }
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nows_p1" | runtype == "nonsws_p1") {
              #no water stress in phase 1 (emergence - early veg), runnos dont matter
              phase_ini <- summ_out$EDAT[which(summ_out$RUNNO == trunno)]
              phase_end <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_TSPAN"] - 1
              wat_amt <- summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_PET"] - summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_ET"]
              wat_add <- wat_amt / summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_TSPAN"] * 2.0 #100% more for drainage / perc. loss
              
              #update weather
              wth_updated <- wth_site
              phase_seq <- substr(paste(phase_ini:phase_end),3,7)
              wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] <- wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] + wat_add
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_updated, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              if (runtype == "nonsws_p1") {
                xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
                xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              } else {
                xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              }
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nows_p2" | runtype == "nonsws_p2") {
              #no water stress in phase 2 (end juvenil - floral init.), runnos dont matter
              phase_ini <- summ_out$EDAT[which(summ_out$RUNNO == trunno)]
              phase_ini <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_TSPAN"]
              phase_end <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_TSPAN"] - 1
              wat_amt <- summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_PET"] - summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_ET"]
              wat_add <- wat_amt / summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_TSPAN"] * 2.0 #100% more for drainage / perc. loss
              
              #update weather
              wth_updated <- wth_site
              phase_seq <- substr(paste(phase_ini:phase_end),3,7)
              wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] <- wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] + wat_add
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_updated, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              if (runtype == "nonsws_p2") {
                xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
                xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              } else {
                xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              }
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nows_p3" | runtype == "nonsws_p3") {
              #no water stress in phase 3 (floral init. - max. lai), runnos dont matter
              phase_ini <- summ_out$EDAT[which(summ_out$RUNNO == trunno)]
              phase_ini <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_TSPAN"] + summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_TSPAN"]
              phase_end <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"FI-LX_TSPAN"] - 1
              wat_amt <- summ_out[which(summ_out$RUNNO == trunno),"FI-LX_PET"] - summ_out[which(summ_out$RUNNO == trunno),"FI-LX_ET"]
              wat_add <- wat_amt / summ_out[which(summ_out$RUNNO == trunno),"FI-LX_TSPAN"] * 2.0 #100% more for drainage / perc. loss
              
              #update weather
              wth_updated <- wth_site
              phase_seq <- substr(paste(phase_ini:phase_end),3,7)
              wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] <- wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] + wat_add
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_updated, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              if (runtype == "nonsws_p3") {
                xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
                xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              } else {
                xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              }
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nows_p4" | runtype == "nonsws_p4") {
              #no water stress in phase 4 (max. lai - start grain fill), runnos dont matter
              phase_ini <- summ_out$EDAT[which(summ_out$RUNNO == trunno)]
              phase_ini <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_TSPAN"] + summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_TSPAN"] + summ_out[which(summ_out$RUNNO == trunno),"FI-LX_TSPAN"]
              phase_end <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"LX-GF_TSPAN"] - 1
              wat_amt <- summ_out[which(summ_out$RUNNO == trunno),"LX-GF_PET"] - summ_out[which(summ_out$RUNNO == trunno),"LX-GF_ET"]
              wat_add <- wat_amt / summ_out[which(summ_out$RUNNO == trunno),"LX-GF_TSPAN"] * 2.0 #100% more for drainage / perc. loss
              
              #update weather
              wth_updated <- wth_site
              phase_seq <- substr(paste(phase_ini:phase_end),3,7)
              wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] <- wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] + wat_add
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_updated, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              if (runtype == "nonsws_p4") {
                xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
                xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              } else {
                xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              }
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nows_p5" | runtype == "nonsws_p5") {
              #no water stress in phase 5 (start grain fill - phys. mat), runnos dont matter
              phase_ini <- summ_out$EDAT[which(summ_out$RUNNO == trunno)]
              phase_ini <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"EM-EJ_TSPAN"] + summ_out[which(summ_out$RUNNO == trunno),"EJ-FI_TSPAN"] + summ_out[which(summ_out$RUNNO == trunno),"FI-LX_TSPAN"] + summ_out[which(summ_out$RUNNO == trunno),"LX-GF_TSPAN"]
              phase_end <- phase_ini + summ_out[which(summ_out$RUNNO == trunno),"GFILL_TSPAN"] - 1
              wat_amt <- summ_out[which(summ_out$RUNNO == trunno),"LX-GF_PET"] - summ_out[which(summ_out$RUNNO == trunno),"LX-GF_ET"]
              wat_add <- wat_amt / summ_out[which(summ_out$RUNNO == trunno),"LX-GF_TSPAN"] * 2.0 #100% more for drainage / perc. loss
              
              #update weather
              wth_updated <- wth_site
              phase_seq <- substr(paste(phase_ini:phase_end),3,7)
              wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] <- wth_updated$RAIN[which(wth_updated$DATE %in% phase_seq)] + wat_add
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_updated, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              if (runtype == "nonsws_p5") {
                xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
                xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              } else {
                xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              }
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nons") {
              xfil_updated <- loc_xfil; xfil_updated$sim_ctrl$NITRO <- "N"; xfil_updated$sim_ctrl$SYMBI <- "N"
              
              #write files in runs folder and run model
              wthfil <- write_wth(in_data=wth_site, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              xfile <- make_xfile(in_data=xfil_updated, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            } else if (runtype == "nohts") {
              #replace SPE file
              system(paste("rm -f MLCER046.SPE && cp -f ",mdata_outdir,"/MLCER046.SPE .",sep=""))
              
              #write input files and run model
              wthfil <- write_wth(in_data=wth_site, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=loc_soil, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
              
              #replace SPE file again
              system(paste("rm -f MLCER046.SPE && cp -f ",dssat_dir,"/MLCER046.SPE .",sep=""))
            } else if (runtype == "noother") {
              soil_updated <- loc_soil
              soil_updated$properties$SLPF <- 0.9
              wthfil <- write_wth(in_data=wth_site, out_file=paste(trun_dir,"/",wth_head$FNAME,sep=""),site_details=wth_head,append=F)
              soilfil <- make_soilfile(in_data=soil_updated, out_file=paste(trun_dir,"/P4.SOL",sep=""),overwrite=T)
              xfile <- make_xfile(in_data=loc_xfil, out_file=paste(trun_dir,"/P4NE1101.MLX",sep=""), overwrite=T)
              system("rm -f *.OUT && ./DSCSM046.EXE MLCER046 B DSSBatch.v46")
              
              #read data and append it
              summ_ns <- get_summary_data()
              allstress_ns <- get_overview_data()
              summ_ns <- merge(summ_ns, allstress_ns, by="RUNNO")
              summ_ns <- summ_ns[which(summ_ns$RUNNO == trunno),]
              summ_ns <- cbind(RUNTYPE=runtype,summ_ns)
              ns_data_all <- rbind(ns_data_all, summ_ns)
              
              #clean up
              system("rm -f *.WTH"); system("rm -f P4.SOL"); system("rm -f P4NE1101.MLX")
            }
          }
        }
        
        #finally, append to global data.frame for this household, and remove objects
        summary_all <- rbind(summary_all, cbind(HYP_RUN=hy_i,summ_out), cbind(HYP_RUN=hy_i,ns_data_all))
        rm(list=c("summ_out","allstress"))
      }
      
      #save output object
      save(summary_all, file=paste(runs_dir,"/model_output_hhid_",hh_mill_xy$hhid[hh_i],"_",year,".RData",sep=""))
      
      #bail out and delete folder
      setwd("~")
      system(paste("rm -rf ",trun_dir,sep=""))
      
      #clean up
      rm(list=c("summary_all"))
    }
  }
}




