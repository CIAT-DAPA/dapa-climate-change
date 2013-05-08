#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

##########################################################################
######### PROJECTION DATA
collate_rcp <- function(cells,runsDir,gcm,intype,co2,varNames,expSel,sdList) {
  cat("collating",gcm,"and",intype,"and",co2,"\n")
  
  #output directory
  outDir <- paste(runsDir,"/_outputs/raw_output/",gcm,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  if (!file.exists(paste(outDir,"/RCP-",intype,"-",co2,".RData",sep=""))) {
    #output directory
    #outDir <- paste(runsDir,"/_outputs",sep="")
    #if (!file.exists(outDir)) {dir.create(outDir)}
    
    #an array for each GCM, co2 parameterisation and input type
    #create an empty array to hold all the information
    out_rcp_data <- array(data=NA,
                          dim=c(nrow(cells),length(expSel),length(sdList),2,28,length(varNames)),
                          dimnames=list(CELL=cells$CELL,GLAM_ENS=expSel,
                                        SOW_DATE=sdList,SEASON=c("IRR","RFD"),
                                        YEAR=1966:1993,VARIABLE=varNames))
    
    for (m in 1:length(expSel)) {
      out_rcp_data <- get_ens_rcp(expID=expSel[m],runsDir,gcm_ens=gcm,
                                  inputType=intype,this_co2=co2,
                                  cells,rcp_array=out_rcp_data)
    }
    
    save(list=c("out_rcp_data"),file=paste(outDir,"/RCP-",intype,"-",co2,".RData",sep=""))
  }
  return("done!")
}

#get historical run data for a whole GLAM ensemble member
get_ens_rcp <- function(expID,runsDir,gcm_ens,inputType,this_co2,cells,rcp_array) {
  cat("GLAM ensemble member",expID,"\n")
  #get data for grid cells
  for (i in 1:nrow(cells)) {
    #cat("grid cell",paste(cells$CELL[i]),"\n")
    rcp_array <- get_proj_data(runsDir,glam_ens=paste(expID),gcm_ens=gcm_ens,
                               input_type=inputType,co2_par=this_co2,
                               loc=paste(cells$CELL[i]),data_array=rcp_array)
  }
  return(rcp_array)
}


#load projection for a GLAM ensemble member, gcm ensemble member and location
#need an array where data will be fitted as input
get_proj_data <- function(runsDir, glam_ens, gcm_ens, input_type, co2_par, loc, data_array) {
  #loc <- cells$CELL[7]
  #glam_ens <- 33
  #input_type <- "allin"
  run_type <- paste(input_type,"_",co2_par,"_",loc,sep="")
  
  data_dir <- paste(runsDir,"/exp-",glam_ens,"_outputs/",gcm_ens,"/",run_type,sep="")
  load(paste(data_dir,"/output.RData",sep=""))
  
  #loop planting dates and put data into the thing
  for (j in 1:length(run_data$RUNS)) {
    sow_offset <- paste(run_data$RUNS[[j]]$SOW_OFFSET)
    rfd_data <- run_data$RUNS[[j]]$DATA$RFD
    data_array[loc,glam_ens,sow_offset,"RFD",,"STG"] <- rfd_data$STG
    data_array[loc,glam_ens,sow_offset,"RFD",,"DUR"] <- rfd_data$DUR
    data_array[loc,glam_ens,sow_offset,"RFD",,"TRADABS"] <- rfd_data$TRADABS
    data_array[loc,glam_ens,sow_offset,"RFD",,"TP_UP"] <- rfd_data$TP_UP
    data_array[loc,glam_ens,sow_offset,"RFD",,"T_TRANS"] <- rfd_data$T_TRANS
    data_array[loc,glam_ens,sow_offset,"RFD",,"TP_TRANS"] <- rfd_data$TP_TRANS
    data_array[loc,glam_ens,sow_offset,"RFD",,"TOTPP"] <- rfd_data$TOTPP
    data_array[loc,glam_ens,sow_offset,"RFD",,"TOTPP_HIT"] <- rfd_data$TOTPP_HIT
    data_array[loc,glam_ens,sow_offset,"RFD",,"TOTPP_WAT"] <- rfd_data$TOTPP_WAT
    data_array[loc,glam_ens,sow_offset,"RFD",,"LAI"] <- rfd_data$LAI
    data_array[loc,glam_ens,sow_offset,"RFD",,"HI"] <- rfd_data$HI
    data_array[loc,glam_ens,sow_offset,"RFD",,"BMASS"] <- rfd_data$BMASS
    data_array[loc,glam_ens,sow_offset,"RFD",,"YIELD"] <- rfd_data$YIELD
    data_array[loc,glam_ens,sow_offset,"RFD",,"T_RAIN"] <- rfd_data$T_RAIN
    data_array[loc,glam_ens,sow_offset,"RFD",,"TBARTOT"] <- rfd_data$TBARTOT
    
    irr_data <- run_data$RUNS[[j]]$DATA$IRR
    data_array[loc,glam_ens,sow_offset,"IRR",,"STG"] <- irr_data$STG
    data_array[loc,glam_ens,sow_offset,"IRR",,"DUR"] <- irr_data$DUR
    data_array[loc,glam_ens,sow_offset,"IRR",,"TRADABS"] <- irr_data$TRADABS
    data_array[loc,glam_ens,sow_offset,"IRR",,"TP_UP"] <- irr_data$TP_UP
    data_array[loc,glam_ens,sow_offset,"IRR",,"T_TRANS"] <- irr_data$T_TRANS
    data_array[loc,glam_ens,sow_offset,"IRR",,"TP_TRANS"] <- irr_data$TP_TRANS
    data_array[loc,glam_ens,sow_offset,"IRR",,"TOTPP"] <- irr_data$TOTPP
    data_array[loc,glam_ens,sow_offset,"IRR",,"TOTPP_HIT"] <- irr_data$TOTPP_HIT
    data_array[loc,glam_ens,sow_offset,"IRR",,"TOTPP_WAT"] <- irr_data$TOTPP_WAT
    data_array[loc,glam_ens,sow_offset,"IRR",,"LAI"] <- irr_data$LAI
    data_array[loc,glam_ens,sow_offset,"IRR",,"HI"] <- irr_data$HI
    data_array[loc,glam_ens,sow_offset,"IRR",,"BMASS"] <- irr_data$BMASS
    data_array[loc,glam_ens,sow_offset,"IRR",,"YIELD"] <- irr_data$YIELD
    data_array[loc,glam_ens,sow_offset,"IRR",,"T_RAIN"] <- irr_data$T_RAIN
    data_array[loc,glam_ens,sow_offset,"IRR",,"TBARTOT"] <- irr_data$TBARTOT
  }
  rm(run_data)
  
  #return result
  return(data_array)
}


##########################################################################
######### BASELINE DATA
collate_his <- function(cells,runsDir,gcm,intype,varNames,expSel) {
  cat("collating",gcm,"and",intype,"\n")
  
  #output directory
  outDir <- paste(runsDir,"/_outputs/raw_output/",gcm,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  if (!file.exists(paste(outDir,"/HIS-",intype,".RData",sep=""))) {
    #create an empty array to hold all the information
    out_his_data <- array(data=NA,
                          dim=c(nrow(cells),length(expSel),2,28,length(varNames)),
                          dimnames=list(CELL=cells$CELL,GLAM_ENS=expSel,SEASON=c("IRR","RFD"),
                                        YEAR=1966:1993,VARIABLE=varNames))
    
    #load baseline for all GLAM ensemble members: his_allin_* and his_bcrain_*
    for (m in 1:length(expSel)) {
      out_his_data <- get_ens_his(expID=expSel[m],runsDir,gcm_ens=gcm,
                                  inputType=intype,cells,his_array=out_his_data)
    }
    
    save(list=c("out_his_data"),file=paste(outDir,"/HIS-",intype,".RData",sep=""))
  }
  return("done!")
}


#get historical run data for a whole GLAM ensemble member
get_ens_his <- function(expID,runsDir,gcm_ens,inputType,cells,his_array) {
  cat("GLAM ensemble member",expID,"\n")
  #get data for grid cells
  for (i in 1:nrow(cells)) {
    #cat("grid cell",paste(cells$CELL[i]),"\n")
    his_array <- get_pres_data(runsDir,glam_ens=paste(expID),gcm_ens=gcm_ens,
                                  input_type=inputType,loc=paste(cells$CELL[i]),
                                  data_array=his_array)
  }
  return(his_array)
}


#load baseline for a GLAM ensemble member, gcm ensemble member and location
#need an array where data will be fitted as input
get_pres_data <- function(runsDir, glam_ens, gcm_ens, input_type, loc, data_array) {
  #loc <- cells$CELL[7]
  #glam_ens <- 33
  #input_type <- "allin"
  run_type <- paste(input_type,"_",loc,sep="")
  
  data_dir <- paste(runsDir,"/exp-",glam_ens,"_outputs/",gcm_ens,"/",run_type,sep="")
  load(paste(data_dir,"/output.RData",sep=""))
  rm(params); rm(setup); g=gc(); rm(g)
  
  #organise all run data
  run_data <- data.frame()
  for (j in 1:length(out_data)) {
    tdata <- out_data[[j]]$DATA
    tdata <- cbind(RUN_TYPE=out_data[[j]]$RUN_TYPE,YGP=as.numeric(out_data[[j]]$YGP),tdata)
    run_data <- rbind(run_data,tdata)
  }
  
  run_data <- run_data[which(round(run_data$YGP,2) == round(optimal$YGP,2)),]
  run_data$LAT <- NULL; run_data$LON <- NULL
  
  #determine if this is both or rainfed, or irr
  if (mean(ir_vls$IRATIO) == 0) {
    rfd_data <- run_data[which(run_data$RUN_TYPE == "RFD"),]
    irr_data <- rfd_data
    irr_data[,4:ncol(irr_data)] <- NA
  } else if (mean(ir_vls$IRATIO) == 1) {
    irr_data <- run_data[which(run_data$RUN_TYPE == "IRR"),]
    rfd_data <- irr_data
    rfd_data[,4:ncol(rfd_data)] <- NA
  } else {
    rfd_data <- run_data[which(run_data$RUN_TYPE == "RFD"),]
    irr_data <- run_data[which(run_data$RUN_TYPE == "IRR"),]
  }
  rm(ir_vls); rm(out_data); g=gc(); rm(g)
  
  #put in rainfed data
  data_array[loc,glam_ens,"RFD",,"YGP"] <- rfd_data$YGP
  data_array[loc,glam_ens,"RFD",,"STG"] <- rfd_data$STG
  data_array[loc,glam_ens,"RFD",,"DUR"] <- rfd_data$DUR
  data_array[loc,glam_ens,"RFD",,"TRADABS"] <- rfd_data$TRADABS
  data_array[loc,glam_ens,"RFD",,"TP_UP"] <- rfd_data$TP_UP
  data_array[loc,glam_ens,"RFD",,"T_TRANS"] <- rfd_data$T_TRANS
  data_array[loc,glam_ens,"RFD",,"TP_TRANS"] <- rfd_data$TP_TRANS
  data_array[loc,glam_ens,"RFD",,"TOTPP"] <- rfd_data$TOTPP
  data_array[loc,glam_ens,"RFD",,"TOTPP_HIT"] <- rfd_data$TOTPP_HIT
  data_array[loc,glam_ens,"RFD",,"TOTPP_WAT"] <- rfd_data$TOTPP_WAT
  data_array[loc,glam_ens,"RFD",,"LAI"] <- rfd_data$LAI
  data_array[loc,glam_ens,"RFD",,"HI"] <- rfd_data$HI
  data_array[loc,glam_ens,"RFD",,"BMASS"] <- rfd_data$BMASS
  data_array[loc,glam_ens,"RFD",,"YIELD"] <- rfd_data$YIELD
  data_array[loc,glam_ens,"RFD",,"T_RAIN"] <- rfd_data$T_RAIN
  data_array[loc,glam_ens,"RFD",,"TBARTOT"] <- rfd_data$TBARTOT
  
  #put in irrigated data
  data_array[loc,glam_ens,"IRR",,"YGP"] <- irr_data$YGP
  data_array[loc,glam_ens,"IRR",,"STG"] <- irr_data$STG
  data_array[loc,glam_ens,"IRR",,"DUR"] <- irr_data$DUR
  data_array[loc,glam_ens,"IRR",,"TRADABS"] <- irr_data$TRADABS
  data_array[loc,glam_ens,"IRR",,"TP_UP"] <- irr_data$TP_UP
  data_array[loc,glam_ens,"IRR",,"T_TRANS"] <- irr_data$T_TRANS
  data_array[loc,glam_ens,"IRR",,"TP_TRANS"] <- irr_data$TP_TRANS
  data_array[loc,glam_ens,"IRR",,"TOTPP"] <- irr_data$TOTPP
  data_array[loc,glam_ens,"IRR",,"TOTPP_HIT"] <- irr_data$TOTPP_HIT
  data_array[loc,glam_ens,"IRR",,"TOTPP_WAT"] <- irr_data$TOTPP_WAT
  data_array[loc,glam_ens,"IRR",,"LAI"] <- irr_data$LAI
  data_array[loc,glam_ens,"IRR",,"HI"] <- irr_data$HI
  data_array[loc,glam_ens,"IRR",,"BMASS"] <- irr_data$BMASS
  data_array[loc,glam_ens,"IRR",,"YIELD"] <- irr_data$YIELD
  data_array[loc,glam_ens,"IRR",,"T_RAIN"] <- irr_data$T_RAIN
  data_array[loc,glam_ens,"IRR",,"TBARTOT"] <- irr_data$TBARTOT
  
  #return result
  return(data_array)
}



