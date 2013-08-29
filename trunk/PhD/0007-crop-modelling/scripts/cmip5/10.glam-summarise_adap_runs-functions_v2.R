#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

##########################################################################
######### PROJECTION DATA
collate_adap <- function(cells,runsDir,gcm,intype,co2,varNames,expSel,sdList,boutDir) {
  cat("collating",gcm,"and",intype,"and",co2,"\n")
  
  #output directory
  outDir <- paste(boutDir,"/_outputs/raw_output/",gcm,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  if (!file.exists(paste(outDir,"/RCP-",intype,"-",co2,".RData",sep=""))) {
    #output directory
    #outDir <- paste(runsDir,"/_outputs",sep="")
    #if (!file.exists(outDir)) {dir.create(outDir)}
    
    #an array for each GCM, co2 parameterisation and input type
    #create an empty array to hold all the information
    out_rcp_data <- array(data=NA,
                          dim=c(nrow(cells),length(expSel),2,28,length(varNames),47),
                          dimnames=list(CELL=cells$CELL,GLAM_ENS=expSel,
                                        SEASON=c("IRR","RFD"),YEAR=2022:2049,
                                        VARIABLE=varNames,ARUN=1:47))
    
    for (m in 1:length(expSel)) {
      #m <- 1
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
    #i <- 1
    #cat("grid cell",paste(cells$CELL[i]),"\n")
    rcp_array <- get_adap_data(runsDir,glam_ens=paste(expID),gcm_ens=gcm_ens,
                               input_type=inputType,co2_par=this_co2,
                               loc=paste(cells$CELL[i]),data_array=rcp_array)
  }
  return(rcp_array)
}


#load projection for a GLAM ensemble member, gcm ensemble member and location
#need an array where data will be fitted as input
get_adap_data <- function(runsDir, glam_ens, gcm_ens, input_type, co2_par, loc, data_array) {
  #loc <- cells$CELL[7]
  #glam_ens <- 33
  #input_type <- "allin"
  run_type <- paste(input_type,"_",co2_par,"_",loc,sep="")
  
  data_dir <- paste(runsDir,"/exp-",glam_ens,"_outputs/",gcm_ens,"/",run_type,sep="")
  load(paste(data_dir,"/output.RData",sep=""))
  
  #loop planting dates and put data into the thing
  for (j in 1:length(arun_data$RUNS)) {
    #j <- 1
    #sow_offset <- paste(arun_data$RUNS[[j]]$SOW_OFFSET)
    rfd_data <- arun_data$RUNS[[j]]$DATA$RFD
    data_array[loc,glam_ens,"RFD",,"STG",paste(j)] <- rfd_data$STG
    data_array[loc,glam_ens,"RFD",,"DUR",paste(j)] <- rfd_data$DUR
    data_array[loc,glam_ens,"RFD",,"TRADABS",paste(j)] <- rfd_data$TRADABS
    data_array[loc,glam_ens,"RFD",,"TP_UP",paste(j)] <- rfd_data$TP_UP
    #data_array[loc,glam_ens,"RFD",,"T_TRANS",paste(j)] <- rfd_data$T_TRANS
    #data_array[loc,glam_ens,"RFD",,"TP_TRANS",paste(j)] <- rfd_data$TP_TRANS
    #data_array[loc,glam_ens,"RFD",,"TOTPP",paste(j)] <- rfd_data$TOTPP
    data_array[loc,glam_ens,"RFD",,"TOTPP_HIT",paste(j)] <- rfd_data$TOTPP_HIT
    data_array[loc,glam_ens,"RFD",,"TOTPP_WAT",paste(j)] <- rfd_data$TOTPP_WAT
    data_array[loc,glam_ens,"RFD",,"LAI",paste(j)] <- rfd_data$LAI
    data_array[loc,glam_ens,"RFD",,"HI",paste(j)] <- rfd_data$HI
    data_array[loc,glam_ens,"RFD",,"BMASS",paste(j)] <- rfd_data$BMASS
    data_array[loc,glam_ens,"RFD",,"YIELD",paste(j)] <- rfd_data$YIELD
    data_array[loc,glam_ens,"RFD",,"T_RAIN",paste(j)] <- rfd_data$T_RAIN
    data_array[loc,glam_ens,"RFD",,"TBARTOT",paste(j)] <- rfd_data$TBARTOT
    data_array[loc,glam_ens,"RFD",,"VPDTOT",paste(j)] <- rfd_data$VPDTOT
    
    irr_data <- arun_data$RUNS[[j]]$DATA$IRR
    data_array[loc,glam_ens,"IRR",,"STG",paste(j)] <- irr_data$STG
    data_array[loc,glam_ens,"IRR",,"DUR",paste(j)] <- irr_data$DUR
    data_array[loc,glam_ens,"IRR",,"TRADABS",paste(j)] <- irr_data$TRADABS
    data_array[loc,glam_ens,"IRR",,"TP_UP",paste(j)] <- irr_data$TP_UP
    #data_array[loc,glam_ens,"IRR",,"T_TRANS",paste(j)] <- irr_data$T_TRANS
    #data_array[loc,glam_ens,"IRR",,"TP_TRANS",paste(j)] <- irr_data$TP_TRANS
    #data_array[loc,glam_ens,"IRR",,"TOTPP",paste(j)] <- irr_data$TOTPP
    data_array[loc,glam_ens,"IRR",,"TOTPP_HIT",paste(j)] <- irr_data$TOTPP_HIT
    data_array[loc,glam_ens,"IRR",,"TOTPP_WAT",paste(j)] <- irr_data$TOTPP_WAT
    data_array[loc,glam_ens,"IRR",,"LAI",paste(j)] <- irr_data$LAI
    data_array[loc,glam_ens,"IRR",,"HI",paste(j)] <- irr_data$HI
    data_array[loc,glam_ens,"IRR",,"BMASS",paste(j)] <- irr_data$BMASS
    data_array[loc,glam_ens,"IRR",,"YIELD",paste(j)] <- irr_data$YIELD
    data_array[loc,glam_ens,"IRR",,"T_RAIN",paste(j)] <- irr_data$T_RAIN
    data_array[loc,glam_ens,"IRR",,"TBARTOT",paste(j)] <- irr_data$TBARTOT
    data_array[loc,glam_ens,"IRR",,"VPDTOT",paste(j)] <- irr_data$VPDTOT
  }
  rm(arun_data)
  
  #return result
  return(data_array)
}




