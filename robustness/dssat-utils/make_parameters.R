#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014

#functions to read the DSSAT '.ECO' file
#currently only MZCER, next is MZIXM

### test for MZCER
#setwd("~/Leeds-work/quest-for-robustness/bin/dssat/csm45_1_23_bin_gfort")
#eco_file <- "./MZCER045.ECO"
#cul_file <- "./MZCER045.CUL"
#spe_file <- "./MZCER045.SPE"
#
#ecoall <- get_ecopar(in_file=eco_file,l=-1) #read in
#ecofil <- make_ecopar(params=ecoall, out_file=eco_file, overwrite=F) #write
#
#culall <- get_culpar(in_file=cul_file,l=-1) #read in
#culfil <- make_culpar(params=culall, out_file=cul_file, overwrite=F) #write
#
#speall <- get_spepar(in_file=spe_file) #read in
#spefil <- make_spepar(params=speall, out_file=spe_file, overwrite=F) #write


### test for MZIXM
#eco_file <- "./MZIXM045.ECO"
#cul_file <- "./MZIXM045.CUL"
#spe_file <- "./MZIXM045.SPE"

#ecoall <- get_ecopar(in_file=eco_file,l=-1) #read in
#ecofil <- make_ecopar(params=ecoall, out_file=eco_file, overwrite=F) #write
#
#culall <- get_culpar(in_file=cul_file,l=-1) #read in
#culfil <- make_culpar(params=culall, out_file=cul_file, overwrite=F) #write
#
#speall <- get_spepar(in_file=spe_file) #read in
#spefil <- make_spepar(params=speall, out_file=spe_file, overwrite=F) #write

#make ecotype parameter file
make_ecopar <- function(params, out_file, overwrite=F) {
  #determine which model
  if (length(grep("IXM",out_file)) != 0) {model <- "ixm"}
  if (length(grep("CER",out_file)) != 0) {model <- "cer"}
  
  #open file in write mode
  if (file.exists(out_file)) {
    if (overwrite) {
      pf <- file(out_file,open="w")
    } else {
      rnum <- round(runif(1,10000,20000),0)
      tmpvar <- unlist(strsplit(out_file,"/",fixed=T))
      pth_ref <- paste(tmpvar[1:(length(tmpvar)-1)],collapse="/")
      out_file <- paste(pth_ref,"/copy-",rnum,"_",tmpvar[length(tmpvar)],sep="")
      pf <- file(out_file,open="w")
    }
  } else {
    pf <- file(out_file,open="w")
  }
  
  #write file and data header
  if (model == "ixm") {
    cat("*MAIZE ECOTYPE COEFFICIENTS: GECER040 MODEL\n",file=pf)
    cat("@ECO#  ECONAME.........  TBASE TOPT  ROPT  P20   DJTI  GDDE  DSGFT  RUE   KCAN  PSTM  PEAR  TSEN  CDAY\n",file=pf)
  } else {
    cat("*MAIZE ECOTYPE COEFFICIENTS: GECER045 MODEL\n",file=pf)
    cat("@ECO#  ECONAME.........  TBASE TOPT  ROPT  P20   DJTI  GDDE  DSGFT  RUE   KCAN  TSEN  CDAY\n",file=pf)
  }
  
  #write all lines
  for (i in 1:nrow(params)) {
    if (model == "ixm") {
      cat(paste(sprintf("%-7s",params$ECO_ID[i]),sprintf("%-18s",params$ECO_NAME[i]),
                sprintf("%1$5.1f%2$5.1f%3$6.1f%4$6.1f%5$6.1f%6$6.1f%7$7.1f%8$5.1f%9$7.2f%10$6.2f%11$6.2f%12$6.1f%13$6.1f",
                        params$TBASE[i],params$TOPT[i],params$ROPT[i],params$P20[i],params$DJTI[i],
                        params$GDDE[i],params$DSGFT[i],params$RUE[i],params$KCAN[i],params$PSTM[i],
                        params$PEAR[i],params$TSEN[i],params$CDAY[i]),"\n",sep=""),file=pf)
    } else {
      cat(paste(sprintf("%-7s",params$ECO_ID[i]),sprintf("%-18s",params$ECO_NAME[i]),
                sprintf("%1$5.1f%2$5.1f%3$6.1f%4$6.1f%5$6.1f%6$6.1f%7$7.1f%8$5.1f%9$7.2f%10$6.1f%11$6.1f",
                        params$TBASE[i],params$TOPT[i],params$ROPT[i],params$P20[i],params$DJTI[i],
                        params$GDDE[i],params$DSGFT[i],params$RUE[i],params$KCAN[i],params$TSEN[i],
                        params$CDAY[i]),"\n",sep=""),file=pf)
    }
  }
  
  #close file
  close(pf)
  
  #return filename
  return(out_file)
}

#make cultivar parameter file
make_culpar <- function(params, out_file, overwrite=F) {
  #determine which model
  if (length(grep("IXM",out_file)) != 0) {model <- "ixm"}
  if (length(grep("CER",out_file)) != 0) {model <- "cer"}
  
  #open file in write mode
  if (file.exists(out_file)) {
    if (overwrite) {
      pf <- file(out_file,open="w")
    } else {
      rnum <- round(runif(1,10000,20000),0)
      tmpvar <- unlist(strsplit(out_file,"/",fixed=T))
      pth_ref <- paste(tmpvar[1:(length(tmpvar)-1)],collapse="/")
      out_file <- paste(pth_ref,"/copy-",rnum,"_",tmpvar[length(tmpvar)],sep="")
      pf <- file(out_file,open="w")
    }
  } else {
    pf <- file(out_file,open="w")
  }
  
  #write file and data header
  if (model == "ixm") {
    cat("*MAIZE CULTIVAR COEFFICIENTS: MZIXM045 MODEL\n",file=pf)
    cat("@VAR#  VRNAME.......... EXPNO   ECO#    P1    P2    P5    G2    G3 PHINT    AX    LX\n",file=pf)
    cat("!                                        1     2     3     4     5     6     7     8\n",file=pf)
  } else {
    cat("*MAIZE CULTIVAR COEFFICIENTS: MZCER045 MODEL\n",file=pf)
    cat("@VAR#  VRNAME.......... EXPNO   ECO#    P1    P2    P5    G2    G3 PHINT\n",file=pf)
  }
  
  #write all lines
  for (i in 1:nrow(params)) {
    if (model == "ixm") {
      cat(paste(sprintf("%-7s",params$CUL_ID[i]),sprintf("%-17s",params$CUL_NAME[i]),
                sprintf("%5s",params$EXPNO[i]),sprintf("%7s",params$ECO_ID[i]),
                sprintf("%1$6.1f%2$6.3f%3$6.1f%4$6.1f%5$6.2f%6$6.2f%7$6.1f%8$6.1f",
                        params$P1[i],params$P2[i],params$P5[i],params$G2[i],params$G3[i],
                        params$PHINT[i],params$AX[i],params$LX[i]),
                "\n",sep=""),file=pf)
    } else {
      cat(paste(sprintf("%-7s",params$CUL_ID[i]),sprintf("%-17s",params$CUL_NAME[i]),
                sprintf("%5s",params$EXPNO[i]),sprintf("%7s",params$ECO_ID[i]),
                sprintf("%1$6.1f%2$6.3f%3$6.1f%4$6.1f%5$6.2f%6$6.2f",
                        params$P1[i],params$P2[i],params$P5[i],params$G2[i],params$G3[i],params$PHINT[i]),
                "\n",sep=""),file=pf)
    }
  }
  
  #close file
  close(pf)
  
  #return filename
  return(out_file)
}


#make species parameter file
make_spepar <- function(params, out_file, overwrite=F) {
  #determine which model
  if (length(grep("IXM",out_file)) != 0) {model <- "ixm"}
  if (length(grep("CER",out_file)) != 0) {model <- "cer"}
  
  #open file in write mode
  if (file.exists(out_file)) {
    if (overwrite) {
      pf <- file(out_file,open="w")
    } else {
      rnum <- round(runif(1,10000,20000),0)
      tmpvar <- unlist(strsplit(out_file,"/",fixed=T))
      pth_ref <- paste(tmpvar[1:(length(tmpvar)-1)],collapse="/")
      out_file <- paste(pth_ref,"/copy-",rnum,"_",tmpvar[length(tmpvar)],sep="")
      pf <- file(out_file,open="w")
    }
  } else {
    pf <- file(out_file,open="w")
  }
  
  #write file and data header
  if (model == "ixm") {
    cat("*MAIZE SPECIES COEFFICIENTS: MZIXM045 MODEL\n",file=pf)
  } else {
    cat("*MAIZE SPECIES COEFFICIENTS: MZCER045 MODEL\n",file=pf)
  }
  cat("\n",file=pf)
  
  #write temperature effects
  cat("*TEMPERATURE EFFECTS\n",file=pf)
  cat("!       TBASE TOP1  TOP2  TMAX\n",file=pf)
  for (i in 1:nrow(params$temp_resp)) {
    cat(paste(sprintf("%7s",params$temp_resp$PROCESS[i]),
              sprintf("%1$5.1f%2$6.1f%3$6.1f%4$6.1f",params$temp_resp$TBASE[i],params$temp_resp$TOP1[i],
                      params$temp_resp$TOP2[i],params$temp_resp$TMAX[i]),
              "\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #photosynthesis parameters
  cat("*PHOTOSYNTHESIS PARAMETERS \n",file=pf)
  cat(paste(sprintf("%7s","PARSR"),sprintf("%1$7.2f",params$photo_param$PARSR),"\n",sep=""),file=pf)
  cat(paste(sprintf("%6s",params$photo_param$co2_resp$AXIS[1]),
            sprintf("%1$6d%2$6d%3$6d%4$6d%5$6d%6$6d%7$6d%8$6d%9$6d%10$6d",
                    as.integer(params$photo_param$co2_resp$PT1[1]),as.integer(params$photo_param$co2_resp$PT2[1]),
                    as.integer(params$photo_param$co2_resp$PT3[1]),as.integer(params$photo_param$co2_resp$PT4[1]),
                    as.integer(params$photo_param$co2_resp$PT5[1]),as.integer(params$photo_param$co2_resp$PT6[1]),
                    as.integer(params$photo_param$co2_resp$PT7[1]),as.integer(params$photo_param$co2_resp$PT8[1]),
                    as.integer(params$photo_param$co2_resp$PT9[1]),as.integer(params$photo_param$co2_resp$PT10[1])),
            "\n",sep=""),file=pf)
  cat(paste(sprintf("%6s",params$photo_param$co2_resp$AXIS[2]),
            sprintf("%1$6.2f%2$6.2f%3$6.2f%4$6.2f%5$6.2f%6$6.2f%7$6.2f%8$6.2f%9$6.2f%10$6.2f",
                    params$photo_param$co2_resp$PT1[2],params$photo_param$co2_resp$PT2[2],
                    params$photo_param$co2_resp$PT3[2],params$photo_param$co2_resp$PT4[2],
                    params$photo_param$co2_resp$PT5[2],params$photo_param$co2_resp$PT6[2],
                    params$photo_param$co2_resp$PT7[2],params$photo_param$co2_resp$PT8[2],
                    params$photo_param$co2_resp$PT9[2],params$photo_param$co2_resp$PT10[2]),
            "\n",sep=""),file=pf)
  
  #extra lines only for ixm
  if (model == "ixm") {
    cat(paste(sprintf("%7s","ASMAX"),sprintf("%1$7.2f",params$photo_param$ASMAX),"\n",sep=""),file=pf)
    cat(paste(sprintf("%4s","XC"),sprintf("%1$10.2f",params$photo_param$XC),"\n",sep=""),file=pf)
    cat(paste(sprintf("%6s","CANH"),sprintf("%1$8.2f",params$photo_param$CANH),"\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #stress response
  cat("*STRESS RESPONSE\n",file=pf)
  for (i in 1:nrow(params$stress_resp)) {
    cat(paste(sprintf("%7s",params$stress_resp$TYPE[i]),sprintf("%1$8.3f",params$stress_resp$FRAC[i]),"\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #seed growth
  cat("*SEED GROWTH PARAMETERS     \n",file=pf)
  cat(paste(sprintf("%8s",params$seed_growth$PARAM[1]),sprintf("%1$6.4f",params$seed_growth$VALUE[1]),"\n",sep=""),file=pf)
  for (i in 2:6) {
    cat(paste(sprintf("%8s",params$seed_growth$PARAM[i]),sprintf("%1$6.1f",params$seed_growth$VALUE[i]),"\n",sep=""),file=pf)
  }
  cat(paste(sprintf("%8s",params$seed_growth$PARAM[7]),sprintf("%1$6.2f",params$seed_growth$VALUE[7]),"\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #emergence initial conditions
  cat("*EMERGENCE INITIAL CONDITIONS  \n",file=pf)
  for (i in 1:nrow(params$emer_cond)) {
    cat(paste(sprintf("%9s",params$emer_cond$PARAM[i]),sprintf("%1$5.2f",params$emer_cond$VALUE[i]),"\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #nitrogen parameters
  cat("*NITROGEN PARAMETERS\n",file=pf)
  for (i in 1:nrow(params$nitrogen)) {
    cat(paste(sprintf("%8s",params$nitrogen$PARAM[i]),sprintf("%1$9.5f",params$nitrogen$VALUE[i]),"\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #leaf area and kernel number parameters (only ixm)
  if (model == "ixm") {
    cat("*LEAF AREA PARAMETERS\n",file=pf)
    cat(paste(sprintf("%1$6.1f%2$8.1f%3$10.3f",params$leaf_area1$A3,params$leaf_area1$A4,params$leaf_area1$AK),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$8.3f%2$6.1f",params$leaf_area2$YK,params$leaf_area2$YLL),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$6.1f%2$8.1f%3$8.1f",params$leaf_area3$SLAX,params$leaf_area3$SLAMX,params$leaf_area3$SLAMN),"\n",sep=""),file=pf)
    cat("\n",file=pf)
    
    cat("*KERNEL NUMBER PARAMETERS\n",file=pf)
    cat(paste(sprintf("%1$6.1f%2$8.1f",params$kernel$ASGDD,params$kernel$BSGDD),"\n",sep=""),file=pf)
    cat("\n",file=pf)
  }
  
  #root parameters
  cat("*ROOT PARAMETERS\n",file=pf)
  for (i in 1:nrow(params$root)) {
    cat(paste(sprintf("%8s",params$root$PARAM[i]),sprintf("%1$6.2f",params$root$VALUE[i]),"\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #respiration parameters (only ixm)
  if (model == "ixm") {
    cat("*RESPIRATION PARAMETERS\n",file=pf)
    cat(paste(sprintf("%1$9.6f%2$7.4f",params$respiration$RES30C,params$respiration$R30C2),"\n",sep=""),file=pf)
    cat("\n",file=pf)
  }
  
  #root parameters (different for each model)
  cat("*PLANT COMPOSITION VALUES\n",file=pf)
  if (model == "ixm") {
    cat(paste(sprintf("%1$6.3f%2$6.3f%3$6.3f%4$6.3f%5$6.3f",params$plant_comp1$PCARLF,params$plant_comp1$PCARST,params$plant_comp1$PCARRT,params$plant_comp1$PCAREA,params$plant_comp1$PCARSD),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$6.3f%2$6.3f%3$6.3f%4$6.3f%5$6.3f",params$plant_comp2$PPROLF,params$plant_comp2$PPROST,params$plant_comp2$PPRORT,params$plant_comp2$PPROEA,params$plant_comp2$PPROSD),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$6.3f%2$6.3f%3$6.3f%4$6.3f%5$6.3f",params$plant_comp3$PLIPLF,params$plant_comp3$PLIPST,params$plant_comp3$PLIPRT,params$plant_comp3$PLIPEA,params$plant_comp3$PLIPSD),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$6.3f%2$6.3f%3$6.3f%4$6.3f%5$6.3f",params$plant_comp4$PLIGLF,params$plant_comp4$PLIGST,params$plant_comp4$PLIGRT,params$plant_comp4$PLIGEA,params$plant_comp4$PLIGSD),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$6.3f%2$6.3f%3$6.3f%4$6.3f%5$6.3f",params$plant_comp5$POALF,params$plant_comp5$POAST,params$plant_comp5$POART,params$plant_comp5$POAEA,params$plant_comp5$POASD),"\n",sep=""),file=pf)
    cat(paste(sprintf("%1$6.3f%2$6.3f%3$6.3f%4$6.3f%5$6.3f",params$plant_comp6$PMINLF,params$plant_comp6$PMINST,params$plant_comp6$PMINRT,params$plant_comp6$PMINEA,params$plant_comp6$PMINSD),"\n",sep=""),file=pf)
  } else {
    for (i in 1:nrow(params$plant_comp)) {
      cat(paste(sprintf("%8s",params$plant_comp$PARAM[i]),sprintf("%1$7.3f",params$plant_comp$VALUE[i]),"\n",sep=""),file=pf)
    }
  }
  cat("\n",file=pf)
  
  #root parameters
  cat("*PHOSPHORUS CONTENT (g [P]/g [shoot])                                                                 \n",file=pf)
  cat(paste(sprintf("%1$8.4f%2$8.4f%3$8.4f",params$phos_cont$EM[1],params$phos_cont$LMAX[1],params$phos_cont$MAT[1]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.1f%2$8.1f%3$8.1f",params$phos_cont$EM[2],params$phos_cont$LMAX[2],params$phos_cont$MAT[2]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.1f%2$8.1f%3$8.1f",params$phos_cont$EM[3],params$phos_cont$LMAX[3],params$phos_cont$MAT[3]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.5f%2$8.5f%3$8.5f",params$phos_cont$EM[4],params$phos_cont$LMAX[4],params$phos_cont$MAT[4]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.4f%2$8.4f%3$8.4f",params$phos_cont$EM[5],params$phos_cont$LMAX[5],params$phos_cont$MAT[5]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.4f%2$8.4f%3$8.4f",params$phos_cont$EM[6],params$phos_cont$LMAX[6],params$phos_cont$MAT[6]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.4f%2$8.4f%3$8.4f",params$phos_cont$EM[7],params$phos_cont$LMAX[7],params$phos_cont$MAT[7]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.1f%2$8.1f%3$8.1f",params$phos_cont$EM[8],params$phos_cont$LMAX[8],params$phos_cont$MAT[8]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.1f%2$8.1f%3$8.1f",params$phos_cont$EM[9],params$phos_cont$LMAX[8],params$phos_cont$MAT[9]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.5f%2$8.5f%3$8.5f",params$phos_cont$EM[10],params$phos_cont$LMAX[10],params$phos_cont$MAT[10]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.4f%2$8.4f%3$8.4f",params$phos_cont$EM[11],params$phos_cont$LMAX[11],params$phos_cont$MAT[11]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.5f%2$8.5f%3$8.5f",params$phos_cont$EM[12],params$phos_cont$LMAX[12],params$phos_cont$MAT[12]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.1f%2$8.1f%3$8.1f",params$phos_cont$EM[13],params$phos_cont$LMAX[13],params$phos_cont$MAT[13]),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.1f%2$8.1f%3$8.1f",params$phos_cont$EM[14],params$phos_cont$LMAX[14],params$phos_cont$MAT[14]),"\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #last few (unidentified) parameters
  cat(paste(sprintf("%1$8.2f%2$8.2f",params$srat$SRATPHOTO,params$srat$SRATPART),"\n",sep=""),file=pf)
  cat(paste(sprintf("%1$8.2f",params$FRACPMOBIL),"\n",sep=""),file=pf)
  
  if (model == "ixm") {
    cat(paste(sprintf("%1$8.2f",params$FRACPUPTAKE),"\n",sep=""),file=pf)
  } else {
    cat(paste(sprintf("%1$8.4f",params$ROOTRAD),"\n",sep=""),file=pf)
  }
  
  #close file
  close(pf)
  
  #return filename
  return(out_file)
}
