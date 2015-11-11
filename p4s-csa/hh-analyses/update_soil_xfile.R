#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#update soils and xfile objects based on maximin hypercube run

#remember:
# * initial soil water increasing with depth
# * initial soil N decreasing with depth
# * PPOE = PPOP
# * update ICDAT, RDATE, SDATE, PFRST, PLAST, based on sampled PDATE
# * RESN for residues = ICREN
# * RESN for manure = RESN in table

update_soil_xfile <- function(hyp_line, xfil_data, soil_data, param_list) {
  #hyp_line <- lhyp1[1,]
  #xfil_data <- loc_xfil
  #soil_data <- loc_soil
  for (i in 1:nrow(param_list)) {
    #i <- 13
    tvalue <- qunif(hyp_line[i],min=param_list$Min[i],max=param_list$Max[i])
    tparam <- paste(param_list$Parameter[i])
    tfile <- paste(param_list$File[i])
    tslot <- paste(param_list$Slot[i])
    
    if (tfile == "xfile") {
      if (tparam == "ICREN") {
        xfil_data[[tslot]][tparam] <- tvalue
        xfil_data$residues$RESN[1] <- tvalue #the same residue
      } else if (tparam == "SH2O_5") {
        xfil_data[[tslot]]$SH2O[1] <- tvalue
      } else if (tparam == "SH2O_200") {
        xfil_data[[tslot]]$SH2O[6] <- tvalue
      } else if (tparam == "SNO3_5") {
        xfil_data[[tslot]]$SNO3[1] <- tvalue
      } else if (tparam == "SNO3_200") {
        xfil_data[[tslot]]$SNO3[6] <- tvalue
      } else if (tparam == "RESN") {
        xfil_data[[tslot]]$RESN[2] <- tvalue #RESN in table is manure
      } else if (tparam == "PDATE") {
        #put value
        xfil_data[[tslot]][tparam] <- paste(as.numeric(paste(xfil_data[[tslot]][tparam][1,])) + round(tvalue))
        
        #update ICDAT, RDATE, SDATE, PFRST, PLAST, based on sampled PDATE
        xfil_data$sim_ctrl$SDATE <- paste(as.numeric(paste(xfil_data$planting$PDATE)) - 1)
        xfil_data$ini_cond_properties$ICDAT <- paste(as.numeric(paste(xfil_data$planting$PDATE)) - 1)
        xfil_data$residues$RDATE <- paste(as.numeric(paste(xfil_data$planting$PDATE)) - 1)
        xfil_data$auto_mgmt$PFRST <- paste(as.numeric(paste(xfil_data$planting$PDATE)) - 10)
        xfil_data$auto_mgmt$PLAST <- paste(as.numeric(paste(xfil_data$planting$PDATE)) + 10)
      } else {
        xfil_data[[tslot]][tparam] <- tvalue
      }
    } else if (tfile == "soilfile") {
      soil_data[[tslot]][tparam] <- tvalue
    } else {
      stop("File value in param_list data.frame is inappropriate!")
    }
  }
  
  #assign PPOE to value of PPOP
  xfil_data$planting$PPOE <- xfil_data$planting$PPOP
  
  #interpolate intermediate depths in ini_cond_profile, xfile
  for (i in 2:5) {
    #i <- 2
    xfil_data$ini_cond_profile$SH2O[i] <- (xfil_data$ini_cond_profile$ICBL[i]-5) / (200-5) * (xfil_data$ini_cond_profile$SH2O[6] - xfil_data$ini_cond_profile$SH2O[1]) + xfil_data$ini_cond_profile$SH2O[1]
    xfil_data$ini_cond_profile$SNO3[i] <- (xfil_data$ini_cond_profile$ICBL[i]-5) / (200-5) * (xfil_data$ini_cond_profile$SNO3[6] - xfil_data$ini_cond_profile$SNO3[1]) + xfil_data$ini_cond_profile$SNO3[1]
  }
  return(list(XFILE=xfil_data,SOIL=soil_data))
}

