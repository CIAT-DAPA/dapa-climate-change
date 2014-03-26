#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012
#modified on Feb 2014 for updated version of maize

#functions to read bits of the glam parameter file
#see meaning of variables in GLAM parameter files


#### this function would read a given set of outputs as specified by the argument `retain`,
#### which can take the following values: "all" or a vector composed of any combination of
#### the following strings, each of which corresponds to a section in the GLAM
#### parameter file:

#ygp: YIELD GAP PARAMETER (first row of parameter file)
#sim_ctr: SIMULATION CONTROLS (lines 3-5 of parameter file)
#mod_mgt: *MODEL MANAGEMENT
#spt_mgt: *SPATIAL MANAGEMENT AND LAI
#soils: *SOIL SPATIAL PARAMS
#drn_upk: *DRAINAGE AND UPTAKE
#evap: *EVAPORATION AND TRANSPIRATION
#bmass: *BIOMASS
#phenol: *PHENOLOGY
#fswsow: *INTELLIGENT SOWING
#hts_fut: *ADDITIONAL VARIABLES CURRENTLY ONLY USED FOR GROUNDNUT
#wheat: *ADDITIONAL WHEAT (SPRING AND WINTER) VARIABLES
#wwin: *ADDITIONAL WINTER WHEAT VARIABLES
#maize: *ADDITIONAL MAIZE VARIABLES
#rice: *ADDITIONAL RICE VARIABLES
#sparei: *SPARE INTEGER VARIABLES
#sparer: *SPARE REAL VARIABLES

#setwd("~/Leeds-work/quest-for-robustness/bin/glam-maize-osx")
#parFile <- "maize_param_base.txt"
#outfile <- "maize_param_run2.txt"
#tpar <- GLAM_get_default("./")
#pfil <- GLAM_create_parfile(params=tpar,outfile=outfile)

#function to write an standard line in the parameter file
#the line will be written in float, if not specified
write_line <- function(parlist,outfile,format="short") {
  pf <- file(outfile,open="a")
  l_name <- names(parlist)
  if (tolower(format) == "short") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11.2f",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9.2f%2$-9.2f%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$Nval),"\n",sep=""),file=pf)
  } else if (tolower(format) == "int") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11d",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9d%2$-9d%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$Nval),"\n",sep=""),file=pf)
  } else if (tolower(format) == "long") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11.4f",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9.4f%2$-9.4f%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$Nval),"\n",sep=""),file=pf)
  } else if (tolower(format) == "string") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11s",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9s%2$-9s%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$Nval),"\n",sep=""),file=pf)
  }
  close(pf)
}

################################################
#function to create a parameter file based on the object 'params', on the name of an output file
#and the name of a base (dummy) file from which parameters are read if these are not
#provided in the 'params' list
GLAM_create_parfile <- function(params,outfile,base_file=NA,overwrite=T) {
  
  #now what i need to do is build a function to write the parameter file
  #based on the object 'params'. This can easily be done if i have checks
  #in the function for each bit of the parameter file that needs to be written
  #(i.e. these need to be with proper names in the params list). If they're not
  #then they should be loaded from a dummy default file
  
  #if base_file=NA it means all needed data is in the 'params' object
  #(first argument of function)
  
  #open file in write mode
  if (file.exists(outfile)) {
    if (overwrite) {
      pf <- file(outfile,open="w")
    } else {
      rnum <- round(runif(1,10000,20000),0)
      tmpvar <- unlist(strsplit(outfile,"/",fixed=T))
      pth_ref <- paste(tmpvar[1:(length(tmpvar)-1)],collapse="/")
      outfile <- paste(pth_ref,"/copy-",rnum,"_",tmpvar[length(tmpvar)],sep="")
      pf <- file(outfile,open="w")
    }
  } else {
    pf <- file(outfile,open="w")
  }
  #attach(pf)
  #header of file
  cat("******* GLAM-R2 parameter file.                          Comments                                       *********\n",file=pf)
  close(pf)
  
  #yield gap parameter line
  if (!"glam_param.ygp" %in% names(params)) {
    glam_param.ygp <- get_ygp(base_file)
  } else {
    glam_param.ygp <- params$glam_param.ygp
  }
  
  write_line(glam_param.ygp,outfile=outfile,format="short")
  
  #general simulation controls (first 5 lines)
  if (!"glam_param.sim_ctr" %in% names(params)) {
    glam_param.sim_ctr <- get_sc(base_file)
  } else {
    glam_param.sim_ctr <- params$glam_param.sim_ctr
  }
  pf <- file(outfile,open="a")
  cat(paste(sprintf("%-12s","YGP_METH"),
            sprintf("%-11s",glam_param.sim_ctr$YGP_METH),
            sprintf("%-7d",glam_param.sim_ctr$MMNO),
            sprintf("%-9d",glam_param.sim_ctr$IMERF),
            sprintf("%-9d",glam_param.sim_ctr$ISHF),
            sprintf("%-9d",glam_param.sim_ctr$IUPT),"\n",
            sprintf("%-12s","NDSLA"),
            sprintf("%-11d",glam_param.sim_ctr$NDSLA),
            sprintf("%-7.1f",glam_param.sim_ctr$SLA_INI),
            sprintf("%-9.1f",glam_param.sim_ctr$ZSMAX),
            sprintf("%-9.1f",glam_param.sim_ctr$SMLON),
            sprintf("%-9s","x"),"\n",
            sprintf("%-12s","TETRS"),
            sprintf("%-11s",glam_param.sim_ctr$TETRS),
            sprintf("%-16s",glam_param.sim_ctr$CROP),
            sprintf("%-9d",glam_param.sim_ctr$IVMETH),
            sprintf("%-9d",glam_param.sim_ctr$IC02),"\n\n",
            sep=""),file=pf)
  
  #model management
  cat("*MODEL MANAGEMENT\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  
  if (!"glam_param.mod_mgt" %in% names(params)) {
    glam_param.mod_mgt <- get_sc(base_file)
  } else {
    glam_param.mod_mgt <- params$glam_param.mod_mgt
  }
  
  cat(paste(sprintf("%-12s","SEASON"),
            sprintf("%-11s",glam_param.mod_mgt$SEASON),
            sprintf("%-7s","b"),
            sprintf("%-9d",glam_param.mod_mgt$ISYR),
            sprintf("%-9d",glam_param.mod_mgt$IEYR),
            sprintf("%-9s",glam_param.mod_mgt$Output_type),"\n",
            sprintf("%-12s","MODE"),
            sprintf("%-11s",glam_param.mod_mgt$MODE),
            sprintf("%-7s","b"),
            sprintf("%-9d",glam_param.mod_mgt$INETCDF),
            sprintf("%-9d",glam_param.mod_mgt$IASCII),
            sprintf("%-9s",glam_param.mod_mgt$HTS),"\n",sep=""),file=pf)
  close(pf)
  write_line(list(NSL=glam_param.mod_mgt$NSL),outfile=outfile,format="int")
  pf <- file(outfile,open="a")
  cat(paste(sprintf("%-12s","I_R"),
            sprintf("%-11d",glam_param.mod_mgt$I_R),
            sprintf("%-7s","o"),
            sprintf("%-9d",1),
            sprintf("%-9d",2),
            sprintf("%-9d",1),"\n",
            sprintf("%-12s","I_E"),
            sprintf("%-11d",glam_param.mod_mgt$I_E),
            sprintf("%-7s","o"),
            sprintf("%-9d",1),
            sprintf("%-9d",2),
            sprintf("%-9d",1),"\n",sep=""),file=pf)
  close(pf)
  write_line(list(ISDAY=glam_param.mod_mgt$ISDAY),outfile=outfile,format="int")
  write_line(list(ASWS=glam_param.mod_mgt$ASWS),outfile=outfile,format="long")
  
  #spatial management and lai
  pf <- file(outfile,open="a")
  cat("\n*SPATIAL MANAGEMENT AND LAI\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.spt_mgt" %in% names(params)) {
    glam_param.spt_mgt <- get_spm(base_file)
  } else {
    glam_param.spt_mgt <- params$glam_param.spt_mgt
  }
  
  
  write_line(list(DLDTMX=glam_param.spt_mgt$DLDTMX),outfile=outfile,format="long")
  write_line(list(SHF_CTE=glam_param.spt_mgt$SHF_CTE),outfile=outfile,format="short")
  write_line(list(EXTC=glam_param.spt_mgt$EXTC),outfile=outfile,format="short")
  write_line(list(IPDATE=glam_param.spt_mgt$IPDATE),outfile=outfile,format="int")
  write_line(list(IHDATE=glam_param.spt_mgt$IHDATE),outfile=outfile,format="int")
  write_line(list(SWF_THRESH=glam_param.spt_mgt$SWF_THRESH),outfile=outfile,format="short")
  
  #soil spatial params
  pf <- file(outfile,open="a")
  cat("\n*SOIL SPATIAL PARAMS\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.soils" %in% names(params)) {
    glam_param.soils <- get_ssp(base_file)
  } else {
    glam_param.soils <- params$glam_param.soils
  }
  
  write_line(list(RLL=glam_param.soils$RLL),outfile=outfile,format="short")
  write_line(list(DUL=glam_param.soils$DUL),outfile=outfile,format="short")
  write_line(list(SAT=glam_param.soils$SAT),outfile=outfile,format="short")
  write_line(list(EFV=glam_param.soils$EFV),outfile=outfile,format="short")
  write_line(list(E_DEPTH=glam_param.soils$E_DEPTH),outfile=outfile,format="short")
  write_line(list(SMCT_FACT=glam_param.soils$SMCT_FACT),outfile=outfile,format="short")
  write_line(list(ALBEDO=glam_param.soils$ALBEDO),outfile=outfile,format="short")
  write_line(list(RKCTE=glam_param.soils$RKCTE),outfile=outfile,format="short")
  
  
  #drainage and uptake
  pf <- file(outfile,open="a")
  cat("\n*DRAINAGE AND UPTAKE\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.drn_upk" %in% names(params)) {
    glam_param.drn_upk <- get_du(base_file)
  } else {
    glam_param.drn_upk <- params$glam_param.drn_upk
  }
  
  write_line(list(D3=glam_param.drn_upk$D3),outfile=outfile,format="short")
  write_line(list(UPCTE=glam_param.drn_upk$UPCTE),outfile=outfile,format="short")
  write_line(list(DLDLAI=glam_param.drn_upk$DLDLAI),outfile=outfile,format="short")
  write_line(list(UPDIFC=glam_param.drn_upk$UPDIFC),outfile=outfile,format="long")
  write_line(list(RLVEF=glam_param.drn_upk$RLVEF),outfile=outfile,format="short")
  
  #evaporation and transpiration
  pf <- file(outfile,open="a")
  cat("\n*EVAPORATION AND TRANSPIRATION\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.evap" %in% names(params)) {
    glam_param.evap <- get_et(base_file)
  } else {
    glam_param.evap <- params$glam_param.evap
  }
  
  write_line(list(EC_CTE=glam_param.evap$EC_CTE),outfile=outfile,format="short")
  write_line(list(R_THRESH=glam_param.evap$R_THRESH),outfile=outfile,format="short")
  write_line(list(CRIT_LAI_T=glam_param.evap$CRIT_LAI_T),outfile=outfile,format="short")
  write_line(list(P_TRANS_MAX=glam_param.evap$P_TRANS_MAX),outfile=outfile,format="short")
  write_line(list(VPD_CTE=glam_param.evap$VPD_CTE),outfile=outfile,format="short")
  write_line(list(VPD_REF=glam_param.evap$VPD_REF),outfile=outfile,format="short")
  
  #biomass
  pf <- file(outfile,open="a")
  cat("\n*BIOMASS\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.bmass" %in% names(params)) {
    glam_param.bmass <- get_bm(base_file)
  } else {
    glam_param.bmass <- params$glam_param.bmass
  }
  
  write_line(list(TE=glam_param.bmass$TE),outfile=outfile,format="short")
  write_line(list(DHDT=glam_param.bmass$DHDT),outfile=outfile,format="long")
  write_line(list(TEN_MAX=glam_param.bmass$TEN_MAX),outfile=outfile,format="short")
  
  #phenology
  pf <- file(outfile,open="a")
  cat("\n*PHENOLOGY\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.phenol" %in% names(params)) {
    glam_param.phenol <- get_phe(base_file)
  } else {
    glam_param.phenol <- params$glam_param.phenol
  }
  
  write_line(list(IEMDAY=glam_param.phenol$IEMDAY),outfile=outfile,format="int")
  write_line(list(GCPLFL=glam_param.phenol$GCPLFL),outfile=outfile,format="short")
  write_line(list(TBFLWR=glam_param.phenol$TBFLWR),outfile=outfile,format="short")
  write_line(list(TOFLWR=glam_param.phenol$TOFLWR),outfile=outfile,format="short")
  write_line(list(TMFLWR=glam_param.phenol$TMFLWR),outfile=outfile,format="short")
  write_line(list(GCFLPF=glam_param.phenol$GCFLPF),outfile=outfile,format="short")
  write_line(list(TBPODF=glam_param.phenol$TBPODF),outfile=outfile,format="short")
  write_line(list(TOPODF=glam_param.phenol$TOPODF),outfile=outfile,format="short")
  write_line(list(TMPODF=glam_param.phenol$TMPODF),outfile=outfile,format="short")
  write_line(list(GCPFLM=glam_param.phenol$GCPFLM),outfile=outfile,format="short")
  write_line(list(TBLMAX=glam_param.phenol$TBLMAX),outfile=outfile,format="short")
  write_line(list(TOLMAX=glam_param.phenol$TOLMAX),outfile=outfile,format="short")
  write_line(list(TMLMAX=glam_param.phenol$TMLMAX),outfile=outfile,format="short")
  write_line(list(GCLMHA=glam_param.phenol$GCLMHA),outfile=outfile,format="short")
  write_line(list(TBHARV=glam_param.phenol$TBHARV),outfile=outfile,format="short")
  write_line(list(TOHARV=glam_param.phenol$TOHARV),outfile=outfile,format="short")
  write_line(list(TMHARV=glam_param.phenol$TMHARV),outfile=outfile,format="short")
  
  #intelligent sowing
  pf <- file(outfile,open="a")
  cat("\n*INTELLIGENT SOWING\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.fswsow" %in% names(params)) {
    glam_param.fswsow <- list(FSWSOW=get_line(parFile,l=82,float=T))
  } else {
    glam_param.fswsow <- params$glam_param.fswsow
  }
  
  write_line(list(FSWSOW=glam_param.fswsow$FSWSOW),outfile=outfile,format="short")
  
  #additional HTS and high CO2 parameterisations
  pf <- file(outfile,open="a")
  cat("\n*ADDITIONAL VARIABLES CURRENTLY ONLY USED FOR GROUNDNUT\n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.hts_fut" %in% names(params)) {
    glam_param.hts_fut <- get_add(base_file)
  } else {
    glam_param.hts_fut <- params$glam_param.hts_fut
  }
  
  write_line(list(SWFF_THR=glam_param.hts_fut$SWFF_THR),outfile=outfile,format="short")
  write_line(list(TENFAC=glam_param.hts_fut$TENFAC),outfile=outfile,format="short")
  write_line(list(B_TE=glam_param.hts_fut$B_TE),outfile=outfile,format="short")
  write_line(list(B_TEN_MAX=glam_param.hts_fut$B_TEN_MAX),outfile=outfile,format="short")
  write_line(list(TCRITMIN=glam_param.hts_fut$TCRITMIN),outfile=outfile,format="short")
  write_line(list(PPCRIT=glam_param.hts_fut$PPCRIT),outfile=outfile,format="short")
  write_line(list(TLINT=glam_param.hts_fut$TLINT),outfile=outfile,format="short")
  write_line(list(TCSLOPE=glam_param.hts_fut$TCSLOPE),outfile=outfile,format="short")
  write_line(list(TLSLOPE=glam_param.hts_fut$TLSLOPE),outfile=outfile,format="short")
  write_line(list(FDWIDTH=glam_param.hts_fut$FDWIDTH),outfile=outfile,format="short")
  write_line(list(FDOFFSET=glam_param.hts_fut$FDOFFSET),outfile=outfile,format="short")
  write_line(list(TLIMMIN=glam_param.hts_fut$TLIMMIN),outfile=outfile,format="short")
  write_line(list(IDURMAX=glam_param.hts_fut$IDURMAX),outfile=outfile,format="int")
  write_line(list(IBAMAX=glam_param.hts_fut$IBAMAX),outfile=outfile,format="int")
  write_line(list(IAAMAX=glam_param.hts_fut$IAAMAX),outfile=outfile,format="int")
  write_line(list(TLINT2=glam_param.hts_fut$TLINT2),outfile=outfile,format="short")
  write_line(list(TETR1=glam_param.hts_fut$TETR1),outfile=outfile,format="short")
  write_line(list(TETR2=glam_param.hts_fut$TETR2),outfile=outfile,format="short")
  
  
  #additional wheat (spring and winter) variables
  pf <- file(outfile,open="a")
  cat("\n*ADDITIONAL WHEAT (SPRING AND WINTER) VARIABLES                  \n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.wheat" %in% names(params)) {
    glam_param.wheat <- get_awht(base_file)
  } else {
    glam_param.wheat <- params$glam_param.wheat
  }
  
  write_line(list(GCPFEN=glam_param.wheat$GCPFEN),outfile=outfile,format="short")
  write_line(list(GCENHA=glam_param.wheat$GCENHA),outfile=outfile,format="short")
  
  
  #additional winter wheat variables
  pf <- file(outfile,open="a")
  cat("\n*ADDITIONAL WINTER WHEAT VARIABLES                  \n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.wwin" %in% names(params)) {
    glam_param.wwin <- get_wwin(base_file)
  } else {
    glam_param.wwin <- params$glam_param.wwin
  }
  
  write_line(list(GCPLTS=glam_param.wwin$GCPLTS),outfile=outfile,format="short")
  write_line(list(TBPLTS=glam_param.wwin$TBPLTS),outfile=outfile,format="short")
  write_line(list(TOPLTS=glam_param.wwin$TOPLTS),outfile=outfile,format="short")
  write_line(list(TMPLTS=glam_param.wwin$TMPLTS),outfile=outfile,format="short")
  write_line(list(GCTSFL=glam_param.wwin$GCTSFL),outfile=outfile,format="short")
  write_line(list(TBTSFL=glam_param.wwin$TBTSFL),outfile=outfile,format="short")
  write_line(list(TOTSFL=glam_param.wwin$TOTSFL),outfile=outfile,format="short")
  write_line(list(TMTSFL=glam_param.wwin$TMTSFL),outfile=outfile,format="short")
  write_line(list(WWSPA1=glam_param.wwin$WWSPA1),outfile=outfile,format="short")
  write_line(list(RLAIFLT=glam_param.wwin$RLAIFLT),outfile=outfile,format="short")
  write_line(list(EN=glam_param.wwin$EN),outfile=outfile,format="short")
  write_line(list(VR=glam_param.wwin$VR),outfile=outfile,format="short")
  write_line(list(FTDRFAC=glam_param.wwin$FTDRFAC),outfile=outfile,format="short")
  write_line(list(FTTSFAC=glam_param.wwin$FTTSFAC),outfile=outfile,format="short")
  write_line(list(VS=glam_param.wwin$VS),outfile=outfile,format="long")
  write_line(list(PS=glam_param.wwin$PS),outfile=outfile,format="long")
  write_line(list(WWSPA2=glam_param.wwin$WWSPA2),outfile=outfile,format="short")
  write_line(list(WWSPA3=glam_param.wwin$WWSPA3),outfile=outfile,format="short")
  write_line(list(WWSPA4=glam_param.wwin$WWSPA4),outfile=outfile,format="short")
  write_line(list(WWSPA5=glam_param.wwin$WWSPA5),outfile=outfile,format="short")
  write_line(list(WWSPA6=glam_param.wwin$WWSPA6),outfile=outfile,format="short")
  write_line(list(WWSPA7=glam_param.wwin$WWSPA7),outfile=outfile,format="short")
  write_line(list(WWSPA8=glam_param.wwin$WWSPA8),outfile=outfile,format="short")
  write_line(list(WWSPA9=glam_param.wwin$WWSPA9),outfile=outfile,format="short")
  
  
  #additional maize variables
  pf <- file(outfile,open="a")
  cat("\n*ADDITIONAL MAIZE VARIABLES                  \n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.maize" %in% names(params)) {
    glam_param.maize <- get_mai(base_file)
  } else {
    glam_param.maize <- params$glam_param.maize
  }
  
  write_line(list(MASPA=glam_param.maize$MASPA),outfile=outfile,format="long")
  write_line(list(TBMAI=glam_param.maize$TBMAI),outfile=outfile,format="short")
  write_line(list(TOMAI=glam_param.maize$TOMAI),outfile=outfile,format="short")
  write_line(list(TMMAI=glam_param.maize$TMMAI),outfile=outfile,format="short")
  write_line(list(TLIMJUV=glam_param.maize$TLIMJUV),outfile=outfile,format="short")
  write_line(list(TLIMSIL=glam_param.maize$TLIMSIL),outfile=outfile,format="short")
  write_line(list(TLIMPFL=glam_param.maize$TLIMPFL),outfile=outfile,format="short")
  write_line(list(TLIMGFP=glam_param.maize$TLIMGFP),outfile=outfile,format="short")
  write_line(list(PPSEN=glam_param.maize$PPSEN),outfile=outfile,format="long")
  write_line(list(TRLAI=glam_param.maize$TRLAI),outfile=outfile,format="string")
  write_line(list(TRKILL=glam_param.maize$TRKILL),outfile=outfile,format="string")
  write_line(list(NMAXTDS=glam_param.maize$NMAXTDS),outfile=outfile,format="int")
  write_line(list(CROPFAIL=glam_param.maize$CROPFAIL),outfile=outfile,format="string")
  write_line(list(NDSMAX=glam_param.maize$NDSMAX),outfile=outfile,format="int")
  write_line(list(IMAIZE6=glam_param.maize$IMAIZE6),outfile=outfile,format="int")
  write_line(list(IMAIZE7=glam_param.maize$IMAIZE7),outfile=outfile,format="int")
  write_line(list(IMAIZE8=glam_param.maize$IMAIZE8),outfile=outfile,format="int")
  write_line(list(IMAIZE9=glam_param.maize$IMAIZE9),outfile=outfile,format="int")
  
  
  #additional rice variables
  pf <- file(outfile,open="a")
  cat("\n*ADDITIONAL RICE VARIABLES                  \n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.rice" %in% names(params)) {
    glam_param.rice <- get_rice(base_file)
  } else {
    glam_param.rice <- params$glam_param.rice
  }
  
  write_line(list(RRICE1=glam_param.rice$RRICE1),outfile=outfile,format="short")
  write_line(list(RRICE2=glam_param.rice$RRICE2),outfile=outfile,format="short")
  write_line(list(RRICE3=glam_param.rice$RRICE3),outfile=outfile,format="short")
  write_line(list(RRICE4=glam_param.rice$RRICE4),outfile=outfile,format="short")
  write_line(list(RRICE5=glam_param.rice$RRICE5),outfile=outfile,format="short")
  write_line(list(RRICE6=glam_param.rice$RRICE6),outfile=outfile,format="short")
  write_line(list(RRICE7=glam_param.rice$RRICE7),outfile=outfile,format="short")
  write_line(list(RRICE8=glam_param.rice$RRICE8),outfile=outfile,format="short")
  write_line(list(RRICE9=glam_param.rice$RRICE9),outfile=outfile,format="short")
  write_line(list(IRICE1=glam_param.rice$IRICE1),outfile=outfile,format="int")
  write_line(list(IRICE2=glam_param.rice$IRICE2),outfile=outfile,format="int")
  write_line(list(IRICE3=glam_param.rice$IRICE3),outfile=outfile,format="int")
  write_line(list(IRICE4=glam_param.rice$IRICE4),outfile=outfile,format="int")
  write_line(list(IRICE5=glam_param.rice$IRICE5),outfile=outfile,format="int")
  write_line(list(IRICE6=glam_param.rice$IRICE6),outfile=outfile,format="int")
  write_line(list(IRICE7=glam_param.rice$IRICE7),outfile=outfile,format="int")
  write_line(list(IRICE8=glam_param.rice$IRICE8),outfile=outfile,format="int")
  write_line(list(IRICE9=glam_param.rice$IRICE9),outfile=outfile,format="int")
  
  
  #spare integer variables
  pf <- file(outfile,open="a")
  cat("\n*SPARE INTEGER VARIABLES                                 \n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.sparei" %in% names(params)) {
    glam_param.sparei <- get_spi(base_file)
  } else {
    glam_param.sparei <- params$glam_param.sparei
  }
  
  write_line(list(ISPARE1=glam_param.sparei$ISPARE1),outfile=outfile,format="string")
  write_line(list(ISPARE2=glam_param.sparei$ISPARE2),outfile=outfile,format="int")
  write_line(list(ISPARE3=glam_param.sparei$ISPARE3),outfile=outfile,format="int")
  write_line(list(ISPARE4=glam_param.sparei$ISPARE4),outfile=outfile,format="int")
  write_line(list(ISPARE5=glam_param.sparei$ISPARE5),outfile=outfile,format="int")
  write_line(list(ISPARE6=glam_param.sparei$ISPARE6),outfile=outfile,format="int")
  write_line(list(ISPARE7=glam_param.sparei$ISPARE7),outfile=outfile,format="int")
  write_line(list(ISPARE8=glam_param.sparei$ISPARE8),outfile=outfile,format="int")
  write_line(list(ISPARE9=glam_param.sparei$ISPARE9),outfile=outfile,format="int")
  
  #spare integer variables
  pf <- file(outfile,open="a")
  cat("\n*SPARE REAL VARIABLES                                    \n",file=pf)
  cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)
  close(pf)
  
  if (!"glam_param.sparer" %in% names(params)) {
    glam_param.sparer <- get_spr(base_file)
  } else {
    glam_param.sparer <- params$glam_param.sparer
  }
  
  write_line(list(CRITPP=glam_param.sparer$CRITPP),outfile=outfile,format="short")
  write_line(list(PPSE=glam_param.sparer$PPSE),outfile=outfile,format="short")
  write_line(list(RUE=glam_param.sparer$RUE),outfile=outfile,format="short")
  write_line(list(RUE_MAX=glam_param.sparer$RUE_MAX),outfile=outfile,format="short")
  write_line(list(TSETCRIT=glam_param.sparer$TSETCRIT),outfile=outfile,format="short")
  write_line(list(TSETZERO=glam_param.sparer$TSETZERO),outfile=outfile,format="short")
  write_line(list(RCO2=glam_param.sparer$RCO2),outfile=outfile,format="short")
  write_line(list(HIMIN=glam_param.sparer$HIMIN),outfile=outfile,format="long")
  write_line(list(SWC_FAC=glam_param.sparer$SWC_FAC),outfile=outfile,format="long")
  write_line(list(TETR3=glam_param.sparer$TETR3),outfile=outfile,format="short")
  write_line(list(TETR4=glam_param.sparer$TETR4),outfile=outfile,format="short")
  write_line(list(TRLAIB=glam_param.sparer$TRLAIB),outfile=outfile,format="short")
  write_line(list(TRLAIO=glam_param.sparer$TRLAIO),outfile=outfile,format="short")
  write_line(list(TRLAIM=glam_param.sparer$TRLAIM),outfile=outfile,format="short")
  write_line(list(TRKILL1=glam_param.sparer$TRKILL1),outfile=outfile,format="short")
  write_line(list(TRKILL2=glam_param.sparer$TRKILL2),outfile=outfile,format="short")
  write_line(list(FSWEMER=glam_param.sparer$FSWEMER),outfile=outfile,format="short")
  write_line(list(SLATTF=glam_param.sparer$SLATTF),outfile=outfile,format="short")
  
  #close the connection
  pf <- file(outfile,open="a")
  cat("\n",file=pf)#; cat("\n",file=pf)
  close(pf)
  
  #return name of output file
  return(outfile)
}



