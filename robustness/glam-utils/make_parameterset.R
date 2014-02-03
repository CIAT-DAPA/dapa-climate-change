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
#hts_fut: *ADDITIONAL VARIABLES CURRENTLY ONLY USED FOR GROUNDNUT AND/OR SPRING WHEAT UNDER HEAT STRESS OR FUTURE CLIMATE
#wheat: *ADDITIONAL WHEAT (SPRING AND WINTER) VARIABLES
#wwin: *ADDITIONAL WINTER WHEAT VARIABLES
#maize: *ADDITIONAL MAIZE VARIABLES
#rice: *ADDITIONAL RICE VARIABLES
#sparei: *SPARE INTEGER VARIABLES
#sparer: *SPARE REAL VARIABLES

setwd("~/Leeds-work/quest-for-robustness/bin/glam-maize-osx")
parFile <- "maize_param_base.txt"
outfile <- "maize_param_test.txt"
tpar <- GLAM_get_default("./")

GLAM_get_par <- function(parFile,retain="all") {
  fullset <- c("all","ygp","sim_ctr","mod_mgt","spt_mgt","soils","drn_upk","evap",
               "phenol","fswsow","hts_fut","wheat","wwin","maize","rice","sparei","sparer")
  if (length(grep(retain[1],fullset)) == 0) {
    cat("variables to be retained not specified, retrieving all\n")
    retain <- "all"
  }
  
  if ("all" %in% tolower(retain) | "ygp" %in% tolower(retain)) glam_param.ygp <- get_ygp(parFile) #read ygp
  if ("all" %in% tolower(retain) | "sim_ctr" %in% tolower(retain)) glam_param.sim_ctr <- get_sc(parFile) #simulation controls
  if ("all" %in% tolower(retain) | "mod_mgt" %in% tolower(retain)) glam_param.mod_mgt <- get_mm(parFile) #model management
  if ("all" %in% tolower(retain) | "spt_mgt" %in% tolower(retain)) glam_param.spt_mgt <- get_spm(parFile) #spatial management and LAI
  if ("all" %in% tolower(retain) | "soils" %in% tolower(retain)) glam_param.soils <- get_ssp(parFile) #soil spatial parameters
  if ("all" %in% tolower(retain) | "drn_upk" %in% tolower(retain)) glam_param.drn_upk <- get_du(parFile) #drainage and uptake
  if ("all" %in% tolower(retain) | "evap" %in% tolower(retain)) glam_param.evap <- get_et(parFile) #evaporation and transpiration
  if ("all" %in% tolower(retain) | "bmass" %in% tolower(retain)) glam_param.bmass <- get_bm(parFile) #biomass
  if ("all" %in% tolower(retain) | "phenol" %in% tolower(retain)) glam_param.phenol <- get_phe(parFile) #phenology
  if ("all" %in% tolower(retain) | "fswsow" %in% tolower(retain)) glam_param.fswsow <- list(FSWSOW=get_line(parFile,l=82,float=T)) #intelligent sowing
  if ("all" %in% tolower(retain) | "hts_fut" %in% tolower(retain)) glam_param.hts_fut <- get_add(parFile) #additional variables
  if ("all" %in% tolower(retain) | "wheat" %in% tolower(retain)) glam_param.wheat <- get_awht(parFile) #additional wheat variables
  if ("all" %in% tolower(retain) | "wwin" %in% tolower(retain)) glam_param.wwin <- get_wwin(parFile) #additional winter wheat variables
  if ("all" %in% tolower(retain) | "maize" %in% tolower(retain)) glam_param.maize <- get_mai(parFile) #additional maize variables
  if ("all" %in% tolower(retain) | "rice" %in% tolower(retain)) glam_param.rice <- get_rice(parFile) #additional rice variables
  if ("all" %in% tolower(retain) | "sparei" %in% tolower(retain)) glam_param.sparei <- get_spi(parFile) #spare integer and real variables
  if ("all" %in% tolower(retain) | "sparer" %in% tolower(retain)) glam_param.sparer <- get_spr(parFile) #spare integer and real variables
  
  #appending everything in the same object
  out_list <- ls(pattern="glam_param.")
  all_obj <- list()
  ct <- 1
  for (obj in out_list) {
    ob <- get(obj)
    all_obj[[ct]] <- c(ob)
    ct <- ct+1
  }
  names(all_obj) <- out_list
  return(all_obj)
}


#read an standard parameter line from the parameter file
get_line <- function(pFile,l=31,float=F,tex=F) {
  toskip <- l-1
  if (!tex) {
    if (float) {
      vo <- read.fortran(pFile,skip=toskip,n=1,format=c("A12","F11","A7","2F9","I9","A110"))
      vo$V1 <- NULL
      names(vo) <- c("Value","Meth","Min","Max","Nval","Comments")
      vo$Comments <- NULL
      vo$Meth <- gsub(" ","",vo$Meth)
    } else {
      vo <- read.fortran(pFile,skip=toskip,n=1,format=c("A12","F11","A7","3F9","A110"))
      vo$V1 <- NULL
      names(vo) <- c("Value","Meth","Min","Max","Nval","Comments")
      vo$Comments <- NULL
      vo$Meth <- gsub(" ","",vo$Meth)
    }
  } else {
    vo <- read.fortran(pFile,skip=toskip,n=1,format=c("A12","A11","A7","2A9","I9","A110"))
    vo$V1 <- NULL
    names(vo) <- c("Value","Meth","Min","Max","Nval","Comments")
    vo$Comments <- NULL
    vo$Value <- gsub(" ","",vo$Value)
    vo$Meth <- gsub(" ","",vo$Meth)
    vo$Min <- gsub(" ","",vo$Min); vo$Max <- gsub(" ","",vo$Max)
  }
  return(vo)
}


#read ygp
get_ygp <- function(pFile) {
  ygp <- list(YGP=get_line(pFile,l=2,float=T))
  return(ygp)
}


#read simulation controls
get_sc <- function(pFile) {
  simC1 <- read.fortran(pFile,skip=2,n=1,format=c("A12","A11","I7","3I9","A110"))
  simC1$V1 <- NULL
  names(simC1) <- c("YGP_METH","MMNO","IMERF","ISHF","IUPT","Comments")
  simC1 <- as.list(simC1)
  simC1$Comments <- NULL
  simC1$YGP_METH <- gsub(" ","",simC1$YGP_METH)
  
  simC2 <- read.fortran(pFile,skip=3,n=1,format=c("A12","I11","F7","F9","F9","A9","A110"))
  simC2$V1 <- NULL; simC2$V6 <- NULL
  names(simC2) <- c("NDSLA","SLA_INI","ZSMAX","SMLON","Comments")
  simC2 <- as.list(simC2)
  simC2$Comments <- NULL
  
  simC3 <- read.fortran(pFile,skip=4,n=1,format=c("A12","A11","A16","I9","I9","A110"))
  simC3$V1 <- NULL
  names(simC3) <- c("TETRS","CROP","IVMETH","IC02","Comments")
  simC3 <- as.list(simC3)
  simC3$CROP <- gsub(" ","",simC3$CROP)
  simC3$TETRS <- gsub(" ","",simC3$TETRS)
  simC3$Comments <- NULL
  
  simC <- c(simC1,simC2,simC3)
  return(simC)
}


#to read model management
get_mm <- function(pFile) {
  mm1 <- read.fortran(pFile,skip=8,n=1,format=c("A12","A11","A7","2I9","A9","A110"))
  mm1$V1 <- NULL; mm1$V3 <- NULL
  names(mm1) <- c("SEASON","ISYR","IEYR","Output_type","Comments")
  mm1 <- as.list(mm1)
  mm1$Comments <- NULL
  mm1$SEASON <- gsub(" ","",mm1$SEASON)
  mm1$Output_type <- gsub(" ","",mm1$Output_type)
  
  mm2 <- read.fortran(pFile,skip=9,n=1,format=c("A12","A11","A7","2I9","A9","A110"))
  mm2$V1 <- NULL; mm2$V3 <- NULL
  names(mm2) <- c("MODE","INETCDF","IASCII","HTS","Comments")
  mm2 <- as.list(mm2)
  mm2$Comments <- NULL
  mm2$MODE <- gsub(" ","",mm2$MODE); mm2$HTS <- gsub(" ","",mm2$HTS)
  
  nsl <- get_line(pFile,l=11,float=F)
  
  mm3 <- read.fortran(pFile,skip=11,n=1,format=c("A12","I11","A144"))
  mm3$V1 <- NULL; mm3$V3 <- NULL
  names(mm3) <- c("I_R")
  mm3 <- as.list(mm3)
  
  mm4 <- read.fortran(pFile,skip=12,n=1,format=c("A12","I11","A144"))
  mm4$V1 <- NULL; mm4$V3 <- NULL
  names(mm4) <- c("I_E")
  mm4 <- as.list(mm4)
  
  isday <- get_line(pFile,l=14,float=F)
  asws <- get_line(pFile,l=15,float=T)
  
  mgtC <- c(mm1,mm2,mm3,mm4,list(NSL=nsl,ISDAY=isday,ASWS=asws))
  return(mgtC)
}

#spatial management and LAI
get_spm <- function(pFile) {
  dldtmx <- get_line(pFile,l=19,float=T)
  shf_cte <- get_line(pFile,l=20,float=T)
  extc <- get_line(pFile,l=21,float=T)
  ipdate <- get_line(pFile,l=22,float=T)
  ihdate <- get_line(pFile,l=23,float=T)
  swf_thresh <- get_line(pFile,l=24,float=T)
  
  spmC <- list(DLDTMX=dldtmx,SHF_CTE=shf_cte,EXTC=extc,IPDATE=ipdate,IHDATE=ihdate,SWF_THRESH=swf_thresh)
}

#soil spatial parameters
get_ssp <- function(pFile) {
  rll <- get_line(pFile,l=28,float=T)
  dul <- get_line(pFile,l=29,float=T)
  sat <- get_line(pFile,l=30,float=T)
  efv <- get_line(pFile,l=31,float=T)
  e_depth <- get_line(pFile,l=32,float=T)
  smct_fact <- get_line(pFile,l=33,float=T)
  albedo <- get_line(pFile,l=34,float=T)
  rkcte <- get_line(pFile,l=35,float=T)
  
  sspC <- list(RLL=rll,DUL=dul,SAT=sat,EFV=efv,E_DEPTH=e_depth,SMCT_FACT=smct_fact,ALBEDO=albedo,RKCTE=rkcte)
  return(sspC)
}

#drainage and uptake
get_du <- function(pFile) {
  d3 <- get_line(pFile,l=39,float=T)
  upcte <- get_line(pFile,l=40,float=T)
  dldlai <- get_line(pFile,l=41,float=T)
  updifc <- get_line(pFile,l=42,float=T)
  rlvef <- get_line(pFile,l=43,float=T)
  dupC <- list(D3=d3,UPCTE=upcte,DLDLAI=dldlai,UPDIFC=updifc,RLVEF=rlvef)
  return(dupC)
}

#evaporation and transpiration
get_et <- function(pFile) {
  ec_cte <- get_line(pFile,l=47,float=T)
  r_thresh <- get_line(pFile,l=48,float=T)
  crit_lai_t <- get_line(pFile,l=49,float=T)
  p_trans_max <- get_line(pFile,l=50,float=T)
  vpd_cte <- get_line(pFile,l=51,float=T)
  vpd_ref <- get_line(pFile,l=52,float=T)
  etC <- list(EC_CTE=ec_cte,R_THRESH=r_thresh,CRIT_LAI_T=crit_lai_t,
              P_TRANS_MAX=p_trans_max,VPD_CTE=vpd_cte,VPD_REF=vpd_ref)
  return(etC)
}


#biomass
get_bm <- function(pFile) {
  te <- get_line(pFile,l=56,float=T)
  dhdt <- get_line(pFile,l=57,float=T)
  ten_max <- get_line(pFile,l=58,float=T)
  bmC <- list(TE=te,DHDT=dhdt,TEN_MAX=ten_max)
  return(bmC)
}


#phenology
get_phe <- function(pFile) {
  iemday <- get_line(pFile,l=62,float=T)
  gcplfl <- get_line(pFile,l=63,float=T)
  tbflwr <- get_line(pFile,l=64,float=T)
  toflwr <- get_line(pFile,l=65,float=T)
  tmflwr <- get_line(pFile,l=66,float=T)
  gcflpf <- get_line(pFile,l=67,float=T)
  tbpodf <- get_line(pFile,l=68,float=T)
  topodf <- get_line(pFile,l=69,float=T)
  tmpodf <- get_line(pFile,l=70,float=T)
  gcpflm <- get_line(pFile,l=71,float=T)
  tblmax <- get_line(pFile,l=72,float=T)
  tolmax <- get_line(pFile,l=73,float=T)
  tmlmax <- get_line(pFile,l=74,float=T)
  gclmha <- get_line(pFile,l=75,float=T)
  tbharv <- get_line(pFile,l=76,float=T)
  toharv <- get_line(pFile,l=77,float=T)
  tmharv <- get_line(pFile,l=78,float=T)
  pheC <- list(IEMDAY=iemday,GCPLFL=gcplfl,TBFLWR=tbflwr,TOFLWR=toflwr,TMFLWR=tmflwr,
               GCFLPF=gcflpf,TBPODF=tbpodf,TOPODF=topodf,TMPODF=tmpodf,
               GCPFLM=gcpflm,TBLMAX=tblmax,TOLMAX=tolmax,TMLMAX=tmlmax,
               GCLMHA=gclmha,TBHARV=tbharv,TOHARV=toharv,TMHARV=tmharv)
  return(pheC)
}


#additional variables for HTS or future climates
get_add <- function(pFile) {
  swff_thr <- get_line(pFile,l=86,float=T) #damage to flowers under water stress, turned off for calibration
  tenfac <- get_line(pFile,l=87,float=T) #TEN_MAX reduction, turned off for calibration as is a CO2 related parameter
  b_te <- get_line(pFile,l=88,float=T) #note b_te is the calibrated value of TE, but for IC02=1
  b_ten_max <- get_line(pFile,l=89,float=T) #note b_ten_max is the calibrated value of TENFAC (above), but for IC02=1
  tcritmin <- get_line(pFile,l=90,float=T) #from HTS parameterisation, not to be changed
  ppcrit <- get_line(pFile,l=91,float=T) #from HTS parameterisation, not to be changed
  tlint <- get_line(pFile,l=92,float=T) #from HTS parameterisation, not to be changed
  tcslope <- get_line(pFile,l=92,float=T) #from HTS parameterisation, not to be changed
  tlslope <- get_line(pFile,l=94,float=T) #from HTS parameterisation, not to be changed
  fdwidth <- get_line(pFile,l=95,float=T) #from HTS parameterisation, not to be changed
  fdoffset <- get_line(pFile,l=96,float=T) #from HTS parameterisation, not to be changed
  tlimmin <- get_line(pFile,l=97,float=T) #from HTS parameterisation, not to be changed
  idurmax <- get_line(pFile,l=98,float=T) #from HTS parameterisation, not to be changed
  ibamax <- get_line(pFile,l=99,float=T) #from HTS parameterisation, not to be changed
  iaamax <- get_line(pFile,l=100,float=T) #from HTS parameterisation, not to be changed
  tetr1 <- get_line(pFile,l=101,float=T) #from transpiration efficiency reduction at high temperature, not to be changed
  tetr2 <- get_line(pFile,l=102,float=T) #from transpiration efficiency reduction at high temperature, not to be changed
  addC <- list(SWFF_THR=swff_thr,TENFAC=tenfac,B_TE=b_te,B_TEN_MAX=b_ten_max,
               TCRITMIN=tcritmin,PPCRIT=ppcrit,TLINT=tlint,TCSLOPE=tcslope,
               TLSLOPE=tlslope,FDWIDTH=fdwidth,FDOFFSET=fdoffset,TLIMMIN=tlimmin,
               IDURMAX=idurmax,IBAMAX=ibamax,IAAMAX=iaamax,TETR1=tetr1,TETR2=tetr2)
  return(addC)
}


#additional wheat (spring and winter) variables
get_awht <- function(pFile) {
  gcpfen <- get_line(pFile,l=106,float=T)
  gcenha <- get_line(pFile,l=107,float=T)
  awhtC <- list(GCPFEN=gcpfen,GCENHA=gcenha)
  return(awhtC)
}


#additional winter wheat varieties
get_wwin <- function(pFile) {
  gcplts <- get_line(pFile,l=111,float=T)
  tbplts <- get_line(pFile,l=112,float=T)
  toplts <- get_line(pFile,l=113,float=T)
  tmplts <- get_line(pFile,l=114,float=T)
  gctsfl <- get_line(pFile,l=115,float=T)
  tbtsfl <- get_line(pFile,l=116,float=T)
  totsfl <- get_line(pFile,l=117,float=T)
  tmtsfl <- get_line(pFile,l=118,float=T)
  wwspa1 <- get_line(pFile,l=119,float=T) #for senescence, see GLAM parameter file
  rlaiflt <- get_line(pFile,l=120,float=T)
  en <- get_line(pFile,l=121,float=T)
  vr <- get_line(pFile,l=122,float=T)
  ftdrfac <- get_line(pFile,l=123,float=T)
  fttsfac <- get_line(pFile,l=124,float=T)
  vs <- get_line(pFile,l=125,float=T)
  ps <- get_line(pFile,l=126,float=T)
  wwspa2 <- get_line(pFile,l=127,float=T)
  wwspa3 <- get_line(pFile,l=128,float=T)
  wwspa4 <- get_line(pFile,l=129,float=T)
  wwspa5 <- get_line(pFile,l=130,float=T)
  wwspa6 <- get_line(pFile,l=131,float=T)
  wwspa7 <- get_line(pFile,l=132,float=T)
  wwspa8 <- get_line(pFile,l=133,float=T)
  wwspa9 <- get_line(pFile,l=134,float=T)
  wwinC <- list(GCPLTS=gcplts,TBPLTS=tbplts,TOPLTS=toplts,TMPLTS=tmplts,GCTSFL=gctsfl,
                TBTSFL=tbtsfl,TOTSFL=totsfl,TMTSFL=tmtsfl,WWSPA1=wwspa1,RLAIFLT=rlaiflt,
                EN=en,VR=vr,FTDRFAC=ftdrfac,FTTSFAC=fttsfac,VS=vs,PS=ps,WWSPA2=wwspa2,
                WWSPA3=wwspa3,WWSPA4=wwspa4,WWSPA5=wwspa5,WWSPA6=wwspa6,WWSPA7=wwspa7,
                WWSPA8=wwspa8,WWSPA9=wwspa9)
  return(wwinC)
}


#additional maize variables
get_mai <- function(pFile) {
  maspa <- get_line(pFile,l=138,float=T)
  tbmai <- get_line(pFile,l=139,float=T)
  tomai <- get_line(pFile,l=140,float=T)
  tmmai <- get_line(pFile,l=141,float=T)
  tlimjuv <- get_line(pFile,l=142,float=T)
  tlimsil <- get_line(pFile,l=143,float=T)
  tlimpfl <- get_line(pFile,l=144,float=T)
  tlimgfp <- get_line(pFile,l=145,float=T)
  ppsen <- get_line(pFile,l=146,float=T)
  trlai <- get_line(pFile,l=147,float=F,tex=T)
  trkill <- get_line(pFile,l=148,float=F,tex=T)
  imaize3 <- get_line(pFile,l=149,float=T)
  imaize4 <- get_line(pFile,l=150,float=T)
  imaize5 <- get_line(pFile,l=151,float=T)
  imaize6 <- get_line(pFile,l=152,float=T)
  imaize7 <- get_line(pFile,l=153,float=T)
  imaize8 <- get_line(pFile,l=154,float=T)
  imaize9 <- get_line(pFile,l=155,float=T)
  maiC <- list(MASPA=maspa,TBMAI=tbmai,TOMAI=tomai,TMMAI=tmmai,TLIMJUV=tlimjuv,
               TLIMSIL=tlimsil,TLIMPFL=tlimpfl,TLIMGFP=tlimgfp,PPSEN=ppsen,TRLAI=trlai,
               TRKILL=trkill,IMAIZE3=imaize3,IMAIZE4=imaize4,IMAIZE5=imaize5,IMAIZE6=imaize6,
               IMAIZE7=imaize7,IMAIZE8=imaize8,IMAIZE9=imaize9)
  return(maiC)
}

#additional rice variables
get_rice <- function(pFile) {
  rrice1 <- get_line(pFile,l=159,float=T)
  rrice2 <- get_line(pFile,l=160,float=T)
  rrice3 <- get_line(pFile,l=161,float=T)
  rrice4 <- get_line(pFile,l=162,float=T)
  rrice5 <- get_line(pFile,l=163,float=T)
  rrice6 <- get_line(pFile,l=164,float=T)
  rrice7 <- get_line(pFile,l=165,float=T)
  rrice8 <- get_line(pFile,l=166,float=T)
  rrice9 <- get_line(pFile,l=167,float=T)
  irice1 <- get_line(pFile,l=168,float=T)
  irice2 <- get_line(pFile,l=169,float=T)
  irice3 <- get_line(pFile,l=170,float=T)
  irice4 <- get_line(pFile,l=171,float=T)
  irice5 <- get_line(pFile,l=172,float=T)
  irice6 <- get_line(pFile,l=173,float=T)
  irice7 <- get_line(pFile,l=174,float=T)
  irice8 <- get_line(pFile,l=175,float=T)
  irice9 <- get_line(pFile,l=176,float=T)
  ricC <- list(RRICE1=rrice1,RRICE2=rrice2,RRICE3=rrice3,RRICE4=rrice4,RRICE5=rrice5,
               RRICE6=rrice6,RRICE7=rrice7,RRICE8=rrice8,RRICE9=rrice9,IRICE1=irice1,
               IRICE2=irice2,IRICE3=irice3,IRICE4=irice4,IRICE5=irice5,IRICE6=irice6,
               IRICE7=irice7,IRICE8=irice8,IRICE9=irice9)
  return(ricC)
}

#spare integer variables
get_spi <- function(pFile) {
  ispare1 <- get_line(pFile,l=180,float=T,tex=T) #note ISPARE1 is a parameter for winter wheat
  ispare2 <- get_line(pFile,l=181,float=T)
  ispare3 <- get_line(pFile,l=182,float=T)
  ispare4 <- get_line(pFile,l=183,float=T)
  ispare5 <- get_line(pFile,l=184,float=T)
  ispare6 <- get_line(pFile,l=185,float=T)
  ispare7 <- get_line(pFile,l=186,float=T)
  ispare8 <- get_line(pFile,l=187,float=T)
  ispare9 <- get_line(pFile,l=188,float=T)
  spiC <- list(ISPARE1=ispare1,ISPARE2=ispare2,ISPARE3=ispare3,ISPARE4=ispare4,ISPARE5=ispare5,
               ISPARE6=ispare6,ISPARE7=ispare7,ISPARE8=ispare8,ISPARE9=ispare9)
  return(spiC)
}

#spare real variables
get_spr <- function(pFile) {
  critpp <- get_line(pFile,l=192,float=T)
  ppse <- get_line(pFile,l=193,float=T)
  rue <- get_line(pFile,l=194,float=T)
  rue_max <- get_line(pFile,l=195,float=T)
  tsetcrit <- get_line(pFile,l=196,float=T)
  tsetzero <- get_line(pFile,l=197,float=T)
  rco2 <- get_line(pFile,l=198,float=T)
  rspare8 <- get_line(pFile,l=199,float=T) #value of HIMIN for TDS in groundnut
  rspare9 <- get_line(pFile,l=200,float=T) #value of SWC_FAC for TDS runs
  tetr3 <- get_line(pFile,l=201,float=T)
  tetr4 <- get_line(pFile,l=202,float=T)
  trlaib <- get_line(pFile,l=203,float=T)
  trlaio <- get_line(pFile,l=204,float=T)
  trlaim <- get_line(pFile,l=205,float=T)
  trkill1 <- get_line(pFile,l=206,float=T)
  trkill2 <- get_line(pFile,l=207,float=T)
  
  sprC <- list(CRITPP=critpp,PPSE=ppse,RUE=rue,RUE_MAX=rue_max,TSETCRIT=tsetcrit,TSETZERO=tsetzero,
               RCO2=rco2,RSPARE8=rspare8,RSPARE9=rspare9,TETR3=tetr3,TETR4=tetr4,TRLAIB=trlaib,
               TRLAIO=trlaio,TRLAIM=trlaim,TRKILL1=trkill1,TRKILL2=trkill2)
  return(sprC)
}


GLAM_get_default <- function(parDir) {
  #get the model parameters from the dummy file
  dumFile <- paste(parDir,"/maize_param_base.txt",sep="")
  GLAM_params <- GLAM_get_par(dumFile,retain=c("all"))
  
  #default settings below
  #simulation controls here
  GLAM_params$glam_param.mod_mgt$MODE <- "SET" #just run with specified parameters
  GLAM_params$glam_param.mod_mgt$SEASON <- "RFD" #rainfed season
  GLAM_params$glam_param.mod_mgt$INETCDF <- 0 #no netcdf output
  GLAM_params$glam_param.mod_mgt$IASCII <- 3 #output ascii and daily
  GLAM_params$glam_param.mod_mgt$I_E <- 2 #I_E = 2, for GLAM-R2
  GLAM_params$glam_param.mod_mgt$I_R <- 2 #I_R = 2, for GLAM-R2
  GLAM_params$glam_param.sim_ctr$MMNO <- 1 #MMNO = 1, meaning calibration based on RMSE
  GLAM_params$glam_param.sim_ctr$IMERF <- 0 #IMERF = 1 (global optimisation)
  GLAM_params$glam_param.sim_ctr$SLA_INI <- 300 #SLA should be on!, SLA_INI = 300 (Challinor and Wheeler 2008)
  GLAM_params$glam_param.sim_ctr$ZSMAX <- 210 #ZSMAX: depth of soil profile should be 210 cm, Challinor et al. (2004)
  GLAM_params$glam_param.mod_mgt$HTS <- "+1" #turn on HTS subroutine, parameters are below
  GLAM_params$glam_param.sim_ctr$IC02 <- 0 #Turn off CO2 enrichment routine
  GLAM_params$glam_param.sim_ctr$ISHF <- 1 #ISHF = 1
  GLAM_params$glam_param.sim_ctr$IUPT <- 1 #IUPT = 1
  GLAM_params$glam_param.sim_ctr$TETRS <- "+1." #turn on TE/RUE temperature responses
  
  #soil stuff to -99 so that it is all taken from soils file
  GLAM_params$glam_param.soils$RLL$Value <- -99
  GLAM_params$glam_param.soils$DUL$Value <- -99
  GLAM_params$glam_param.soils$SAT$Value <- -99
  
  #IC02 should be off (IC02==0), and TENFAC should be -99
  #These would be activated for future simulations with high CO2 concentrations
  GLAM_params$glam_param.hts_fut$TENFAC$Value <- -99
  
  #SWFF_THR: damage to flowers due to water stress. Turned on. Challinor et al. (2006)
  GLAM_params$glam_param.hts_fut$SWFF_THR$Value <- 0.2
  
  #end of defaults
  return(GLAM_params)
}


#function to write an standard line in the parameter file
#the line will be written in float, if not specified
write_line <- function(parlist,outfile,format="short") {
  pf <- file(outfile,open="a")
  l_name <- names(parlist)
  if (tolower(format) == "short") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11.2f",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9.2f%2$-9.2f%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$NVAL),"\n",sep=""),file=pf)
  } else if (tolower(format) == "int") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11d",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9d%2$-9d%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$NVAL),"\n",sep=""),file=pf)
  } else if (tolower(format) == "long") {
    cat(paste(sprintf("%-12s",l_name),sprintf("%-11.4f",parlist[[l_name]]$Value),
              sprintf("%-7s",parlist[[l_name]]$Meth),
              sprintf("%1$-9.4f%2$-9.4f%3$-9d",parlist[[l_name]]$Min,parlist[[l_name]]$Max,parlist[[l_name]]$NVAL),"\n",sep=""),file=pf)
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
            sprintf("%-16s",glam_param.sim_ctr$RunID),
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
  cat("\n*ADDITIONAL VARIABLES CURRENTLY ONLY USED FOR GROUNDNUT AND/OR SPRING WHEAT UNDER HEAT STRESS OR FUTURE CLIMATE\n",file=pf)
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
  
  write_line(list(RMAIZE1=glam_param.maize$RMAIZE1),outfile=outfile,format="long")
  write_line(list(RMAIZE2=glam_param.maize$RMAIZE2),outfile=outfile,format="short")
  write_line(list(RMAIZE3=glam_param.maize$RMAIZE3),outfile=outfile,format="short")
  write_line(list(RMAIZE4=glam_param.maize$RMAIZE4),outfile=outfile,format="short")
  write_line(list(RMAIZE5=glam_param.maize$RMAIZE5),outfile=outfile,format="short")
  write_line(list(RMAIZE6=glam_param.maize$RMAIZE6),outfile=outfile,format="short")
  write_line(list(RMAIZE7=glam_param.maize$RMAIZE7),outfile=outfile,format="short")
  write_line(list(RMAIZE8=glam_param.maize$RMAIZE8),outfile=outfile,format="short")
  write_line(list(RMAIZE9=glam_param.maize$RMAIZE9),outfile=outfile,format="short")
  write_line(list(IMAIZE1=glam_param.maize$IMAIZE1),outfile=outfile,format="int")
  write_line(list(IMAIZE2=glam_param.maize$IMAIZE2),outfile=outfile,format="int")
  write_line(list(IMAIZE3=glam_param.maize$IMAIZE3),outfile=outfile,format="int")
  write_line(list(IMAIZE4=glam_param.maize$IMAIZE4),outfile=outfile,format="int")
  write_line(list(IMAIZE5=glam_param.maize$IMAIZE5),outfile=outfile,format="int")
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
  
  write_line(list(ISPARE1=glam_param.sparei$ISPARE1),outfile=outfile,format="int")
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
  
  write_line(list(RSPARE1=glam_param.sparer$RSPARE1),outfile=outfile,format="long")
  write_line(list(RSPARE2=glam_param.sparer$RSPARE2),outfile=outfile,format="long")
  write_line(list(RSPARE3=glam_param.sparer$RSPARE3),outfile=outfile,format="short")
  write_line(list(RSPARE4=glam_param.sparer$RSPARE4),outfile=outfile,format="short")
  write_line(list(RSPARE5=glam_param.sparer$RSPARE5),outfile=outfile,format="short")
  write_line(list(RSPARE6=glam_param.sparer$RSPARE6),outfile=outfile,format="short")
  write_line(list(RSPARE7=glam_param.sparer$RSPARE7),outfile=outfile,format="short")
  write_line(list(RSPARE8=glam_param.sparer$RSPARE8),outfile=outfile,format="short")
  write_line(list(RSPARE9=glam_param.sparer$RSPARE9),outfile=outfile,format="short")
  
  #close the connection
  #detach(pf)
  #close(pf)
  pf <- file(outfile,open="a")
  cat("\n",file=pf); cat("\n",file=pf)
  close(pf)
  
  #return name of output file
  return(outfile)
}



