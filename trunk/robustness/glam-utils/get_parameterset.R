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
#outfile <- "maize_param_run.txt"
#tpar <- GLAM_get_default("./")
#pfil <- GLAM_create_parfile(params=tpar,outfile=outfile)

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
  tlint2 <- get_line(pFile,l=101,float=T) #from HTS parameterisation, not to be changed
  tetr1 <- get_line(pFile,l=102,float=T) #from transpiration efficiency reduction at high temperature, not to be changed
  tetr2 <- get_line(pFile,l=103,float=T) #from transpiration efficiency reduction at high temperature, not to be changed
  addC <- list(SWFF_THR=swff_thr,TENFAC=tenfac,B_TE=b_te,B_TEN_MAX=b_ten_max,
               TCRITMIN=tcritmin,PPCRIT=ppcrit,TLINT=tlint,TCSLOPE=tcslope,
               TLSLOPE=tlslope,FDWIDTH=fdwidth,FDOFFSET=fdoffset,TLIMMIN=tlimmin,
               IDURMAX=idurmax,IBAMAX=ibamax,IAAMAX=iaamax,TLINT2=tlint2,TETR1=tetr1,TETR2=tetr2)
  return(addC)
}


#additional wheat (spring and winter) variables
get_awht <- function(pFile) {
  gcpfen <- get_line(pFile,l=107,float=T)
  gcenha <- get_line(pFile,l=108,float=T)
  awhtC <- list(GCPFEN=gcpfen,GCENHA=gcenha)
  return(awhtC)
}


#additional winter wheat varieties
get_wwin <- function(pFile) {
  gcplts <- get_line(pFile,l=112,float=T)
  tbplts <- get_line(pFile,l=113,float=T)
  toplts <- get_line(pFile,l=114,float=T)
  tmplts <- get_line(pFile,l=115,float=T)
  gctsfl <- get_line(pFile,l=116,float=T)
  tbtsfl <- get_line(pFile,l=117,float=T)
  totsfl <- get_line(pFile,l=118,float=T)
  tmtsfl <- get_line(pFile,l=119,float=T)
  wwspa1 <- get_line(pFile,l=120,float=T) #for senescence, see GLAM parameter file
  rlaiflt <- get_line(pFile,l=121,float=T)
  en <- get_line(pFile,l=122,float=T)
  vr <- get_line(pFile,l=123,float=T)
  ftdrfac <- get_line(pFile,l=124,float=T)
  fttsfac <- get_line(pFile,l=125,float=T)
  vs <- get_line(pFile,l=126,float=T)
  ps <- get_line(pFile,l=127,float=T)
  wwspa2 <- get_line(pFile,l=128,float=T)
  wwspa3 <- get_line(pFile,l=129,float=T)
  wwspa4 <- get_line(pFile,l=130,float=T)
  wwspa5 <- get_line(pFile,l=131,float=T)
  wwspa6 <- get_line(pFile,l=132,float=T)
  wwspa7 <- get_line(pFile,l=133,float=T)
  wwspa8 <- get_line(pFile,l=134,float=T)
  wwspa9 <- get_line(pFile,l=135,float=T)
  wwinC <- list(GCPLTS=gcplts,TBPLTS=tbplts,TOPLTS=toplts,TMPLTS=tmplts,GCTSFL=gctsfl,
                TBTSFL=tbtsfl,TOTSFL=totsfl,TMTSFL=tmtsfl,WWSPA1=wwspa1,RLAIFLT=rlaiflt,
                EN=en,VR=vr,FTDRFAC=ftdrfac,FTTSFAC=fttsfac,VS=vs,PS=ps,WWSPA2=wwspa2,
                WWSPA3=wwspa3,WWSPA4=wwspa4,WWSPA5=wwspa5,WWSPA6=wwspa6,WWSPA7=wwspa7,
                WWSPA8=wwspa8,WWSPA9=wwspa9)
  return(wwinC)
}


#additional maize variables
get_mai <- function(pFile) {
  maspa <- get_line(pFile,l=139,float=T)
  tbmai <- get_line(pFile,l=140,float=T)
  tomai <- get_line(pFile,l=141,float=T)
  tmmai <- get_line(pFile,l=142,float=T)
  tlimjuv <- get_line(pFile,l=143,float=T)
  tlimsil <- get_line(pFile,l=144,float=T)
  tlimpfl <- get_line(pFile,l=145,float=T)
  tlimgfp <- get_line(pFile,l=146,float=T)
  ppsen <- get_line(pFile,l=147,float=T)
  trlai <- get_line(pFile,l=148,float=F,tex=T)
  trkill <- get_line(pFile,l=149,float=F,tex=T)
  nmaxtds <- get_line(pFile,l=150,float=F)
  cropfail <- get_line(pFile,l=151,float=F,tex=T)
  ndsmax <- get_line(pFile,l=152,float=F)
  imaize6 <- get_line(pFile,l=153,float=T)
  imaize7 <- get_line(pFile,l=154,float=T)
  imaize8 <- get_line(pFile,l=155,float=T)
  imaize9 <- get_line(pFile,l=156,float=T)
  maiC <- list(MASPA=maspa,TBMAI=tbmai,TOMAI=tomai,TMMAI=tmmai,TLIMJUV=tlimjuv,
               TLIMSIL=tlimsil,TLIMPFL=tlimpfl,TLIMGFP=tlimgfp,PPSEN=ppsen,TRLAI=trlai,
               TRKILL=trkill,NMAXTDS=nmaxtds,CROPFAIL=cropfail,NDSMAX=ndsmax,IMAIZE6=imaize6,
               IMAIZE7=imaize7,IMAIZE8=imaize8,IMAIZE9=imaize9)
  return(maiC)
}

#additional rice variables
get_rice <- function(pFile) {
  rrice1 <- get_line(pFile,l=160,float=T)
  rrice2 <- get_line(pFile,l=161,float=T)
  rrice3 <- get_line(pFile,l=162,float=T)
  rrice4 <- get_line(pFile,l=163,float=T)
  rrice5 <- get_line(pFile,l=164,float=T)
  rrice6 <- get_line(pFile,l=165,float=T)
  rrice7 <- get_line(pFile,l=166,float=T)
  rrice8 <- get_line(pFile,l=167,float=T)
  rrice9 <- get_line(pFile,l=168,float=T)
  irice1 <- get_line(pFile,l=169,float=T)
  irice2 <- get_line(pFile,l=170,float=T)
  irice3 <- get_line(pFile,l=171,float=T)
  irice4 <- get_line(pFile,l=172,float=T)
  irice5 <- get_line(pFile,l=173,float=T)
  irice6 <- get_line(pFile,l=174,float=T)
  irice7 <- get_line(pFile,l=175,float=T)
  irice8 <- get_line(pFile,l=176,float=T)
  irice9 <- get_line(pFile,l=177,float=T)
  ricC <- list(RRICE1=rrice1,RRICE2=rrice2,RRICE3=rrice3,RRICE4=rrice4,RRICE5=rrice5,
               RRICE6=rrice6,RRICE7=rrice7,RRICE8=rrice8,RRICE9=rrice9,IRICE1=irice1,
               IRICE2=irice2,IRICE3=irice3,IRICE4=irice4,IRICE5=irice5,IRICE6=irice6,
               IRICE7=irice7,IRICE8=irice8,IRICE9=irice9)
  return(ricC)
}

#spare integer variables
get_spi <- function(pFile) {
  ispare1 <- get_line(pFile,l=181,float=T,tex=T) #note ISPARE1 is a parameter for winter wheat
  ispare2 <- get_line(pFile,l=182,float=T)
  ispare3 <- get_line(pFile,l=183,float=T)
  ispare4 <- get_line(pFile,l=184,float=T)
  ispare5 <- get_line(pFile,l=185,float=T)
  ispare6 <- get_line(pFile,l=186,float=T)
  ispare7 <- get_line(pFile,l=187,float=T)
  ispare8 <- get_line(pFile,l=188,float=T)
  ispare9 <- get_line(pFile,l=189,float=T)
  spiC <- list(ISPARE1=ispare1,ISPARE2=ispare2,ISPARE3=ispare3,ISPARE4=ispare4,ISPARE5=ispare5,
               ISPARE6=ispare6,ISPARE7=ispare7,ISPARE8=ispare8,ISPARE9=ispare9)
  return(spiC)
}

#spare real variables
get_spr <- function(pFile) {
  critpp <- get_line(pFile,l=193,float=T)
  ppse <- get_line(pFile,l=194,float=T)
  rue <- get_line(pFile,l=195,float=T)
  rue_max <- get_line(pFile,l=196,float=T)
  tsetcrit <- get_line(pFile,l=197,float=T)
  tsetzero <- get_line(pFile,l=198,float=T)
  rco2 <- get_line(pFile,l=199,float=T)
  himin <- get_line(pFile,l=200,float=T)
  swc_fac <- get_line(pFile,l=201,float=T)
  tetr3 <- get_line(pFile,l=202,float=T)
  tetr4 <- get_line(pFile,l=203,float=T)
  trlaib <- get_line(pFile,l=204,float=T)
  trlaio <- get_line(pFile,l=205,float=T)
  trlaim <- get_line(pFile,l=206,float=T)
  trkill1 <- get_line(pFile,l=207,float=T)
  trkill2 <- get_line(pFile,l=208,float=T)
  fswemer <- get_line(pFile,l=209,float=T)
  slattf <- get_line(pFile,l=210,float=T)
  
  sprC <- list(CRITPP=critpp,PPSE=ppse,RUE=rue,RUE_MAX=rue_max,TSETCRIT=tsetcrit,TSETZERO=tsetzero,
               RCO2=rco2,HIMIN=himin,SWC_FAC=swc_fac,TETR3=tetr3,TETR4=tetr4,TRLAIB=trlaib,
               TRLAIO=trlaio,TRLAIM=trlaim,TRKILL1=trkill1,TRKILL2=trkill2,FSWEMER=fswemer,
               SLATTF=slattf)
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

