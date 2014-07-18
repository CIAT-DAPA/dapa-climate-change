#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014

#functions to read the DSSAT '.ECO', '.CUL', and '.SPE' file
#currently only MZCER

#### testing for MZCER045
#setwd("~/Leeds-work/quest-for-robustness/bin/dssat/csm45_1_23_bin_gfort")
#eco_file <- "MZCER045.ECO"
#ecoall <- get_ecopar(in_file=eco_file,l=-1)

#cul_file <- "MZCER045.CUL"
#culall <- get_culpar(in_file=cul_file,l=-1)

#spe_file <- "MZCER045.SPE"
#speall <- get_spepar(in_file=spe_file)


#### testing for MZIXM045
#eco_file <- "MZIXM045.ECO"
#ecoall <- get_ecopar(in_file=eco_file,l=-1)

#cul_file <- "MZIXM045.CUL"
#culall <- get_culpar(in_file=cul_file,l=-1)

#spe_file <- "MZIXM045.SPE"
#speall <- get_spepar(in_file=spe_file)

#read the ecotype file from the parameter file
get_ecopar <- function(in_file, l=-1) {
  #determine which model
  if (length(grep("IXM",in_file)) != 0) {model <- "ixm"}
  if (length(grep("CER",in_file)) != 0) {model <- "cer"}
  
  #negative l means i will get all lines
  if (l == -1) {
    if (model == "cer") {
      toskip <- 21 #21 commentary lines in file
      vo <- read.fortran(in_file,skip=toskip,format=c("A7","A18","2F5","4F6","F7","F5","F7","2F6"))
    } else {
      toskip <- 25 #21 commentary lines in file
      vo <- read.fortran(in_file,skip=toskip,format=c("A7","A18","2F5","4F6","F7","F5","F7","4F6"))
    }
  } else {
    if (model == "cer") {
      toskip <- l-1+21
      #determine maximum number of lines
      all_lin <- read.fortran(in_file,skip=21,format=c("A7","A18","2F5","4F6","F7","F5","F7","2F6"))
      if (l > nrow(all_lin)) {
        warning("maximum number of lines exceeded, all lines retrieved")
        vo <- all_lin
      } else {
        vo <- read.fortran(in_file,skip=toskip,n=1,format=c("A7","A18","2F5","4F6","F7","F5","F7","2F6"))
      }
    } else {
      toskip <- l-1+25
      #determine maximum number of lines
      all_lin <- read.fortran(in_file,skip=25,format=c("A7","A18","2F5","4F6","F7","F5","F7","4F6"))
      if (l > nrow(all_lin)) {
        warning("maximum number of lines exceeded, all lines retrieved")
        vo <- all_lin
      } else {
        vo <- read.fortran(in_file,skip=toskip,n=1,format=c("A7","A18","2F5","4F6","F7","F5","F7","4F6"))
      }
    }
  }
  
  if (model == "cer") {
    names(vo) <- c("ECO_ID","ECO_NAME","TBASE","TOPT","ROPT","P20","DJTI","GDDE","DSGFT","RUE","KCAN",
                   "TSEN","CDAY")
  } else {
    names(vo) <- c("ECO_ID","ECO_NAME","TBASE","TOPT","ROPT","P20","DJTI","GDDE","DSGFT","RUE","KCAN",
                   "PSTM","PEAR","TSEN","CDAY")
  }
  return(vo)
}


#read cultivars from .CUL file
get_culpar <- function(in_file, l=-1) {
  #determine which model
  if (length(grep("IXM",in_file)) != 0) {model <- "ixm"}
  if (length(grep("CER",in_file)) != 0) {model <- "cer"}
  
  #negative l means i will get all lines
  if (l == -1) {
    if (model == "cer") {
      toskip <- 46 #46 commentary lines in file
      vo <- read.fortran(in_file,skip=toskip,format=c("A7","A17","A5","A7","6F6"))
    } else {
      toskip <- 50
      vo <- read.fortran(in_file,skip=toskip,format=c("A7","A17","A5","A7","8F6"))
    }
  } else {
    if (model == "cer") {
      toskip <- l-1+46
      #determine maximum number of lines
      all_lin <- read.fortran(in_file,skip=46,format=c("A7","A17","A5","A7","6F6"))
      if (l > nrow(all_lin)) {
        warning("maximum number of lines exceeded, all lines retrieved")
        vo <- all_lin
      } else {
        vo <- read.fortran(in_file,skip=toskip,n=1,format=c("A7","A17","A5","A7","6F6"))
      }
    } else {
      toskip <- l-1+50
      #determine maximum number of lines
      all_lin <- read.fortran(in_file,skip=50,format=c("A7","A17","A5","A7","8F6"))
      if (l > nrow(all_lin)) {
        warning("maximum number of lines exceeded, all lines retrieved")
        vo <- all_lin
      } else {
        vo <- read.fortran(in_file,skip=toskip,n=1,format=c("A7","A17","A5","A7","8F6"))
      }
    }
  }
  if (model == "cer") names(vo) <- c("CUL_ID","CUL_NAME","EXPNO","ECO_ID","P1","P2","P5","G2","G3","PHINT")
  if (model == "ixm") names(vo) <- c("CUL_ID","CUL_NAME","EXPNO","ECO_ID","P1","P2","P5","G2","G3","PHINT","AX","LX")
  return(vo)
}

#read the ecotype file from the parameter file
get_spepar <- function(in_file) {
  #determine which model
  if (length(grep("IXM",in_file)) != 0) {model <- "ixm"}
  if (length(grep("CER",in_file)) != 0) {model <- "cer"}
  
  #counter of extra lines for compatibility between the two models
  ic <- 0
  
  #initialise blank object
  spepar <- list()
  
  #temperature effects
  spepar$temp_resp <- read.fortran(in_file,skip=4,n=2,format=c("A7","F5","3F6"))
  names(spepar$temp_resp) <- c("PROCESS","TBASE","TOP1","TOP2","TMAX")
  
  #photosynthesis parameters
  spepar$photo_param <- list()
  spepar$photo_param$PARSR <- as.numeric(read.fortran(in_file,skip=8,n=1,format=c("A7","F7"))$V2)
  spepar$photo_param$co2_resp <- read.fortran(in_file,skip=9,n=2,format=c("A6","10F6"))
  names(spepar$photo_param$co2_resp) <- c("AXIS",paste("PT",1:10,sep=""))
  
  if (model == "ixm") {
    spepar$photo_param$ASMAX <- as.numeric(read.fortran(in_file,skip=11,n=1,format=c("A7","F7"))$V2)
    spepar$photo_param$XC <- as.numeric(read.fortran(in_file,skip=12,n=1,format=c("A7","F7"))$V2)
    spepar$photo_param$CANH <- as.numeric(read.fortran(in_file,skip=13,n=1,format=c("A7","F7"))$V2)
    ic <- ic+3
  }
  
  #stress response
  spepar$stress_resp <- read.fortran(in_file,skip=13+ic,n=3,format=c("A7","F8"))
  names(spepar$stress_resp) <- c("TYPE","FRAC")
  
  #seed growth parameters
  spepar$seed_growth <- read.fortran(in_file,skip=18+ic,n=7,format=c("A8","F6"))
  names(spepar$seed_growth) <- c("PARAM","VALUE")
  
  #emergence initial conditions
  spepar$emer_cond <- read.fortran(in_file,skip=27+ic,n=6,format=c("A9","F5"))
  names(spepar$emer_cond) <- c("PARAM","VALUE")
  
  #nitrogen parameters
  spepar$nitrogen <- read.fortran(in_file,skip=35+ic,n=6,format=c("A8","F9"))
  names(spepar$nitrogen) <- c("PARAM","VALUE")
  
  #leaf area and kernel parameters (only for IXIM)
  if (model == "ixm") {
    spepar$leaf_area1 <- read.fortran(in_file,skip=46,n=1,format=c("F6","F8","F10")); names(spepar$leaf_area1) <- c("A3","A4","AK")
    spepar$leaf_area2 <- read.fortran(in_file,skip=47,n=1,format=c("F8","F6")); names(spepar$leaf_area2) <- c("YK","YLL")
    spepar$leaf_area3 <- read.fortran(in_file,skip=48,n=1,format=c("F6","2F8")); names(spepar$leaf_area3) <- c("SLAX","SLAMX","SLAMN")
    spepar$kernel <- read.fortran(in_file,skip=51,n=1,format=c("F6","F8")); names(spepar$kernel) <- c("ASGDD","BSGDD")
    ic <- ic+5+3
  }
  
  #root parameters
  spepar$root <- read.fortran(in_file,skip=43+ic,n=4,format=c("A8","F6"))
  names(spepar$root) <- c("PARAM","VALUE")
  
  #respiration parameters (only IXM)
  if (model == "ixm") {
    spepar$respiration <- read.fortran(in_file,skip=60,n=1,format=c("F9","F7")); names(spepar$respiration) <- c("RES30C","R30C2")
    ic <- ic+3
  }
  
  #plant composition values
  if (model == "cer") {
    spepar$plant_comp <- read.fortran(in_file,skip=49+ic,n=5,format=c("A8","F7"))
    names(spepar$plant_comp) <- c("PARAM","VALUE")
  } else {
    spepar$plant_comp1 <- read.fortran(in_file,skip=63,n=1,format=c("5F6")); names(spepar$plant_comp1) <- c("PCARLF","PCARST","PCARRT","PCAREA","PCARSD")
    spepar$plant_comp2 <- read.fortran(in_file,skip=64,n=1,format=c("5F6")); names(spepar$plant_comp2) <- c("PPROLF","PPROST","PPRORT","PPROEA","PPROSD")
    spepar$plant_comp3 <- read.fortran(in_file,skip=65,n=1,format=c("5F6")); names(spepar$plant_comp3) <- c("PLIPLF","PLIPST","PLIPRT","PLIPEA","PLIPSD")
    spepar$plant_comp4 <- read.fortran(in_file,skip=66,n=1,format=c("5F6")); names(spepar$plant_comp4) <- c("PLIGLF","PLIGST","PLIGRT","PLIGEA","PLIGSD")
    spepar$plant_comp5 <- read.fortran(in_file,skip=67,n=1,format=c("5F6")); names(spepar$plant_comp5) <- c("POALF", "POAST", "POART", "POAEA", "POASD")
    spepar$plant_comp6 <- read.fortran(in_file,skip=68,n=1,format=c("5F6")); names(spepar$plant_comp6) <- c("PMINLF","PMINST","PMINRT","PMINEA","PMINSD")
    ic <- ic+1
  }
  
  #phosphorous content
  spepar$phos_cont <- read.fortran(in_file,skip=56+ic,n=14,format=c("3F8"))
  names(spepar$phos_cont) <- c("EM","LMAX","MAT")
  
  #last few (unidentified) parameters
  spepar$srat <- read.fortran(in_file,skip=71+ic,n=1,format=c("2F8")); names(spepar$srat) <- c("SRATPHOTO","SRATPART")
  spepar$FRACPMOBIL <- read.fortran(in_file,skip=72+ic,n=1,format=c("F8"))$V1
  
  if (model == "ixm") {
    spepar$FRACPUPTAKE <- read.fortran(in_file,skip=73+ic,n=1,format=c("F8"))$V1
  } else {
    spepar$ROOTRAD <- read.fortran(in_file,skip=73+ic,n=1,format=c("F8"))$V1
  }
  
  return(spepar)
}

