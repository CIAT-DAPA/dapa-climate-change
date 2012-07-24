#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#get parameter set from a given GLAM run
#this is gridcell specific
GLAM_get_run_param <- function(asc_dir,cal_dir,exp_dir,crop="gnut",prefix="fcal_",gridcell,ygp="opt",ipdate="rfd") {
  #if yield gap parameter is =="opt" then grab the optimal ygp from ygp.RData
  if (!is.numeric(ipdate)) {
    if (!ipdate %in% c("rfd","irr")) {
      warning("ipdate was mis-specified, optimal rainfed sowing dates will be retrieved")
      ipdate <- "rfd"
    }
  } else {
    if (ipdate > 365) {
      warning("ipdate is above 365, using 160 as default")
      ipdate <- 160
    }
  }
  
  #directory with run data
  run_dir <- paste(cal_dir,"/",exp_dir,"/gridcells/",prefix,gridcell,sep="")
  
  #grab ygp value
  if (ygp == "opt") {
    ygp_file <- paste(run_dir,"/ygp.RData",sep="")
    load(ygp_file)
    ygp_value <- optimal$YGP
    rm(optimal); rm(optimised); g=gc(); rm(g)
  } else {
    ygp_value <- ygp
  }
  
  #grab ipdate value
  if (ipdate == "rfd") {
    ipd_file <- paste(run_dir,"/ipdate.RData",sep="")
    load(ipd_file)
    ipd_value <- optimal$IPDATE
    rm(optimal); rm(optimised); g=gc(); rm(g)
  } else if (ipdate == "irr") {
    ipd_file <- paste(asc_dir,"/sow/sowing_",gridcell,"_irr.txt",sep="")
    ipd_value <- read.fortran(ipd_file,format=c("2I4","I6"))
    ipd_value <- as.numeric(ipd_value)[3]
  } else {
    ipd_value <- ipdate
  }
  
  #name of parameter file
  par_file <- paste(run_dir,"/glam-r2-param-",tolower(crop),"-run.txt",sep="")
  if (!file.exists(par_file)) {
    par_file <- paste(run_dir,"/glam-r2-param-",tolower(crop),"-run-rfd.txt",sep="")
  }
  
  #update parameter file
  params <- GLAM_get_par(par_file,retain="all") #get run parameter set
  params$glam_param.ygp$YGP$Value <- ygp_value #replace ygp
  params$glam_param.spt_mgt$IPDATE$Value <- ipd_value #replace ipdate
  
  return(params)
}


#get soil data files for a given gridcell
get_soil_files <- function(run_setup,asc_dir,gridcell,codes_prefix="soilcodes_",types_prefix="soiltypes_") {
  if (!is.list(run_setup)) {
    warning("run_setup has to be a list, so an empty one was created")
    run_setup <- list()
  }
  soil_dir <- paste(asc_dir,"/soil",sep="") #soil data folder
  scod_file <- paste(soil_dir,"/",codes_prefix,gridcell,".txt",sep="") #soil codes file
  styp_file <- paste(soil_dir,"/",types_prefix,gridcell,".txt",sep="") #soil types file
  
  #soil codes filename into list
  if (file.exists(scod_file)) {
    run_setup$SOILCODES <- scod_file
  } else {
    run_setup$SOILCODES <- "nofile"
  }
  
  #soil types filename into list
  if (file.exists(scod_file)) {
    run_setup$SOILTYPES <- styp_file #assign value to list
  } else {
    run_setup$SOILCODES <- "nofile"
  }
  return(run_setup)
}


#get wth data
get_wth_data <- function(run_setup,asc_dir,gridcell,wth_root,run_type="rfd",yi=1966,yf=1993) {
  if (!is.list(run_setup)) {
    warning("run_setup has to be a list, so an empty one was created")
    run_setup <- list()
  }
  
  if (!run_type %in% c("rfd","irr")) {
    stop("run_type was mis-specified")
  }
  
  wth_dir <- paste(asc_dir,"/wth",sep="") #wth data folder
  wth_dir <- paste(wth_dir,"/",run_type,"_",gridcell,sep="")
  
  count <- 0
  for (yr in yi:yf) {
    wth_file <- paste(wth_dir,"/",wth_root,"001001",yr,".wth",sep="")
    if (file.exists(wth_file)) {count <- count+1}
  }
  
  if (count == length(yi:yf)) {
    run_setup$WTH_DIR <- wth_dir
  } else {
    run_setup$WTH <- paste(length(yi:yf)-(count)," missing files",sep="")
  }
  run_setup$WTHROOT <- wth_root
  run_setup$RUN_TYPE <- run_type
  return(run_setup)
}


#load irrigation data
load_irr_data <- function(rs_dir,rs_prefix="raw-",yi=1966,yf=1993,xy,ext=".asc",cell_ids=NA) {
  rs_list <- paste(rs_dir,"/",rs_prefix,yi:yf,ext,sep="")
  if (require(raster) & require(rgdal)) {rstk <- stack(rs_list)} #load raster
  irr_data <- as.data.frame(extract(rstk,xy))
  names(irr_data) <- paste("Y",yi:yf,sep="")
  irr_data <- cbind(CELL=cell_ids,X=xy[,1],Y=xy[,2],irr_data)
  return(irr_data)
}


#get irr data (data and sowing date)
get_irr_data <- function(irr_data,gridcell,yi=1966,yf=1993) {
  #check the data is in proper format
  if (!is.data.frame(irr_data)) {
    stop("irr_data must be a data frame")
  }
  irr_data$X <- NULL; irr_data$Y <- NULL
  #check the number of years is present
  if (length(grep("Y",names(irr_data))) != length(yi:yf)) {
    stop("number of years does not match")
  }
  
  #grab irrigated fraction data
  iratio <- as.numeric(irr_data[which(irr_data$CELL==gridcell),paste("Y",yi:yf,sep="")])
  iratio <- data.frame(YEAR=yi:yf,IRR_RATIO=iratio)
  iratio$IRR_RATIO[which(iratio$IRR_RATIO > 1)] <- 1
  return(iratio)
}

b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
# cal_dir <- paste(b_dir,"/calib",sep="")
# inputs_dir <- paste(b_dir,"/inputs",sep="")
# asc_dir <- paste(b_dir,"/inputs/ascii",sep="")
# exp_dir <- "exp-02_outputs"
# prefix <- "fcal_"
# gridcell <- 636
# ygp <- "opt"
# ipdate <- "opt"
# crop <- "gnut"
# codes_prefix="soilcodes_"
# types_prefix="soiltypes_"
# run_type <- "rfd"
# wth_root <- "ingc"
# yi=1966;yf=1993
# rs_dir <- paste(b_dir,"/irrigated_ratio",sep="")

# rs_prefix <- "raw-"
# ext <- ".asc"
# xy <- read.csv(paste(inputs_dir,"/calib-cells-selection-v6.csv",sep=""))
# xy <- cbind(x=xy$X,y=xy$Y)
# cell_ids <- read.csv(paste(inputs_dir,"/calib-cells-selection-v6.csv",sep=""))$CELL


#run configuration
run_setup <- list()
run_setup$B_DIR <- b_dir
run_setup$CAL_DIR <- paste(run_setup$B_DIR,"/calib",sep="")
run_setup$INPUTS_DIR <- paste(run_setup$B_DIR,"/inputs",sep="")
run_setup$ASC_DIR <- paste(run_setup$INPUTS_DIR,"/ascii",sep="")
run_setup$RUNS_DIR <- paste(run_setup$B_DIR,"/runs/testing",sep="")
run_setup$CROP <- "gnut"
run_setup$YEARS <- 1966:1993
run_setup$EXP_DIR <- "exp-02_outputs"
run_setup$GRID <- paste(run_setup$INPUTS_DIR,"/calib-cells-selection-v6.csv",sep="")
run_setup$PREFIX <- "fcal_"
run_setup$GRIDCELL <- 636
run_setup$YGP <- "opt"
run_setup$IPDATE <- "opt"
run_setup$CODES_PREFIX <- "soilcodes_"
run_setup$TYPES_PREFIX <- "soiltypes_"
run_setup$WTH_ROOT <- "ingc"
run_setup$IRR_RS_DIR <- paste(run_setup$B_DIR,"/irrigated_ratio",sep="")
run_setup$IRR_RS_PREFIX <- "raw-"
run_setup$IRR_RS_EXT <- ".asc"


#run model
prepare_GLAM <- function(run_setup) {
  #creating output base folder
  if (!file.exists(run_setup$RUNS_DIR)) {dir.create(run_setup$RUNS_DIR,recursive=T)}
  
  #load grid
  g_xy <- read.csv(run_setup$GRID)
  g_ids <- g_xy$CELL
  g_xy <- cbind(x=g_xy$X,y=g_xy$Y)
  
  #get irrigation data
  idata <- load_irr_data(rs_dir=run_setup$IRR_RS_DIR,rs_prefix=run_setup$IRR_RS_PREFIX,
                            yi=min(run_setup$YEARS),yf=max(run_setup$YEARS),xy=g_xy,
                            ext=run_setup$IRR_RS_EXT,cell_ids=g_ids)
  idata <- get_irr_data(irr_data=idata,gridcell=run_setup$GRIDCELL,yi=min(run_setup$YEARS),
                        yf=max(run_setup$YEARS))
  run_setup$IRATIO <- idata
  
  #create run dir
  
  run_setup$RUN_DIR <- "some thing"
  
  #get parameter set(s): if there is some irrigated fraction then grab
  #the two parameter sets run_type <- "rfd" or "irr", accordingly
  
  
  #get soil files
  
  
  #get wth files
  
  
  #copy necessary files to required folders
  
  
  #return run_setup
  
}



#run model under given configuration












