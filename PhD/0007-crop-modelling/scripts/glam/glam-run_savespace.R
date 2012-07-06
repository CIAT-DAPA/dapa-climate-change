#Julian Ramirez-Villegas
#June 2012
#CIAT / CCAFS / UoL


#functions to save space in the hard drive by only keeping strictly needed files 
#from a given GLAM optimisation run. If specific runs need to be re-done then
#it is best to just re-run the model again for that particular configuration.

#these functions should be run after a given optimisation run is completed and fully summarised

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"


#eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#b_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"

#details of crop and base folder of runs and data
crop_name <- "gnut"
crop_name_long <- "groundnut"
zones <- c(1:5)
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")



#details of experiment i want to get
exp_id <- 2
if (exp_id < 10) {exp_id <- paste("0",exp_id,sep="")} else {exp_id <- paste(exp_id)}

#folders
cal_dir <- paste(crop_dir,"/calib",sep="")
exp_dir <- paste(cal_dir,"/exp-",exp_id,sep="")

#output folders
oxp_dir <- paste(cal_dir,"/exp-",exp_id,"_outputs",sep="")
if (!file.exists(oxp_dir)) {dir.create(oxp_dir)}

oge_dir <- paste(oxp_dir,"/general",sep="")
if (!file.exists(oge_dir)) {dir.create(oge_dir)}

ogc_dir <- paste(oxp_dir,"/gridcells",sep="")
if (!file.exists(ogc_dir)) {dir.create(ogc_dir)}

opt_dir <- paste(oxp_dir,"/optimisation",sep="")
if (!file.exists(opt_dir)) {dir.create(opt_dir)}


#####
#files that I want to keep for a given experiment:
#three categories
#####

#general stuff (folder 'general')
#1. calib_all_cells.csv
x <- file.copy(from=paste(exp_dir,"/calib_all_cells.csv",sep=""),to=paste(oge_dir,"/calib_all_cells.csv",sep=""),overwrite=T)
if (!x) {stop("an error occurred while copying calib_all_cells.csv, check before proceeding")}

#2. calib_results_spat folder
x <- file.copy(from=paste(exp_dir,"/calib_results_spat",sep=""),to=oge_dir,overwrite=T,recursive=T)
if (!x) {stop("an error occurred while copying calib_results_spat, check before proceeding")}

#3. copy each gridcell
#individual gridcells (folder 'gridcells')
cells <- read.csv(paste(oge_dir,"/calib_all_cells.csv",sep=""))
for (cell in cells$CELL) {
  #cell <- cells$CELL[1]
  cat("copying gridpoint",cell,"\n")
  
  cell_dir <- paste(ogc_dir,"/fcal_",cell,sep="")
  if (!file.exists(cell_dir)) {dir.create(cell_dir)}
  
  if (!file.exists(paste(cell_dir,"/",tolower(crop_name_long),".out",sep=""))) {
    #1. ygp optimisation (output.RData) for each gridcell
    x <- file.copy(from=paste(exp_dir,"/fcal_",cell,"/iter-ygp/output.RData",sep=""),
                   to=paste(cell_dir,"/ygp.RData",sep=""),overwrite=T)
    if (!x) {stop("an error occurred while copying iter-ygp/output.RData, check before proceeding")}
    
    #2. ipdate optimisation (output.RData) for each gridcell
    x <- file.copy(from=paste(exp_dir,"/fcal_",cell,"/iter-ipdate/output.RData",sep=""),
                   to=paste(cell_dir,"/ipdate.RData",sep=""),overwrite=T)
    if (!x) {stop("an error occurred while copying iter-ipdate/output.RData, check before proceeding")}
    
    #3. sowing date file for each gridcell (./optim_ipdate/opt_fcal_[aaa].txt), only if exists
    sow_file <- paste(exp_dir,"/optim_ipdate/opt_fcal_",cell,".txt",sep="")
    if (file.exists(sow_file)) {
      x <- file.copy(from=sow_file,to=cell_dir,overwrite=T)
      if (!x) {stop("an error occurred while copying optim_ipdate/opt_fcal_[aaa].txt, check before proceeding")}
    }
    
    #4. parameter file for each gridcell and best run (optimal ygp with optimal ipdate)
    load(paste(cell_dir,"/ygp.RData",sep=""))
    best_run <- which(optimised$YGP$VALUE == optimal$YGP)
    
    par_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal$YGP,"/glam-r2-param-",tolower(crop_name),"-run-rfd.txt",sep="")
    if (file.exists(par_file)) {
      x <- file.copy(from=par_file,to=cell_dir,overwrite=T)
      if (!x) {stop("an error occurred while copying rainfed parameter file, check before proceeding")}
    }
    
    par_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal$YGP,"/glam-r2-param-",tolower(crop_name),"-run.txt",sep="")
    if (file.exists(par_file)) {
      x <- file.copy(from=par_file,to=cell_dir,overwrite=T)
      if (!x) {stop("an error occurred while copying unique parameter file, check before proceeding")}
    }
    
    par_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/IRR_run-",best_run,"_",optimal$YGP,"/glam-r2-param-",tolower(crop_name),"-run.txt",sep="")
    if (file.exists(par_file)) {
      x <- file.copy(from=par_file,to=cell_dir,overwrite=T)
      if (!x) {stop("an error occurred while copying irrigated parameter file, check before proceeding")}
    }
    
    #5. filenames file for each gridcell and best run (i.e. best YGP value)
    fnm_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal$YGP,"/filenames-",tolower(crop_name),"-run.txt",sep="")
    x <- file.copy(from=fnm_file,to=cell_dir,overwrite=T)
    if (!x) {stop("an error occurred while copying filenames-aaa-run.txt, check before proceeding")}
    
    #6. keep [cropname].out for the best run
    out_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal$YGP,"/output/",tolower(crop_name_long),".out",sep="")
    x <- file.copy(from=out_file,to=cell_dir,overwrite=T)
    if (!x) {stop("an error occurred while copying aaa.out, check before proceeding")}
    rm(optimal); rm(optimised); g=gc(); rm(g)
  }
}


#4. whole run (folder 'optimisation')


#1. calib.csv
#2. output.RData for each iteration
#3. keep [cropname].out for best run of last optimised parameter for each iteration. 
#   Last optimised parameter should be obtained from the output.RData of each iteration









