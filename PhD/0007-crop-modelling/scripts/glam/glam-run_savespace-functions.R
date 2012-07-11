#Julian Ramirez-Villegas
#June 2012
#CIAT / CCAFS / UoL


#functions to save space in the hard drive by only keeping strictly needed files 
#from a given GLAM optimisation run. If specific runs need to be re-done then
#it is best to just re-run the model again for that particular configuration.

#function to copy key things from a given GLAM optimisation experiment and then remove 
#the remainder of saved things from the NFS
savespace <- function(exp_id,b_dir,zones=c(1:5),crop_name,crop_name_long,dump=F) {
  
  #formatting experiment id
  if (exp_id < 10) {exp_id <- paste("0",exp_id,sep="")} else {exp_id <- paste(exp_id)}
  
  #folders of inputs
  crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")
  cal_dir <- paste(crop_dir,"/calib",sep="")
  exp_dir <- paste(cal_dir,"/exp-",exp_id,sep="")
  
  #starting compression of experiment
  cat("\nstarting compression of experiment",exp_id,"\n")
  #output folders
  oxp_dir <- paste(cal_dir,"/exp-",exp_id,"_outputs",sep="")
  if (!file.exists(oxp_dir)) {dir.create(oxp_dir)}
  
  oge_dir <- paste(oxp_dir,"/general",sep="")
  if (!file.exists(oge_dir)) {dir.create(oge_dir)}
  
  ogc_dir <- paste(oxp_dir,"/gridcells",sep="")
  if (!file.exists(ogc_dir)) {dir.create(ogc_dir)}
  
  opt_dir <- paste(oxp_dir,"/optimisation",sep="")
  if (!file.exists(opt_dir)) {dir.create(opt_dir)}
  
  #output check file
  con_fil <- paste(oxp_dir,"/savespace.wsp",sep="")
  if (!file.exists(con_fil)) {
    #####
    #files that I want to keep for a given experiment: three categories
    #####
    
    #general stuff (folder 'general')
    cat("copying general run details...\n")
    #1. calib_all_cells.csv
    x <- file.copy(from=paste(exp_dir,"/calib_all_cells.csv",sep=""),to=paste(oge_dir,"/calib_all_cells.csv",sep=""),overwrite=T)
    if (!x) {stop("an error occurred while copying calib_all_cells.csv, check before proceeding")}
    
    #2. calib_results_spat folder
    x <- file.copy(from=paste(exp_dir,"/calib_results_spat",sep=""),to=oge_dir,overwrite=T,recursive=T)
    if (!x) {stop("an error occurred while copying calib_results_spat, check before proceeding")}
    
    
    #3. copy each gridcell
    #individual gridcells (folder 'gridcells')
    cat("copying cell-specific calibration...\n")
    cells <- read.csv(paste(oge_dir,"/calib_all_cells.csv",sep=""))
    for (cell in cells$CELL) {
      #cell <- cells$CELL[1]
      #cat("copying gridpoint",cell,"\n")
      
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
        
        #6. keep [cropname].out for the best run (rfd and irr)
        out_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal$YGP,"/output/",tolower(crop_name_long),".out",sep="")
        if (file.exists(out_file)) {
          x <- file.copy(from=out_file,to=paste(cell_dir,"/",tolower(crop_name_long),"_RFD.out",sep=""),overwrite=T)
          if (!x) {stop("an error occurred while copying aaa.out rfd, check before proceeding")}
        }
        
        out_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/IRR_run-",best_run,"_",optimal$YGP,"/output/",tolower(crop_name_long),".out",sep="")
        if (file.exists(out_file)) {
          x <- file.copy(from=out_file,to=paste(cell_dir,"/",tolower(crop_name_long),"_IRR.out",sep=""),overwrite=T)
          if (!x) {stop("an error occurred while copying aaa.out irr, check before proceeding")}
        }
        
        #7. keep [cropname].out for run with ygp=1
        ygp1_dir <- paste(cell_dir,"/ygp_1",sep="")
        if (!file.exists(ygp1_dir)) {dir.create(ygp1_dir)}
        
        ygp1_run <- which(optimised$YGP$VALUE == 1)
        
        out_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/RFD_run-",ygp1_run,"_",1,"/output/",tolower(crop_name_long),".out",sep="")
        if (file.exists(out_file)) {
          x <- file.copy(from=out_file,to=paste(ygp1_dir,"/",tolower(crop_name_long),"_RFD.out",sep=""),overwrite=T)
          if (!x) {stop("an error occurred while copying aaa.out for ygp=1 rfd, check before proceeding")}
        }
        
        out_file <- paste(exp_dir,"/fcal_",cell,"/iter-ygp/ygp/IRR_run-",ygp1_run,"_",1,"/output/",tolower(crop_name_long),".out",sep="")
        if (file.exists(out_file)) {
          x <- file.copy(from=out_file,to=paste(ygp1_dir,"/",tolower(crop_name_long),"_IRR.out",sep=""),overwrite=T)
          if (!x) {stop("an error occurred while copying aaa.out for ygp=1 irr, check before proceeding")}
        }
        
        rm(optimal); rm(optimised); g=gc(); rm(g)
      }
    }
    
    
    #4. whole run (folder 'optimisation')
    cat("copying zone-based optimisation runs...\n")
    for (z in zones) {
      #z <- zones[1]
      
      #1. calib.csv
      oz_dir <- paste(opt_dir,"/z",z,"_rfd_irr",sep="")
      if (!file.exists(oz_dir)) {dir.create(oz_dir)}
      
      x <- file.copy(from=paste(exp_dir,"/z",z,"_rfd_irr/calib.csv",sep=""),to=paste(oz_dir,"/",sep=""),overwrite=T)
      if (!x) {stop("an error occurred while copying calib.csv for a zone, check before proceeding")}
      
      #iteration data
      iter_list <- list.files(paste(exp_dir,"/z",z,"_rfd_irr",sep=""),pattern="iter-")
      for (itr in iter_list) {
        #2. output.RData for each iteration
        x <- file.copy(from=paste(exp_dir,"/z",z,"_rfd_irr/",itr,"/output.RData",sep=""),
                       to=paste(oz_dir,"/",itr,"_output.RData",sep=""),overwrite=T)
        if (!x) {stop("an error occurred while copying output.RData for an iteration of a given zone, check!")}
        
        #3. keep [cropname].out for best run of last optimised parameter for each iteration. 
        #   Last optimised parameter should be obtained from the output.RData of each iteration
        load(paste(oz_dir,"/",itr,"_output.RData",sep=""))
        parname <- names(optimal)[length(optimal)]
        run_num <- which(optimised[[parname]]["VALUE"] == optimal[[parname]])
        run_val <- optimised[[parname]][run_num,"VALUE"]
        
        in_file <- paste(exp_dir,"/z",z,"_rfd_irr/",itr,"/",tolower(parname),"/RFD_run-",run_num,"_",run_val,"/output/",tolower(crop_name_long),".out",sep="")
        x <- file.copy(from=in_file,to=paste(oz_dir,"/",itr,"_RFD_",tolower(crop_name_long),".out",sep=""),overwrite=T)
        if (!x) {stop("an error occurred while copying rainfed aaa.out for an iteration of a given zone, check!")}
        
        in_file <- paste(exp_dir,"/z",z,"_rfd_irr/",itr,"/",tolower(parname),"/IRR_run-",run_num,"_",run_val,"/output/",tolower(crop_name_long),".out",sep="")
        x <- file.copy(from=in_file,to=paste(oz_dir,"/",itr,"_IRR_",tolower(crop_name_long),".out",sep=""),overwrite=T)
        if (!x) {stop("an error occurred while copying rainfed aaa.out for an iteration of a given zone, check!")}
      }
    }
    
    #writing control file
    zz <- file(con_fil,"w")
    cat("workspace saved on ",date(),sep="",file=zz)
    close(zz)
    
    if (dump) {
      cat("dump all previous run data...\n")
      setwd(cal_dir)
      system(paste("rm -rf ",exp_dir,sep=""))
    } else {
      cat("no dumping to be done!\n")
    }
    
  } else {
    cat("experiment has been processed before")
  }
  return("done!")
}


