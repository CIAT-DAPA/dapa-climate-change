#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
#stop("!")

#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
#2. load objects (initial conditions and yields)
#3. create a list of 100 seeds
#4. select ME and grid cells to optimise on
#5. create meteorology for selected grid cells
#6. iteratively optimise over the list of parameters (with a defined number of iterations)

################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
  eval(parse(text=args[[i]]))
}
#should have read *me_i*, *iter*, *i* (i=param order)

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
#wd <- "/nfs/a101/earjr/quest-for-robustness"
wd <- "~/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
calib_dir <- paste(runs_dir,"/ppe_optimisation_t4",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c",sep="")

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_orig <- read.csv(paste(mdata_dir,"/parameter_list_glam.txt",sep=""),sep="\t",header=T)

###
#2. load objects (initial conditions and yields)
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

###
#3. create a list of 10 seeds
set.seed(2302) #fixed seed to make it replicable
seed_list <- round(runif(10, 1000, 9999),0)

###
#4. select ME and grid cells to optimise on
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[me_i]

xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]
set.seed(2059) #randomly choose grid cells (use fixed seed to make it replicable)
xy_sel <- xy_me[sample(1:nrow(xy_me), 10, replace=F),]
row.names(xy_me) <- 1:nrow(xy_me); row.names(xy_sel) <- 1:nrow(xy_sel)

#ensure SAT is not below DUL
corr_loc <- xy_sel$LOC[which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]
corr_fac <- mean((xy_sel$SAT[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]-xy_sel$DUL[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]),na.rm=T)
xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)] <- xy_sel$DUL[which(xy_sel$LOC %in% corr_loc)] + corr_fac
xy_main$SAT[which(xy_main$LOC %in% corr_loc)] <- xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)]

#load all runs data.frame
load(paste(mdata_dir,"/glam-all_optim_runs.RData",sep=""))

####################################################################################
#all the *STEPS and *SEEDS for the given ITER & PARAM can be submitted simultaneously
#each time calibrate() needs to be run. calibrate should take ~36 min
cat("...collating output for iter=",iter," and parameter sequence i=",i,"\n")

#this i need to do somehow in the driver script so i can send the "j" value into R from it
#select seeds*steps to run
dfsel <- dfall[which(dfall$ITER == iter & dfall$PARAM_ORDER == i),]
dfsel$ITER <- NULL

#check what has already been done
for (seed in seed_list) {
  #seed <- seed_list[1]
  tdfsel <- dfsel[which(dfsel$SEED == seed),]
  tparam <- paste(dfall$PARAM_NAME[which(dfall$SEED == seed & dfall$ITER == iter & dfall$PARAM_ORDER == i & dfall$STEP == 1)])
  out_dir <- paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
  save_file <- paste(out_dir,"/opt-",tdfsel$PARAM_NAME[1],".RData",sep="")
  if (file.exists(save_file)) {dfsel <- dfsel[-which(dfsel$SEED == seed),]}
}

####### the below has to comme in a second script called summarise or something like that
#######
#determine optimal value per seed for this *iter* and *i* (for parameter "i")
for (seed in seed_list) {
  #seed <- seed_list[1]
  tdfsel <- dfsel[which(dfsel$SEED == seed),]
  tparam <- paste(dfall$PARAM_NAME[which(dfall$SEED == seed & dfall$ITER == iter & dfall$PARAM_ORDER == i & dfall$STEP == 1)])
  
  #outdir and save_file
  out_dir <- paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
  if (!file.exists(out_dir)) {dir.create(out_dir)}
  save_file <- paste(out_dir,"/opt-",tparam,".RData",sep="")
  
  if (!file.exists(save_file)) {
    #if there are rows it means the parameter has just been done (all its steps)
    #hence load and append into single object
    for (k in 1:nrow(tdfsel)) {
      #k <- 1
      load(paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,"_param-",tdfsel$PARAM_NAME[k],"_step-",tdfsel$STEP[k],"/opt-",tdfsel$PARAM_NAME[k],".RData",sep=""))
      out_row <- r_list[[1]]
      ygp_all <- r_list[[2]]
      this_cal <- r_list[[3]]
      yr_out <- r_list[[4]]
      rm(r_list)
      system(paste("rm -rf ",calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,"_param-",tdfsel$PARAM_NAME[k],"_step-",tdfsel$STEP[k],sep=""))
      
      if (k == 1) {
        out_all <- out_row
        out_raw <- ygp_all
        cal_all <- this_cal
        cal_raw <- yr_out
      } else {
        out_all <- rbind(out_all,out_row)
        out_raw <- rbind(out_raw,ygp_all)
        cal_all <- rbind(cal_all,this_cal)
        cal_raw <- rbind(cal_raw,yr_out)
      }
    }
    r_list <- list(OPTIMISATION=out_all, RAW_OPTIMISATION=out_raw, 
                   CALIBRATION=cal_all, RAW_CALIBRATION=cal_raw)
    save(list=c("r_list"),file=save_file)
  }
}



