#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))


#input directories and model
bDir <- "F:/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
pDir <- paste(cDir,"/params",sep="") #parameter files

#load cell details
cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
cell <- 636
method <- "lin"

#get defaults
params <- GLAM_get_default(x=cells,cell=636,parDir=pDir)
params$glam_param.mod_mgt$ISYR <- 1966 #start year
params$glam_param.mod_mgt$IEYR <- 1993 #end year

#load list of parameters to optimise, ranges, and number of steps
opt_rules <- read.table(paste(pDir,"/optimisation-rules.txt",sep=""),sep="\t",header=T)

#extract irrigation rates
irDir <- paste(cDir,"/irrigated_ratio",sep="")
library(raster)
ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==cell)],Y=cells$Y[which(cells$CELL==cell)]))
ir_vls <- as.numeric(ir_vls)
ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1

#now the optimisation routine
optimised <- list()
optimal <- list()

####Name of the set of simulations
sim_name <- "only_rfd" #only_rfd #gj_rfd_irr

#do various iterations to test for local minima
for (itr in 1:10) {
  setwd(cDir)
  for (rw in 1:nrow(opt_rules)) {
    parname <- paste(opt_rules$param[rw])
    where <- paste(opt_rules$sect[rw])
    nstep <- opt_rules$n.steps[rw]
    
    cat("\ncalibrating",parname,"using",nstep,"steps\n")
    
    #run the optimisation routine for mixture system
    optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=nstep,
                                              cell=cell,method="lin",cropName=cropName,bDir=bDir,iter=itr,
                                              iratio=ir_vls,simset=sim_name)
    
    #run the optimisation routine for only rainfed system
    #optimised[[parname]] <- GLAM_optimise_rfd(GLAM_params=params,sect=where,param=parname,n.steps=nstep,
    #                                      cell=cell,method="lin",cropName=cropName,bDir=bDir,iter=itr,
    #                                      simset="only_rfd")
    
    
    #plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
    optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
    cat(parname,":",optimal[[parname]],"\n")
    if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
    
    #updating parameter set
    if (parname == "TO" | parname == "TB" | parname == "TM") {
      params[[where]][[paste(parname,"FLWR",sep="")]]$Value <- optimal[[parname]]
      params[[where]][[paste(parname,"PODF",sep="")]]$Value <- optimal[[parname]]
      params[[where]][[paste(parname,"LMAX",sep="")]]$Value <- optimal[[parname]]
      params[[where]][[paste(parname,"HARV",sep="")]]$Value <- optimal[[parname]]
    } else {
      params[[where]][[parname]]$Value <- optimal[[parname]]
    }
    
    #comprising everything into a single table
    out_param <- data.frame(iter=itr,param=parname,sect=where,n.step=nstep,
                            opt_val=optimal[[parname]],
                            min_rmse=min(optimised[[parname]]$RMSE),
                            max_rmse=max(optimised[[parname]]$RMSE))
    if (rw == 1) {
      out_glam <- out_param
    } else {
      out_glam <- rbind(out_glam,out_param)
    }
  }
  
  save(list=c("optimised","optimal"),file=paste(cDir,"/calib/",sim_name,"/iter-",itr,"/output.RData",sep=""))
  
  #store all iterations in one matrix
  if (itr == 1) {
    out_itr <- out_glam
  } else {
    out_itr <- rbind(out_itr,out_glam)
  }
  
}
write.csv(out_itr,paste(cDir,"/calib/",sim_name,"/calib.csv",sep=""),quote=T,row.names=F)


###############################################
#final calibration of YGP
#load the calib.csv, last iteration
cal_data <- read.csv(paste(cDir,"/calib/",sim_name,"/calib.csv",sep=""))
optimal <- cal_data[which(cal_data$iter==10),]

#update the parameter set
for (rw in 1:nrow(optimal)) {
  pname <- paste(optimal$param[rw])
  where <- paste(optimal$sect[rw])
  
  if (pname == "TB" | pname == "TO" | pname == "TM") {
    params[[where]][[paste(pname,"FLWR",sep="")]][,"Value"] <- optimal$opt_val[rw]
    params[[where]][[paste(pname,"PODF",sep="")]][,"Value"] <- optimal$opt_val[rw]
    params[[where]][[paste(pname,"LMAX",sep="")]][,"Value"] <- optimal$opt_val[rw]
    params[[where]][[paste(pname,"HARV",sep="")]][,"Value"] <- optimal$opt_val[rw]
  } else {
    params[[where]][[pname]][,"Value"] <- optimal$opt_val[rw]
  }
}


#run the optimiser for a rainfed system for YGP, 20 steps
ir_vls$IRATIO <- 0
optimal <- list()
optimised[["YGP"]] <- GLAM_optimise(GLAM_params=params,sect="glam_param.ygp",param="YGP",n.steps=20,
                                      cell=cell,method="lin",cropName=cropName,bDir=bDir,iter="final_ygp",
                                      iratio=ir_vls,simset=sim_name)
optimal[["YGP"]] <- optimised[["YGP"]]$VALUE[which(optimised[["YGP"]]$RMSE == min(optimised[["YGP"]]$RMSE))]
cat("YGP",":",optimal[["YGP"]],"\n")
if (length(optimal[["YGP"]]) > 1) {optimal[["YGP"]] <- optimal[["YGP"]][round(length(optimal[["YGP"]])/2,0)]}

save(list=c("optimised","optimal"),file=paste(cDir,"/calib/",sim_name,"/iter-final_ygp/output.RData",sep=""))





