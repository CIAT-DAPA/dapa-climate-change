#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014
#stop("!")

#determine MAXILOC

################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
for(arg_i in 1:length(args)) {
  eval(parse(text=args[[arg_i]]))
}
#should have read *me_i*
#me_i <- 1

#input directories
wd <- "~/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")

#load objects (initial conditions)
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))

#select ME and its corresponding initial data
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[me_i]
xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]
loc_list <- xy_me$LOC #list of locations

#write procfile file
jfil <- paste(wd,"/scratch/maxiloc/maxiloc_",me_i,".txt",sep="")
pfil <- file(jfil,open="w")
cat(length(loc_list),"\n",file=pfil)
close(pfil)
