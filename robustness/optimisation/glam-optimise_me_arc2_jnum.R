#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
#stop("!")

#determine MAXJNUM

################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
for(arg_i in 1:length(args)) {
  eval(parse(text=args[[arg_i]]))
}
#should have read *me_i*, *iter*, *i* (param order) and *j* (step in parameter)
#iter <- 1; i <- 1

#input directories
wd <- "~/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")

#load all runs data.frame
load(paste(mdata_dir,"/glam-all_optim_runs.RData",sep=""))

#this i need to do somehow in the driver script so i can send the "j" value into R from it
#select seeds*steps to run
dfsel <- dfall[which(dfall$ITER == iter & dfall$PARAM_ORDER == i),]

#write procfile file
jfil <- paste(wd,"/scratch/procfiles/maxjnum_",iter,"_",i,".txt",sep="")
pfil <- file(jfil,open="w")
cat(nrow(dfsel),"\n",file=pfil)
close(pfil)


