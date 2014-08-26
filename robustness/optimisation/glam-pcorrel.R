#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
stop("!")

#calculate a correlation matrix for parameters across the 100 runs performed

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
wd <- "/nfs/a101/earjr/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
calib_dir <- paste(runs_dir,"/ppe_optimisation_t4",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")

#output dir
out_dir <- paste(calib_dir,"/analysis",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#load initial conditions
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))

#create a list of 10 seeds
set.seed(2302) #fixed seed to make it replicable
seed_list <- round(runif(10, 1000, 9999),0)

#select ME and grid cells to optimise on
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[1]

#load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_orig <- read.csv(paste(mdata_dir,"/parameter_list_glam.txt",sep=""),sep="\t",header=T)

#iterate iter and seeds to load all parameter values
out_df <- data.frame()
for (iter in 1:10) {
  cat("...processing iter=",iter,"\n")
  
  for (seed in seed_list) {
    #iter <- 1
    #seed <- seed_list[1]
    cat("\n...processing seed=",seed,"\n")
    
    opt_dir <- paste(calib_dir,"/optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
    
    #loop parameters to load data
    row_out <- c(iter,seed)
    for (param in paste(param_orig$PARAM)) {
      cat("...loading param=",param,"\n")
      #param <- paste(param_orig$PARAM)[1]
      #load data
      load(file=paste(opt_dir,"/opt-",param,".RData",sep=""))
      
      #get optimal value
      opt_val <- r_list$OPTIMISATION$VALUE[which(r_list$OPTIMISATION$RMSE == min(r_list$OPTIMISATION$RMSE))]
      if (length(opt_val) > 1) {opt_val <- opt_val[ceiling(length(opt_val)/2)]}
      rm(r_list)
      
      #append value
      row_out <- c(row_out, opt_val)
    }
    row_out <- as.data.frame(t(row_out))
    names(row_out) <- c("ITER","SEED",paste(param_orig$PARAM))
    
    #append row
    out_df <- rbind(out_df, row_out)
  }
}
write.csv(out_df, paste(out_dir,"/parameter_values.csv",sep=""),row.names=F)

#calculate the correlation matrix (n=100)
all_data <- out_df[,paste(param_orig$PARAM)]
cor_all <- cor(all_data, method="pearson")
write.csv(cor_all, paste(out_dir,"/correlations_all.csv",sep=""),row.names=F)

#calculate the correlation matrix (iter > 5, n=50)
n50_data <- out_df[which(out_df$ITER > 5),paste(param_orig$PARAM)]
cor_n50 <- cor(n50_data, method="pearson")
write.csv(cor_n50, paste(out_dir,"/correlations_n50.csv",sep=""),row.names=F)

#calculate the correlation matrix (iter > 7, n=30)
n30_data <- out_df[which(out_df$ITER > 7),paste(param_orig$PARAM)]
cor_n30 <- cor(n30_data, method="pearson")
write.csv(cor_n30, paste(out_dir,"/correlations_n30.csv",sep=""),row.names=F)



