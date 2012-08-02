#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2012

#determine the parameter set with the best skill

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
maxiter <- 15
version <- "c"
selection <- "v6"
base_exp <- 33 #change if you have done any other experiment

#run <- 1
#expID <- "10"

#eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
# maxiter <- 10
# version <- "d"
# selection <- "v4"
# base_exp <- 31 #change if you have done any other experiment

source(paste(src.dir,"/glam/glam-randomiser-functions.R",sep=""))

cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(NA,sample(1:9999,49))
#seeds <- c(NA)

expIDs <- c(base_exp:((base_exp-1)+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)

#output summary folders
sum_out <- paste(cropDir,"/calib/results_exp",sep="")
set_odir <- paste(sum_out,"/summary_exp_",runs_ref$EXPID[1],"-",runs_ref$EXPID[nrow(runs_ref)],sep="")


##############################################################################
##############################################################################
#using the skill_all data.frame, here do a K-S test from the one which presents
#the lowest median RMSE. See what is the significance of the result

for (z in 1:5) {
  cat("summarising zone",z,"...\n")
  #define folders
  z_odir <- paste(set_odir,"/z",z,"_rfd_irr",sep="")
  sum_spatial <- paste(z_odir,"/spatial",sep="")
  psel_dir <- paste(z_odir,"/parameter_selection",sep="")
  if (!file.exists(psel_dir)) {dir.create(psel_dir)}
  
  #load the skill_spatial.csv
  skill_all <- read.csv(paste(sum_spatial,"/skill_spatial.csv",sep=""))
  
  #####order runs according to performance in each skill parameter
  #####columns as follows:
  # a. percent of negative correlations should be the lowest (1 best, n worst)
  # b. mean ccoef should be the highest
  # c. median ccoef should be the highest
  # d. highest high-likelihood ccoef should be the highest
  # e. mean rmse lowest
  # f. median rmse lowest
  # g. lowest high-likelihood rmse 
  # h. mean prmse lowest
  # i. median prmse lowest
  # j. lowest high-likelihood prmse
  if (!file.exists(paste(psel_dir,"/runs_comparison.csv",sep=""))) {
    #create output data.frame
    out_data <- data.frame(EXP=names(skill_all)[5:ncol(skill_all)])
    
    sk_data <- skill_all[skill_all$METRIC == "CCOEF",]
    #a. percent negative correlations should be the lowest
    fun <- function(x) {y <- length(which(x<0)); return(y)}
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=fun)
    out_data$a <- 0
    out_data$a[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    #b. mean ccoef should be the highest
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=mean)
    out_data$b <- 0
    out_data$b[order(x,decreasing=T)] <- 1:nrow(out_data)
    
    #c. median ccoef should be the highest
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=median)
    out_data$c <- 0
    out_data$c[order(x,decreasing=T)] <- 1:nrow(out_data)
    
    #d. highest highest-likelihood ccoef
    fun <- function(x) {dp <- density(x);y <- dp$x[which(dp$y==max(dp$y))]; return(y)}
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=fun)
    out_data$d <- 0
    out_data$d[order(x,decreasing=T)] <- 1:nrow(out_data)
    
    sk_data <- skill_all[skill_all$METRIC == "RMSE",]
    #e. mean rmse lowest
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=mean)
    out_data$e <- 0
    out_data$e[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    #f. median rmse lowest
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=median)
    out_data$f <- 0
    out_data$f[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    #g. lowest high-likelihood rmse
    fun <- function(x) {dp <- density(x);y <- dp$x[which(dp$y==max(dp$y))]; return(y)}
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=fun)
    out_data$g <- 0
    out_data$g[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    sk_data <- skill_all[skill_all$METRIC == "P_RMSE",]
    #h. mean prmse lowest
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=mean)
    out_data$h <- 0
    out_data$h[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    #i. median prmse lowest
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=median)
    out_data$i <- 0
    out_data$i[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    #j. lowest high-likelihood prmse
    fun <- function(x) {dp <- density(x);y <- dp$x[which(dp$y==max(dp$y))]; return(y)}
    x <- apply(sk_data[,5:ncol(sk_data)],2,FUN=fun)
    out_data$j <- 0
    out_data$j[order(x,decreasing=F)] <- 1:nrow(out_data)
    
    #calculate total
    out_data$total <- 0
    out_data$total <- rowSums(out_data[,2:(ncol(out_data)-1)])
    
    #write out_data
    write.csv(out_data,paste(psel_dir,"/runs_comparison.csv",sep=""),quote=T,row.names=F)
  } else {
    out_data <- read.csv(paste(psel_dir,"/runs_comparison.csv",sep=""))
  }
  
  
  #plot(density(best_dist))
  #lines(density(sk_data[,"EXP.33"]))
  
  if (!file.exists(paste(psel_dir,"/runs_ks_discard.csv",sep=""))) {
    #select the single best run, based on one particular skill measure
    #a
    sk_data <- skill_all[skill_all$METRIC == "CCOEF",]
    ks_all <- data.frame(EXP=out_data$EXP)
    best_post <- out_data$EXP[which(out_data$a == min(out_data$a))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$a <- ks_selection(sk_data,best_post)$SIGNIF
    
    #b
    best_post <- out_data$EXP[which(out_data$b == min(out_data$b))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$b <- ks_selection(sk_data,best_post)$SIGNIF
    
    #c
    best_post <- out_data$EXP[which(out_data$c == min(out_data$c))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$c <- ks_selection(sk_data,best_post)$SIGNIF
    
    #d
    best_post <- out_data$EXP[which(out_data$d == min(out_data$d))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$d <- ks_selection(sk_data,best_post)$SIGNIF
    
    #e
    sk_data <- skill_all[skill_all$METRIC == "RMSE",]
    best_post <- out_data$EXP[which(out_data$e == min(out_data$e))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$e <- ks_selection(sk_data,best_post)$SIGNIF
    
    #f
    best_post <- out_data$EXP[which(out_data$f == min(out_data$f))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$f <- ks_selection(sk_data,best_post)$SIGNIF
    
    #g
    best_post <- out_data$EXP[which(out_data$g == min(out_data$g))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$g <- ks_selection(sk_data,best_post)$SIGNIF
    
    #h
    sk_data <- skill_all[skill_all$METRIC == "P_RMSE",]
    best_post <- out_data$EXP[which(out_data$h == min(out_data$h))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$h <- ks_selection(sk_data,best_post)$SIGNIF
    
    #i
    best_post <- out_data$EXP[which(out_data$i == min(out_data$i))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$i <- ks_selection(sk_data,best_post)$SIGNIF
    
    #j
    best_post <- out_data$EXP[which(out_data$j == min(out_data$j))]
    best_dist <- sk_data[,paste(best_post)]
    ks_all$j <- ks_selection(sk_data,best_post)$SIGNIF
    
    #count the proportion of skill measures that were
    fun_count <- function(x,val) {x[which(x=="NS*")] <- "NS";length(which(x==val))/length(x)}
    ks_all$RATIO_NS <- apply(ks_all[,2:11],1,fun_count,"NS")
    ks_all$RATIO_0.1 <- apply(ks_all[,2:11],1,fun_count,"*")
    ks_all$RATIO_0.05 <- apply(ks_all[,2:11],1,fun_count,"**")
    ks_all$RATIO_0.001 <- apply(ks_all[,2:11],1,fun_count,"***")
    
    #create variable ISSEL that has 1 whenever experiments are not different at a
    #significance level of p<0.05. These experiments are to be used in any further runs
    ks_all$ISSEL <- 0
    ks_all$ISSEL[which((ks_all$RATIO_NS+ks_all$RATIO_0.1) > 0.5)] <- 1
    
    write.csv(ks_all,paste(psel_dir,"/runs_ks_discard.csv",sep=""),quote=T,row.names=F)
  } else {
    ks_all <- read.csv(paste(psel_dir,"/runs_ks_discard.csv",sep=""))
  }
  
  #final runs discarding. Remove any parameter set that gives a negative spatial
  #correlation coefficient
  if (!file.exists(paste(psel_dir,"/runs_discard.csv",sep=""))) {
    skill_spat <- read.csv(paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""))
    ks_all$EXP <- as.numeric(gsub("EXP.","",ks_all$EXP))
    names(ks_all)[1] <- "EXPID"
    sel_parset <- merge(ks_all,skill_spat,by="EXPID",sort=F)
    sel_parset$ISSEL_F <- sel_parset$ISSEL
    sel_parset$ISSEL_F[which(sel_parset$CCOEF_YSD < 0)] <- 0
    sel_parset$ISSEL_F[which(sel_parset$CCOEF_Y < 0)] <- 0
    write.csv(sel_parset,paste(psel_dir,"/runs_discard.csv",sep=""),quote=T,row.names=F)
  } else {
    sel_parset <- read.csv(paste(psel_dir,"/runs_discard.csv",sep=""))
  }
}


# plot(density(sk_data[,paste(best_post)]))
# lines(density(sk_data[,"EXP.79"]))
# lines(density(sk_data[,"EXP.80"]))




