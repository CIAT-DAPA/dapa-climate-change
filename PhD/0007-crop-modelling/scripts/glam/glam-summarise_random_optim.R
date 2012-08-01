#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#summarise and make charts of randomized optimizer runs

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
runs_ref2 <- expand.grid(SID=runs_ref$SID,RUN=1:5)
runs_ref <- merge(runs_ref2,runs_ref,by="SID",all=T,sort=F)

# #dodgy, need to remove the below. this is for testing purposes
# runs_ref <- runs_ref[1:145,]
# seeds <- unique(runs_ref$SEED)
# #dodgy, need to remove the above this is for testing purposes

#load the list of parameters that have been optimised
opt_list <- read.table(paste(cropDir,"/params/optimisation-rules.txt",sep=""),header=T,sep="\t")
par_list <- opt_list$param

#output summary folders
sum_out <- paste(cropDir,"/calib/results_exp",sep="")
if (!file.exists(sum_out)) {dir.create(sum_out)}

set_odir <- paste(sum_out,"/summary_exp_",runs_ref$EXPID[1],"-",runs_ref$EXPID[nrow(runs_ref)],sep="")
if (!file.exists(set_odir)) {dir.create(set_odir)}


#for a given zone
zones <- unique(runs_ref$RUN)

for (z in zones) {
  #z <- zones[1]
  cat("summarising zone",z,"...\n")
  
  #output zone dir
  z_odir <- paste(set_odir,"/z",z,"_rfd_irr",sep="")
  if (!file.exists(z_odir)) {dir.create(z_odir)}
  
  if (!file.exists(paste(z_odir,"/plot_data.RData",sep=""))) {
    #output list
    plot_list <- list()
    pdfs_list <- list()
    #here loop params
    for (pid in 1:length(par_list)) {
      param <- paste(par_list[pid])
      par_res <- data.frame()
      sctr <- 1
      #load parameter response of all seeds
      for (seed in seeds) {
        #seed <- seeds[2]
        if (is.na(seed)) {
          idexp <- runs_ref$EXPID[which(is.na(runs_ref$SEED))][1]
        } else {
          idexp <- runs_ref$EXPID[which(runs_ref$SEED == seed)][1]
        }
        
        #load parameter response from output.RData in last iteration of a given seed
        r_dir <- paste(cropDir,"/calib/exp-",idexp,"_outputs/optimisation/z",z,"_rfd_irr",sep="")
        load(paste(r_dir,"/iter-",maxiter,"_output.RData",sep=""))
        
        #get optimal value
        this_opt <- optimal[[param]]
        
        #get response curve
        this_res <- optimised[[param]]
        names(this_res) <- c("VALUE",paste("S.",seed,sep=""))
        
        #bind into output data.frame
        if (sctr == 1) {
          par_res <- rbind(par_res,this_res)
          par_res <- cbind(PARAM=param,par_res)
          pdf_res <- data.frame(PARAM=param,SEED=paste("S.",seed,sep=""),VALUE=this_opt)
        } else {
          par_res <- merge(par_res,this_res,by="VALUE",all=T,sort=F)
          pdf_res <- rbind(pdf_res,data.frame(PARAM=param,SEED=paste("S.",seed,sep=""),VALUE=this_opt))
        }
        sctr <- sctr+1
      }
      
      #put the data into the plotting list
      plot_list[[param]] <- par_res
      pdfs_list[[param]] <- pdf_res
    }
    
    #response curves data
    plot_data <- do.call("rbind",plot_list)
    row.names(plot_data) <- 1:nrow(plot_data)
    write.csv(plot_data,paste(z_odir,"/plot_data.csv",sep=""),row.names=F,quote=T)
    
    #pdfs data
    pdfs_data <- do.call("rbind",pdfs_list)
    row.names(pdfs_data) <- 1:nrow(pdfs_data)
    write.csv(pdfs_data,paste(z_odir,"/parameter_values.csv",sep=""),row.names=F,quote=T)
    
    #save plot_list into a summary folder and create a folder for plots
    save(list=c("plot_list","pdfs_data"),file=paste(z_odir,"/plot_data.RData",sep=""))
    
  } else {
    load(paste(z_odir,"/plot_data.RData",sep=""))
  }
  
  
  
  ##########here do all the plots
  #plot each parameter response in a different graph with all seeds
  pr_odir <- paste(z_odir,"/param_response_curves",sep="")
  if (!file.exists(pr_odir)) {dir.create(pr_odir)}
  
  pf_odir <- paste(z_odir,"/parameter_pdfs",sep="")
  if (!file.exists(pf_odir)) {dir.create(pf_odir)}
  
  for (i in 1:length(plot_list)) {
    x <- plot_list[[i]]
    param <- names(plot_list)[i]
    
    ylims <- c(min(x[,3:ncol(x)]),max(x[,3:ncol(x)]))
    fig_name <- paste(pr_odir,"/",tolower(param),".tif",sep="")
    
    #loop through columns to make the plot
    for (cid in 3:ncol(x)) {
      if (cid == 3) {
        tiff(fig_name,res=300,compression="lzw",height=1000,width=1250,pointsize=8)
        par(mar=c(3,3,2,1))
        plot(x$VALUE,x[,cid],ty="l",ylim=ylims,
             main=paste(param,sep=""),lwd=0.75,col="grey 50",
             xlab="Parameter value",ylab="RMSE (kg/ha)")
      } else {
        lines(x$VALUE,x[,cid],ty="l",lwd=0.75,col="grey 50")
      }
    }
    grid(nx=10,ny=10)
    dev.off()
    
    #make density plot
    dp <- density(pdfs_list[[param]][,"VALUE"])
    dp$y <- dp$y/max(dp$y)
    pf_name <- paste(pf_odir,"/",tolower(param),".tif",sep="")
    tiff(pf_name,res=300,compression="lzw",height=1000,width=1250,pointsize=8)
    par(mar=c(5,5,2,1))
    plot(dp$x,dp$y,ty="l",ylim=c(0,1),
         main=paste(param,sep=""),lwd=0.75,col="black",
         xlab="Parameter value",ylab="Normalised PDF")
    grid(nx=10,ny=10)
    dev.off()
  }
  
  
  ###
  #seed response curves
  sd_odir <- paste(z_odir,"/seeds_response_curves",sep="")
  if (!file.exists(sd_odir)) {dir.create(sd_odir)}
  
  #plot a multi-panel figure with all the optimised parameters for each seed
  for (seed in seeds) {
    #seed <- seeds[1]
    if (is.na(seed)) {
      idexp <- runs_ref$EXPID[which(is.na(runs_ref$SEED))][1]
    } else {
      idexp <- runs_ref$EXPID[which(runs_ref$SEED == seed)][1]
    }
    
    #number of panels should be the number of parameters
    npan <- nrow(opt_list)
    nc_p <- round(npan/5,0)
    
    #windows()
    fig_name <- paste(sd_odir,"/exp-",idexp,".tif",sep="")
    tiff(fig_name,res=300,compression="lzw",height=1600,width=2560,pointsize=8)
    par(mfrow=c(5,nc_p),mar=c(2.5,3,2,0.5))
    
    for (pid in 1:length(par_list)) {
      param <- paste(par_list[pid])
      seed_col <- paste("S.",seed,sep="")
      x <- plot_list[[param]]
      
      plot(x$VALUE,x[,seed_col],ty="l",main=paste(param,sep=""),lwd=0.75,ylab="RMSE (kg/ha)")
      grid()
    }
    dev.off()
  }
  
}






