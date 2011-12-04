#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/EcoCrop-gnut_ps-functions.R",sep=""))

#base dir
bd <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT"

#function to get the summarised data and plot the results in boxplot
shuffleSummary <- function(bDir) {
  experiments <- createExperiments(bDir)
  for (i in 1:nrow(experiments)) {
    cat("processing",paste(ty),"/",paste(va),"/",paste(sc),"\n")
    ty <- experiments$TYPE[i]; va <- experiments$VAR[i]; sc <- experiments$SCALE[i] #get data from matrix
    sp_folder <- paste(bDir,"/shuffle-perturb/climate/",va,"_",ty,"_",sc,sep="")
    performance <- read.csv(paste(sp_folder,"/performance.csv",sep=""))
    if (i==1) {
      perf <- performance
    } else {
      perf <- rbind(perf,performance)
    }
  }
  write.csv(perf,paste(bDir,"/shuffle-perturb_results/s-performance.csv",sep=""),quote=F,row.names=F)
}

#read in files again to avoid re-running the above
perf <- read.csv(paste(bd,"/shuffle-perturb_results/s-performance.csv",sep=""))

#here you need to plot the stuff


####################################################################################
####################################################################################
#function to summarise experiments
summariseExperiments <- function(bDir) {
  experiments <- createExperiments(bDir) #change when perturbed runs are ready
  for (i in 1:nrow(experiments)) {
    ty <- experiments$TYPE[i]; va <- experiments$VAR[i]; sc <- experiments$SCALE[i] #get data from matrix
    cat("\n")
    cat("processing",paste(ty),"/",paste(va),"/",paste(sc),"\n")
    #list folders and loop to get what we need
    sp_folder <- paste(bDir,"/shuffle-perturb/climate/",va,"_",ty,"_",sc,sep="")
    if (!file.exists(paste(sp_folder,"/timeseries.csv",sep=""))) {
      f_list <- list.files(sp_folder); f_list <- f_list[grep("s-",f_list)]
      
      for (f_name in f_list) {
        if (ty == "s") { #get experimental details
          p <- NA
          s <- as.numeric(gsub(paste(va,"_s-",sep=""),"",f_name))
        } else {
          p <- as.numeric(gsub("p-","",strsplit(f_name,"_")[[1]][2]))
          s <- as.numeric(gsub("s-","",strsplit(f_name,"_")[[1]][3]))
        }
        cat("reading in experiment s =",s,"/ p =",p,"\n")
        PSDataFolder <- paste(sp_folder,"/",f_name,sep="") #data folder
        acc <- read.csv(paste(PSDataFolder,"/accuracy-metrics.csv",sep="")) #load optimisation curve
        eva <- read.csv(paste(PSDataFolder,"/evaluation.csv",sep="")) #load time series
        
        #get the performance row
        per_row <- data.frame(TYPE=ty,VAR=va,SCALE=sc,P=p,SEED=s,
                              RMSE.TEST=acc$TEST.ERROR,OR.TEST=acc$TEST.OMISSION.RATE,
                              RMSE.TRAIN=acc$TRAIN.ERROR,OR.TRAIN=acc$TRAIN.OMISSION.RATE,
                              TPR=eva$TPR,FPR=eva$FPR,TNR=eva$TNR)
        if (f_name==f_list[1]) { #summarise
          performance <- per_row
        } else {
          performance <- rbind(performance,per_row)
        }
      }
      
      write.csv(performance,paste(sp_folder,"/performance.csv",sep=""),row.names=F,quote=F)
      rm(performance);rm(timeseries);g=gc();rm(g)
    }
  }
}
