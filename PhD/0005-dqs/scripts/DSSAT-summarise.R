#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/DSSAT-functions.R",sep=""))

#base dir
bd <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"

#config
experiments <- createExpMat(bd)[11:19,]
for (i in 1:nrow(experiments)) {
  ty <- experiments$TYPE[i]; va <- experiments$VAR[i]; sc <- experiments$SCALE[i] #get data from matrix
  cat("\n")
  cat("processing",paste(ty),"/",paste(va),"/",paste(sc),"\n")
  #list folders and loop to get what we need
  sp_folder <- paste(bd,"/GJ-weather/shuf-pert/",ty,"_",va,"_",sc,sep="")
  f_list <- list.files(sp_folder); f_list <- f_list[grep("s-",f_list)]
  
  for (f_name in f_list) {
    cat("reading in experiment",f_name,"\n")
    PSDataFolder <- paste(sp_folder,"/",f_name,sep="") #data folder
    opt <- read.csv(paste(PSDataFolder,"/optimisation.csv",sep="")) #load optimisation curve
    tse <- read.csv(paste(PSDataFolder,"/timeseries.csv",sep="")) #load time series
    best_slpf <- opt$SLPF[which(opt$RMSE == min(opt$RMSE,na.rm=T))] #get best slpf
    if (length(best_slpf)>1) { #get highest best slpf in the case there is more than 1
      best_slpf <- max(best_slpf)
    }
    #get metrics for best slpf
    best_rmse <- opt$RMSE[which(opt$SLPF==best_slpf)]
    best_corr <- opt$CORR[which(opt$SLPF==best_slpf)]
    best_tser <- tse[which(tse$SLPF==best_slpf),]
    if (ty == "s") { #get experimental details
      p <- NA
      s <- as.numeric(gsub("s-","",f_name))
    } else {
      p <- as.numeric(gsub("p-","",strsplit(f_name,"_")[[1]][1]))
      s <- as.numeric(gsub("s-","",strsplit(f_name,"_")[[1]][2]))
    }
    #get the performance row
    per_row <- data.frame(TYPE=ty,VAR=va,SCALE=sc,P=p,SEED=s,SLPF=best_slpf,RMSE=best_rmse,CORR=best_corr)
    #get the best time series
    best_tse <- data.frame(TYPE=ty,VAR=va,SCALE=sc,P=p,SEED=s,SLPF=best_slpf,
                           YEAR=best_tser$YEAR,HWAH=best_tser$HWAH,OBYL=best_tser$OBYL)
    if (f_name==f_list[1]) { #summarise
      performance <- per_row
      timeseries <- best_tse
    } else {
      performance <- rbind(performance,per_row)
      timeseries <- rbind(timeseries,best_tse)
    }
  }
  
  write.csv(performance,paste(sp_folder,"/performance.csv",sep=""),row.names=F,quote=F)
  write.csv(timeseries,paste(sp_folder,"/timeseries.csv",sep=""),row.names=F,quote=F)
  
  rm(performance);rm(timeseries);g=gc();rm(g)
}
