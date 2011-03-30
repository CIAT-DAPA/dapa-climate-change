#Julian Ramirez
#CIAT / University of Leeds

#Perform the whole anusplin fit of monthly data at 30 arc-seconds

require(raster)
require(foreign)
rm(list=ls());gc(T);gc()
source("writeDatFile.R"); source("createFitFile.R"); source("createValFile.R"); source("createPrjFile.R"); source("accuracy.R")

#Cross-validation information and general set up
#Do it by tiles and in thirds

splineFitting <- function(anuDir="C:/anu/Anuspl43/bin", stDir, rDir, oDir, nfolds=100, train.per=0.85, vn="rain", ntiles=3, unix=F) {
  #Defining units
  if (vn == "rain") {u <- "millimetres"} else {u <- "degrees"}
  
  #stDir is where station data files are
  #rDir is where latitude, longitude and altitude data files are
  #oDir is where outputs will be
  
  #Reading altitude raster (regional level)
  cat("Reading mask file \n")
  msk <- raster(paste(rDir, "/altitude.asc", sep=""))
  xt <- extent(msk)
  xt@xmin <- xt@xmin - 5
  xt@xmax <- xt@xmax + 5
  xt@ymin <- xt@ymin - 5
  xt@ymax <- xt@ymax + 5
  
  #Reading station data and selecting stations
  cat("Reading stations data \n")
  st <- read.dbf(paste(stDir,"/wc_", vn, "_stations.dbf",sep=""))
  st.reg <- st[which(st$LONG >= xt@xmin & st$LONG <= xt@xmax & st$LAT >= xt@ymin & st$LAT <= xt@ymax),]
  st.reg.10y <- st.reg[which(st.reg$NYEARS >= 10),]
  
  for (fold in 1:nfolds) {
    
    cat("Performing fold", fold, "\n")
    
    cat("Selecting train and test data \n")
    #Selecting train and test data
    train <- sample(1:nrow(st.reg.10y), nrow(st.reg.10y)*train.per)
    st.reg.10y$TRAIN_TEST <- NA
    st.reg.10y$TRAIN_TEST[train] <- "TRAIN"
    st.reg.10y$TRAIN_TEST[-train] <- "TEST"
    
    #Creating output directory
    fDir <- paste(oDir, "/fold-", fold, sep=""); if (!file.exists(fDir)) {dir.create(fDir)}
    
    #Fold specific file creation
    for (tile in 1:ntiles) {
      
      cat("Analysing tile", tile, "\n")
      
      #Read tile altitude raster
      cat("Reading tile-specific altitude raster \n")
      alt <- raster(paste(rDir, "/tile-", tile, "/altitude.asc", sep=""))
      xtt <- extent(msk)
      
      #Selecting stations
      st.sel.10y <- st[which(st.reg.10y$LONG >= xtt@xmin & st.reg.10y$LONG <= xtt@xmax & st.reg.10y$LAT >= xtt@ymin & st.reg.10y$LAT <= xtt@ymax),]
      
      #Output fold directory
      tDir <- paste(fDir, "/tile-", tile, sep=""); if (!file.exists(tDir)) {dir.create(tDir)}
      setwd(tDir)
      
      #Input data file creation
      cat("Creating input files \n")
      rFile <- writeDat(st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TRAIN"),], paste(vn, "_train.dat",sep=""))
      eFile <- writeDat(st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TEST"),], paste(vn, "_test.dat",sep=""))
      
      #Creating run file
      cat("Running \n")
      createRunFile(st.sel.10y, filename=paste(vn, "_train.dat", sep=""), variable=vn, 
      varUnits=u, ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=2, nsurf=12)
      #Running
      if (unix) {
        fw <- file(paste(vn, "_fit.sh", sep=""), open="w")
        cat("wine ", anuDir, "/splina.exe < ", vn, "fit.cmd > ", vn, "fit.log\n", sep="", file=fw)
        close(fw)
        system(paste("sh ", vn, "_fit.sh", sep=""))
      } else {
        fw <- file(paste(vn, "_fit.bat", sep=""), open="w")
        cat(anuDir, "/splina < ", vn, "fit.cmd > ", vn, "fit.log\n", sep="", file=fw)
        close(fw)
        system(paste(vn, "_fit.bat", sep=""))
      }
      
      #Creating validation run file
      cat("Evaluating \n")
      createValFile(variable=vn, filename=paste(vn, "_test.dat",sep=""), nsurf=0)
      #Running]
      if (unix) {
        fw <- file(paste(vn, "_val.sh", sep=""), open="w")
        cat("wine ", anuDir, "/lappnt.exe < ", vn, "val.cmd > ", vn, "val.log\n", sep="", file=fw)
        close(fw)
        system(paste("sh ", vn, "_val.sh", sep=""))
      } else {
        fw <- file(paste(vn, "_val.bat", sep=""), open="w")
        cat(anuDir, "/lappnt < ", vn, "val.cmd > ", vn, "val.log\n", sep="", file=fw)
        close(fw)
        system(paste(vn, "_val.bat", sep=""))
      }
      
      #Creating projection file
      cat("Projecting \n")
      createPrjFile(msk, variable=vn, nsurf=0, gridfiles=c(paste(rDir,"/tile-", tile, "/longitude.asc",sep=""),paste(rDir,"/tile-", tile,"/latitude.asc",sep=""),paste(rDir,"/tile-", tile,"/altitude.asc",sep="")))
      #Running
      if (unix) {
        fw <- file(paste(vn, "_prj.sh", sep=""), open="w")
        cat("wine ", anuDir, "/lapgrd.exe < ", vn, "prj.cmd > ", vn, "prj.log\n", sep="", file=fw)
        close(fw)
        system(paste("sh ", vn, "_prj.sh", sep=""))
      } else {
        fw <- file(paste(vn, "_prj.bat", sep=""), open="w")
        cat(anuDir, "/lapgrd < ", vn, "prj.cmd > ", vn, "prj.log\n", sep="", file=fw)
        close(fw)
        system(paste(vn, "_prj.bat", sep=""))
      }
      
      #Crossvalide data metrics
      cat("Accuracy metrics \n")
      train.data <- st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TRAIN"),]
      test.data <- st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TEST"),]
      
      acc <- accuracy(trainMx=train.data, testMx=test.data, variable=vn)
      write.csv(acc$FITTED, paste(vn, "_fitted-values.csv", sep=""), quote=F, row.names=F)
      write.csv(acc$TEST, paste(vn, "_test-values.csv", sep=""), quote=F, row.names=F)
      write.csv(acc$METRICS, paste(vn, "_metrics.csv", sep=""), quote=F, row.names=F)
    }
  }
}

