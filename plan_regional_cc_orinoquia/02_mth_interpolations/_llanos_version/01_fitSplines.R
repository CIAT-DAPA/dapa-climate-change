#Julian Ramirez
#CIAT / University of Leeds

# setwd("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/00-scripts/02_mth_interpolations/_llanos_version")
# anuDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/00-scripts/02_mth_interpolations/anu/Anuspl43/bin"
# stDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/stations-averages"
# rDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/_region"
# oDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/outputs/rhum"
# train.per <- 0.70
# vn <- "rhum"
# tile <- 1
# ntiles <- 1
# suffix <-"lla"
# nfolds <- 25
# source("01_fitSplines.R")

require(raster)
require(foreign)
#rm(list=ls());gc(T);gc()
source("writeDatFile.R"); source("createFitFile.R"); source("createValFile.R"); source("createPrjFile.R"); source("accuracy.R")

#Cross-validation information and general set up
#Do it by tiles and in thirds

splineFitting <- function(anuDir="C:/Users/jardila/Desktop/monthly-interpolations/anu/Anuspl43/bin", stDir, rDir, oDir, nfolds=25, train.per=0.85, vn="rain", ntiles=1, unix=F, suffix="some") {
  
  #Defining units
  if (vn == "rain") {u <- "millimetres"} else if (vn == "rhum") {u <- "undefined"} else {u <- "degrees"}
  
  #stDir is where station data files are
  #rDir is where latitude, longitude and altitude data files are
  #oDir is where outputs will be
  
  #Create output directory
  if (!file.exists(oDir)) {dir.create(oDir, recursive = TRUE)}
  
  if (!file.exists(paste0(oDir, "\tmp"))) {dir.create(paste0(oDir, "\tmp"), recursive = TRUE)}
  rasterOptions(tmpdir= paste0(oDir, "\tmp"))
  
  #Reading altitude raster (regional level)
  cat("Reading mask file \n")
  msk <- raster(paste(rDir, "/alt-prj-",suffix, ".asc", sep=""))
  xt <- extent(msk)
  xt@xmin <- xt@xmin #- 75
  xt@xmax <- xt@xmax #- 71
  xt@ymin <- xt@ymin #+ 1
  xt@ymax <- xt@ymax #+ 5
  rm(msk); g=gc()
  
  #Reading station data and selecting stations
  cat("Reading stations data \n")
  st <- read.csv(paste(stDir,"/", vn, "_", suffix,".csv",sep=""))
  st.reg <- st[which(st$LONG >= xt@xmin & st$LONG <= xt@xmax & st$LAT >= xt@ymin & st$LAT <= xt@ymax),]
  st.reg.10y <- st.reg[which(st.reg$NYEARS >= 15),]
  
  #Cleansing from any -9999 value
  for (i in 10:21) {
    OKrow <- which(st.reg.10y[,i] != -9999.9)
    NArow <- which(st.reg.10y[,i] == -9999.9)
    if (length(NArow) >= 1) {
      st.reg.10y <- st.reg.10y[OKrow,]
    }
  }
  
  #Cross-validated folds
  for (fold in 1:nfolds) {
  
    cat("Performing fold", fold, "\n")
    
    cat("Selecting train and test data \n")
    #Selecting train and test data
    train <- sample(1:nrow(st.reg.10y), nrow(st.reg.10y)*train.per)
    st.reg.10y$TRAIN_TEST <- NA
    st.reg.10y$TRAIN_TEST[train] <- "TRAIN"
    st.reg.10y$TRAIN_TEST[-train] <- "TEST"
    
    #Creating output directory and writing regression file there (if it does not exist)
    fDir <- paste(oDir, "/fold-", fold, sep=""); if (!file.exists(fDir)) {dir.create(fDir,recursive = TRUE)}
    if (!file.exists(paste(fDir, "/", vn, "_", suffix,".csv", sep=""))) {
      write.csv(st.reg.10y, paste(fDir, "/", vn, "_", suffix,".csv", sep=""))
    } else {
      st.reg.10y <- read.csv(paste(fDir, "/", vn, "_", suffix,".csv", sep=""))
    }
    
    #Fold specific file creation
    for (tile in 1:ntiles) {
      
      cat("Analysing tile", tile, "\n")
      
      #Output fold directory
      tDir <- paste(fDir, "/tile-", tile, sep=""); if (!file.exists(tDir)) {dir.create(tDir,recursive=TRUE)}
      setwd(tDir)
      
      if (!file.exists(paste(vn, "-status.anu", sep=""))) {
        
        #Cleaning the folder if the status.anu file does not exist
        cat("Cleaning if necessary \n")
        lfil <- list.files()
        for (ff in lfil) {
          file.remove(ff)
        }
        
        #Read tile altitude raster
        cat("Reading tile-specific altitude raster \n")
        alt <- raster(paste(rDir, "/tile-", tile, "/altitude.asc", sep=""))
        xtt <- extent(alt)
        
        #Selecting stations
        st.sel.10y <- st.reg.10y[which(st.reg.10y$LONG >= xtt@xmin & st.reg.10y$LONG <= xtt@xmax & st.reg.10y$LAT >= xtt@ymin & st.reg.10y$LAT <= xtt@ymax),]
        
        #Input data file creation
        cat("Creating input files \n")
        rFile <- writeDat(st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TRAIN"),], paste(vn, "_train.dat",sep=""))
        eFile <- writeDat(st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TEST"),], paste(vn, "_test.dat",sep=""))
        
        #Creating run file
        cat("Running \n")
        createRunFile(st.sel.10y, filename=paste(vn, "_train.dat", sep=""), variable=vn, 
                      varUnits=u, ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=3, nsurf=12)
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
        createPrjFile(alt, variable=vn, nsurf=0, gridfiles=c(paste(rDir,"/tile-", tile, "/longitude.asc",sep=""),paste(rDir,"/tile-", tile,"/latitude.asc",sep=""),paste(rDir,"/tile-", tile,"/altitude.asc",sep="")))
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
        
       # If run was successful then erase .cov file, and zip asciigrids
#         if (file.exists(paste(vn,".lis",sep="")) & file.exists(paste(vn, ".out", sep=""))) {
#           cat("Run was successful, collecting garbage \n")
#           file.remove(paste(vn, ".cov", sep=""))
#           
#            if (unix) {
#            for (aif in 1:12) {
#               system(paste("zip", paste(vn, "_", aif, ".zip", sep=""), paste(vn, "_", aif, ".asc", sep="")))
#               file.remove(paste(vn, "_", aif, ".asc", sep=""))
#              }
#           } else {
#              for (aif in 1:12) {
#                system(paste("7z", "a", paste(vn, "_", aif, ".zip", sep=""), paste(vn, "_", aif, ".asc", sep="")))
#                file.remove(paste(vn, "_", aif, ".asc", sep=""))
#              }
#            }
#         }
#         
        #Crossvalide data metrics
        cat("Accuracy metrics \n")
        train.data <- st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TRAIN"),]
        test.data <- st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TEST"),]
        
        acc <- accuracy(trainMx=train.data, testMx=test.data, variable=vn)
        write.csv(acc$FITTED, paste(vn, "_fitted-values.csv", sep=""), quote=F, row.names=F)
        write.csv(acc$TEST, paste(vn, "_test-values.csv", sep=""), quote=F, row.names=F)
        write.csv(acc$METRICS, paste(vn, "_metrics.csv", sep=""), quote=F, row.names=F)
        
        # Writing status file
        zz <- file(paste(vn, "-status.anu", sep=""), "w")
        cat("Process finished on", date(), "\n", file=zz)
        close(zz) 
        

        #Statistical consistency
        
#         a <- c()
#         
#         for (i in 1:25){
#           b <- read.csv(paste(oDir, "/fold-", i, "/tile-1/", vn, "_metrics.csv", sep=""))
#           a <- rbind(a, b)
#         }
#         write.csv(a, paste(vn, "_all_metrics.csv", sep=""), row.names=F)

      } else {
        cat("Tile already processed \n")
        removeTmpFiles(h=0.0025)
      }
    }
  }
}


otp <- splineFitting(anuDir, stDir, rDir, oDir, nfolds, train.per, vn, ntiles, unix=F, suffix)




