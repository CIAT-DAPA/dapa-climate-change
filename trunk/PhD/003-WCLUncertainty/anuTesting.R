#Julian Ramirez
#CIAT / University of Leeds

#Perform anusplin test runs over sub saharan africa and India to detect errors in station data

require(raster)
require(foreign)
rm(list=ls());gc(T);gc()
source("writeDatFile.R"); source("createFitFile.R"); source("createValFile.R"); source("createPrjFile.R"); source("accuracy.R")

cleansing <- function(anuDir="C:/anu/Anuspl43/bin", rDir, stDir, oDir, vn="rain", ntiles=3, round=0, unix=F) {
  #Defining units
  if (vn == "rain") {u <- "millimetres"} else {u <- "degrees"}
  cat("Reading stations \n")
  st <- read.dbf(paste(stDir,"/wc_", vn, "_stations.dbf",sep=""))
  
  #Round directory
  roDir <- paste(oDir, "/round-", round, sep=""); if (!file.exists(roDir)) {dir.create(roDir)}
  
  for (tile in 1:ntiles) {
    
    #Output cleansing round directory
    tDir <- paste(roDir, "/tile-", tile, sep=""); if (!file.exists(tDir)) {dir.create(tDir)}
    setwd(tDir)
    
    if (!file.exists(paste(vn, "-status.anu", sep=""))) {
      
      cat("Cleaning if necessary \n")
      lfil <- list.files()
      for (ff in lfil) {
        file.remove(ff)
      }
      
      #Reading mask
      cat("Reading mask file \n")
      msk <- raster(paste(rDir, "/tile-", tile, "/altitude.asc", sep=""))
      xt <- extent(msk)
      xt@xmin <- xt@xmin - 5
      xt@xmax <- xt@xmax + 5
      xt@ymin <- xt@ymin - 5
      xt@ymax <- xt@ymax + 5
      
      #Reading station data and selecting stations
      cat("Selecting stations \n")
      st.sel <- st[which(st$LONG >= xt@xmin & st$LONG <= xt@xmax & st$LAT >= xt@ymin & st$LAT <= xt@ymax),]
      st.sel.10y <- st.sel[which(st.sel$NYEARS >= 10),]
      
      #Cleansing from any -9999 value
      for (i in 9:20) {
        OKrow <- which(st.sel.10y[,i] != -9999.9)
        NArow <- which(st.sel.10y[,i] == -9999.9)
        if (length(NArow) >= 1) {
          st.sel.10y <- st.sel.10y[OKrow,]
        }
      }
      
      #Input data file creation
      cat("Creating input file \n")
      rFile <- writeDat(st.sel.10y, paste(vn, "_train.dat",sep=""))
      
      #Creating run file
      cat("Fitting surfaces \n")
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
      
      #Creating projection file
      cat("Projecting surfaces \n")
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
      
      #Here load and analyse the station deviations file
      cat("Analysing deviation results \n")
      dev.data <- read.fortran(paste(vn, ".res",sep=""), c("I7","A11","F12"))
      dev.data <- dev.data[,2:3]; names(dev.data) <- c("ID","RESIDUAL")
      top.st <- dev.data[1:5,]
      
      for (rw in 1:nrow(top.st)) {
        rmStat <- which(st.sel.10y$ID == gsub(" ", "", top.st$ID[rw]))
        if (rw == 1) {st.list <- c(rmStat)} else {st.list <- c(st.list, rmStat)}
      }
      new.st <- st.sel.10y[-st.list,]
      write.dbf(new.st, paste("wc_", vn, "_stations.dbf",sep=""))
      
      #If run was successful then erase .cov file, and zip asciigrids
      if (file.exists(paste(vn,".lis",sep=""))) {
        cat("Run was successful, collecting garbage \n")
        file.remove(paste(vn, ".cov", sep=""))
        
        if (unix) {
          for (aif in 1:12) {
            system(paste("zip", paste(vn, "_", aif, ".zip", sep=""), paste(vn, "_", aif, ".asc", sep="")))
            file.remove(paste(vn, "_", aif, ".asc", sep=""))
          }
        } else {
          for (aif in 1:12) {
            system(paste("7za", "a", paste(vn, "_", aif, ".zip", sep=""), paste(vn, "_", aif, ".asc", sep="")))
            file.remove(paste(vn, "_", aif, ".asc", sep=""))
          }
        }
      }
      
      #Accuracy metrics
      cat("Calculating accuracy metrics \n")
      acc <- accuracy(trainMx=st.sel.10y, testMx=NULL, variable=vn)
      write.csv(acc$FITTED, paste(vn, "_fitted-values.csv", sep=""), quote=F, row.names=F)
      write.csv(acc$METRICS, paste(vn, "_metrics.csv", sep=""), quote=F, row.names=F)
      
      #Writing status file
      zz <- file(paste(vn, "-status.anu", sep=""), "w")
      cat("Process finished on", date(), "\n", file=zz)
      close(zz)
    } else {
      cat("Tile already processed \n")
    }
  }
  return(roDir)
}
