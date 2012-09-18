#Julian Ramirez
#eejarv@leeds.ac.uk
#Sep 2012

#This R script computes thes sensitivity of EcoCrop suitability
#index based on a set of parameters, varying a number of them

require(rgdal)
require(raster)

#calculating the constraints implies simply changing the values of certain
#model parameters at a given value
consCalc <- function(climPath='', Gmin=90,Gmax=90,Tkmp=0,Tmin=10,Topmin=16,
                     Topmax=25,Tmax=35,Rmin=150,Ropmin=300,Ropmax=400,Rmax=600,
                     outfolder='', cropname='',ext=".asc",sensTable) {
  
  #check for type in sensTable
  if (class(sensTable) != "list") {
    stop("sensTable has to be a list")
  }
  
  #check consistency of list
  if (length(sensTable) != 8) {
    stop("All parameters need to be specified in the table, so length must be 8")
  }
  
  if (!"Tmin" %in% names(sensTable)) {stop("Tmin must be specified in sensTable")}
  if (!"Topmin" %in% names(sensTable)) {stop("Topmin must be specified in sensTable")}
  if (!"Topmax" %in% names(sensTable)) {stop("Topmax must be specified in sensTable")}
  if (!"Tmax" %in% names(sensTable)) {stop("Tmax must be specified in sensTable")}
  if (!"Rmin" %in% names(sensTable)) {stop("Rmin must be specified in sensTable")}
  if (!"Ropmin" %in% names(sensTable)) {stop("Ropmin must be specified in sensTable")}
  if (!"Ropmax" %in% names(sensTable)) {stop("Ropmax must be specified in sensTable")}
  if (!"Rmax" %in% names(sensTable)) {stop("Rmax must be specified in sensTable")}
  
  #################################################################
  #checking for model parameter consistency
  
  ### Tmin
  if (tolower(sensTable$Tmin$TYPE) == "p") {
    newval <- Tmin + (sensTable$Tmin$VALUE/100)*Tmin
  } else if (tolower(sensTable$Tmin$TYPE) == "a") {
    newval <- Tmin + sensTable$Tmin$VALUE
  } else if (tolower(sensTable$Tmin$TYPE) == "f") {
    if (sensTable$Tmin$VALUE > 1) {
      warning("Tmin change type set as f, but above 1, dividing by 100")
      sensTable$Tmin$VALUE <- sensTable$Tmin$VALUE/100
    }
    newval <- Tmin + sensTable$Tmin$VALUE*Tmin
  }
  
  if (newval  >= Topmin) {
    warning("New Tmin value > Topmin, correcting")
    perdif <- abs(Topmin-Tmin)/Tmin*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Tmin$VALUE <- newdif
  }
  
  ### Topmin
  if (tolower(sensTable$Topmin$TYPE) == "p") {
    newval <- Topmin + (sensTable$Topmin$VALUE/100)*Topmin
  } else if (tolower(sensTable$Topmin$TYPE) == "a") {
    newval <- Topmin + sensTable$Topmin$VALUE
  } else if (tolower(sensTable$Topmin$TYPE) == "f") {
    if (sensTable$Topmin$VALUE > 1) {
      warning("Topmin change type set as f, but above 1, dividing by 100")
      sensTable$Topmin$VALUE <- sensTable$Topmin$VALUE/100
    }
    newval <- Topmin + sensTable$Topmin$VALUE*Topmin
  }
  
  if (newval  >= Topmax) {
    warning("New Topmin value > Topmax, correcting")
    perdif <- abs(Topmax-Topmin)/Topmin*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Topmin$VALUE <- newdif
  }
  
  ### Topmax
  if (tolower(sensTable$Topmax$TYPE) == "p") {
    newval <- Topmax + (sensTable$Topmax$VALUE/100)*Topmax
  } else if (tolower(sensTable$Topmax$TYPE) == "a") {
    newval <- Topmax + sensTable$Topmax$VALUE
  } else if (tolower(sensTable$Topmax$TYPE) == "f") {
    if (sensTable$Topmax$VALUE > 1) {
      warning("Topmax change type set as f, but above 1, dividing by 100")
      sensTable$Topmax$VALUE <- sensTable$Topmax$VALUE/100
    }
    newval <- Topmax + sensTable$Topmax$VALUE*Topmax
  }
  if (newval  <= Topmin) {
    warning("New Topmax value < Topmin, correcting")
    perdif <- abs(Topmax-Topmin)/Topmax*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Topmax$VALUE <- newdif
  }
  
  ### Tmax
  if (tolower(sensTable$Tmax$TYPE) == "p") {
    newval <- Tmax + (sensTable$Tmax$VALUE/100)*Tmax
  } else if (tolower(sensTable$Tmax$TYPE) == "a") {
    newval <- Tmax + sensTable$Tmax$VALUE
  } else if (tolower(sensTable$Tmax$TYPE) == "f") {
    if (sensTable$Tmax$VALUE > 1) {
      warning("Tmax change type set as f, but above 1, dividing by 100")
      sensTable$Tmax$VALUE <- sensTable$Tmax$VALUE/100
    }
    newval <- Tmax + sensTable$Tmax$VALUE*Tmax
  }
  if (newval  <= Topmax) {
    warning("New Tmax value < Topmax, correcting")
    perdif <- abs(Tmax-Topmax)/Tmax*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Tmax$VALUE <- newdif*sign(sensTable$Tmax$VALUE)
  }
  
  ###########################################################
  ### Rmin
  if (tolower(sensTable$Rmin$TYPE) == "p") {
    newval <- Rmin + (sensTable$Rmin$VALUE/100)*Rmin
  } else if (tolower(sensTable$Rmin$TYPE) == "a") {
    newval <- Rmin + sensTable$Rmin$VALUE
  } else if (tolower(sensTable$Rmin$TYPE) == "f") {
    if (sensTable$Rmin$VALUE > 1) {
      warning("Rmin change type set as f, but above 1, dividing by 100")
      sensTable$Rmin$VALUE <- sensTable$Rmin$VALUE/100
    }
    newval <- Rmin + sensTable$Rmin$VALUE*Rmin
  }
  
  if (newval  >= Ropmin) {
    warning("New Rmin value > Ropmin, correcting")
    perdif <- abs(Ropmin-Rmin)/Rmin*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Rmin$VALUE <- newdif
  }
  
  ### Ropmin
  if (tolower(sensTable$Ropmin$TYPE) == "p") {
    newval <- Ropmin + (sensTable$Ropmin$VALUE/100)*Ropmin
  } else if (tolower(sensTable$Ropmin$TYPE) == "a") {
    newval <- Ropmin + sensTable$Ropmin$VALUE
  } else if (tolower(sensTable$Ropmin$TYPE) == "f") {
    if (sensTable$Ropmin$VALUE > 1) {
      warning("Ropmin change type set as f, but above 1, dividing by 100")
      sensTable$Ropmin$VALUE <- sensTable$Ropmin$VALUE/100
    }
    newval <- Ropmin + sensTable$Ropmin$VALUE*Ropmin
  }
  if (newval  >= Ropmax) {
    warning("New Ropmin value > Ropmax, correcting")
    perdif <- abs(Ropmax-Ropmin)/Ropmin*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Ropmin$VALUE <- newdif
  }
  
  ### Ropmax
  if (tolower(sensTable$Ropmax$TYPE) == "p") {
    newval <- Ropmax + (sensTable$Ropmax$VALUE/100)*Ropmax
  } else if (tolower(sensTable$Ropmax$TYPE) == "a") {
    newval <- Ropmax + sensTable$Ropmax$VALUE
  } else if (tolower(sensTable$Ropmax$TYPE) == "f") {
    if (sensTable$Ropmax$VALUE > 1) {
      warning("Ropmax change type set as f, but above 1, dividing by 100")
      sensTable$Ropmax$VALUE <- sensTable$Ropmax$VALUE/100
    }
    newval <- Ropmax + sensTable$Ropmax$VALUE*Ropmax
  }
  if (newval  <= Ropmin) {
    warning("New Ropmax value < Ropmin, correcting")
    perdif <- abs(Ropmax-Ropmin)/Ropmax*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Ropmax$VALUE <- newdif
  }
  
  ### Rmax
  if (tolower(sensTable$Rmax$TYPE) == "p") {
    newval <- Rmax + (sensTable$Rmax$VALUE/100)*Rmax
  } else if (tolower(sensTable$Rmax$TYPE) == "a") {
    newval <- Rmax + sensTable$Rmax$VALUE
  } else if (tolower(sensTable$Rmax$TYPE) == "f") {
    if (sensTable$Rmax$VALUE > 1) {
      warning("Rmax change type set as f, but above 1, dividing by 100")
      sensTable$Rmax$VALUE <- sensTable$Rmax$VALUE/100
    }
    newval <- Rmax + sensTable$Rmax$VALUE*Rmax
  }
  if (newval  <= Ropmax) {
    warning("New Rmax value < Ropmax, correcting")
    perdif <- abs(Rmax-Ropmax)/Rmax*100
    newdif <- ceiling(perdif*0.25)
    sensTable$Rmax$VALUE <- newdif*sign(sensTable$Rmax$VALUE)
  }
  
  #Checking if outfolder does exist, and creating it if necessary
  if (!file.exists(outfolder)) {
    dir.create(outfolder, recursive=T)
  }
  
  #baseline run (parameters as they are)
  out_rs <- paste(outfolder,"/control/",cropname,"_suitability.asc",sep="")
  
  if (!file.exists(out_rs)) {
    cat("Performing control run\n")
    eco <- suitCalc(climPath=climPath, 
                    Gmin=Gmin,Gmax=Gmax,Tkmp=Tkmp,Tmin=Tmin,Topmin=Topmin,
                    Topmax=Topmax,Tmax=Tmax,Rmin=Rmin,Ropmin=Ropmin,
                    Ropmax=Ropmax,Rmax=Rmax, 
                    outfolder=paste(outfolder,"/control",sep=""), 
                    cropname=cropname,ext=".tif")
    sens_0 <- eco[[5]]
    rm(eco); g=gc(); rm(g)
  } else {
    sens_0 <- raster(out_rs)
  }
  
  #perform each sensitivity run
  for (i in 1:length(sensTable)) {
    t_Tmin <- Tmin; t_Topmin <- Topmin; t_Topmax <- Topmax; t_Tmax <- Tmax
    t_Rmin <- Rmin; t_Ropmin <- Ropmin; t_Ropmax <- Ropmax; t_Rmax <- Rmax
    
    parname <- names(sensTable)[i]
    cat("Performing sensitivity run for",parname,"\n")
    if (tolower(sensTable[[parname]]$TYPE) == "p") {
      newval <- get(parname) + (sensTable[[parname]][["VALUE"]]/100)*get(parname)
      sensTable[[parname]][["RUN_VALUE"]] <- newval
    } else if (tolower(sensTable[[parname]]$TYPE) == "a") {
      newval <- get(parname) + sensTable[[parname]][["VALUE"]]
      sensTable[[parname]][["RUN_VALUE"]] <- newval
    } else if (tolower(sensTable[[parname]]$TYPE) == "f") {
      newval <- get(parname) + (sensTable[[parname]][["VALUE"]])*get(parname)
      sensTable[[parname]][["RUN_VALUE"]] <- newval
    } else {
      stop("change type must be either p (percent), a (absolute) or f (fraction)")
    }
    
    #perform each model run, only modifying this particular parameter
    if (sensTable[[parname]][["RUN_VALUE"]] != get(parname)) {
      sensTable[[parname]]["STATUS"] <- 1
      
      #performing the model run
      assign(paste("t_",parname,sep=""),newval)
      out_rs <- paste(outfolder,"/",parname,"/",cropname,"_suitability.asc",sep="")
      
      if (!file.exists(out_rs)) {
        eco <- suitCalc(climPath=climPath, 
                        Gmin=Gmin,Gmax=Gmax,Tkmp=Tkmp,Tmin=t_Tmin,Topmin=t_Topmin,
                        Topmax=t_Topmax,Tmax=t_Tmax,Rmin=t_Rmin,Ropmin=t_Ropmin,
                        Ropmax=t_Ropmax,Rmax=t_Rmax, 
                        outfolder=paste(outfolder,"/",tolower(parname),sep=""), 
                        cropname=cropname,ext=".tif")
        sensTable[[parname]]["SUIT_RS"] <- eco[[5]]
        rm(eco); g=gc(); rm(g)
      } else {
        sensTable[[parname]]["SUIT_RS"] <- raster(out_rs)
      }
    } else {
      sensTable[[parname]]["STATUS"] <- 0
      sensTable[[parname]]["SUIT_RS"] <- sens_0
    }
  }
  
  #sensitivity runs results
  sensStk <- stack(c(sens_0,sensTable$Tmin$SUIT_RS,sensTable$Topmin$SUIT_RS,
                     sensTable$Topmax$SUIT_RS,sensTable$Tmax$SUIT_RS,
                     sensTable$Rmin$SUIT_RS,sensTable$Ropmin$SUIT_RS,
                     sensTable$Ropmax$SUIT_RS,sensTable$Rmax$SUIT_RS))
  
  whichRun_rs <- raster(sensStk, 0)
  suitVal_rs <- raster(sensStk, 0)
  ordRun_rs <- raster(sensStk, 0)
  
  outfolder_rs <- paste(outfolder,"/results",sep="")
  if (!file.exists(outfolder_rs)) {dir.create(outfolder_rs,recursive=T)}
  
  whichRunName <- paste(outfolder_rs, "/which_run.asc", sep="")
  suitValName <- paste(outfolder_rs, "/suitability.asc", sep="")
  ordRunName <- paste(outfolder_rs, "/order.asc", sep="")
  
  bs <- blockSize(sensStk, n=12, minblocks=2)
  cat("(", bs$n, " chunks) \n", sep="")
  pb <- pbCreate(bs$n, type='text', style=3)
  for (b in 1:bs$n) {
    iniCell <- 1+(bs$row[b]-1)*ncol(suitVal_rs)
    finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(suitVal_rs)
    allCells <- iniCell:finCell
    validCells <- allCells[which(!is.na(sensStk[[1]][allCells]))]
    rowVals <- extract(sensStk,validCells)
    
    rasVals <- apply(rowVals, 1, findMax)
    whVecSuit <- rasVals[1,]
    valVecSuit <- rasVals[2,]
    ordVecSuit <- rasVals[3,]
    rm(rasVals)
    
    whichRun_rs[validCells] <- whVecSuit
    suitVal_rs[validCells] <- valVecSuit
    ordRun_rs[validCells] <- ordVecSuit
    
    pbStep(pb, b)
  }
  pbClose(pb)
  
  #writing output rasters
  whichRun_rs <- writeRaster(whichRun_rs, whichRunName, format='ascii', overwrite=TRUE)
  suitVal_rs <- writeRaster(suitVal_rs, suitValName, format='ascii', overwrite=TRUE)
  ordRun_rs <- writeRaster(ordRun_rs, ordRunName, format='ascii', overwrite=TRUE)
  
  #write sensTable into a textfile
  logFileName <- paste(outfolder, "/", cropname, ".constraints", sep="")
  createLog <- makeConsFile(logFileName, sensTable, climPath, cropname, Gmin, Gmax, Tkmp, Tmin, Topmin, Topmax, Tmax, Rmin, Ropmin, Ropmax, Rmax)
  
  return(stack(whichRun_rs, suitVal_rs, ordRun_rs))
}



findMax <- function(dataPixel) {
  ctrl <- dataPixel[1]
  if (is.na(ctrl)) {
    return(c(NA,NA))
  } else {
    sens <- dataPixel[2:length(dataPixel)] - ctrl
    sens_on <- order(sens)
    sens_or <- sens[sens_on]
    sens_on <- sens_on[which(sens_or<0)]
    sens_on <- as.numeric(paste(sens_on,collapse=""))
    
    min_val <- min(sens)
    min_whc <- which(sens == min_val)
    if (length(min_whc) > 1 & min_val >= 0) {
      min_whc <- 0
    } else if (length(min_whc) > 1 & min_val < 0)  {
      min_whc <- as.numeric(paste(min_whc,collapse=""))
    }
    result <- c(min_whc,min_val,sens_on)
    return(result)
  }
}



makeConsFile <- function(filePathName, sensTable, climPath, cropname, Gmin, Gmax, Tkmp, Tmin, Topmin, Topmax, Tmax, Rmin, Ropmin, Ropmax, Rmax) {
  con <- file(filePathName, "w")
  writeLines(paste("CLIMATE_FILES:", climPath), con)
  writeLines(paste("CROP:", cropname), con)
  writeLines(paste("GMIN:", Gmin), con)
  writeLines(paste("GMAX:", Gmax), con)
  writeLines(paste("TKMP:", Tkmp), con)
  writeLines(paste("TMIN:", Tmin), con)
  writeLines(paste("TOPMIN:", Topmin), con)
  writeLines(paste("TOPMAX:", Topmax), con)
  writeLines(paste("TMAX:", Tmax), con)
  writeLines(paste("RMIN:", Rmin), con)
  writeLines(paste("ROPMIN:", Ropmin), con)
  writeLines(paste("ROPMAX:", Ropmax), con)
  writeLines(paste("RMAX:", Rmax), con)
  cat("\n", file=con)
  writeLines("---------------------", con)
  writeLines("Sensitivity runs", con)
  writeLines("---------------------", con)
  writeLines("TMIN:", con)
  writeLines(paste("   VALUE=", sensTable$Tmin$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Tmin$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Tmin$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Tmin$SUIT_RS@file@name,sep=""), con)
  writeLines("TOPMIN:", con)
  writeLines(paste("   VALUE=", sensTable$Topmin$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Topmin$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Topmin$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Topmin$SUIT_RS@file@name,sep=""), con)
  writeLines("TOPMAX:", con)
  writeLines(paste("   VALUE=", sensTable$Topmax$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Topmax$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Topmax$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Topmax$SUIT_RS@file@name,sep=""), con)
  writeLines("TMAX:", con)
  writeLines(paste("   VALUE=", sensTable$Tmax$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Tmax$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Tmax$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Tmax$SUIT_RS@file@name,sep=""), con)
  writeLines("RMIN:", con)
  writeLines(paste("   VALUE=", sensTable$Rmin$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Rmin$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Rmin$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Rmin$SUIT_RS@file@name,sep=""), con)
  writeLines("ROPMIN:", con)
  writeLines(paste("   VALUE=", sensTable$Ropmin$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Ropmin$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Ropmin$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Ropmin$SUIT_RS@file@name,sep=""), con)
  writeLines("ROPMAX:", con)
  writeLines(paste("   VALUE=", sensTable$Ropmax$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Ropmax$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Ropmax$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Ropmax$SUIT_RS@file@name,sep=""), con)
  writeLines("RMAX:", con)
  writeLines(paste("   VALUE=", sensTable$Rmax$VALUE,sep=""), con)
  writeLines(paste("   TYPE=", sensTable$Rmax$TYPE,sep=""), con)
  writeLines(paste("   RUN_VALUE=", sensTable$Rmax$RUN_VALUE,sep=""), con)
  writeLines(paste("   OUT_RS=", sensTable$Rmax$SUIT_RS@file@name,sep=""), con)
  close(con)
}


