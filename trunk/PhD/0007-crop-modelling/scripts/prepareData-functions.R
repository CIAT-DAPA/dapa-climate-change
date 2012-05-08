#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#May 2012

#############################################################
# function to wrap the climate data extraction from a given cell
#############################################################
prepareCellData <- function(cell) {
  #loading required functions/packages
  library(raster); library(rgdal)
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir,"/watbal.R",sep=""))
  source(paste(src.dir2,"/climateSignals-functions.R",sep=""))
  
  #grab cell details (lon,lat)
  x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]
  
  cat("/n########################################################\n")
  cat("#### Extracting data for gridcell",cell,"#### \n")
  cat("#### located in",x,"and",y,"####\n")
  cat("########################################################\n")
  
  #############################################################
  #extract daily rainfall in a loop of years
  cat("/n########################################################\n")
  cat("#### Extracting daily rainfall \n")
  cat("########################################################\n")
  
  oDataDir <- paste(oDir,"/rain",sep="")
  if (!file.exists(oDataDir)) {dir.create(oDataDir)}
  
  if (!file.exists(paste(oDataDir,"/cell-",cell,".csv",sep=""))) {
    allDD <- as.data.frame(matrix(ncol=367,nrow=0))
    names(allDD) <- c("YEAR",paste("DAY",1:366,sep=""))
    for (year in y_iyr:y_eyr) {
      cat("extracting",year,": ")
      nd <- leap(year)
      rain <- extractDaily(ncFile,x,y,year,nd,mthRainAsc)
      if (nd == 365) {
        rain <- rbind(rain,c(366,NA))
      }
      
      row_df <- as.data.frame(t(c(year,rain$RAIN)))
      names(row_df) <- c("YEAR",paste("DAY",1:366,sep=""))
      allDD <- rbind(allDD,row_df)
    }
    write.csv(allDD,paste(oDataDir,"/cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
  
  #############################################################
  #extract ERA40 solar radiation data
  cat("/n########################################################\n")
  cat("#### Extracting daily solar rad from ERA40 \n")
  cat("########################################################\n")
  
  oDataDir <- paste(oDir,"/srad_e40",sep="")
  if (!file.exists(oDataDir)) {dir.create(oDataDir)}
  
  if (!file.exists(paste(oDataDir,"/cell-",cell,".csv",sep=""))) {
    allDD_srad <- as.data.frame(matrix(ncol=367,nrow=0))
    names(allDD_srad) <- c("YEAR",paste("DAY",1:366,sep=""))
    for (year in y_iyr:y_eyr) {
      cat("extracting",year,": ")
      nd <- leap(year)
      srad <- extractERA(era40Dir,x,y,year,nd,varName="srad")
      if (nd == 365) {
        srad <- rbind(srad,c(366,NA))
      }
      
      row_df <- as.data.frame(t(c(year,srad$SRAD)))
      names(row_df) <- c("YEAR",paste("DAY",1:366,sep=""))
      allDD_srad <- rbind(allDD_srad,row_df)
    }
    write.csv(allDD_srad,paste(oDataDir,"/cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
  
  
  #############################################################
  #extract minimum temperature from CRU datasets (monthly)
  cat("/n########################################################\n")
  cat("#### Extracting monthly min temperature from CRU \n")
  cat("########################################################\n")
  
  oDataDir <- paste(oDir,"/cru_tmn",sep="")
  if (!file.exists(oDataDir)) {dir.create(oDataDir)}
  
  if (!file.exists(paste(oDataDir,"/cell-",cell,".csv",sep=""))) {
    allDD_tmin <- as.data.frame(matrix(ncol=13,nrow=0))
    names(allDD_tmin) <- c("YEAR",paste("MONTH",1:12,sep=""))
    for (year in y_iyr:y_eyr) {
      cat("extracting",year,": monthly min temperature \n")
      tmin_stk <- stack(paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",year,"_",1:12,".asc",sep=""))
      tmin_vals <- extract(tmin_stk,cbind(X=x,Y=y))*0.1
      row_df <- as.data.frame(t(c(year,tmin_vals)))
      names(row_df) <- c("YEAR",paste("MONTH",1:12,sep=""))
      allDD_tmin <- rbind(allDD_tmin,row_df)
    }
    write.csv(allDD_tmin,paste(oDataDir,"/cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
  
  
  #############################################################
  #extract maximum temperature from CRU datasets (monthly)
  cat("/n########################################################\n")
  cat("#### Extracting monthly max temperature from CRU \n")
  cat("########################################################\n")
  
  oDataDir <- paste(oDir,"/cru_tmx",sep="")
  if (!file.exists(oDataDir)) {dir.create(oDataDir)}
  
  if (!file.exists(paste(oDataDir,"/cell-",cell,".csv",sep=""))) {
    allDD_tmax <- as.data.frame(matrix(ncol=13,nrow=0))
    names(allDD_tmax) <- c("YEAR",paste("MONTH",1:12,sep=""))
    for (year in y_iyr:y_eyr) {
      cat("extracting",year,": monthly max temperature \n")
      tmax_stk <- stack(paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",year,"_",1:12,".asc",sep=""))
      tmax_vals <- extract(tmax_stk,cbind(X=x,Y=y))*0.1
      row_df <- as.data.frame(t(c(year,tmax_vals)))
      names(row_df) <- c("YEAR",paste("MONTH",1:12,sep=""))
      allDD_tmax <- rbind(allDD_tmax,row_df)
    }
    write.csv(allDD_tmax,paste(oDataDir,"/cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
  
  #############################################################
  #extracting solar radiation data (climatological)
  cat("/n########################################################\n")
  cat("#### Extracting climatological solar radiation from CRU \n")
  cat("########################################################\n")
  
  oDataDir <- paste(oDir,"/cru_srad",sep="")
  if (!file.exists(oDataDir)) {dir.create(oDataDir)}
  
  if (!file.exists(paste(oDataDir,"/cell-",cell,".csv",sep=""))) {
    allDD_srad <- as.data.frame(matrix(ncol=13,nrow=0))
    names(allDD_srad) <- c("YEAR",paste("MONTH",1:12,sep=""))
    cat("extracting : climatological solar radiation \n")
    srad_stk <- stack(paste(sradDir,"/srad_1dd/srad_",1:12,".asc",sep=""))
    srad_vals <- extract(srad_stk,cbind(X=x,Y=y))
    row_df <- as.data.frame(t(c(year,srad_vals)))
    names(row_df) <- c("YEAR",paste("MONTH",1:12,sep=""))
    allDD_srad <- rbind(allDD_srad,row_df)
    write.csv(allDD_srad,paste(oDataDir,"/cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
}


