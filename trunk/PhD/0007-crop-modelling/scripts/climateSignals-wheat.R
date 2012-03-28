#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#March 2012
stop("Do not runt the whole thing")

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/watbal.R",sep=""))

src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/climateSignals-functions.R",sep=""))

library(raster)

#Climate signals on yield for Indian Groundnuts

bDir <- "F:/PhD-work/crop-modelling"
sradDir <- paste(bDir,"/climate-data/CRU_CL_v1-1_data",sep="")
tempDir <- paste(bDir,"/climate-data/CRU_TS_v3-1_data",sep="")

y_iyr <- 1966
y_eyr <- 1995

#Planting dates as in literature
#     1. Early: 10 November (Lobell et al. 2012)
#     2. Normal: 25 November (Lobell et al. 2012)
#     3. Late: 10 December (Lobell et al. 2012)
#     4. Last Dates upto which sowing would be economical are 
#        (http://krishisewa.com/articles/w_sowing.html)
#        zones at (http://krishisewa.com/krishi/Azone.html)
              #upto 25th December - in North-West plain Zone
              #upto 10th December - in North-East plain & Central Zone
              #upto 30th November - in Peninsular Zone
#     5. Very early: mid-October (De et al. 1983) (http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=4596924)
#     6. Planting should be done when tmean < 22C

ncFile <- paste(bDir,"/climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
ydDir <- paste(bDir,"/GLAM/climate-signals-yield/GNUT/raster/gridded",sep="")
oDir <- paste(bDir,"/GLAM/climate-signals-yield/GNUT/signals",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#create mask
metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)

############
#determine planting date using pjones algorithm?

pCells <- data.frame(CELL=1:ncell(msk))
pCells$X <- xFromCell(msk,pCells$CELL); pCells$Y <- yFromCell(msk,pCells$CELL)
pCells$Z <- extract(msk,cbind(X=pCells$X,Y=pCells$Y))
pCells <- pCells[which(!is.na(pCells$Z)),]
write.csv(pCells,paste(oDir,"/cells-process.csv",sep=""),quote=F,row.names=F)

# plot(msk)
# text(x=pCells$X,y=pCells$Y,labels=pCells$CELL,cex=0.35)

###Parameters
sd_default=165; ed_default=225
thresh=0.5
tbase=0; topt=22; tmax=30
tcrit=34; tlim=100

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=4) #initiate cluster

#export functions and data
sfExport("pCells")
sfExport("oDir")
sfExport("ncFile")
sfExport("ydDir")
sfExport("bDir")
sfExport("sradDir")
sfExport("tempDir")
sfExport("y_iyr")
sfExport("y_eyr")
sfExport("src.dir")
sfExport("src.dir2")

#run the control function
system.time(sfSapply(as.vector(pCells$CELL), cell_wrapper))

#stop the cluster
sfStop()


#cell_wrapper(600)


#for a given cell extract the yield data and make the correlation
techn <- "loe"

calcSignals <- function(techn,ydDir,oDir) {
  #loading yield data stack
  yd_stk <- stack(paste(ydDir,"/",techn,"/",techn,"-",(y_iyr-1900):(y_eyr-1900),".asc",sep=""))
  
  #loop through gridcells
  for (cell in pCells$CELL) {
    cat("Processing cell",cell,"\n")
    x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]
    
    yd_vals <- extract(yd_stk,cbind(X=x,Y=y))
    if (file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
      cl_data <- read.csv(paste(oDir,"/climate_cell-",cell,".csv",sep=""))
      
      all_data <- cl_data
      all_data$YIELD <- t(yd_vals)
      all_data <- all_data[which(all_data$YIELD!=0),]
      all_data <- all_data[which(!is.na(all_data$YIELD)),]
      
      env_vars <- names(all_data)[2:30]
      
      #loop through each possible variable
      for (evar in env_vars) {
        #perform the correlation test
        if (nrow(all_data)>=2) {
          ct <- cor.test(all_data$YIELD,all_data[,evar])
        } else {
          ct <- list()
          ct$estimate <- NA
          ct$p.value <- NA
        }
        if (evar == env_vars[1]) {
          out_row <- data.frame(CELL=cell,LON=x,LAT=y,NOBS=nrow(all_data),R=ct$estimate,PVAL=ct$p.value)
          names(out_row)[5:6] <- c(paste(evar,c(".R",".PVAL"),sep=""))
          out_all <- out_row
        } else {
          out_row <- data.frame(R=ct$estimate,PVAL=ct$p.value)
          names(out_row) <- c(paste(evar,c(".R",".PVAL"),sep=""))
          out_all <- cbind(out_all,out_row)
        }
      }
    } else {
      out_all <- c(cell,x,y,0,rep(NA,times=58))
    }
    
    if (cell == pCells$CELL[1]) {
      out_sign <- out_all
    } else {
      out_sign <- rbind(out_sign,out_all)
    }
  }
  oSignDir <- paste(oDir,"/1dd_signals",sep="")
  if (!file.exists(oSignDir)) {dir.create(oSignDir)}
  
  oTechDir <- paste(oSignDir,"/loe-signals",sep="")
  if (!file.exists(oTechDir)) {dir.create(oTechDir)}
  
  write.csv(out_sign,paste(oTechDir,"/signals-",techn,".csv",sep=""),quote=F,row.names=F)
  
  #write rasters
  rs_names <- names(out_sign)[5:ncol(out_sign)]
  cat("Writing rasters\n")
  for (rname in rs_names) {
    out_rs <- raster(msk)
    out_rs[pCells$CELL] <- out_sign[,rname]
    #plot(out_rs)
    out_rs <- writeRaster(out_rs,paste(oTechDir,"/",techn,"-",rname,".asc",sep=""),format="ascii")
  }
  return(oTechDir)
}





