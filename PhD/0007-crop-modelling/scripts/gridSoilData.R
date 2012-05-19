#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#May 2012

library(raster); library(maptools); data(wrld_simpl)

#load functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glam-soil-functions.R",sep=""))


#directories
wd <- "F:/PhD-work/crop-modelling/GLAM/soil-data"
dataDir <- paste(wd,"/HWSD",sep="")

#GLAM                    #USDA
#1. sandy                13. sand
#2. sandy loam           11. sandy loam
#3. silt loam            7. silt loam
#4. clay loam            5. clay loam
#5. clay                 1. and 3. clay
#6. lithosol
#7. organic

#need to create new soiltypes for these if they end up existing in the 1dd gridcells
#of India
#2. silty clay
#4. silty clay loam
#6. silt
#8. sandy clay
#9. loam
#10. sandy clay loam
#12. loamy sand

#for a given gridcell get the percents of each category (soil units)
#and also the percents of sand, silt and clay, which need to be calculated using the
#weighted average of:
                    #each sub-class in the soil unit
                    #each soil-unit in the gridcell

#read soil raster
srs <- raster(paste(dataDir,"/soilind",sep=""))

#read soil properties
sData <- read.csv(paste(dataDir,"/HWSD_DATA.csv",sep=""))

#read gridcell raster
msk <- raster(paste(dataDir,"/msk",sep=""))
msk[which(!is.na(msk[]))] <- which(!is.na(msk[]))
allCells <- msk[which(!is.na(msk[]))]

#pick a cell
#cell <- 427

#loop through cells
for (cell in allCells) {
  cat("cell",which(allCells==cell),"of",length(allCells),"\n")
  #crop the srs into the gridcell
  x <- as.numeric(xFromCell(msk,cell))
  y <- as.numeric(yFromCell(msk,cell))
  
  xt <- extent((x-0.5),(x+0.5),(y-0.5),(y+0.5))
  cSoil <- crop(srs,xt)
  a_grid <- raster:::area(cSoil)
  a_grid[which(is.na(cSoil[]))] <- NA
  
  #get how many sub-classes
  sub_c <- unique(cSoil[],na.rm=T)
  ctr <- 1
  for (sun in sub_c) {
    sunData <- sData[which(sData$MU_GLOBAL == sun),]
    
    #remove dunes if there is any (i.e. crops won't be grown in dunes)
    dsrow <- which(sunData$ISSOIL!=0)
    sunData <- sunData[dsrow,]
    
    sunDD <- data.frame(SHARE=sunData$SHARE,SU74=paste(sunData$SU_SYM74),SU85=paste(sunData$SU_SYM85),
                   SU90=paste(sunData$SU_SYM90),SAND=sunData$T_SAND,SILT=sunData$T_SILT,
                   CLAY=sunData$T_CLAY,OC=sunData$T_OC)
    
    if (nrow(sunDD) > 1) {
      #get main category in that soil unit
      mainCat74 <- paste(sunDD$SU74[which(sunDD$SHARE == max(sunDD$SHARE))][1])
      mainCat85 <- paste(sunDD$SU85[which(sunDD$SHARE == max(sunDD$SHARE))][1])
      mainCat90 <- paste(sunDD$SU90[which(sunDD$SHARE == max(sunDD$SHARE))][1])
      
      #weighted average of contents in soil unit
      sand <- sum((sunDD$SAND*sunDD$SHARE))/sum(sunDD$SHARE)
      silt <- sum((sunDD$SILT*sunDD$SHARE))/sum(sunDD$SHARE)
      clay <- sum((sunDD$CLAY*sunDD$SHARE))/sum(sunDD$SHARE)
      oc <- sum((sunDD$OC*sunDD$SHARE))/sum(sunDD$SHARE)
      
      #classify the texture
      tt <- data.frame(CLAY=clay,SILT=silt,SAND=sand,OC=oc)
      clss <- TT.points.in.classes(tt,class.sys="USDA.TT")
      clss <- clss[1,which(clss[1,]==1)]
      clss <- names(clss)
      
      #area of the mapping unit in this cell
      xysu <- as.data.frame(xyFromCell(cSoil,which(cSoil[]==sun)))
      xysu$AREA <- extract(a_grid,cbind(X=xysu$x,Y=xysu$y))
      asu <- sum(xysu$AREA)/sum(a_grid[],na.rm=T)
      
      #produce an output row for that
      out_row <- data.frame(MU=sun,CAT74=mainCat74,CAT85=mainCat85,CAT90=mainCat90,
                            SAND=sand,SILT=silt,CLAY=clay,OC=oc,CLASS=clss,AREA_FRAC=asu)
      
      if (ctr == 1) {
        out_all <- out_row
      } else {
        out_all <- rbind(out_all,out_row)
      }
      ctr <- ctr+1
    }
    
  }
  
  out_all$CELL <- cell
  
  if (cell == allCells[1]) {
    out_cells <- out_all
  } else {
    out_cells <- rbind(out_cells,out_all)
  }
  
}

out_cells$SOIL_CAT <- paste(out_cells$CAT74,out_cells$CAT85,out_cells$CAT90,sep="")
write.csv(out_cells,paste(dataDir,"/cellValues.csv",sep=""),quote=T,row.names=F)


#new loop to get the mean hydrological characteristics and dominant soil profile
#in each gridcell
for (cell in allCells) {
  cat(cell,"\n")
  cellChars <- out_cells[which(out_cells$CELL==cell),]
  
  #calculate mean characteristics
  sand <- sum(cellChars$SAND*cellChars$AREA_FRAC)/sum(cellChars$AREA_FRAC)
  silt <- sum(cellChars$SILT*cellChars$AREA_FRAC)/sum(cellChars$AREA_FRAC)
  clay <- sum(cellChars$CLAY*cellChars$AREA_FRAC)/sum(cellChars$AREA_FRAC)
  oc <- sum(cellChars$OC*cellChars$AREA_FRAC)/sum(cellChars$AREA_FRAC)
  
  #classify the texture
  tt <- data.frame(CLAY=clay,SILT=silt,SAND=sand,OC=oc)
  clss <- TT.points.in.classes(tt,class.sys="USDA.TT")
  clss <- clss[1,which(clss[1,]==1)]
  clss <- names(clss)
  
  #calculate hydro characteristics
  rll <- hydroChars(sand=sand,clay=clay)$RLL
  dul <- hydroChars(sand=sand,clay=clay)$DUL
  sat <- hydroChars(sand=sand,clay=clay)$SAT
  
  #determine dominant profile
  soilProfs <- unique(cellChars$SOIL_CAT)
  for (spr in soilProfs) {
    sprChars <- cellChars[which(cellChars$SOIL_CAT == spr),]
    sprFrac <- sum(sprChars$AREA_FRAC)/sum(cellChars$AREA_FRAC)
    out_row <- data.frame(PROF=paste(spr),FRAC=sprFrac)
    
    if (spr == soilProfs[1]) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
  }
  domSoil <- paste(out_all$PROF[which(out_all$FRAC == max(out_all$FRAC))])
  
  out_pc <- data.frame(CELL=cell,DOMSOIL=domSoil,SAND=sand,SILT=silt,CLAY=clay,OC=oc,
                       TCLASS=clss,RLL=rll,DUL=dul,SAT=sat)
  
  if (cell == allCells[1]) {
    out_allpc <- out_pc
  } else {
    out_allpc <- rbind(out_allpc,out_pc)
  }
  
}

write.csv(out_allpc,paste(dataDir,"/cell_soil_characteristics.csv",sep=""),quote=T,row.names=F)

#rs <- raster(msk)
#rs[allCells] <- out_allpc$OC

#get mean, max, min soil hydro characteristics per class
litho <- out_allpc[which(out_allpc$DOMSOIL == "I"),]
type <- "Lithosol"
rll_l <- sprintf("%.2f",round(min(litho$RLL),2))
rll <- sprintf("%.2f",round(mean(litho$RLL),2))
rll_u <- sprintf("%.2f",round(max(litho$RLL),2))

dul_l <- sprintf("%.2f",round(min(litho$DUL),2))
dul <- sprintf("%.2f",round(mean(litho$DUL),2))
dul_u <- sprintf("%.2f",round(max(litho$DUL),2))

sat_l <- sprintf("%.2f",round(min(litho$SAT),2))
sat <- sprintf("%.2f",round(mean(litho$SAT),2))
sat_u <- sprintf("%.2f",round(max(litho$SAT),2))

#available soil water
asw_l <- sprintf("%.2f",round(min(litho$DUL)-min(litho$RLL),2))
asw_u <- sprintf("%.2f",round(mean(litho$DUL)-mean(litho$RLL),2))

out_line <- paste("1          ",
                  substr(rll,2,4),"    ",substr(rll_l,2,4),"    ",substr(rll_u,2,4),"    ",
                  substr(dul,2,4),"    ",substr(dul_l,2,4),"    ",substr(dul_u,2,4),"    ",
                  substr(sat,2,4),"    ",substr(sat_l,2,4),"    ",substr(sat_u,2,4),"    ",
                  substr(asw_l,2,4),"    ",substr(asw_u,2,4),"     ",type,"\n",sep="")

ff <- file(paste(dataDir,"/GLAM_soiltypes.txt",sep=""),"w")
cat("SOIL_CODE  RLL    RLL_L  RLL_U  DUL   DUL_L   DUL_U  SAT    SAT_L  SAT_U  ASW_L  ASW_U  SOIL_TYPE\n",file=ff)
cat(out_line,file=ff)

litho$SOIL_CODE <- 1

nonlitho <- out_allpc[which(out_allpc$DOMSOIL != "I"),]
tcl <- unique(nonlitho$TCLASS)

nonlitho$SOIL_CODE <- NA
ctr <- 2
for (clss in tcl) {
  #assign soil code value to grid
  nonlitho$SOIL_CODE[which(nonlitho$TCL == clss)] <- ctr
  
  
  type <- paste(clss)
  rll_l <- min(nonlitho$RLL[which(nonlitho$TCL == clss)])
  rll <- mean(nonlitho$RLL[which(nonlitho$TCL == clss)])
  rll_u <- max(nonlitho$RLL[which(nonlitho$TCL == clss)])
  
  dul_l <- min(nonlitho$DUL[which(nonlitho$TCL == clss)])
  dul <- mean(nonlitho$DUL[which(nonlitho$TCL == clss)])
  dul_u <- max(nonlitho$DUL[which(nonlitho$TCL == clss)])
  
  sat_l <- min(nonlitho$SAT[which(nonlitho$TCL == clss)])
  sat <- mean(nonlitho$SAT[which(nonlitho$TCL == clss)])
  sat_u <- max(nonlitho$SAT[which(nonlitho$TCL == clss)])
  
  #available soil water
  asw_l <- min(nonlitho$DUL[which(nonlitho$TCL == clss)])-min(nonlitho$RLL[which(nonlitho$TCL == clss)])
  asw_u <- mean(nonlitho$DUL[which(nonlitho$TCL == clss)])-mean(nonlitho$RLL[which(nonlitho$TCL == clss)])
  
  out_line <- paste(paste(ctr,"          ",sep=""),
                    substr(rll,2,4),"    ",substr(rll_l,2,4),"    ",substr(rll_u,2,4),"    ",
                    substr(dul,2,4),"    ",substr(dul_l,2,4),"    ",substr(dul_u,2,4),"    ",
                    substr(sat,2,4),"    ",substr(sat_l,2,4),"    ",substr(sat_u,2,4),"    ",
                    substr(asw_l,2,4),"    ",substr(asw_u,2,4),"     ",type,"\n",sep="")
  cat(out_line,file=ff)
  ctr <- ctr+1
}
close(ff)


#assigning categories to the raster
rs <- raster(msk)
allAgain <- rbind(litho,nonlitho)
rs[allAgain$CELL] <- allAgain$SOIL_CODE
rs <- writeRaster(rs,paste(dataDir,"/IND_GLAM_soilCodes.asc",sep=""),format="ascii",overwrite=F)
plot(rs,col=rev(terrain.colors(6)))






