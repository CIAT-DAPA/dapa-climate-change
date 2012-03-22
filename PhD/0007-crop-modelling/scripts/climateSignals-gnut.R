#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#March 2012
stop("Do not runt the whole thing")


#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/watbal.R",sep=""))

library(raster)

#Climate signals on yield for Indian Groundnuts

bDir <- "D:/CIAT_work/crop-modelling"

y_iyr <- 1966
y_eyr <- 1995

#Planting dates as in literature
#summer (only separated in Sardana and Kandhola 2007):
#         1. End of April to early May
#khariff: summer rainfed. 
#         1. From first week of June to last week of July (Talawar 2004) (regular and late monsoon)
#         2. From end of May to early June (Sardana and Kandhola 2007) (pre-monsoon)
#         3. Third week of June (Singh and Oswalt 1995)
#         4. First half of July (Singh et al. 1986)
#         5. 20th April (normal summer) (Harinath and Vasanthi 1998 http://www.indianjournals.com/ijor.aspx?target=ijor:ijpp1&volume=26&issue=2&article=001)
#rabi: winter irrigated
#         1. From mid December to mid January (Ramadoss and Myers 2004 http://www.regional.org.au/au/asa/2004/poster/4/1/2/1243_ramadoss.htm)
#         2. Mid September to first of November
#         3. Early rabi: 5-20 October (Harinath and Vasanthi 1998 http://www.indianjournals.com/ijor.aspx?target=ijor:ijpp1&volume=26&issue=2&article=001)
#         4. Normal rabi: 5 November (Harinath and Vasanthi 1998 http://www.indianjournals.com/ijor.aspx?target=ijor:ijpp1&volume=26&issue=2&article=001)

ncFile <- paste(bDir,"/climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
ydDir <- paste(bDir,"/GLAM/climate-signals-yield/GNUT/raster/gridded",sep="")

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

#plot(msk)
#text(x=pCells$X,y=pCells$Y,labels=pCells$CELL,cex=0.35)

year <- 1966
nd <- leap(year)
cell <- 565
x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]

#loop and extract days
for (d in 1:nd) {
  cat(d," ")
  ncPos <- findNCPos(year,d)
  rs <- raster(ncFile,band=ncPos)
  out_row <- data.frame(DAY=d,RAIN=extract(rs,cbind(X=x,Y=y)))
  if (d==1) {
    out_all <- out_row
  } else {
    out_all <- rbind(out_all,out_row)
  }
}
cat("\n")

plot(out_all$DAY,out_all$RAIN,ty="l")

#i need to first calculate the potential evapotranspiration. I will use the Priestley-Taylor equation
#main references:
#                 *Weis and Menzel (2008)
#                 *Challinor et al. (2004)
#

#need to load monthly temperature data
#need to load monthly solar radiation data
#scale them both to daily using (a) a linear function between monthly middle days, or 
#                               (b) a sinusoidal fit between monthly middle days



#function to create mask from yield adn met grids
maskCreate <- function(met,yld) {
  #create mask with pixels that are not NA in both
  met[which(!is.na(met[]))] <- 1
  yld[which(!is.na(yld[]))] <- 1
  
  #get coordinates and values of rasters
  cellMx <- data.frame(CELL=1:ncell(yld))
  cellMx$X <- xFromCell(yld,cellMx$CELL); cellMx$Y <- yFromCell(yld,cellMx$CELL)
  cellMx$VALS.YLD <- extract(yld,cbind(X=cellMx$X,Y=cellMx$Y))
  cellMx$VALS.MET <- extract(met,cbind(X=cellMx$X,Y=cellMx$Y))
  cellMx$VALS.OUT <- 1
  cellMx$VALS.OUT[which(is.na(cellMx$VALS.YLD) | is.na(cellMx$VALS.MET))] <- NA
  
  #assign mask data
  msk <- raster(yld)
  msk[cellMx$CELL] <- cellMx$VALS.OUT
  return(msk)
}


#find out what is the continous date that i refer to in the nc file
findNCPos <- function(thisYr,thisDay) {
  tropMet_iyr <- 1960
  counter <- 0
  
  for (yr in tropMet_iyr:thisYr) {
    if (yr<thisYr) {
      nd <- leap(yr)
      for (j in 1:nd) {
        counter <- counter+1
      }
    } else {
      for (j in 1:thisDay) {
        counter <- counter+1
      }
    }
  }
  return(counter)
}



