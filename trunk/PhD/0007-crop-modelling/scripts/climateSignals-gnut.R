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

# plot(msk)
# text(x=pCells$X,y=pCells$Y,labels=pCells$CELL,cex=0.35)

cell <- 959 #565

for (year in y_iyr:y_eyr) {
  cat("\nProcessing year",year,"\n")
  nd <- leap(year)
  x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]
  
  out_all <- extractDaily(ncFile,x,y,year,nd)
  
  #i need to first calculate the potential evapotranspiration. I will use the Priestley-Taylor equation
  #main references:
  #                 *Weis and Menzel (2008)
  #                 *Challinor et al. (2004)
  #
  
  #need to load monthly temperature data
  #read 14 months
  tmin_stk <- stack(c(paste(tempDir,"/monthly_grids/tmn/tmn_",(year-1),"_12",sep=""),
                    paste(tempDir,"/monthly_grids/tmn/tmn_",year,"_",1:12,sep=""),
                    paste(tempDir,"/monthly_grids/tmn/tmn_",(year+1),"_1",sep="")))
  tmin_vals <- extract(tmin_stk,cbind(X=x,Y=y))*0.1
  daily_tmin <- linearise(tmin_vals)[16:(nd+15)] #interpolate to daily
  out_all$TMIN <- daily_tmin #put data into matrix
  
  tmax_stk <- stack(c(paste(tempDir,"/monthly_grids/tmx/tmx_",(year-1),"_12",sep=""),
                    paste(tempDir,"/monthly_grids/tmx/tmx_",year,"_",1:12,sep=""),
                    paste(tempDir,"/monthly_grids/tmx/tmx_",(year+1),"_1",sep="")))
  tmax_vals <- extract(tmax_stk,cbind(X=x,Y=y))*0.1
  daily_tmax <- linearise(tmax_vals)[16:(nd+15)] #interpolate to daily
  out_all$TMAX <- daily_tmax #put data into matrix
  
  #load monthly solar radiation data
  srad_stk <- stack(c(paste(sradDir,"/srad/srad_","12.asc",sep=""),
                    paste(sradDir,"/srad/srad_",1:12,".asc",sep=""),
                    paste(sradDir,"/srad/srad_","1.asc",sep="")))
  srad_vals <- extract(srad_stk,cbind(X=x,Y=y))
  daily_srad <- linearise(srad_vals)[16:(nd+15)] #interpolate to daily
  out_all$SRAD <- daily_srad*60*60*24/1000000 #put into matrix and convert from W/m^2 to MJ/m^2/day
  
  #Calculate the water balance
  out_all$ETMAX <- NA; out_all$AVAIL <- NA; out_all$ERATIO <- NA
  out_all$CUM_RAIN <- NA; out_all$RUNOFF <- NA; out_all$DEMAND <- NA
  
  #calculate water balance stuff
  out_all <- watbal_wrapper(out_all)
  
  #calculate growing seasons
  gs <- gsl_find(out_all$ERATIO,ea_thresh=0.5,n_start=5,n_end=12,sd_default=165,ed_default=225)
  gs$GSL <- gs$END-gs$START
  
  # plot(out_all$DAY,out_all$RAIN,ty="l")
  # plot(out_all$DAY,out_all$TMIN,ty="l")
  # plot(out_all$DAY,out_all$TMAX,ty="l")
  # plot(out_all$DAY,out_all$SRAD,ty="l")
  # plot(out_all$DAY,out_all$ERATIO,ty="l"); abline(h=0.35,col="red",lty=2); abline(h=0.5,col="red")
  # plot(out_all$DAY,out_all$RUNOFF,ty="l")
  # plot(out_all$DAY,out_all$AVAIL,ty="l")
  # plot(out_all$DAY,out_all$ETMAX,ty="l")
  # plot(out_all$DAY,out_all$CUM_RAIN,ty="l")
  # plot(out_all$DAY,out_all$DEMAND,ty="l")
  # plot(out_all$DAY,out_all$ERATIO,ty="l",ylim=c(-0.05,1)); abline(h=0.35,col="red",lty=2); abline(h=0.5,col="red")
  
  # for (i in 1:nrow(gs)) {
  #   lines(x=gs[i,1:2],y=c(-0.025,-0.025),lwd=2,col="blue")
  # }
  
  #select the longest growing season
  gs_sel <- gs[which(gs$GSL==max(gs$GSL)),]
  gs_data <- gs_metrics(out_all,gs_sel,thresh=0.5,tbase=10,topt=28,tmax=50,tcrit=34,tlim=40)
  gs_data <- cbind(YEAR=year,gs_data)
  
  if (year==y_iyr) {
    
  } else {
    
  }
}
