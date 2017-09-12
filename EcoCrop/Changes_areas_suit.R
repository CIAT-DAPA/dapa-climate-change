## extracting current crop suitability by region, identifying losses in areas that are currently suitable,
## indentifying total change in suitability by region.
#load packages
require(raster)
library(rgdal)
library(maptools)
require(stringr)

base_dir <-"//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/Additional/Cooper"

shp_dir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/Additional/Cooper/regions_grid/extent_fut_climate"
fut_dir <- paste(base_dir,"/Future_Suit/Fin/rcp_60/analyses/runs-future/",sep="") #ecocrop future runs
current_dir = paste(base_dir, "/Current_Suit/analyses/runs/", sep="")

#crops
crops=list.files(current_dir)
crops=crops[grep("_suitability.tif",crops)]
crops=crops[-c(7:8)] #removing high and low altitude sorghum. they have already been modified somehow


#models
models=list.files(fut_dir)

#thresholds
thresh=cbind(crops, c(0.18,0.55,0.38,0.48,0.80,0.54,0.59,0.37)*100)#Change this for each crp to necces threshold

## snapping and masking EcoCrop results
current=raster(paste0(current_dir,crops[1]))
future=raster(paste0(fut_dir,models[1],"/crop",crops[1]))
#future=resample(future,current,method="ngb") ####Resample the rasters as they have diff extents. This puts them ontop of eachother and sometimes current does not have a value where future does, so then nearest neighbour interp is used
#mask=(future*current)>=0 #Tells R to show just those areas where Future and Current are pres and to create a mask 

##mask by region
region = list.files(paste(shp_dir), full.names=TRUE)
region = region[grep(".tif",region)]
region = lapply(region, FUN=raster)
region = stack(region)
region[[1]][!is.na(region[[1]][])]=1
region[[2]][!is.na(region[[2]][])]=2
region[[3]][!is.na(region[[3]][])]=3
region[[4]][!is.na(region[[4]][])]=4
region[[5]][!is.na(region[[5]][])]=5
region=merge(region)
region=resample(region,mask,method="ngb")
region=mask*region

mask=region>=0


##loop from here
res=list() #list for the 3 measurements
res[[1]] = matrix(ncol = 8, nrow = 19, dimnames=list(models, crops))# making a dataframes that we will fill, 8 rops 19 gcms
res[[2]] = matrix(ncol = 8, nrow = 19, dimnames=list(models, crops))
res[[3]] = matrix(ncol = 8, nrow = 19, dimnames=list(models, crops))

ls=list() #list for the 5 regions which has the 3 above results 
ls[[1]] = res
ls[[2]] = res
ls[[3]] = res
ls[[4]] = res
ls[[5]] = res

for (c in 1:length(crops)){
  cat(paste("Processing : ",crops[c], "\n", sep=""))
  
  current=raster(paste0(current_dir,crops[c]))
  
  for (m in 1:length(models)){
    cat(paste("Processing : ",models[m], "\n", sep=""))
    
    future=raster(paste0(fut_dir,models[m],"/crop",crops[c]))
    ##snapping and masking raw data
    future=resample(future,mask,method="ngb")
    future=future*mask #snap future rasters to the mask (ie mask created above) do not need to snap current as was used to create the mask and so has same extent
    current=current*mask
    
    ##applying the threshold
    suit_now=current   >= as.numeric(thresh[c,2]) #Tells R to extract jsut areas => to threshold
    suit_future=future >= as.numeric(thresh[c,2]) #suit_future = 1
    
    ##keying the results
    suit_now[suit_now>0]=10 #suit_now = 10
    
    combined = suit_now+suit_future #assigning key values i.e. 0, 1, 10, 11
    
    ####################
    for(i in unique(region)){
      id_region=region
      id_region[!id_region==i]=NA #Tells R to give all regions except specified region NA
      id_region[!is.na(id_region)]=1 #Gives each region a value of 1 
      vals=freq(combined*id_region)
      
      if("0"  %in% vals[,1]){ID_0  =as.numeric(vals[which(vals[,1]==0 ),2])} else {ID_0  = 0}#This calculates and assigns the respective codes to the regions- 0 = Never suit. 1= Suit Future. 10=Suit new/ Future no. 11=new and suit in the future
      if("1"  %in% vals[,1]){ID_1  =as.numeric(vals[which(vals[,1]==1 ),2])} else {ID_1  = 0}#%in% similar function to grep
      if("10" %in% vals[,1]){ID_10 =as.numeric(vals[which(vals[,1]==10),2])} else {ID_10 = 0}
      if("11" %in% vals[,1]){ID_11 =as.numeric(vals[which(vals[,1]==11),2])} else {ID_11 = 0}
      
      tot_pix=ID_0 + ID_1 + ID_10 + ID_11
      
      ls[[i]][[1]][m,c] = ID_10/(ID_10+ID_11)*100   #calc  Future suit same pixels as current #ls is the empty list or dataframe to fill, 5 in total (1 for each regio) i is region, 1 is variable, m is model and c is crop
      ls[[i]][[2]][m,c] = (ID_10+ID_11)/tot_pix*100 #Calc number of pixels with current suit
      ls[[i]][[3]][m,c] = (ID_1+ID_11)/tot_pix*100  #Calc number of pixels with futu suit
    }
  }
}

saved=ls

###########################################################################################################################

region_code=c("CAF","EAF","SAF","SAF","WAF")
crop_names=c("Banana","Cassava","Common bean","Finger millet","Ground nut","Pearl millet","Sorghum","Yam")

##percentage loss of currently suitable areas
for(m in 1:length(region_code)){
  res = ls[[m]][[1]]
  #(results[m], header=TRUE)
  #res = res[,2:9]
  par(mar=c(5, 7, 4, 2) + 0.1) # c(bottom, left, top, right) ##Sets the distance of the boxes from the frame
  boxplot(res, use.cols = TRUE, horizontal = TRUE, ylab = "", #expression(bold(Crops))
          xlab = expression(bold(Percentage~loss~of~currently~suitable~areas)),axes=F,frame.plot=TRUE,ylim=c(0,100)) #Tells axis is F, and then adds it later
  axis(1)
  axis(2, labels = crop_names, at=seq(1,8,1), las=1) #at=seq(1,8,1), ##las= 1 Tells R to put the labels horizantal for the y axis.. Y axis (axis 2) in R always
  abline(v=c(seq(20,80,20)), col="gray", lty=2) ##Tells R to giveit horizantal dashed line at from -30 - 20 at intervals of 10
  #title(main = paste("Suitability Change (%) until 2050s", sub = "under RCP 6.0"))
  legend("topright",legend=paste(region_code[m]),box.col="black",cex=0.9) #This prints for each boxplot the corresponding region
}

## Percentage change in suitable area
for(m in 1:length(region_code)){
  res = ls[[m]][[3]]-ls[[m]][[2]] #3 and 2 Refer to the calculations. Future areas - current areas 
  #(results[m], header=TRUE)
  #res = res[,2:9]
  par(mar=c(5, 7, 4, 2) + 0.1) # c(bottom, left, top, right) ##Sets the distance of the boxes from the frame
  boxplot(res, use.cols = TRUE, horizontal = TRUE, ylab = "", #expression(bold(Crops))
          xlab = expression(bold("Percentage change in suitable area")),axes=F,frame.plot=TRUE,ylim=c(-40,40)) #Tells axis is F, and then adds it later
  axis(1)
  axis(2, labels = crop_names, at=seq(1,8,1), las=1) #at=seq(1,8,1), ##las= 1 Tells R to put the labels horizantal for the y axis.. Y axis (axis 2) in R always
  abline(v=c(seq(-40,40,20)), col="gray", lty=2) ##Tells R to giveit horizantal dashed line at from -30 - 20 at intervals of 10
  #title(main = paste("Suitability Change (%) until 2050s", sub = "under RCP 6.0"))
  legend("topright",legend=paste(region_code[m]),box.col="black",cex=0.9) #This prints for each boxplot the corresponding region
}



pos_legend=c("topleft","topright","topright","topright","topleft")
## percentage future suitable area referenced with current (red)
for(m in 1:length(region_code)){
  res = ls[[m]][[3]]
  #(results[m], header=TRUE)
  #res = res[,2:9]
  par(mar=c(5, 7, 4, 2) + 0.1) # c(bottom, left, top, right) ##Sets the distance of the boxes from the frame
  boxplot(res, use.cols = TRUE, horizontal = TRUE, ylab = "", #expression(bold(Crops))
          xlab = expression(bold("Percentage of future suitable area")),axes=F,frame.plot=TRUE,ylim=c(0,100)) #Tells axis is F, and then adds it later
  res = ls[[m]][[2]]
  boxplot(res, use.cols = TRUE, horizontal = TRUE,border="red", ylab = "", #expression(bold(Crops))
          xlab = expression(bold("Percentage of future suitable area")),axes=F,frame.plot=TRUE,ylim=c(0,100),add=T) #Tells axis is F, and then adds it later
  
  axis(1)
  axis(2, labels = crop_names, at=seq(1,8,1), las=1) #at=seq(1,8,1), ##las= 1 Tells R to put the labels horizantal for the y axis.. Y axis (axis 2) in R always
  abline(v=c(seq(0,100,20)), col="gray", lty=2) ##Tells R to giveit horizantal dashed line at from -30 - 20 at intervals of 10
  #title(main = paste("Suitability Change (%) until 2050s", sub = "under RCP 6.0"))
  legend(pos_legend[m],legend=paste(region_code[m]),box.col="black",cex=0.9) #This prints for each boxplot the corresponding region
}

dput(ls)
