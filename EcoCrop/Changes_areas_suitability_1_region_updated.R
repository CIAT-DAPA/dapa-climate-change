## extracting current crop suitability by region, identifying losses in areas that are currently suitable,
## indentifying total change in suitability by region.
## load packages
require(raster);require(rgdal);require(maptools);require(stringr)

### Directories
base_dir <-"D:/Backup_JG/Bounderlands/EcoCrop/ouput/Colombia"
fut_dir <- paste(base_dir,"/2050/",sep="") #ecocrop future runs
current_dir = paste(base_dir, "/Current/", sep="")

crops=list.files(current_dir, pattern="_suitability.tif")
models=list.files(fut_dir)
models=models[-grep("_suitability.tif",models)]

#thresholds
thresh=cbind(crops, c(0.50,0.50,0.50,0.50,0.50,0.50)*100)#Change this for each crop to necces threshold

## snapping and masking EcoCrop results
current=raster(paste0(current_dir,crops[1]))
future=raster(paste0(fut_dir,models[1],"/",crops[1]))

##loop from here
res=list() #list for the 3 measurements
res[[1]] = matrix(ncol = 6, nrow = 19, dimnames=list(models, crops))# making a dataframes that we will fill, 6 crops 19 gcms
res[[2]] = matrix(ncol = 6, nrow = 19, dimnames=list(models, crops))
res[[3]] = matrix(ncol = 6, nrow = 19, dimnames=list(models, crops))

ls=list() #list for the 5 regions which has the 3 above results 
ls[[1]] = res

for (c in 1:length(crops)){
  cat(paste("Processing : ",crops[c],"\n", sep=""))
  current=raster(paste0(current_dir,crops[c]))
  
  for (m in 1:length(models)){
    cat(paste("Processing : ",models[m], "\n", sep=""))
    
    future=raster(paste0(fut_dir,models[m],"/",crops[c]))
        
    ##applying the threshold
    suit_now=current   >= as.numeric(thresh[c,2]) #Tells R to extract jsut areas => to threshold
    suit_future=future >= as.numeric(thresh[c,2]) #suit_future = 1
    
    ##keying the results
    suit_now[suit_now>0]=10 #suit_now = 10
    combined = suit_now+suit_future #assigning key values i.e. 0, 1, 10, 11
    vals=freq(combined)
    
    if("0"  %in% vals[,1]){ID_0  =as.numeric(vals[which(vals[,1]==0 ),2])} else {ID_0  = 0}#This calculates and assigns the respective codes to the regions- 0 = Never suit. 1= Suit Future. 10=Suit new/ Future no. 11=new and suit in the future
    if("1"  %in% vals[,1]){ID_1  =as.numeric(vals[which(vals[,1]==1 ),2])} else {ID_1  = 0}#%in% similar function to grep
    if("10" %in% vals[,1]){ID_10 =as.numeric(vals[which(vals[,1]==10),2])} else {ID_10 = 0}
    if("11" %in% vals[,1]){ID_11 =as.numeric(vals[which(vals[,1]==11),2])} else {ID_11 = 0}
    
    tot_pix=ID_0 + ID_1 + ID_10 + ID_11
    
    ls[[1]][[1]][m,c] = ID_10/(ID_10+ID_11)*100   #calc  Future suit same pixels as current #ls is the empty list or dataframe to fill, 5 in total (1 for each regio) i is region, 1 is variable, m is model and c is crop
    ls[[1]][[2]][m,c] = (ID_10+ID_11)/tot_pix*100 #Calc number of pixels with current suit
    ls[[1]][[3]][m,c] = (ID_1+ID_11)/tot_pix*100  #Calc number of pixels with futu suit
  }
}
saved=ls

### Graphics to EcoCrop

region_code=c("Colombia_Nariño")
crop_names=c("café","caña","frijol","maiz","platano","yuca")
pos_legend=c("topleft","topright","topright","topright","topleft")

### Percentage loss of currently suitable areas
  res = ls[[1]][[1]]
  par(mar=c(5, 7, 4, 2) + 0.1) # c(bottom, left, top, right) ##Sets the distance of the boxes from the frame
  boxplot(res, use.cols = TRUE, horizontal = TRUE, ylab = "", #expression(bold(Crops))
          xlab = expression(bold(Percentage~loss~of~currently~suitable~areas)),axes=F,frame.plot=TRUE,ylim=c(0,100)) #Tells axis is F, and then adds it later
  axis(1)
  axis(2, labels = crop_names, at=seq(1,6,1), las=1) #at=seq(1,8,1), ##las= 1 Tells R to put the labels horizantal for the y axis.. Y axis (axis 2) in R always
  abline(v=c(seq(20,80,20)), col="gray", lty=2) ##Tells R to giveit horizantal dashed line at from -30 - 20 at intervals of 10
  title(main = paste("Suitability Change (%) until 2050s", sub = "under CIMP3 SRSA2"))
  legend("topright",legend=paste(region_code[1]),box.col="black",cex=0.9) #This prints for each boxplot the corresponding region

### Percentage change in suitable area

  res = ls[[1]][[3]]-ls[[1]][[2]] #3 and 2 Refer to the calculations. Future areas - current areas 
  par(mar=c(5, 7, 4, 2) + 0.1) # c(bottom, left, top, right) ##Sets the distance of the boxes from the frame
  boxplot(res, use.cols = TRUE, horizontal = TRUE, ylab = "", #expression(bold(Crops))
          xlab = expression(bold("Percentage change in suitable area")),axes=F,frame.plot=TRUE,ylim=c(-40,40)) #Tells axis is F, and then adds it later
  axis(1)
  axis(2, labels = crop_names, at=seq(1,6,1), las=1) #at=seq(1,8,1), ##las= 1 Tells R to put the labels horizantal for the y axis.. Y axis (axis 2) in R always
  abline(v=c(seq(-40,40,20)), col="gray", lty=2) ##Tells R to giveit horizantal dashed line at from -30 - 20 at intervals of 10
  title(main = paste("Suitability Change (%) until 2050s", sub = "under CIMP3 SRSA2"))
  legend("topright",legend=paste(region_code[1]),box.col="black",cex=0.9) #This prints for each boxplot the corresponding region

## percentage future suitable area referenced with current (red)

res = ls[[1]][[3]]
par(mar=c(5, 7, 4, 2) + 0.1) # c(bottom, left, top, right) ##Sets the distance of the boxes from the frame
boxplot(res, use.cols = TRUE, horizontal = TRUE, ylab = "", #expression(bold(Crops))
        xlab = expression(bold("Percentage of future suitable area")),axes=F,frame.plot=TRUE,ylim=c(0,100)) #Tells axis is F, and then adds it later
res = ls[[1]][[2]]
boxplot(res, use.cols = TRUE, horizontal = TRUE,border="red", ylab = "", #expression(bold(Crops))
        xlab = expression(bold("Percentage of future suitable area")),axes=F,frame.plot=TRUE,ylim=c(0,100),add=T) #Tells axis is F, and then adds it later

axis(1)
axis(2, labels = crop_names, at=seq(1,6,1), las=1) #at=seq(1,8,1), ##las= 1 Tells R to put the labels horizantal for the y axis.. Y axis (axis 2) in R always
abline(v=c(seq(0,100,20)), col="gray", lty=2) ##Tells R to giveit horizantal dashed line at from -30 - 20 at intervals of 10
title(main = paste("Suitability Change (%) until 2050s", sub = "under CIMP3 SRSA2"))
legend(pos_legend[2],legend=region_code,box.col="black",cex=0.9) #This prints for each boxplot the corresponding region

dput(ls)
