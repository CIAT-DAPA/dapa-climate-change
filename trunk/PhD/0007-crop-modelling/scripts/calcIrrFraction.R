#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)
data(wrld_simpl)

#src.dir<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "/home/jramirez/dapa-climate-change/PhD/0007-crop-modelling/scripts"
#src.dir<-"~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#set the working folder
bDir <- "F:/PhD-work/crop-modelling/GLAM"
#bDir <- "/andromeda_data1/jramirez/crop-modelling/GLAM"
#bDir <- "~/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
cd <- paste(bDir,"/climate-signals-yield/",toupper(cropName),sep="")


#series of years
iyr <- 66; fyr <- 94
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}


##############################################################################
##############################################################################

method <- "raw"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
irrDir <- paste(cDir,"/irrigated_area/raster/gridded/",sep="")
harDir <- paste(cDir,"/harvested_area/raster/gridded/",sep="")

outDir <- paste(cDir,"/irrigated_ratio",sep="")
if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}

for (yr in tser) {
  cat("processing year",yr,"\n")
  
  if (!file.exists(paste(outDir,"/",method,"-",yr,".asc",sep=""))) {
    irr <- raster(paste(irrDir,"/",method,"-",yr,".asc",sep=""))
    har <- raster(paste(harDir,"/",method,"-",yr,".asc",sep=""))
    har[which(har[] == 0)] <- 1
    
    #calculate ratio
    ratio <- irr/har
    writeRaster(ratio,paste(outDir,"/",method,"-",yr,".asc",sep=""),format="ascii")
    #windows(); plot(stack(irr,har,ratio))
    #beware that irrigation ratios are in some cases > 1. In such cases just limit to 1
    #that should do
    rm(ratio); g=gc(); rm(g)
  }
}


#plot:
#    1. average, std and cv yield, 
#    2. average area harvested and 
#    3. average irrigation fraction

#loading yearly yields and calculating avg, std, cv
tser2 <- substr(tser,3,4)
for (meth in c("lin","loe","qua","fou")) {
  cat("loading data for method",meth,"\n")
  yldDir <- paste(bDir,"/climate-signals-yield/",toupper(cropName),"/raster/gridded/",meth,sep="")
  ystk <- stack(paste(yldDir,"/",meth,"-",tser2,".asc",sep=""))
  avg <- mean(ystk,na.rm=T)
  std <- calc(ystk, function(x) sd(x,na.rm=T))
  cv <- std/avg*100
  
  if (meth == "lin") {
    all_avg <- avg
    all_std <- std
    all_cv <- cv
  } else {
    all_avg <- c(all_avg,avg)
    all_std <- c(all_std,std)
    all_cv <- c(all_cv,cv)
  }
}

stk_avg <- stack(all_avg)
stk_std <- stack(all_std)
stk_cv <- stack(all_cv)

#load mean gridded area harvested
aha <- stack(paste(harDir,"/",method,"-",tser,".asc",sep=""))
mha <- mean(aha)

#load mean gridded area irrigated
iar <- stack(paste(irrDir,"/",method,"-",tser,".asc",sep=""))
mia <- mean(iar)

#load mean gridded fraction irrigated
ifr <- stack(paste(outDir,"/",method,"-",tser,".asc",sep=""))
mif <- mean(ifr)


#################################################################
#now the plots
imgOutDir <- paste(cDir,"/figures",sep="")
if (!file.exists(imgOutDir)) {dir.create(imgOutDir)}

ht <- 1000
fct <- (mha@extent@xmin-mha@extent@xmax)/(mha@extent@ymin-mha@extent@ymax)
wt <- ht*(fct+.1)

#get colors
cat("Get legend stuff \n")
#brks <- unique(quantile(c(mha[],mia[]),na.rm=T,probs=seq(0,1,by=0.05)))
#brks <- seq(min(c(mha[],mia[]),na.rm=T),max(c(mha[],mia[]),na.rm=T),length.out=100)
brks <- seq(0,max(c(mha[],mia[]),na.rm=T),length.out=50)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("blue","light blue","green","yellow","orange","red"))(length(brks)))
#cols <- c("grey 70",colorRampPalette(c("pink","red"))(length(brks)-2))
#cols <- c(colorRampPalette(c("pink","red"))(10))

rs <- mha
rs[which(rs[] < 15)] <- NA

xyNo <- xyFromCell(mha,which(mha[]<15))
xyNo <- SpatialPoints(xyNo)
pts <- list("sp.points", xyNo, pch = 4, col = "black", cex=0.6, lwd=0.6,first=F)
wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=10), norths=seq(-90,90,by=10))
grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)

##########################################
#harvested area!!!
cat("HA: Now the plot \n")
tiffName <- paste(imgOutDir,"/mean_area_harvested.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
#par(mar=c(3,3,1,3.5))
spplot(mha,sp.layout=list(pts,wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
#grid()
dev.off()

##########################################
#irrigated area!!!
cat("IA: Now the plot \n")
tiffName <- paste(imgOutDir,"/mean_area_irrigated.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(mia,sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


##########################################
#irrigated fraction!!!
cat("IF: Now the plot \n")
xyNo <- xyFromCell(mif,which(mif[] <= 0.0))
xyNo <- SpatialPoints(xyNo)
pts1 <- list("sp.points", xyNo, pch = 4, col = "black", cex=0.6, lwd=0.6,first=F)

xyNo <- xyFromCell(mif,which(mif[] > 0 & mif[] <= 0.01))
xyNo <- SpatialPoints(xyNo)
pts2 <- list("sp.points", xyNo, pch = 4, col = "white", cex=0.6, lwd=0.6,first=F)

xyNo <- xyFromCell(mif,which(mif[] > 0.01 & mif[] <= 0.03))
xyNo <- SpatialPoints(xyNo)
pts3 <- list("sp.points", xyNo, pch = 20, col = "white", cex=0.25, lwd=0.6,first=F)

mifp <- mif
mif[which(mif[] > 1)] <- 1

brks <- seq(0,max(mif[],na.rm=T),length.out=50)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("blue","light blue","green","yellow","orange","red"))(length(brks)))

tiffName <- paste(imgOutDir,"/mean_fraction_irrigated.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(mif,sp.layout=list(wld,pts3,pts2,pts1,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


# pCells <- data.frame(CELL=1:ncell(mif))
# pCells$X <- xFromCell(mif,pCells$CELL); pCells$Y <- yFromCell(mif,pCells$CELL)
# pCells$Z <- extract(mif,cbind(X=pCells$X,Y=pCells$Y))
# pCells <- pCells[which(!is.na(pCells$Z)),]
# windows()
# plot(mif,col="grey 80")
# text(x=pCells$X,y=pCells$Y,labels=round(pCells$Z,3),cex=0.4)


##########################################
#mean yields for different methods!!!
brks <- seq(0,max(c(stk_avg[]),na.rm=T),length.out=50)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("blue","light blue","green","yellow","orange","red"))(length(brks)))

cat("Linear: Now the plot \n")
tiffName <- paste(imgOutDir,"/mean_yield_lin.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_avg[[1]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


cat("Loess: Now the plot \n")
tiffName <- paste(imgOutDir,"/mean_yield_loe.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_avg[[2]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


#qua
cat("Quadratic: Now the plot \n")
tiffName <- paste(imgOutDir,"/mean_yield_qua.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_avg[[3]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


#fou
cat("Fourier: Now the plot \n")
tiffName <- paste(imgOutDir,"/mean_yield_fou.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_avg[[4]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


##########################################
#std yields for different methods!!!
brks <- seq(0,max(c(stk_std[]),na.rm=T),length.out=50)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("pink","red"))(length(brks)))

cat("Linear: Now the plot \n")
tiffName <- paste(imgOutDir,"/stdv_yield_lin.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_std[[1]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


cat("Loess: Now the plot \n")
tiffName <- paste(imgOutDir,"/stdv_yield_loe.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_std[[2]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


#qua
cat("Quadratic: Now the plot \n")
tiffName <- paste(imgOutDir,"/stdv_yield_qua.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_std[[3]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


#fou
cat("Fourier: Now the plot \n")
tiffName <- paste(imgOutDir,"/stdv_yield_fou.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_std[[4]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


##########################################
#cv yields for different methods!!!
brks <- seq(0,max(c(stk_cv[]),na.rm=T),length.out=50)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("pink","red"))(length(brks)))

cat("Linear: Now the plot \n")
tiffName <- paste(imgOutDir,"/cv_yield_lin.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_cv[[1]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


cat("Loess: Now the plot \n")
tiffName <- paste(imgOutDir,"/cv_yield_loe.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_cv[[2]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


#qua
cat("Quadratic: Now the plot \n")
tiffName <- paste(imgOutDir,"/cv_yield_qua.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_cv[[3]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()


#fou
cat("Fourier: Now the plot \n")
tiffName <- paste(imgOutDir,"/cv_yield_fou.tif",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
spplot(stk_cv[[4]],sp.layout=list(wld,grli),col.regions=cols,cuts=50,cex=0.5,
       at=brks,pretty=brks.lab)
dev.off()

