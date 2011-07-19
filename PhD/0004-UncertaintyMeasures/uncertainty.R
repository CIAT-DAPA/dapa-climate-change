#Extract GCM anomaly values for a given point and do density plots
#F:\climate_change\IPCC_CMIP3\SRES_A1B\anomalies\bccr_bcm2_0\2010_2039
library(raster)
rm(list=ls()); g<-gc(T)

extractGCMData <- function(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963) {
	
	#Defining input directory
	sres <- paste("SRES_", sres, sep="")
	sresDir <- paste(bDir, "/", sres, "/anomalies", sep="")
	
	#Listing the GCMs
	gcmList <- list.files(sresDir)
	
	#Defining if dividing according to variable
	if (variable == "prec") {
		divide <- F
	} else {
		divide <- T
	}
	
	#Create data frame for target site
	xy <- data.frame(X=x, Y=y)
	
	for (gcm in gcmList) {
		cat("Processing model", gcm, "\n")
		dataDir <- paste(sresDir, "/", gcm, "/", period, sep="")
		
		#Looping through months to extract data, for those cases in which more than 1 month is specified
		if (length(month) > 1) {
			for (mth in month) {
				#Defining and reading first raster
				in.raster <- paste(dataDir, "/", variable, "_", mth, ".asc", sep="")
				in.rs <- raster(in.raster)
				
				#Creating list for stack
				if (mth == month[1]) {
					in.stk <- in.rs
				} else {
					in.stk <- c(in.stk,in.rs)
				}
			}
			
			#Now creating stack and calculating mean/total
			in.stk <- stack(in.stk)
      
			#Create output raster by dividing or keeping the sum of the values
			out.rs <- sum(in.stk)
			if (divide) {out.rs <- out.rs / length(month)}
			
		} else {
			in.raster <- paste(dataDir, "/", variable, "_", mth, ".asc", sep="")
			out.rs <- raster(in.raster)
		}
		
		#Now extract the values of the specified cell
		value <- extract(out.rs, xy)
		
		if (gcm == gcmList[1]) {
			value.list <- value
		} else {
			value.list <- c(value.list, value)
		}
	}
	return (value.list)
}

#Shannon's entropy function, vector is first discretised
shannon.e <- function(pix.data, ndiv=8) {
  #Calculate range and interval
  pix.range <- max(pix.data) - min(pix.data)
  int.lg <- pix.range / ndiv
  
  #generate breaks (equally distant) and calculate probabilities for each break
  for (d in 1:ndiv) {
    if (d == 1) {
      ll <- min(pix.data)
      ul <- ll+int.lg
      ncases <- length(which(pix.data >= ll & pix.data < ul))
      hi <- (ncases/length(pix.data))*log(ncases/length(pix.data))
    } else if (d == ndiv) {
      ll <- ul
      ul <- max(pix.data)
      ncases <- length(which(pix.data >= ll & pix.data <= ul))
      hi <- (ncases/length(pix.data))*log(ncases/length(pix.data))
    } else {
      ll <- ul
      ul <- ll+int.lg
      ncases <- length(which(pix.data >= ll & pix.data <= ul))
      hi <- (ncases/length(pix.data))*log(ncases/length(pix.data))
    }
    
    #cat("Interval", d, "from", ll, "to", ul, "\n")
    #cat(ncases, "cases \n")
    
    if (d == 1) {
      classes <- data.frame(INT=d, LL=ll, UL=ul, CASES=ncases)
      H <- hi
    } else {
      classes <- rbind(classes, c(d,ll,ul,ncases))
      H <- c(H,hi)
    }
    
  }
  H.total <- -(sum(H,na.rm=T))
  
  return(list(CLASSES=classes, Hvect=H, H=H.total))
}

wd <- "C:/CIAT_work/CCAFS/gcm-uncertainties" #"F:/PhD-work/climate-data-assessment/gcm-uncertainties"
setwd(wd)

#Loading site list
sites <- read.csv("sites.csv")

#Extracting data for each site
i <- 1
for (site in sites$Site) {
  cat("\n")
  cat("Processing", paste(site), "\n")
  
  #Extracting site details
  ctry <- sites$Country[which(sites$Site == site)]
  loc.x <- sites$x[which(sites$Site == site)]
  loc.y <- sites$y[which(sites$Site == site)]
  
  #Extracting site precipitation anomalies
  assign(paste("prec.data.",i,sep=""),extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=1:12, sres="A1B", x=loc.x, y=loc.y))
  assign(paste("prec.h.",i,sep=""),shannon.e(get(paste("prec.data.",i,sep="")), ndiv=8)$H)
  prec.data <- get(paste("prec.data.",i,sep=""))
  prec.h <- get(paste("prec.h.",i,sep=""))
  
  #Extracting site temperature anomalies
  assign(paste("temp.data.",i,sep=""),extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="tmean", period="2020_2049", month=1:12, sres="A1B", x=loc.x, y=loc.y))
  assign(paste("temp.h.",i,sep=""),shannon.e(get(paste("temp.data.",i,sep="")), ndiv=8)$H)
  temp.data <- get(paste("temp.data.",i,sep=""))
  temp.h <- get(paste("temp.h.",i,sep=""))
  
  #Calculating model agreement
  nposit <- length(which(prec.data > 0))
  nnegat <- length(which(prec.data < 0))
  if (mean(prec.data) < 0) {ag.prec <- nnegat/length(prec.data)} else {ag.prec <- nposit/length(prec.data)}
  
  #Putting metrics together into a data frame
  out.data <- data.frame(
    Country=ctry, 
    Site=site, 
    x=loc.x, 
    y=loc.y, 
    Mean.prec=mean(prec.data), 
    Max.prec=max(prec.data),
    Min.prec=min(prec.data),
    Range.prec=max(prec.data)-min(prec.data),
    StD.prec=sd(prec.data), 
    CV.prec=sd(prec.data)/mean(prec.data)*100, 
    AG.prec=ag.prec,
    H.prec=prec.h,
    Mean.temp=mean(temp.data), 
    Max.temp=max(temp.data),
    Min.temp=min(temp.data),
    Range.temp=max(temp.data)-min(temp.data),
    StD.temp=sd(temp.data), 
    CV.temp=sd(temp.data)/mean(temp.data)*100, 
    H.temp=temp.h)
  
  if (site == sites$Site[1]) {
    out.all <- out.data
    prec.out.extracted <- prec.data
    temp.out.extracted <- temp.data
  } else {
    out.all <- rbind(out.all, out.data)
    prec.out.extracted <- rbind(prec.out.extracted,prec.data)
    temp.out.extracted <- rbind(temp.out.extracted,temp.data)
  }
  
  i <- i+1
}

write.csv(out.all,"sites-data.csv",quote=F,row.names=F)
write.csv(prec.out.extracted,"prec.data.csv",quote=F,row.names=F)
write.csv(temp.out.extracted,"temp.data.csv",quote=F,row.names=F)

################################################
############Plotting precipitation
################################################
#Determining limits for precipitation
#Now plotting
for (j in 1:nrow(sites)) {
  plot.data <- as.numeric(prec.out.extracted[j,])
  pd <- density(plot.data)
  
  x.norm <- (pd$x - mean(pd$x))/(sd(pd$x))
  y.norm <- (pd$y - mean(pd$y))/(sd(pd$y)) #pd$y / max(pd$y)
  
  if (j == 1) {
    x.norm.all <- x.norm
    y.norm.all <- y.norm
  } else {
    x.norm.all <- c(x.norm.all,x.norm)
    y.norm.all <- c(y.norm.all,y.norm)
  }
  
}
xlims <- c(min(x.norm.all),max(x.norm.all))
ylims <- c(min(y.norm.all),max(y.norm.all))

cols <- c("black","red","blue","dark green","gray","purple","orange")
#Now plotting precipitation
tiff("prec-uncertainties.tif", pointsize=6, width=1000, height=1000, units="px", compression="lzw",bg="white",res=300)
for (j in 1:nrow(sites)) {
  plot.data <- as.numeric(prec.out.extracted[j,])
  pd <- density(plot.data)
  
  x.norm <- (pd$x - mean(pd$x))/(sd(pd$x))
  y.norm <- (pd$y - mean(pd$y))/(sd(pd$y)) #pd$y / max(pd$y)
  
  if (j == 1) {
    plot(x.norm,y.norm,type="l",col=cols[j],xlab="Normalised delta",ylab="Normalised probability",xlim=xlims,ylim=ylims)
    lines(x.norm,y.norm,col=cols[j],lw=2)
  } else {
    lines(x.norm,y.norm,col=cols[j],lw=2)
  }
  
}

legend(0.8,1,sites$Site,cex=0.8,col=cols,pch=NA,lty=1)
dev.off()

#############################################
###########Now for temperature
#############################################
#Determining limits for temperature
#Now plotting
for (j in 1:nrow(sites)) {
  plot.data <- as.numeric(temp.out.extracted[j,])
  pd <- density(plot.data)
  
  x.norm <- (pd$x - mean(pd$x))/(sd(pd$x))
  y.norm <- (pd$y - mean(pd$y))/(sd(pd$y)) #pd$y / max(pd$y)
  
  if (j == 1) {
    x.norm.all <- x.norm
    y.norm.all <- y.norm
  } else {
    x.norm.all <- c(x.norm.all,x.norm)
    y.norm.all <- c(y.norm.all,y.norm)
  }
  
}
xlims <- c(min(x.norm.all),max(x.norm.all))
ylims <- c(min(y.norm.all),max(y.norm.all))

cols <- c("black","red","blue","dark green","gray","purple","orange")
#Now plotting temperature
tiff("temp-uncertainties.tif", pointsize=6, width=1000, height=1000, units="px", compression="lzw",bg="white",res=300)
for (j in 1:nrow(sites)) {
  plot.data <- as.numeric(temp.out.extracted[j,])
  pd <- density(plot.data)
  
  x.norm <- (pd$x - mean(pd$x))/(sd(pd$x))
  y.norm <- (pd$y - mean(pd$y))/(sd(pd$y)) #pd$y / max(pd$y)
  
  if (j == 1) {
    plot(x.norm,y.norm,type="l",col=cols[j],xlab="Normalised delta",ylab="Normalised probability",xlim=xlims,ylim=ylims)
    lines(x.norm,y.norm,col=cols[j],lw=2)
  } else {
    lines(x.norm,y.norm,col=cols[j],lw=2)
  }
  
}

legend(0.8,1,sites$Site,cex=0.8,col=cols,pch=NA,lty=1)
dev.off()
