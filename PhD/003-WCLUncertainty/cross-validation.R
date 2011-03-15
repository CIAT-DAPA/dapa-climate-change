#Julian Ramirez
#March 2011

require(raster)
require(fields)
require(foreign)

#Base directory and stuff
bDir <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties"
variable <- "rain" #tean,tmin,tmax
oDir <- paste(bDir, "/outputs/", variable, sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#Cross-validation and tile-division data
ntiles <- 5
overlap <- 1000
nfolds <- 1
train.per <- 0.85
test.per <- 0.15

#Define extent of analyses
extent <- data.frame(xn=-18,xx=98,yn=-47.5,yx=40.5)
extent$xn <- extent$xn-10; extent$yn <- extent$yn-10
extent$xx <- extent$xx+10; extent$yx <- extent$yx+10

#Loading dataset
stDir <- paste(bDir, "/input-data", sep="")
st <- read.dbf(paste(stDir, "/wc_", variable, "_stations.dbf", sep=""))
st <- st[which(st$LONG >= extent$xn & st$LONG <= extent$xx),]
st <- st[which(st$LAT >= extent$yn & st$LAT <= extent$yx),]

#Loading mask
rs <- raster(paste(bDir, "/mask-srtm/srtm30b.asc", sep=""))

#Sort and select tiles
lat.sorted <- order(st$LAT, decreasing=T)
nst.per.tile <- round(nrow(st)/ntiles)

months <- c("JAN") #,"FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

cat("Performing", nfolds, "-fold cross validation \n")
for (fold in 1:nfolds) {
  cat("\n")
  cat("Cross-validation fold", fold, "\n")
  #Defining test and train samples cross validation data
  train <- sample(1:nrow(st), nrow(st)*train.per)
  st$TRAIN_TEST <- NA; st$TRAIN_TEST[train] <- "TRAIN"
  st$TRAIN_TEST[-train] <- "TEST"
  
  for (i in 1:ntiles) {
    #Defining tile and selecting rows
    if (i == 1) {
      inirow <- 1 + nst.per.tile * (i - 1)
      finrow <- nst.per.tile * i + overlap
      upper.lim <- inirow
      lower.lim <- finrow - overlap
    } else if (i == ntiles) {
      inirow <- 1 + nst.per.tile * (i - 1) - overlap
      finrow <- nrow(st)
      upper.lim <- inirow + overlap
      lower.lim <- finrow
    } else {
      inirow <- 1 + nst.per.tile * (i - 1) - overlap
      finrow <- nst.per.tile * i + overlap
      upper.lim <- inirow + overlap
      lower.lim <- finrow - overlap
    }
    lim.rows <- lat.sorted[upper.lim:lower.lim]
    lim.st <- st[lim.rows,]
    lims <- c(i, min(lim.st$LAT), max(lim.st$LAT))
    if (i == 1) {summ.lims <- lims} else {summ.lims <- rbind(summ.lims, lims)}
    
    rows <- lat.sorted[inirow:finrow]
    sel.st <- st[rows,]
    cat("... \n")
    cat("tile:", i, "\n")
    cat("starts:", inirow, "\n")
    cat("ends:", finrow, "\n")
    cat("#rows:", length(rows), "\n")
    cat("... \n")
    
    #Selecting the area on the mask for projection
    if (i == 1) {
      crop.extent <- extent(rs@extent@xmin,rs@extent@xmax,min(sel.st$LAT),rs@extent@ymax)
    } else if (i == ntiles) {
      crop.extent <- extent(rs@extent@xmin,rs@extent@xmax,rs@extent@ymin,max(sel.st$LAT))
    } else {
      crop.extent <- extent(rs@extent@xmin,rs@extent@xmax,min(sel.st$LAT),max(sel.st$LAT))
    }
    projrs <- crop(rs,crop.extent)
    
    #Extracting x,y,alt data for projection
    xyz <- xyFromCell(projrs,1:ncell(projrs))
    xyz <- cbind(xyz,xyValues(projrs,xyz))
    xyz.not.na <- which(!is.na(xyz[,3]))
    xyz.pred <- xyz[xyz.not.na,]
    
    #Looping months
    
    for (m in months) {
      cat("Processing", m, "\n")
      m.col <- which(names(st) == m)
      
      #Month output folder and subfolder
      mDir <- paste(oDir, "/", tolower(m), sep="")
      if (!file.exists(mDir)) {dir.create(mDir)}
      
      evalDir <- paste(mDir, "/evaluation", sep=""); if (!file.exists(evalDir)) {dir.create(evalDir)}
      fitDir <- paste(mDir, "/fits", sep=""); if (!file.exists(fitDir)) {dir.create(fitDir)}
      summDir <- paste(mDir, "/summary", sep=""); if (!file.exists(summDir)) {dir.create(summDir)}
      intDir <- paste(mDir, "/interpolations", sep=""); if (!file.exists(intDir)) {dir.create(intDir)}
      
      #Selecting sets
      fit.st <- sel.st[which(sel.st$TRAIN_TEST == "TRAIN"),]
      test.st <- sel.st[which(sel.st$TRAIN_TEST == "TEST"),]
      
      #Fitting the surface
      x <- cbind(fit.st$LONG, fit.st$LAT, fit.st$ALT)
      y <- fit.st[,m.col]
      #cat("Fitting", date(), "\n")
      fit <- Tps(x,y,m=2,p=NULL,scale.type="range")
      #cat("Fitted", date(), "\n")
      
      fit.vals <- as.data.frame(cbind(fit$fitted.values,fit$y)); names(fit.vals) <- c("fitted","measured")
      write.csv(fit.vals, paste(fitDir, "/fitted-tile-", i, "-fold-", fold, ".csv", sep=""), row.names=F, quote=F)
      
      #cat("Evaluating \n")
      #Predicting over random test data
      y.meas <- test.st[,m.col]
      x.test <- cbind(test.st$LONG,test.st$LAT,test.st$ALT)
      y.pred <- predict(fit,x.test)
      test.vals <- as.data.frame(cbind(y.pred,y.meas)); names(test.vals) <- c("predicted","measured")
      write.csv(test.vals, paste(evalDir, "/eval-tile-", i, "-fold-", fold, ".csv", sep=""), row.names=F, quote=F)
      
      #Calculate rsq and rmse
      corr <- cor.test(y.meas,y.pred,method="pearson")
      rsq <- corr$estimate ^ 2
      pval <- corr$p.value
      
      rmse <- y.meas - y.pred; rmse <- rmse ^ 2
      rmse <- sqrt(sum(rmse) / length(y.meas))
      
      out.stats <- data.frame(FOLD=fold, MONTH=m, TILE=i, R2=rsq, P.VAL=pval, RMSE=rmse)
      if (fold == 1 & i == 1 & m == "JAN") {cv.summ <- out.stats} else {cv.summ <- rbind(cv.summ, out.stats)}
      
      #Projection
      #cat("Projecting", date(), "\n")
      out.pred <- predict(fit,xyz.pred)
      #cat("Projected", date(), "\n")
      outrs <- projrs
      outrs[xyz.not.na] <- out.pred
      outrs <- writeRaster(outrs, paste(intDir, "/tile-", i, "-fold-", fold, ".asc", sep=""), format='ascii')
    }
  }
  summ.lims <- as.data.frame(summ.lims); names(summ.lims) <- c("TILE","LAT.MIN", "LAT.MAX")
  write.csv(summ.lims, paste(summDir, "/limits.csv", sep=""), row.names=F, quote=F)
}
write.csv(cv.summ, paste(summDir, "/accuracy.csv", sep=""), row.names=F, quote=F)
