#Julian Ramirez
#CIAT / University of Leeds

#Accuracy metrics function
accuracy <- function(trainMx=NULL, testMx=NULL, variable="rain") {
  
  #Join in one single matrix the ID, LONG, LAT, ALT, real values and pred. values
  if (!is.null(trainMx)) {
    #Loading Interpolation points
    fname <- paste(variable, ".lis", sep="")
    iNames <- c("ID","LONG","LAT","ALT.1000","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT",
    "NOV","DEC","PRED.JAN","PRED.FEB","PRED.MAR","PRED.APR","PRED.MAY","PRED.JUN","PRED.JUL","PRED.AUG",
    "PRED.SEP","PRED.OCT","PRED.NOV","PRED.DEC","ERR.JAN","ERR.FEB","ERR.MAR","ERR.APR","ERR.MAY","ERR.JUN",
    "ERR.JUL","ERR.AUG","ERR.SEP","ERR.OCT","ERR.NOV","ERR.DEC")
    if (variable == "rain") {
      ipts <- read.fortran(fname, format=list(c("I6","A8","F14","2F10"),c("12F8"),c("12F8"),c("12F8"),c("I1")))
    } else if (variable == "tean") {
      ipts <- read.fortran(fname, format=list(c("I6","A10","F11","2F10"),c("12F8"),c("12F8"),c("12F8"),c("I1")))
    } else {
      ipts <- read.fortran(fname, format=list(c("I6","A11","3F10"),c("12F8"),c("12F8"),c("12F8"),c("I1")))
    }
    ipts <- ipts[,2:41]; names(ipts) <- iNames
    
    fit.vals <- data.frame(ID=ipts$ID, LONG=ipts$LONG, LAT=ipts$LAT,
    JAN=ipts$JAN, PRED.JAN=ipts$PRED.JAN,
    FEB=ipts$FEB, PRED.FEB=ipts$PRED.FEB,
    MAR=ipts$MAR, PRED.MAR=ipts$PRED.MAR,
    APR=ipts$APR, PRED.APR=ipts$PRED.APR,
    MAY=ipts$MAY, PRED.MAY=ipts$PRED.MAY,
    JUN=ipts$JUN, PRED.JUN=ipts$PRED.JUN,
    JUL=ipts$JUL, PRED.JUL=ipts$PRED.JUL,
    AUG=ipts$AUG, PRED.AUG=ipts$PRED.AUG,
    SEP=ipts$SEP, PRED.SEP=ipts$PRED.SEP,
    OCT=ipts$OCT, PRED.OCT=ipts$PRED.OCT,
    NOV=ipts$NOV, PRED.NOV=ipts$PRED.NOV,
    DEC=ipts$DEC, PRED.DEC=ipts$PRED.DEC)
    
    #Fit metrics per month
    for (mth in 1:12) {
      p1 <- mth+3+(mth-1); p2 <- mth+4+(mth-1)
      y.meas <- fit.vals[,p1]
      y.pred <- fit.vals[,p2]
      
      #Calculate rsq and rmse
      corr <- cor.test(y.meas,y.pred,method="pearson")
      rsq <- corr$estimate ^ 2
      pval <- corr$p.value
      rmse <- y.meas - y.pred; rmse <- rmse ^ 2
      rmse <- sqrt(sum(rmse) / length(y.meas))
      
      #Output data frame
      out.fit <- data.frame(MONTH=mth, R2=rsq, P.VAL=pval, RMSE=rmse)
      if (mth == 1) {otMx.fit <- out.fit} else {otMx.fit <- rbind(otMx.fit, out.fit)}
    }
  }
  
  if (!is.null(testMx)) {
    #Loading validation points
    fname <- paste(variable, ".out", sep="")
    vNames <- c("ID","LONG","LAT","ALT","PRED.JAN","PRED.FEB","PRED.MAR","PRED.APR","PRED.MAY","PRED.JUN","PRED.JUL","PRED.AUG",
    "PRED.SEP","PRED.OCT","PRED.NOV","PRED.DEC","ERR.JAN","ERR.FEB","ERR.MAR","ERR.APR","ERR.MAY","ERR.JUN",
    "ERR.JUL","ERR.AUG","ERR.SEP","ERR.OCT","ERR.NOV","ERR.DEC")
    vpts <- read.fortran(fname, format=c("A10","2F10","F8","24F9"))
    names(vpts) <- vNames
    
    #Join in one single matrix the ID, LONG, LAT, ALT, real values and pred. values
    test.vals <- data.frame(ID=testMx$ID, LONG=testMx$LONG, LAT=testMx$LAT,
    JAN=testMx$JAN, PRED.JAN=vpts$PRED.JAN,
    FEB=testMx$FEB, PRED.FEB=vpts$PRED.FEB,
    MAR=testMx$MAR, PRED.MAR=vpts$PRED.MAR,
    APR=testMx$APR, PRED.APR=vpts$PRED.APR,
    MAY=testMx$MAY, PRED.MAY=vpts$PRED.MAY,
    JUN=testMx$JUN, PRED.JUN=vpts$PRED.JUN,
    JUL=testMx$JUL, PRED.JUL=vpts$PRED.JUL,
    AUG=testMx$AUG, PRED.AUG=vpts$PRED.AUG,
    SEP=testMx$SEP, PRED.SEP=vpts$PRED.SEP,
    OCT=testMx$OCT, PRED.OCT=vpts$PRED.OCT,
    NOV=testMx$NOV, PRED.NOV=vpts$PRED.NOV,
    DEC=testMx$DEC, PRED.DEC=vpts$PRED.DEC)
    
    #Fit metrics per month
    for (mth in 1:12) {
      p1 <- mth+3+(mth-1); p2 <- mth+4+(mth-1)
      y.meas <- test.vals[,p1]
      y.pred <- test.vals[,p2]
      
      #Calculate rsq and rmse
      corr <- cor.test(y.meas,y.pred,method="pearson")
      rsq <- corr$estimate ^ 2
      pval <- corr$p.value
      rmse <- y.meas - y.pred; rmse <- rmse ^ 2
      rmse <- sqrt(sum(rmse) / length(y.meas))
      
      #Output data frame
      out.test <- data.frame(MONTH=mth, R2=rsq, P.VAL=pval, RMSE=rmse)
      if (mth == 1) {otMx.test <- out.test} else {otMx.test <- rbind(otMx.test, out.test)}
    }
  }
  
  #Join the two matrices (validation and fit)
  if (!is.null(testMx) & !is.null(trainMx)) {
    outMx <- data.frame(MONTH=otMx.fit$MONTH, R2.FIT=otMx.fit$R2, P.VAL.FIT=otMx.fit$P.VAL, RMSE.FIT=otMx.fit$RMSE,
    R2.TEST=otMx.test$R2, P.TEST.FIT=otMx.test$P.VAL, RMSE.TEST=otMx.test$RMSE)
    return(list(FITTED=fit.vals, TEST=test.vals, METRICS=outMx))
  } else if (!is.null(trainMx) & is.null(testMx)) {
    outMx <- data.frame(MONTH=otMx.fit$MONTH, R2.FIT=otMx.fit$R2, P.VAL.FIT=otMx.fit$P.VAL, RMSE.FIT=otMx.fit$RMSE)
    return(list(FITTED=fit.vals, METRICS=outMx))
  } else if (is.null(trainMx) & !is.null(testMx)) {
    outMx <- data.frame(MONTH=otMx.test$MONTH, R2.TEST=otMx.test$R2, P.TEST.FIT=otMx.test$P.VAL, RMSE.TEST=otMx.test$RMSE)
    return(list(TEST=test.vals, METRICS=outMx))
  } else if (is.null(trainMx) & is.null(testMx)) {
    return(NA)
  }
}
