#Julian Ramirez
#CIAT / University of Leeds

#Accuracy metrics function
accuracy <- function(trainMx=NULL, testMx=NULL, variable="rain") {
  
  #Join in one single matrix the ID, LONG, LAT, ALT, real values and pred. values
  if (!is.null(trainMx)) {
    fit.vals <- trainMx
    
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
    #Join in one single matrix the ID, LONG, LAT, ALT, real values and pred. values
    test.vals <- testMx
    
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
