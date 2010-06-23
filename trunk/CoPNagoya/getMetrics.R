#To get and store in a csv (comma separated values) file the set of evaluation metrics and the thresholds that are likely to be used for model development. This is specific for the case when you have presence-only data, and have used a cross-validation approach, plus a model building using all the data.

getMetrics <- function(crossValDir, foldSuffix, nFolds, absRunDir, outMetDir) {
  
  cat("Metrics... \n")
  
  nFolds <- nFolds - 1
  
  crossValMxrFile <- paste(crossValDir, "//maxentResults.csv", sep="")
  crossValMxrData <- read.csv(crossValMxrFile)
  
  runMxrFile <- paste(absRunDir, "//maxentResults.csv", sep="")
  runMxrData <- read.csv(runMxrFile)
  
  #Number of samples
  
  totSamples <- runMxrData$X.Training.samples
  trainSamples <- crossValMxrData$X.Training.samples[11]
  testSamples <- crossValMxrData$X.Test.samples[11]
  
  #AUC
  
  trainAUCAvg <- crossValMxrData$Training.AUC[11]
  trainAUCStd <- sd(crossValMxrData$Training.AUC[1:10])
  testAUCAvg <- crossValMxrData$Test.AUC[11]
  testAUCStd <- sd(crossValMxrData$Test.AUC[1:10])
  totalAUC <- runMxrData$Training.AUC
  
  someMets <- matrix(ncol=11, nrow=(nFolds+1))
  
  for (fold in 0:nFolds) {
    
    #Loading basic files
    
    backPredFile <- paste(crossValDir, "//", foldSuffix, "_", fold, ".csv", sep="")
    backPredData <- read.csv(backPredFile)
    
    samPredFile <- paste(crossValDir, "//", foldSuffix, "_", fold, "_samplePredictions.csv", sep="")
    samPredData <- read.csv(samPredFile)
    
    omRatesFile <- paste(crossValDir, "//", foldSuffix, "_", fold, "_omission.csv", sep="")
    omRatesData <- read.csv(omRatesFile)
    
    trainPreds <- samPredData$Logistic.prediction[which(samPredData$Test.or.train == "train")]
    matchTrain <- rep(1, times=length(trainPreds))
    
    testPreds <- samPredData$Logistic.prediction[which(samPredData$Test.or.train == "test")]
    matchTest <- rep(1, times=length(testPreds))
    
    allPreds <- c(trainPreds, testPreds)
    matchAll <- rep(1, times=length(allPreds))
    
    backPreds <- backPredData[,3]
    matchBack <- rep(0.5, times=length(backPreds))
    
    #Correlation coefficients
    
    someMets[fold+1,1] <- cor(x=c(trainPreds, backPreds), y=c(matchTrain, matchBack))
    someMets[fold+1,2] <- cor(x=c(testPreds, backPreds), y=c(matchTest, matchBack))
    someMets[fold+1,3] <- cor(x=c(allPreds, backPreds), y=c(matchAll, matchBack))
    
    #LogDev
    
    someMets[fold+1,4] <- mean(-2 * log(trainPreds))
    someMets[fold+1,5] <- mean(-2 * log(testPreds))
    someMets[fold+1,6] <- mean(-2 * log(allPreds))
    
    #RMSQD
    
    someMets[fold+1,7] <- (mean((trainPreds - matchTrain)^2))^0.5
    someMets[fold+1,8] <- (mean((testPreds - matchTest)^2))^0.5
    someMets[fold+1,9] <- (mean((allPreds - matchAll)^2))^0.5
    
    #Prevalence (avg. over presence sites) threshold
    someMets[fold+1,10] <- mean(trainPreds)
    
    #Upper-left corner ROC curve threshold
   
    Spec <- omRatesData$Fractional.area
    Sens <- 1-omRatesData$Training.omission
    logVals <- omRatesData$Corresponding.logistic.value
    
    absDif <- abs(1-(Sens+Spec))
    
    someMets[fold+1,11] <- logVals[which(absDif[] == min(absDif))]
  }
  
  #Averaging the metrics
  
  trainRAvg <- mean(someMets[,1])
  trainRStd <- sd(someMets[,1])
  testRAvg <- mean(someMets[,2])
  testRStd <- sd(someMets[,2])
  allRAvg <- mean(someMets[,3])
  allRStd <- sd(someMets[,3])
  
  trainLDAvg <- mean(someMets[,4])
  trainLDStd <- sd(someMets[,4])
  testLDAvg <- mean(someMets[,5])
  testLDStd <- sd(someMets[,5])
  allLDAvg <- mean(someMets[,6])
  allLDStd <- sd(someMets[,6])
  
  trainRMSQDAvg <- mean(someMets[,7])
  trainRMSQDStd <- sd(someMets[,7])
  testRMSQDAvg <- mean(someMets[,8])
  testRMSQDStd <- sd(someMets[,8])
  allRMSQDAvg <- mean(someMets[,9])
  allRMSQDStd <- sd(someMets[,9])
  
  #The metrics matrix and output file
  
  metMatrix <- as.data.frame(matrix(ncol=26, nrow=1))
  names(metMatrix) <- c("NSamples", "TrainSamples", "TestSamples", "TrainAUC", "TrainAUCSD", "TestAUC", "TestAUCSD", "AUC", "TrainR", "TrainRSD", "TestR", "TestRSD", "AllR", "AllRSD", "TrainLogD", "TrainLogDSD", "TestLogD", "TestLogDSD", "AllLogD", "AllLogDSD", "TrainRMSQD", "TrainRMSQDSD", "TestRMSQD", "TestRMSQDSD", "AllRMSQD", "AllRMSQDSD")
  
  metMatrix[1,1] <- totSamples
  metMatrix[1,2] <- trainSamples
  metMatrix[1,3] <- testSamples
  metMatrix[1,4] <- trainAUCAvg
  metMatrix[1,5] <- trainAUCStd
  metMatrix[1,6] <- testAUCAvg
  metMatrix[1,7] <- testAUCStd
  metMatrix[1,8] <- totalAUC
  metMatrix[1,9] <- trainRAvg
  metMatrix[1,10] <- trainRStd
  metMatrix[1,11] <- testRAvg
  metMatrix[1,12] <- testRStd
  metMatrix[1,13] <- allRAvg
  metMatrix[1,14] <- allRStd
  metMatrix[1,15] <- trainLDAvg
  metMatrix[1,16] <- trainLDStd
  metMatrix[1,17] <- testLDAvg
  metMatrix[1,18] <- testLDStd
  metMatrix[1,19] <- allLDAvg
  metMatrix[1,20] <- allLDStd
  metMatrix[1,21] <- trainRMSQDAvg
  metMatrix[1,22] <- trainRMSQDStd
  metMatrix[1,23] <- testRMSQDAvg
  metMatrix[1,24] <- testRMSQDStd
  metMatrix[1,25] <- allRMSQDAvg
  metMatrix[1,26] <- allRMSQDStd
  
  outMetsFile <- paste(outMetDir, "//metrics.csv", sep="")
  out <- write.csv(metMatrix, outMetsFile, quote=F, row.names=F)
  
  #Ten percentile threshold
  tpThreshAvg <- crossValMxrData$X10.percentile.training.presence.logistic.threshold[11]
  tpThreshStd <- sd(crossValMxrData$X10.percentile.training.presence.logistic.threshold[1:10])
  
  #Prevalence threshold
  prThreshAvg <- mean(someMets[,10])
  prThreshStd <- sd(someMets[,10])
  
  #Fixed threshold
  fxThresh <- 0.5
  
  #Maximum training sensitivity plus specificity
  mspsThreshAvg <- mean(crossValMxrData$Maximum.training.sensitivity.plus.specificity.logistic.threshold[1:10])
  mspsThreshStd <- sd(crossValMxrData$Maximum.training.sensitivity.plus.specificity.logistic.threshold[1:10])
  
  #Equal training sensitivity and specificity
  esasThreshAvg <- mean(crossValMxrData$Equal.training.sensitivity.and.specificity.logistic.threshold[1:10])
  esasThreshStd <- sd(crossValMxrData$Equal.training.sensitivity.and.specificity.logistic.threshold[1:10])
  
  #Balance training omission
  btoThreshAvg <- mean(crossValMxrData$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[1:10])
  btoThreshStd <- sd(crossValMxrData$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[1:10])
  
  #ROC Curve based threshold (upper left corner prob.)
  rcbThreshAvg <- mean(someMets[,11])
  rcbThreshStd <- sd(someMets[,11])
  
  #The thresholds matrix and output file
  
  threshMatrix <- as.data.frame(matrix(ncol=13, nrow=1))
  names(threshMatrix) <- c("TenPercentile", "TenPercentileSD", "Prevalence", "PrevalenceSD", "FixedValue", "MaxTrainSensSpec", "MaxTrainSesnsSpecSD", "EqualTrainSensSpec", "EqualTrainSensSpecSD", "BalanceTrainOmission", "BalanceTrainOmissionSD", "UpperLeftROC", "UpperLeftROCSD")
  
  threshMatrix[1,1] <- tpThreshAvg
  threshMatrix[1,2] <- tpThreshStd
  threshMatrix[1,3] <- prThreshAvg
  threshMatrix[1,4] <- prThreshStd
  threshMatrix[1,5] <- fxThresh
  threshMatrix[1,6] <- mspsThreshAvg
  threshMatrix[1,7] <- mspsThreshStd
  threshMatrix[1,8] <- esasThreshAvg
  threshMatrix[1,9] <- esasThreshStd
  threshMatrix[1,10] <- btoThreshAvg
  threshMatrix[1,11] <- btoThreshStd
  threshMatrix[1,12] <- rcbThreshAvg
  threshMatrix[1,13] <- rcbThreshStd
  
  outThresholdFile <- paste(outMetDir, "//thresholds.csv", sep="")
  out <- write.csv(threshMatrix, outThresholdFile, quote=F, row.names=F)
  return("Done!")
}