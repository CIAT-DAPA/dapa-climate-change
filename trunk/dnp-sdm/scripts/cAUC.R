stop("!")

library(dismo)

#predictors and bg
files <- list.files(path=paste(system.file(package="dismo"),'/ex', sep=''), pattern='grd', full.names=TRUE )
predictors <- stack(files)
backgr <- randomPoints(predictors, 1000)

#samples stuff
nr <- nrow(bradypus)
s <- sample(nr, 0.25 * nr)
pres_train <- bradypus[-s, ]
pres_test <- bradypus[s, ]
nr <- nrow(backgr)
s <- sample(nr, 0.25 * nr)
back_train <- backgr[-s, ]
back_test <- backgr[s, ]

#calculate spatial sorting bias as the ratio of the (min) distance between
#testing pres. and training pres. divided by testing abs. and train pres.
sb <- ssb(cbind(x=pres_test$lon,y=pres_test$lat), back_test, cbind(x=pres_train$lon,y=pres_train$lat))
sb[,1] / sb[,2]


#selecting a pair-wise distance sample
i <- pwdSample(cbind(x=pres_test$lon,y=pres_test$lat), back_test, cbind(pres_train$lon,pres_train$lat), n=1, tr=0.1)
pres_test_pwd <- pres_test[!is.na(i[,1]), ]; pres_test_pwd <- cbind(x=pres_test_pwd$lon,y=pres_test_pwd$lat)
back_test_pwd <- back_test[na.omit(as.vector(i)), ]

#recalculating spatial sorting bias (to check)
sb2 <- ssb(pres_test_pwd, back_test_pwd, cbind(x=pres_train$lon,y=pres_train$lat))
sb2[1]/ sb2[2]

#fitting model (bioclim)
bc <- bioclim(predictors, cbind(x=pres_train$lon,y=pres_train$lat))

#evaluating model using spatially biased testing presence/absence
eval_st <- evaluate(bc, p=cbind(x=pres_test$lon,y=pres_test$lat), a=back_test, x=predictors)

#evaluating model using corrected sample
eval_null <- evaluate(bc, p=pres_test_pwd, a=back_test_pwd, x=predictors)

eval_st@auc
eval_null@auc

cauc <- eval_st@auc + .5 - max(c(0.5,eval_null@auc))

