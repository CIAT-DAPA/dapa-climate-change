#JRV 2013
#CIAT / CCAFS

############################################################
### functions to fit models using BIOMOD2 library
############################################################

#function to fit and evaluate a geographically null model with provided configuration
run_null_model <- function(bDir,sppName,seed,npa) {
  require(biomod2); require(raster); require(dismo)
  
  #i/o dirs
  dataDir <- paste(bDir,"/vif-analysis",sep="")
  modDir <- paste(bDir,"/models",sep="")
  bgDir <- paste(bDir,"/bg-areas",sep="")
  envDir <- paste(bDir,"/env-data",sep="")
  bioDir <- paste(envDir,"/bioclim_gtiff",sep="")
  solDir <- paste(envDir,"/soil",sep="")
  topDir <- paste(envDir,"/topography",sep="")
  
  cat("\nanalysing seed=",seed,"with",npa,"pseudo absences\n")
  
  #1. output model and config directory
  cat("loading presence and pseudo-absence data\n")
  outDir <- paste(modDir,"/NULL/SD-",seed,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  outgdDir <- paste(outDir,"/GD_MODEL/",sppName,sep="")
  if (!file.exists(outgdDir)) {dir.create(outgdDir,recursive=T)}
  
  outevDir <- paste(outDir,"/EVAL/",sppName,sep="")
  if (!file.exists(outevDir)) {dir.create(outevDir,recursive=T)}
  
  if (!file.exists(paste(outevDir,"/PA-",npa,".RData",sep=""))) {
    #2. load species data (from VIF analysis output)
    spp_data <- read.csv(paste(dataDir,"/",sppName,"/",sppName,"_full.csv",sep="")) #full set
    spp_data <- spp_data[,c("x","y")]
    
    #3. load background data / create if needs be
    pab_data <- get_pa(spp_name=sppName,spp_data=spp_data,n_pa=npa,bg_dir=bgDir,bio_dir=bioDir,soil_dir=solDir,topo_dir=topDir)
    pab_data <- pab_data[,c("x","y")] #remove extra variables
    
    #4. select train / test samples
    cat("bootstrapped selection 25/75 % of data \n")
    set.seed(seed); sp_sel <- sample(1:nrow(spp_data),size=round((nrow(spp_data)*.25),0))
    set.seed(seed); pa_sel <- sample(1:nrow(pab_data),size=round((nrow(pab_data)*.25),0))
    
    spp_tr <- spp_data[-sp_sel,]; rownames(spp_tr) <- 1:nrow(spp_tr)
    spp_te <- spp_data[sp_sel,]; rownames(spp_te) <- 1:nrow(spp_te)
    
    pab_tr <- pab_data[-pa_sel,]; rownames(pab_tr) <- 1:nrow(pab_tr)
    pab_te <- pab_data[pa_sel,]; rownames(pab_te) <- 1:nrow(pab_te)
    
    #5. fit the geodist model
    if (!file.exists(paste(outgdDir,"/output_geodist_null_model.RData",sep=""))) {
      cat("fitting geographically null model\n")
      gd <- geoDist(spp_tr,lonlat=T)
      gd_model <- list(TRAIN_P=spp_tr,TEST_P=spp_te,TRAIN_PA=pab_tr,TEST_PA=pab_te,MODEL=gd)
      save(list=c("gd_model"),file=paste(outgdDir,"/output_geodist_null_model.RData",sep=""))
    } else {
      cat("loading geographically null model\n")
      load(file=paste(outgdDir,"/output_geodist_null_model.RData",sep=""))
      gd <- gd_model$MODEL
      rm(gd_model); g=gc(); rm(g)
    }
    
    #6. model evaluation
    #a. correct sampling strategy
    cat("evaluating the model \n")
    sb <- ssb(p=cbind(x=spp_te$x,y=spp_te$y), a=cbind(x=pab_te$x,y=pab_te$y), reference=cbind(x=spp_tr$x,y=spp_tr$y))
    cat("spatial sorting bias is:",sb[,1] / sb[,2],"\n")
    
    i <- pwdSample(cbind(x=spp_te$x,y=spp_te$y), cbind(x=pab_te$x,y=pab_te$y), cbind(x=spp_tr$x,y=spp_tr$y), n=1, tr=0.1, warn=F)
    spp_te_pwd <- spp_te[!is.na(i[,1]),]
    pab_te_pwd <- pab_te[na.omit(as.vector(i)),]
    
    sb2 <- ssb(cbind(x=spp_te_pwd$x,y=spp_te_pwd$y), cbind(x=pab_te_pwd$x,y=pab_te_pwd$y), cbind(x=spp_tr$x,y=spp_tr$y))
    cat("corrected spatial sorting bias is:",sb2[1]/ sb2[2],"\n")
    
    #b. predict over test and training datasets, and over ssb-corrected dataset
    pred_spp_tr <- predict(gd,spp_tr); pred_spp_te <- predict(gd,spp_te) #presence
    pred_pab_tr <- predict(gd,pab_tr); pred_pab_te <- predict(gd,pab_te) #pseudo-absence
    pred_pwd_spp <- predict(gd,spp_te_pwd); pred_pwd_pab <- predict(gd,pab_te_pwd)
    
    #c. calculate respective AUC values
    eval_bc <- evaluate(p=pred_pwd_spp, a=pred_pwd_pab)
    eval_te <- evaluate(p=pred_spp_te, a=pred_pab_te)
    eval_tr <- evaluate(p=pred_spp_tr, a=pred_pab_tr)
    #c_auc <- eval_te@auc + .5 - max(c(0.5,eval_bc@auc))
    
    cat("training AUC is",eval_tr@auc,"\n")
    cat("test AUC is",eval_te@auc,"\n")
    cat("test AUC (bias corrected) is",eval_bc@auc,"\n")
    
    #d. calculate other evaluation metrics for the null model
    tss_tr <- getEvalMetric(fit_vals=c(pred_spp_tr,pred_pab_tr),
                            obs_vals=c(rep(1,length(pred_spp_tr)),rep(0,length(pred_pab_tr))),
                            tstat='TSS')
    tss_te <- getEvalMetric(fit_vals=c(pred_spp_te,pred_pab_te),
                            obs_vals=c(rep(1,length(pred_spp_te)),rep(0,length(pred_pab_te))),
                            tstat='TSS')
    tss_bc <- getEvalMetric(fit_vals=c(pred_pwd_spp,pred_pwd_pab),
                            obs_vals=c(rep(1,length(pred_pwd_spp)),rep(0,length(pred_pwd_pab))),
                            tstat='TSS')
    
    kappa_tr <- getEvalMetric(fit_vals=c(pred_spp_tr,pred_pab_tr),
                              obs_vals=c(rep(1,length(pred_spp_tr)),rep(0,length(pred_pab_tr))),
                              tstat='KAPPA')
    kappa_te <- getEvalMetric(fit_vals=c(pred_spp_te,pred_pab_te),
                              obs_vals=c(rep(1,length(pred_spp_te)),rep(0,length(pred_pab_te))),
                              tstat='KAPPA')
    kappa_bc <- getEvalMetric(fit_vals=c(pred_pwd_spp,pred_pwd_pab),
                              obs_vals=c(rep(1,length(pred_pwd_spp)),rep(0,length(pred_pwd_pab))),
                              tstat='KAPPA')
    
    
    #7. final object of model eval
    null_eval <- data.frame(NPR_FIT=nrow(spp_tr),NAB_FIT=nrow(pab_tr),NPR_TST=nrow(spp_te),NAB_TST=nrow(pab_te),
                            SSB1=(sb[,1] / sb[,2]),SSB2=(sb2[,1] / sb2[,2]),AUC_FIT=eval_tr@auc,AUC_TST=eval_te@auc,AUC_SSB=eval_bc@auc,
                            TSS_FIT=tss_tr,TSS_TST=tss_te,TSS_SSB=tss_bc,KAPPA_FIT=kappa_tr,KAPPA_TST=kappa_te,KAPPA_SSB=kappa_bc)
    
    #8. save object
    cat("saving final object\n")
    save(list=c("null_eval"),file=paste(outevDir,"/PA-",npa,".RData",sep=""))
  } else {
    cat("already fitted and evaluated\n")
  }
  return(outevDir)
}



#function to run a model with provided configuration
run_model <- function(base_dir,env_dir,spp_name,seed,npa,alg,vset,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  #i/o dirs
  max_dir <- paste(base_dir,"/bin",sep="")
  data_dir <- paste(base_dir,"/input-samples",sep="")
  mod_dir <- paste(base_dir,"/fitting",sep="")
  bg_dir <- paste(base_dir,"/pseudo-absences",sep="")
  bio_dir <- paste(env_dir,"/climate/bio_ind_30s",sep="")
  sol_dir <- paste(env_dir,"/soil",sep="")
  int_dir <- paste(env_dir,"/crop_intensity",sep="")
  msk_dir <- paste(env_dir,"/mask",sep="")
  
  #1. output model and config directory
  cat("loading presence and pseudo-absence data\n")
  outDir <- paste(mod_dir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  #2. load species data (from VIF analysis output)
  spp_data <- read.csv(paste(data_dir,"/",spp_name,"/",spp_name,"_",varList$CLIM[vset],".csv",sep=""))
  
  #3. load background data / create if needs be
  pab_data <- get_pa(spp_name=spp_name,n_pa=npa,bg_dir=bg_dir,msk_dir=msk_dir,int_dir=int_dir,
                     bio_dir=bio_dir,soil_dir=sol_dir,size=12500)
  pab_data <- pab_data[,names(spp_data)] #remove extra variables
  
  #run only if final stored objects do not exist
  if (!file.exists(paste(outDir,"/",gsub("\\_","\\.",spp_name),"/fitted_model.RData",sep=""))) {
    cat("\n-------------------------------------------------------\n")
    cat("---- processing species", spp_name,"\n")
    cat("-------------------------------------------------------\n")
    
    #if soil is within variables to be modelled then make factor else remove from data.frame
    if (!varList$SOIL[vset]) {
      spp_data$soildul <- NULL
      pab_data$soildul <- NULL
    }
    
    #here create the necessary variable interactions!!
    #create additional predictors using interactions
    if (alg != "GAM") {
      spp_data <- make_interactions(spp_data)
      pab_data <- make_interactions(pab_data)
    }
    
    #4. select 20 % test data using a given seed 
    cat("selecting train/test data using given seed\n")
    set.seed(seed); sp_sel <- sample(1:nrow(spp_data),size=round((nrow(spp_data)*.20),0))
    set.seed(seed); pa_sel <- sample(1:nrow(pab_data),size=round((nrow(pab_data)*.20),0))
    
    spp_tr <- spp_data[-sp_sel,]; rownames(spp_tr) <- 1:nrow(spp_tr)
    spp_te <- spp_data[sp_sel,]; rownames(spp_te) <- 1:nrow(spp_te)
    
    pab_tr <- pab_data[-pa_sel,]; rownames(pab_tr) <- 1:nrow(pab_tr)
    pab_te <- pab_data[pa_sel,]; rownames(pab_te) <- 1:nrow(pab_te)
    
    #5. Model fitting
    #go to models directory
    setwd(outDir)
    
    #training: 
    #explanatory variables have to contain the pseudo-absence data AND the presence data
    #make occurrences into appropriate format (vector resp_var)
    cat("configuring input data and model parameters\n")
    expl_var <- rbind(spp_tr,pab_tr)
    expl_var <- SpatialPointsDataFrame(coords=cbind(expl_var$x,expl_var$y),data=expl_var[,3:ncol(expl_var)])
    resp_var <- c(rep(1,times=nrow(spp_tr)),rep(NA,times=nrow(pab_tr)))
    
    #formatting data
    sp_bData <- BIOMOD_FormatingData(resp.var = resp_var,expl.var = expl_var, 
                                     resp.xy=coordinates(expl_var),
                                     resp.name = spp_name, 
                                     PA.strategy="random",
                                     PA.nb.rep=1,PA.nb.absences=nrow(pab_tr))
    
    #selecting model features (!change features as needed)
    sp_mOpt <- BIOMOD_ModelingOptions()
    sp_mOpt@MAXENT$path_to_maxent.jar <- max_dir
    sp_mOpt@MAXENT$maximumiterations <- 500
    sp_mOpt@MAXENT$quadratic <- F
    sp_mOpt@MAXENT$product <- F
    sp_mOpt@MAXENT$threshold <- F
    sp_mOpt@MAXENT$hinge <- F
    sp_mOpt@GBM$n.trees <- 2000
    sp_mOpt@GLM$control$maxit <- 100
    sp_mOpt@GLM$type <- "simple" #simple | quadratic | polynomial
    #sp_mOpt@GAM$algo <- "GAM_gam"
    sp_mOpt@GAM$k <- 3
    sp_mOpt@ANN$maxit <- 500
    
    #perform the modelling
    out_obj <- paste(outDir,"/",sp_bData@sp.name,"/",sp_bData@sp.name,".",model_class,".models.out",sep="")
    
    #note that the biomod output is simply a linear scaling from 0 to 1000!!
    cat("running model\n")
    if (!file.exists(out_obj)) {
      sp_mOut <- BIOMOD_Modeling(sp_bData,
                                 models = alg,
                                 models.options = sp_mOpt,
                                 NbRunEval=1,
                                 DataSplit=100,
                                 Prevalence=0.5,
                                 VarImport=10,
                                 models.eval.meth = c('KAPPA','TSS','ROC'),
                                 SaveObj = TRUE,
                                 rescal.all.models = TRUE,
                                 do.full.models = FALSE,
                                 modeling.id = model_class)
    } else {
      sp_mOut <- get(load(out_obj)) #load model as it already exists
    }
    
    #6. model evaluation
    #evaluate model against left-out presences and pseudo-absences
    #function find.optim.stat(Stat=ROC,)
    
    #a. calculate spatial sorting bias as the ratio of the (min) distance between
    #testing pres. and training pres. divided by testing abs. and train pres.
    cat("bias-correcting the AUC\n")
    sb <- ssb(p=cbind(x=spp_te$x,y=spp_te$y), a=cbind(x=pab_te$x,y=pab_te$y), reference=cbind(x=spp_tr$x,y=spp_tr$y))
    cat("spatial sorting bias is:",sb[,1] / sb[,2],"\n")
    
    #selecting a pair-wise distance sample
    i <- pwdSample(cbind(x=spp_te$x,y=spp_te$y), cbind(x=pab_te$x,y=pab_te$y), cbind(x=spp_tr$x,y=spp_tr$y), n=1, tr=0.33, warn=F)
    spp_te_pwd <- spp_te[!is.na(i[,1]),]
    pab_te_pwd <- pab_te[na.omit(as.vector(i)),]
    
    sb2 <- ssb(cbind(x=spp_te_pwd$x,y=spp_te_pwd$y), cbind(x=pab_te_pwd$x,y=pab_te_pwd$y), cbind(x=spp_tr$x,y=spp_tr$y))
    cat("corrected spatial sorting bias is:",sb2[1]/ sb2[2],"\n")
    
    #b. predict over evaluation and ssb-eval samples
    tmodel <- get(BIOMOD_LoadModels(sp_mOut,models=alg)) #load specific model
    
    spp_tr_pred <- as.numeric(predict(tmodel,spp_tr[,3:ncol(spp_tr)]))
    pab_tr_pred <- as.numeric(predict(tmodel,pab_tr[,3:ncol(pab_tr)]))
    spp_pred <- as.numeric(predict(tmodel,spp_te[,3:ncol(spp_te)]))
    pab_pred <- as.numeric(predict(tmodel,pab_te[,3:ncol(pab_te)]))
    spp_pwd_pred <- as.numeric(predict(tmodel,spp_te_pwd[,3:ncol(spp_te_pwd)]))
    pab_pwd_pred <- as.numeric(predict(tmodel,pab_te_pwd[,3:ncol(pab_te_pwd)]))
    
    #for spatially biased samples (normal case) the addsamplestobackground option
    #is generally switched on! (case in biomod2), thus the AUC has to use
    #both presences and pseudo-absences in the pseudo-absences.
    if (alg == "MAXENT") {
      pab_tr_pred <- c(pab_tr_pred,spp_tr_pred)
      pab_pred <- c(pab_pred,spp_pred)
      pab_pwd_pred <- c(pab_pwd_pred,spp_pwd_pred)
    }
    
    #c. calculate cAUC
    #for some unknown reason the AUC reported in getModelsEvaluations() is not the same
    #as calculated separately using dismo package functions. So I decided to stick with
    #dismo's implementation (which gives the same result as Find.Optim.Stat, but it provides
    #the actual ROC curve)
    #eval_u = evaluation with all evaluation data
    #eval_c = evaluation with bias-corrected data
    #eval_t = evaluation with training data
    eval_u <- evaluate(p=spp_pred, a=pab_pred)
    eval_c <- evaluate(p=spp_pwd_pred, a=pab_pwd_pred)
    eval_t <- evaluate(p=spp_tr_pred, a=pab_tr_pred)
    c_auc <- eval_u@auc + .5 - max(c(0.5,eval_c@auc))
    
    #calculate TSS and Kappa
    tss_u <- getEvalMetric(fit_vals=c(spp_pred,pab_pred),
                           obs_vals=c(rep(1,length(spp_pred)),rep(0,length(pab_pred))),
                           tstat='TSS')
    tss_t <- getEvalMetric(fit_vals=c(spp_tr_pred,pab_tr_pred),
                           obs_vals=c(rep(1,length(spp_tr_pred)),rep(0,length(pab_tr_pred))),
                           tstat='TSS')
    
    kappa_u <- getEvalMetric(fit_vals=c(spp_pred,pab_pred),
                             obs_vals=c(rep(1,length(spp_pred)),rep(0,length(pab_pred))),
                             tstat='KAPPA')
    kappa_t <- getEvalMetric(fit_vals=c(spp_tr_pred,pab_tr_pred),
                             obs_vals=c(rep(1,length(spp_tr_pred)),rep(0,length(pab_tr_pred))),
                             tstat='KAPPA')
    
    #final object of model eval
    sp_mEval <- data.frame(NPR_FIT=nrow(spp_tr),NAB_FIT=nrow(pab_tr),NPR_TST=nrow(spp_te),NAB_TST=nrow(pab_te),
                           SSB1=(sb[,1] / sb[,2]),SSB2=(sb2[,1] / sb2[,2]),AUC_FIT=eval_t@auc,AUC_TST=eval_u@auc,AUC_SSB=c_auc,
                           TSS_FIT=tss_t,TSS_TST=tss_u,KAPPA_FIT=kappa_t,KAPPA_TST=kappa_u)
    
    #save object with all necessary details
    cat("saving final objects\n")
    save(list=c("sp_bData","sp_mEval","sp_mOut"),file=paste(outDir,"/",sp_bData@sp.name,"/fitted_model.RData",sep=""))
    
    #go back to base directory
    setwd(base_dir)
  }
  
  #return object
  return(outDir)
}


#adaptation of biomod2 function Find.Optim.Stat
getEvalMetric <- function(fit_vals,obs_vals,tstat='TSS') {
  if (length(unique(obs_vals)) == 1 | length(unique(fit_vals)) == 1) {
    warning("\nObserved or fitted data contains a unique value. Be carefull with this model predictions\n", immediate. = T)
  }
  
  #set of thresholds
  if (length(unique(fit_vals)) == 1) {
    valToTest <- unique(fit_vals)
    valToTest <- c(mean(c(0, valToTest)), mean(c(1, valToTest)))
  } else {
    mini <- max(min(quantile(fit_vals, 0.05, na.rm = T), na.rm = T), 0)
    maxi <- min(max(quantile(fit_vals, 0.95, na.rm = T), na.rm = T), 1)
    valToTest <- unique(c(seq(mini, maxi, length.out = 100), mini, maxi))
    if (length(valToTest) < 3) {
      valToTest <- c(mean(0, mini), valToTest, mean(1, maxi))
    }
  }
  calcStat <- sapply(lapply(valToTest, function(x) {return(table(fit_vals > x, obs_vals))}), calculate.stat, stat = tstat)
  #calcStat <- 1 - abs(getStatOptimValue(tstat) - calcStat) #unsure of reasons for this (i.e. irrelevant for kappa and TSS)
  bestStat <- max(calcStat, na.rm = T)
  return(bestStat)
}

#make interactions before constructing models
#only daystcrit is linear only
make_interactions <- function(input_data) {
  modvar <- names(input_data)
  
  #daystcrit: minrain seasrain
  if ("daystcrit" %in% modvar) {
    if ("minrain" %in% modvar) {input_data$minrain_daystcrit <- input_data$daystcrit * input_data$minrain}
    if ("seasrain" %in% modvar) {input_data$seasrain_daystcrit <- input_data$daystcrit * input_data$seasrain}
  }
  
  #dindex: sindex totgdd
  if ("dindex" %in% modvar) {
    input_data$dindex2 <- input_data$dindex^2
    if ("sindex" %in% modvar) {input_data$sindex_dindex <- input_data$dindex * input_data$sindex}
    if ("totgdd" %in% modvar) {input_data$totgdd_dindex <- input_data$dindex * input_data$totgdd}
    if ("soildul" %in% modvar) {input_data$soildul_daystcrit <- input_data$daystcrit * input_data$soildul}
  }
  
  #meantemp: sindex
  if ("meanmeantemp" %in% modvar) {
    input_data$meanmeantemp2 <- input_data$meanmeantemp^2
    if ("sindex" %in% modvar) {input_data$sindex_meanmeantemp <- input_data$meanmeantemp * input_data$sindex}
    if ("minrain" %in% modvar) {input_data$minrain_meanmeantemp <- input_data$meanmeantemp * input_data$minrain}
    if ("seasrain" %in% modvar) {input_data$seasrain_meanmeantemp <- input_data$meanmeantemp * input_data$seasrain}
  }
  
  #maxtemp: sindex
  if ("maxmaxtemp" %in% modvar) {
    input_data$maxmaxtemp2 <- input_data$maxmaxtemp^2
    if ("sindex" %in% modvar) {input_data$sindex_maxmaxtemp <- input_data$maxmaxtemp * input_data$sindex}
    if ("minrain" %in% modvar) {input_data$minrain_maxmaxtemp <- input_data$maxmaxtemp * input_data$minrain}
    if ("seasrain" %in% modvar) {input_data$seasrain_maxmaxtemp <- input_data$maxmaxtemp * input_data$seasrain}
  }
  
  #mintemp: sindex
  if ("minmintemp" %in% modvar) {
    input_data$minmintemp2 <- input_data$minmintemp^2
    if ("sindex" %in% modvar) {input_data$sindex_minmintemp <- input_data$minmintemp * input_data$sindex}
    if ("minrain" %in% modvar) {input_data$minrain_minmintemp <- input_data$minmintemp * input_data$minrain}
    if ("seasrain" %in% modvar) {input_data$seasrain_minmintemp <- input_data$minmintemp * input_data$seasrain}
  }
  
  #minrain: daystcrit mintemp maxtemp meantemp
  if ("minrain" %in% modvar) {
    input_data$minrain2 <- input_data$minrain^2
    if ("totvpd" %in% modvar) {input_data$totvpd_minrain <- input_data$minrain * input_data$totvpd}
    if ("totgdd" %in% modvar) {input_data$totgdd_minrain <- input_data$minrain * input_data$totgdd}
    if ("soildul" %in% modvar) {input_data$soildul_minrain <- input_data$minrain * input_data$soildul}
  }
  
  #seasrain: daystcrit mintemp maxtemp meantemp
  if ("seasrain" %in% modvar) {
    input_data$seasrain2 <- input_data$seasrain^2
    if ("totvpd" %in% modvar) {input_data$totvpd_seasrain <- input_data$seasrain * input_data$totvpd}
    if ("totgdd" %in% modvar) {input_data$totgdd_seasrain <- input_data$seasrain * input_data$totgdd}
    if ("soildul" %in% modvar) {input_data$soildul_seasrain <- input_data$seasrain * input_data$soildul}
  }
  
  if ("setmax" %in% modvar) {
    input_data$setmax2 <- input_data$setmax^2
    if ("sindex" %in% modvar) {input_data$sindex_setmax <- input_data$setmax * input_data$sindex}
    if ("totgdd" %in% modvar) {input_data$totgdd_setmax <- input_data$setmax * input_data$totgdd}
    if ("totvpd" %in% modvar) {input_data$totvpd_setmax <- input_data$setmax * input_data$totvpd}
    if ("soildul" %in% modvar) {input_data$soildul_setmax <- input_data$setmax * input_data$soildul}
  }
  
  #rest of quadratic terms
  if ("sindex" %in% modvar) {
    input_data$sindex2 <- input_data$sindex^2
    #dindex: above
    #maxtemp, meantemp, mintemp: above
    #setmax: above
    if ("totgdd" %in% modvar) {input_data$totgdd_sindex <- input_data$sindex * input_data$totgdd}
    if ("totvpd" %in% modvar) {input_data$totvpd_sindex <- input_data$sindex * input_data$totvpd}
    if ("soildul" %in% modvar) {input_data$soildul_sindex <- input_data$sindex * input_data$soildul}
  }
  
  
  #totgdd: sindex vpd etmax seasrain dindex
  if ("totgdd" %in% modvar) {
    input_data$totgdd2 <- input_data$totgdd^2
    #dindex: above
    #minrain seasrain: above
    #sindex: above
    #setmax: above
    #sindex: above
    if ("totvpd" %in% modvar) {input_data$totvpd_totgdd <- input_data$totgdd * input_data$totvpd}
  }
  
  #totvpd: seasrain sindex minrain
  if ("totvpd" %in% modvar) {
    input_data$totvpd2 <- input_data$totvpd^2
    #minrain seasrain: above
    #sindex: above
    #totgdd: above
    if ("soildul" %in% modvar) {input_data$soildul_totvpd <- input_data$totvpd * input_data$soildul}
  }
  
  #soildul: seasrain, minrain, setmax, totvpd (all above)
  if ("soildul" %in% modvar) {input_data$soildul2 <- input_data$soildul^2}
  
  #return object
  return(input_data)
}



### function to get a given number of pseudo-absences (with env data)
get_pa <- function(spp_name,n_pa,bg_dir,msk_dir,int_dir,bio_dir,soil_dir,size=12500) {
  if (!file.exists(bg_dir)) {dir.create(bg_dir)}
  
  #check existence
  if (!file.exists(paste(bg_dir,"/",spp_name,"_bg-",n_pa,".RData",sep=""))) {
    cat("pseudo-absences don't exist yet, drawing pseudo absences, seed:",n_pa,"\n")
    #background area loading / creation
    bg_df <- load_bg(spp_name,bg_dir,msk_dir,bio_dir,soil_dir)
    
    #select PA from bg_df (using given seed). 
    #Sample according to where there is more harvested area
    cat("sampling with likelihood\n")
    int_rs <- raster(paste(int_dir,"/ci_ind.tif",sep=""))
    bg_df$prob <- extract(int_rs,bg_df)
    bg_df <- bg_df[which(!is.na(bg_df$prob)),]
    bg_df$prob <- bg_df$prob / sum(bg_df$prob)
    rm(int_rs); g=gc(); rm(g)
    
    set.seed(n_pa); pa_sel <- sample(1:nrow(bg_df),size,prob=bg_df$prob)
    bg_sel <- bg_df[pa_sel,]; rm(bg_df); g=gc(); rm(g)
    rownames(bg_sel) <- 1:nrow(bg_sel)
    bg_sel$prob <- NULL
    
    #5. extract climate data for pseudo absences
    #list all predictors
    pred_list <- list.files(bio_dir,pattern="\\.tif")
    pred_list <- pred_list[which(pred_list != "annrain.tif")]
    
    #extract climate for location data ### here i am!!!
    cat("extracting climate predictors for",nrow(bg_sel),"pseudo-absences \n")
    bg_data <- bg_sel
    for (pred in pred_list) {
      #pred <- pred_list[1]
      cat(pred,"...\n")
      pred_rs <- raster(paste(bio_dir,"/",pred,sep=""))
      bg_data <- cbind(bg_data,value=as.data.frame(extract(pred_rs,bg_sel)))
      names(bg_data)[ncol(bg_data)] <- paste(gsub("\\.tif","",pred))
      rm(pred_rs); g=gc(); rm(g)
    }
    
    #extract soil data
    cat("extracting soil\n")
    soil_rs <- raster(paste(soil_dir,"/dul_ind.tif",sep=""))
    bg_data <- cbind(bg_data,val=as.data.frame(extract(soil_rs,bg_sel)))
    names(bg_data)[ncol(bg_data)] <- "soildul"
    
    #check missing
    cat("final checks\n")
    bg_data$NAs <- apply(bg_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
    bg_data <- bg_data[which(bg_data$NAs == 0),]
    bg_data$NAs <- NULL
    rownames(bg_data) <- 1:nrow(bg_data)
    
    #sind * 10000 for formatting reasons
    bg_data$sindex <- bg_data$sindex * 10000
    bg_data$soildul <- bg_data$soildul * 100
    bg_data$annrain <- NULL #remove annual rainfall
    
    #write both objects into RData file
    cat("saving...\n")
    write.csv(bg_data,paste(bg_dir,"/",spp_name,"_bg-",n_pa,".csv",sep=""),row.names=F,quote=T)
    save(list=c("bg_data"),file=paste(bg_dir,"/",spp_name,"_bg-",n_pa,".RData",sep=""))
  } else {
    cat("pseudo-absences did exist, loading...\n")
    load(file=paste(bg_dir,"/",spp_name,"_bg-",n_pa,".RData",sep=""))
  }
  return(bg_data)
}


#### load background area / create if doesnt exist
load_bg <- function(spp_name,back_dir,msk_dir,bio_dir,soil_dir) {
  #filename
  bg_file <- paste(back_dir,"/",spp_name,"_bg.RData",sep="")
  if (!file.exists(bg_file)) {
    cat("creating background area as it didn't exist\n")
    
    #load verification files
    msk <- raster(paste(msk_dir,"/mask_30s.tif",sep=""))
    other_rs <- c(paste(soil_dir,"/dul_ind.tif",sep=""),
                  paste(bio_dir,"/seasrain.tif",sep=""))
    
    #get xy from bg
    cat("make data frame\n")
    bg_df <- as.data.frame(xyFromCell(msk,which(!is.na(msk[]))))
    
    #removing grid cells that are NA
    for (ors in other_rs) {
      cat("remove NAs from",ors,"\n")
      msk <- raster(ors); msk <- readAll(msk)
      bg_df$value <- extract(msk,cbind(x=bg_df$x,y=bg_df$y))
      bg_df <- bg_df[which(!is.na(bg_df$value)),]
      bg_df$value <- NULL
      rm(msk); g=gc(); rm(g)
    }
    
    #save object
    cat("saving...\n")
    save(list=c("bg_df"),file=bg_file)
  } else {
    cat("loading background area as it did exist\n")
    load(bg_file)
  }
  return(bg_df)
}


######################################
#declare function for analysis of VIF (variance inflation factor)
vif_analysis <- function(spp_name,occ_file,bio_dir,vif_dir,sol_dir) {
  if (!file.exists(vif_dir)) {dir.create(vif_dir,recursive=T)}
  
  spp_dir <- paste(vif_dir,"/",spp_name,sep="")
  if (!file.exists(spp_dir)) {dir.create(spp_dir)}
  
  if (!file.exists(paste(spp_dir,"/",spp_name,"_samples.RData",sep=""))) {
    cat("\nprocessing species",spp_name,"\n")
    
    #load occurrences
    #note names of x,y fields HAVE to be LON and LAT (respectively)
    spp <- read.csv(occ_file)
    loc_data <- data.frame(x=spp$LON,y=spp$LAT)
    
    #keep only unique occurrences
    loc_data <- unique(loc_data)
    
    #list all predictors
    pred_list <- list.files(bio_dir,pattern="\\.tif")
    
    #extract climate for location data
    cat("extracting climate predictors for",nrow(loc_data),"occurrences \n")
    bio_data <- loc_data
    for (pred in pred_list) {
      #pred <- pred_list[1]
      cat(pred,"...\n")
      pred_rs <- raster(paste(bio_dir,"/",pred,sep=""))
      bio_data <- cbind(bio_data,value=as.data.frame(extract(pred_rs,loc_data)))
      names(bio_data)[ncol(bio_data)] <- paste(gsub("\\.tif","",pred))
      rm(pred_rs); g=gc(); rm(g)
    }
    
    soil_rs <- raster(paste(sol_dir,"/dul_ind.tif",sep=""))
    bio_data <- cbind(bio_data,val=as.data.frame(extract(soil_rs,loc_data)))
    names(bio_data)[ncol(bio_data)] <- "soildul"
    
    #check for NAs
    bio_data$NAs <- apply(bio_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
    bio_data <- bio_data[which(bio_data$NAs == 0),]
    bio_data$NAs <- NULL
    
    #sind * 10000 for formatting reasons
    bio_data$sindex <- bio_data$sindex * 10000
    bio_data$soildul <- bio_data$soildul * 100
    bio_data$annrain <- NULL #remove annual rainfall
    
    #write data file (full climate)
    write.csv(bio_data,paste(spp_dir,"/",spp_name,"_full.csv",sep=""),row.names=F,quote=T)
    
    #vif analysis
    vif_res <- bio_data[,3:ncol(bio_data)]
    vif_res <- vifstep(vif_res,th=5) #threshold could be changed
    
    #listing predictors
    sel_var <- paste(vif_res@results$Variables)
    
    #filtering out useless variables in bio_data
    #!Remember sind has been scaled * 10000
    #(so the projection raster has to be scaled * 10000 before projecting)
    bio_sel <- bio_data[,c("x","y",sel_var)]
    
    #write sub-selected file (this is subset model)
    write.csv(bio_sel,paste(spp_dir,"/",spp_name,"_subset.csv",sep=""),row.names=F,quote=T)
    
    #output object (to be stored as RData)
    out_obj <- list(SPP=spp_name,RAW_OCC=spp,LOC_DATA=loc_data,ENV_FULL=bio_data,ENV_SUBSET=bio_sel,VIF_OUTPUT=vif_res)
    save(list=c("out_obj"),file=paste(spp_dir,"/",spp_name,"_samples.RData",sep=""))
    cat("done!, check object",paste(spp_name,"_samples.RData",sep=""),"\n")
  } else {
    cat("\nspecies",spp_name,"existed. Loading... \n")
    load(paste(spp_dir,"/",spp_name,"_samples.RData",sep=""))
  }
  return(out_obj)
}



