#JRV 2013
#CIAT / CCAFS

############################################################
### functions to fit models using BIOMOD2 library
############################################################

#function to run a model with provided configuration
run_model <- function(bDir,sppName,seed,npa,alg,vset,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  #i/o dirs
  maxDir <- paste(bDir,"/bin",sep="")
  dataDir <- paste(bDir,"/vif-analysis",sep="")
  modDir <- paste(bDir,"/models",sep="")
  bgDir <- paste(bDir,"/bg-areas",sep="")
  envDir <- paste(bDir,"/env-data",sep="")
  bioDir <- paste(envDir,"/bioclim_gtiff",sep="")
  solDir <- paste(envDir,"/soil",sep="")
  topDir <- paste(envDir,"/topography",sep="")
  
  #1. output model and config directory
  cat("loading presence and pseudo-absence data\n")
  outDir <- paste(modDir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  #2. load species data (from VIF analysis output)
  spp_data <- read.csv(paste(dataDir,"/",sppName,"/",sppName,"_",varList$CLIM[vset],".csv",sep=""))
  
  #3. load background data / create if needs be
  pab_data <- get_pa(spp_name=sppName,
                     spp_data=read.csv(paste(dataDir,"/",sppName,"/",sppName,"_full.csv",sep="")),
                     n_pa=npa,bg_dir=bgDir,bio_dir=bioDir,soil_dir=solDir,topo_dir=topDir)
  pab_data <- pab_data[,names(spp_data)] #remove extra variables
  
  #run only if final stored objects do not exist
  if (!file.exists(paste(outDir,"/",gsub("\\_","\\.",sppName),"/fitting.RData",sep=""))) {
    cat("\n-------------------------------------------------------\n")
    cat("---- processing species", sppName,"\n")
    cat("-------------------------------------------------------\n")
    
    #if soil is within variables to be modelled then make factor else remove from data.frame
    if (varList$SOIL[vset]) {
      #commented out JRV because ordinal variables are treated as continuous
      #spp_data$soildrain_final <- as.factor(spp_data$soildrain_final)
      #pab_data$soildrain_final <- as.factor(pab_data$soildrain_final)
    } else {
      spp_data$soildrain_final <- NULL
      pab_data$soildrain_final <- NULL
    }
    
    #if topo is _not_ within variables to be modelled then remove from data.frame
    if (!varList$TOPO[vset]) {
      spp_data$aspect <- NULL; spp_data$slope <- NULL
      pab_data$aspect <- NULL; pab_data$slope <- NULL
    }
    
    #4. select 25 % test data using a given seed 
    cat("selecting train/test data using given seed\n")
    set.seed(seed); sp_sel <- sample(1:nrow(spp_data),size=round((nrow(spp_data)*.30),0))
    set.seed(seed); pa_sel <- sample(1:nrow(pab_data),size=round((nrow(pab_data)*.30),0))
    
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
                                     resp.name = sppName, 
                                     PA.strategy="random",
                                     PA.nb.rep=1,PA.nb.absences=npa)
    
    #selecting model features (!change features as needed)
    sp_mOpt <- BIOMOD_ModelingOptions()
    sp_mOpt@MAXENT$path_to_maxent.jar <- maxDir
    sp_mOpt@MAXENT$maximumiterations <- 500
    sp_mOpt@GBM$n.trees <- 2000
    sp_mOpt@GLM$control$maxit <- 100
    sp_mOpt@RF$ntree <- 1000
    #sp_mOpt@RF$do.classif <- F
    #sp_mOpt@RF$mtry <- 2
    #sp_mOpt@GLM$interaction.level <- 1 #removed: JRV Jun 9 (interaction takes too long)
    #sp_mOpt@GLM$type <- "quadratic" #simple | quadratic | polynomial
    #sp_mOpt@ANN$maxit <- 500
    
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
                                 VarImport=5,
                                 models.eval.meth = c('KAPPA','TSS','ROC'),
                                 SaveObj = TRUE,
                                 rescal.all.models = FALSE,
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
    save(list=c("sp_bData","sp_mEval","sp_mOut"),file=paste(outDir,"/",sp_bData@sp.name,"/fitting.RData",sep=""))
    
    #go back to base directory
    setwd(bDir)
  }
  
  #return object
  return(outDir)
}



#function to fit and evaluate a geographically null model with provided configuration
run_null_model <- function(bDir,sppName,alg,seed,npa) {
  require(biomod2); require(raster); require(dismo)
  
  #i/o dirs
  dataDir <- paste(bDir,"/vif-analysis",sep="")
  modDir <- paste(bDir,"/models",sep="")
  bgDir <- paste(bDir,"/bg-areas",sep="")
  envDir <- paste(bDir,"/env-data",sep="")
  bioDir <- paste(envDir,"/bioclim_gtiff",sep="")
  solDir <- paste(envDir,"/soil",sep="")
  topDir <- paste(envDir,"/topography",sep="")
  
  cat("\nanalysing seed=",seed,"and seed=",npa,"for pseudo absences for",alg,"\n")
  
  #1. output model and config directory
  cat("creating directories\n")
  outDir <- paste(modDir,"/",alg,"_NULL/SD-",seed,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  outgdDir <- paste(outDir,"/GD_MODEL/",sppName,sep="")
  if (!file.exists(outgdDir)) {dir.create(outgdDir,recursive=T)}
  
  outevDir <- paste(outDir,"/EVAL/",sppName,sep="")
  if (!file.exists(outevDir)) {dir.create(outevDir,recursive=T)}
  
  if (!file.exists(paste(outevDir,"/PA-",npa,".RData",sep=""))) {
    cat("loading presence and pseudo-absence data\n")
    #2. load species data (from VIF analysis output)
    spp_data <- read.csv(paste(dataDir,"/",sppName,"/",sppName,"_full.csv",sep="")) #full set
    
    #3. load background data / create if needs be
    pab_data <- get_pa(spp_name=sppName,spp_data=spp_data,alg=alg,n_pa=npa,bg_dir=bgDir,
                       bio_dir=bioDir,soil_dir=solDir,topo_dir=topDir,size=12500)
    
    #remove extra variables
    spp_data <- spp_data[,c("x","y")]
    pab_data <- pab_data[,c("x","y")]
    
    #4. select train / test samples
    cat("bootstrapped selection 30/70 % of data \n")
    set.seed(seed); sp_sel <- sample(1:nrow(spp_data),size=round((nrow(spp_data)*.30),0))
    set.seed(seed); pa_sel <- sample(1:nrow(pab_data),size=round((nrow(pab_data)*.30),0))
    
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
    
    i <- pwdSample(cbind(x=spp_te$x,y=spp_te$y), cbind(x=pab_te$x,y=pab_te$y), cbind(x=spp_tr$x,y=spp_tr$y), n=1, tr=0.33, warn=F)
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
  bestStat <- max(calcStat, na.rm = T)
  return(bestStat)
}


### function to get a given number of pseudo-absences (with env data)
get_pa <- function(spp_name,spp_data,alg,n_pa,bg_dir,bio_dir,soil_dir,topo_dir,size=12500) {
  #check existence
  if (!file.exists(paste(bg_dir,"/",spp_name,"/sdm_",alg,"_bg_",n_pa,".RData",sep=""))) {
    cat("pseudo-absences don't exist yet, drawing pseudo-absences seed=",n_pa,"\n")
    
    #background area loading / creation
    bg_file <- paste(bg_dir,"/",spp_name,"/sampling_bias/sampling_bias_",alg,".RData",sep="")
    load(bg_file); bg_df <- bg_bias; rm(bg_bias); g=gc(); rm(g)
    bg_df$bias <- bg_df$bias / sum(bg_df$bias)
    
    #select PA from bg_df (without defining seed)
    cat("sampling\n")
    set.seed(n_pa); pa_sel <- sample(1:nrow(bg_df),size,prob=bg_df$bias)
    bg_sel <- bg_df[pa_sel,]; rm(bg_df); g=gc(); rm(g)
    bg_sel <- bg_sel[,c("x","y")]
    rownames(bg_sel) <- 1:nrow(bg_sel)
    
    #5. extract climate data for pseudo absences
    cat("extracting climate\n")
    bio_stk <- stack(c(paste(bio_dir,"/",names(spp_data)[3:(ncol(spp_data)-3)],".tif",sep="")))
    bg_data <- as.data.frame(extract(bio_stk,bg_sel))
    bg_data <- cbind(bg_sel,bg_data)
    bg_data$sind <- bg_data$sind * 10000
    
    #extract soil data
    cat("extracting soil\n")
    soil_rs <- stack(paste(soil_dir,"/soildrain_final.tif",sep=""))
    bg_data <- cbind(bg_data,as.data.frame(extract(soil_rs,bg_sel))) #for background
    
    #extract topography data
    cat("extracting topography\n")
    top_stk <- stack(paste(topo_dir,"/",c("aspect.tif","slope.tif"),sep=""))
    bg_data <- cbind(bg_data,as.data.frame(extract(top_stk,bg_sel))) #for pa
    bg_data$slope <- bg_data$slope * 100
    
    #check missing
    cat("final checks\n")
    bg_data$NAs <- apply(bg_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
    bg_data <- bg_data[which(bg_data$NAs == 0),]
    bg_data$NAs <- NULL
    rownames(bg_data) <- 1:nrow(bg_data)
    
    #write both objects into RData file
    cat("saving...\n")
    write.csv(bg_data,paste(bg_dir,"/",spp_name,"/sdm_",alg,"_bg_",n_pa,".csv",sep=""),row.names=F,quote=T)
    save(list=c("bg_data"),file=paste(bg_dir,"/",spp_name,"/sdm_",alg,"_bg_",n_pa,".RData",sep=""))
  } else {
    cat("pseudo-absences did exist, loading...\n")
    load(file=paste(bg_dir,"/",spp_name,"/sdm_",alg,"_bg_",n_pa,".RData",sep=""))
  }
  return(bg_data)
}




