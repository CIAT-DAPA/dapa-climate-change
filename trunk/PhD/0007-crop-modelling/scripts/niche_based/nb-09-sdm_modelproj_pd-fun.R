#JRV 2013
#CIAT / CCAFS

############################################################
### functions to fit models using BIOMOD2 library
############################################################

#function to run a model with provided configuration
proj_model <- function(base_dir,env_dir,spp_name,seed,npa,alg,vset,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  #i/o dirs
  max_dir <- paste(base_dir,"/bin",sep="")
  data_dir <- paste(base_dir,"/input-samples",sep="")
  mod_dir <- paste(base_dir,"/fitting",sep="")
  bg_dir <- paste(base_dir,"/pseudo-absences",sep="")
  bio_dir <- paste(env_dir,"/climate/bio_ind_30s",sep="")
  sol_dir <- paste(env_dir,"/soil",sep="")
  msk_dir <- paste(env_dir,"/mask",sep="")
  
  #2. load model object
  out_dir <- paste(mod_dir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")
  fit_file <- paste(out_dir,"/",gsub("\\_","\\.",spp_name),"/",gsub("\\_","\\.",spp_name),".",model_class,".models.out",sep="")
  setwd(out_dir)
  sp_mOut <- get(load(fit_file)) #load model outputs
  tmodel <- get(BIOMOD_LoadModels(sp_mOut,models=alg)) #load specific model
  
  #3. load data frame (*_bg.RData) in pseudo-absences folder
  load(paste(bg_dir,"/",spp_name,"_bg.RData",sep=""))
  
  #4. extract all data that is in the model
  varmodel <- tmodel@expl_var_names
  if (!file.exists(paste(bg_dir,"/",spp_name,"_bg_env.RData",sep=""))) {
    pred_list <- c(list.files(bio_dir,pattern="\\.tif"),"dul_ind.tif")
    pred_list <- pred_list[which(pred_list != "annrain.tif")]
    bg_data <- bg_df
    for (pred in pred_list) {
      #pred <- pred_list[1]
      cat(pred,"...\n")
      if (pred == "dul_ind.tif") {
        pred_rs <- raster(paste(sol_dir,"/",pred,sep=""))
      } else {
        pred_rs <- raster(paste(bio_dir,"/",pred,sep=""))
      }
      bg_data <- cbind(bg_data,value=as.data.frame(extract(pred_rs,bg_df)))
      
      if (pred == "dul_ind.tif") {
        names(bg_data)[ncol(bg_data)] <- "soildul"
      } else {
        names(bg_data)[ncol(bg_data)] <- gsub("\\.tif","",pred)
      }
      rm(pred_rs); g=gc(); rm(g)
    }
    #check missing
    cat("final checks\n")
    bg_data$NAs <- apply(bg_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
    bg_data <- bg_data[which(bg_data$NAs == 0),]
    bg_data$NAs <- NULL
    rownames(bg_data) <- 1:nrow(bg_data)
    
    #scale other variables
    bg_data$sindex <- bg_data$sindex * 10000
    bg_data$soildul <- bg_data$soildul * 100
    
    #write object
    cat("write object\n")
    save(list=c("bg_data"),file=paste(bg_dir,"/",spp_name,"_bg_env.RData",sep=""))
  } else {
    cat("loading mask data\n")
    load(paste(bg_dir,"/",spp_name,"_bg_env.RData",sep=""))
  }
  
  ### remove unnecessary predictors in projection data.frame
  uniqvar <- unique(gsub("2","",varmodel))
  uniqvar <- unique(unlist(strsplit(uniqvar,"_",fixed=T)))
  remvar <- uniqvar[!uniqvar %in% names(bg_data)]
  if (length(remvar) > 0) {
    for (pred in remvar) {
      #pred <- remvar[1]
      bg_data[,pred] <- NULL
    }
  }
  
  ### make required interaction terms
  for (pred in varmodel) {
    #pred <- varmodel[12]
    if (!pred %in% names(bg_data)) {
      cat("making additional predictor",pred,"...\n")
      if (length(grep("_",pred)) > 0) {
        pred1 <- unlist(strsplit(pred,"_",fixed=T))[1]
        pred2 <- unlist(strsplit(pred,"_",fixed=T))[2]
        bg_data$value <- bg_data[,pred1] * bg_data[,pred2]
      } else {
        predlin <- gsub("2","",pred)
        bg_data$value <- bg_data[,predlin] ^ 2
      }
      names(bg_data)[ncol(bg_data)] <- pred
    }
  }
  
  #5. project the model
  prj_data <- as.numeric(predict(tmodel,bg_data[,3:ncol(bg_data)]))
  
  #6. assess the model in the same way that ecocrop was assessed
  #aa. load mask to make a raster
  msk <- raster(paste(msk_dir,"/mask_30s.tif",sep=""))
  tcells <- cellFromXY(msk,bg_data[,c("x","y")])
  out_rs <- raster(msk)
  out_rs[tcells] <- prj_data
  
  #b. assess the model
  eval_dir <- paste(out_dir,"/dis_eval",sep="")
  if (!file.exists(eval_dir)) {dir.create(eval_dir)}
  evrs <- raster(paste(base_dir,"/eval-msk/pa_fine.tif",sep=""))
  
  #ath: thresholds (prevalence and SSS)
  ath <- sp_mOut@models.evaluation@val["ROC","Cutoff",1,1,1] * 0.001 #SSS
  eval_sss <- eval_sdm(rsl=out_rs,eval_rs=evrs,rocplot=T,plotdir=eval_dir,
                      filename="rocplot_sss.jpg",thresh=ath)
  evtable <- cbind(THRESH="SSS",TVAL=ath,eval_sss$METRICS)
  
  #predict over presence points and evaluate over prevalence threshold
  locdata <- get(load(sp_mOut@formated.input.data@link))
  locdata <- locdata@data.env.var[which(locdata@data.species == 1),]
  locdata <- as.numeric(predict(tmodel,locdata))
  ath <- mean(locdata,na.rm=T)
  eval_pre <- eval_sdm(rsl=out_rs,eval_rs=evrs,rocplot=T,plotdir=eval_dir,
                       filename="rocplot_pre.jpg",thresh=ath)
  evtable <- rbind(evtable,cbind(THRESH="PREV",TVAL=ath,eval_pre$METRICS))
  
  #here i am!!!
  
  
  ########################################################################
  ########################################################################
  spp_tr_pred <- as.numeric(predict(tmodel,spp_tr[,3:ncol(spp_tr)]))
  pab_tr_pred <- as.numeric(predict(tmodel,pab_tr[,3:ncol(pab_tr)]))
  
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
  
  #return object
  return(outDir)
}


#assess the accuracy of EcoCrop's spatial prediction using a gridded dataset
#of presence and absence
eval_sdm <- function(rsl,eval_rs,rocplot=F,plotdir="./img",filename="test.jpg",thresh=0) {
  pa_rsl <- rsl; pa_rsl[which(rsl[]>thresh)] <- 1 #bin the prediction
  
  met <- xyFromCell(eval_rs,1:ncell(eval_rs))
  met <- cbind(met,PRE=extract(pa_rsl,met[,1:2]))
  met <- cbind(met,OBS=extract(eval_rs,met[,1:2]))
  met <- cbind(met,PRE_VAL=extract(rsl,met[,1:2]))
  met <- as.data.frame(met)
  met <- met[which(!is.na(met$PRE)),]; met <- met[which(!is.na(met$OBS)),] #get rid of NAs
  
  #get the values *1 is observed and 2 is prediction
  ntp <- length(which(met$PRE > 0 & met$OBS == 1))
  tpr <- ntp/length(which(met$OBS == 1))
  #false negative rate
  nfp <- length(which(met$PRE == 0 & met$OBS == 1))
  fpr <- nfp/length(which(met$OBS == 1))
  #true negative rate (if absences are available)
  if (length(which(met$OBS == 0)) != 0) {
    ntn <- length(which(met$PRE > 0 & met$OBS == 0))
    tnr <- ntn / length(which(met$OBS == 0))
  } else {tnr <- NA}
  
  #calculate the auc
  #to prevent integer overflow in AUC calculation
  if (nrow(met) > 50000) {
    set.seed(1234); met <- met[sample(1:nrow(met),20000,),]
  }
  ab_p <- met$PRE_VAL[which(met$OBS == 0)]
  pr_p <- met$PRE_VAL[which(met$PRE == 1)]
  deval <- evaluate(p=pr_p,a=ab_p)
  rm(met); g=gc(); rm(g)
  
  if (rocplot) {
    if (!file.exists(plotdir)) {stop("Check your plotting directory as it doesnt seem to exist")}
    jpeg(paste(plotdir,"/",filename,sep=""), quality=100, height=1024, width=1024,pointsize=8,res=300)
    par(mar=c(4,4,1,1))
    plot(deval@FPR,deval@TPR,main=NA,ty="l",lty=1,lwd=1.5,col="black",
         xlim=c(0,1),ylim=c(0,1),xlab="False positive rate",ylab="True positive rate")
    abline(0,1,col="grey 50",)
    grid()
    text(0,1,paste("AUC=",round(deval@auc,4),sep=""),col="black",adj=c(0,1))
    dev.off()
  }
  
  met.final <- data.frame(TPR=tpr, FPR=fpr, TNR=tnr, AUC=deval@auc, KAPPA=max(deval@kappa))
  retobj <- list(EVAL=deval,METRICS=met.final)
  return(retobj)
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


