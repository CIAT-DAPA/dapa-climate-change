#JRV 2013
#CIAT / CCAFS

############################################################
### functions to fit models using BIOMOD2 library
############################################################

#function to run a model with provided configuration
proj_model <- function(base_dir,env_dir,spp_name,seed,npa,alg,vset,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  ## temporary: during development
  #base_dir=sdmDir
  #env_dir=envDir
  #spp_name=this_sppName
  #seed=this_seed
  #npa=this_npa
  #alg=this_alg
  #vset=this_vset
  #model_class="model_fit"
  ###
  
  #i/o dirs
  max_dir <- paste(base_dir,"/bin",sep="")
  data_dir <- paste(base_dir,"/input-samples",sep="")
  mod_dir <- paste(base_dir,"/fitting",sep="")
  bg_dir <- paste(base_dir,"/pseudo-absences",sep="")
  bio_dir <- paste(env_dir,"/climate/bio_ind_2_5min",sep="")
  sol_dir <- paste(env_dir,"/soil",sep="")
  msk_dir <- paste(env_dir,"/mask",sep="")
  
  out_dir <- paste(mod_dir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")
  eval_dir <- paste(out_dir,"/dis_eval",sep="")
  proj_dir <- paste(out_dir,"/proj/baseline",sep="")
  if (!file.exists(eval_dir)) {dir.create(eval_dir)}
  if (!file.exists(proj_dir)) {dir.create(proj_dir,recursive=T)}
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("fitting a",alg,"model, with tr_seed=",seed,", pa_seed=",npa,", vset=",vset,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  if (!file.exists(paste(eval_dir,"/evaluation.RData",sep=""))) {
    #2. load model object
    cat("load model objects\n")
    fit_file <- paste(out_dir,"/",gsub("\\_","\\.",spp_name),"/",gsub("\\_","\\.",spp_name),".",model_class,".models.out",sep="")
    setwd(out_dir)
    sp_mOut <- get(load(fit_file)) #load model outputs
    tmodel <- get(BIOMOD_LoadModels(sp_mOut,models=alg)) #load specific model
    
    #3. load 2.5min mask and create data.frame from it
    cat("load 2.5 arc-min mask\n")
    msk <- raster(paste(msk_dir,"/mask_2_5min.tif",sep=""))
    
    #4. extract all data that is in the model
    varmodel <- tmodel@expl_var_names
    if (!file.exists(paste(bg_dir,"/",spp_name,"_bg_env_2_5min.RData",sep=""))) {
      cat("extract env data for mask\n")
      if (!file.exists(paste(bg_dir,"/",spp_name,"_bg_2_5min.RData",sep=""))) {
        bg_df <- as.data.frame(xyFromCell(msk,which(!is.na(msk[]))))
        save(list=c("bg_df"),file=paste(bg_dir,"/",spp_name,"_bg_2_5min.RData",sep=""))
      } else {
        load(paste(bg_dir,"/",spp_name,"_bg_2_5min.RData",sep=""))
      }
      
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
      save(list=c("bg_data"),file=paste(bg_dir,"/",spp_name,"_bg_env_2_5min.RData",sep=""))
    } else {
      cat("loading pre-existing env mask data\n")
      load(paste(bg_dir,"/",spp_name,"_bg_env_2_5min.RData",sep=""))
    }
    
    ### remove unnecessary predictors in projection data.frame
    cat("drop unnecessary predictors\n")
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
    cat("make needed interaction terms\n")
    for (pred in varmodel) {
      #pred <- varmodel[12]
      if (!pred %in% names(bg_data)) {
        cat("   making additional predictor",pred,"...\n")
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
    cat("predicting over the whole area (2.5 arc-min)\n")
    if (alg == "MAXENT") {
      cat("   few changes because maxent...\n")
      tmodel@model_options$path_to_maxent.jar <- max_dir
      #for some reason biomod will treat -9999 as nodata for maxent
      bg_data$NAs <- apply(bg_data,1,FUN=function(x) {nac <- length(which(x == -9999)); return(nac)})
      if (length(which(bg_data$NAs > 0)) > 0) {
        for (i in which(bg_data$NAs > 0)) {
          bg_data[i,which(bg_data[i,] == -9999)] <- -9999.05
        }
      }
      bg_data$NAs <- NULL
    }
    prj_data <- predict(tmodel,bg_data[,3:ncol(bg_data)])
    
    #6. assess the model in the same way that ecocrop was assessed
    #a. load mask to make a raster
    cat("create 2.5 arc-min raster\n")
    tcells <- cellFromXY(msk,bg_data[,c("x","y")])
    out_rs <- raster(msk)
    out_rs[tcells] <- prj_data
    
    #b. assess the model
    cat("evaluating the 2.5 arc-min prediction\n")
    evrs <- raster(paste(base_dir,"/eval-msk/pa_fine.tif",sep=""))
    
    #ath: thresholds (SSS: maximum sensitivity and specificity)
    cat("   sss threshold\n")
    ath_sss <- sp_mOut@models.evaluation@val["ROC","Cutoff",1,1,1] * 0.001 #SSS
    eval_sss <- eval_sdm(rsl=out_rs,eval_rs=evrs,rocplot=T,plotdir=eval_dir,
                        filename="rocplot_2_5min.jpg",thresh=ath_sss)
    evtable <- cbind(THRESH="SSS",TVAL=ath_sss,eval_sss$METRICS)
    
    #predict over presence points and evaluate over prevalence threshold
    cat("   prevalence threshold\n")
    locdata <- get(load(sp_mOut@formated.input.data@link))
    locdata <- locdata@data.env.var[which(locdata@data.species == 1),]
    locdata <- as.numeric(predict(tmodel,locdata))
    ath_pre <- mean(locdata,na.rm=T)
    eval_pre <- eval_sdm(rsl=out_rs,eval_rs=evrs,rocplot=F,thresh=ath_pre)
    evtable <- rbind(evtable,cbind(THRESH="PRE",TVAL=ath_pre,eval_pre$METRICS))
    
    #evaluate over 10% (of prediction) threshold
    cat("   ten per cent\n")
    ath_ten <- as.numeric(quantile(prj_data,probs=0.1))
    eval_fix <- eval_sdm(rsl=out_rs,eval_rs=evrs,rocplot=F,thresh=ath_ten)
    evtable <- rbind(evtable,cbind(THRESH="TEN",TVAL=ath_ten,eval_fix$METRICS))
    
    ### 1dd projections and evaluations
    #7. load the imd_cru_climatology_1dd
    #load mask and data.frame with locations
    cat("load 1dd mask\n")
    msk1d <- raster(paste(msk_dir,"/mask_1dd.tif",sep=""))
    
    #extract env data
    if (!file.exists(paste(bg_dir,"/",spp_name,"_bg_env_1dd.RData",sep=""))) {
      cat("extract env data for 1dd mask\n")
      if (!file.exists(paste(bg_dir,"/",spp_name,"_bg_1dd.RData",sep=""))) {
        bg_df1d <- as.data.frame(xyFromCell(msk1d,which(!is.na(msk1d[]))))
        save(list=c("bg_df1d"),file=paste(bg_dir,"/",spp_name,"_bg_1dd.RData",sep=""))
      } else {
        load(paste(bg_dir,"/",spp_name,"_bg_1dd.RData",sep=""))
      }
      
      pred_list <- c(list.files(paste(env_dir,"/climate/imd_cru_climatology_1dd/1960_2000_bio",sep=""),
                                pattern="\\.tif"),"dul_ind_1dd.tif")
      pred_list <- pred_list[which(pred_list != "annrain.tif")]
      bg_data1d <- bg_df1d
      for (pred in pred_list) {
        #pred <- pred_list[1]
        cat(pred,"...\n")
        if (pred == "dul_ind_1dd.tif") {
          pred_rs <- raster(paste(sol_dir,"/",pred,sep=""))
        } else {
          pred_rs <- raster(paste(env_dir,"/climate/imd_cru_climatology_1dd/1966_1993_bio/",pred,sep=""))
        }
        bg_data1d <- cbind(bg_data1d,value=as.data.frame(extract(pred_rs,bg_df1d)))
        
        if (pred == "dul_ind_1dd.tif") {
          names(bg_data1d)[ncol(bg_data1d)] <- "soildul"
        } else {
          names(bg_data1d)[ncol(bg_data1d)] <- gsub("\\.tif","",pred)
        }
        rm(pred_rs); g=gc(); rm(g)
      }
      #check missing
      cat("final checks\n")
      bg_data1d$NAs <- apply(bg_data1d,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
      bg_data1d <- bg_data1d[which(bg_data1d$NAs == 0),]
      bg_data1d$NAs <- NULL
      rownames(bg_data1d) <- 1:nrow(bg_data1d)
      
      #scale other variables
      bg_data1d$sindex <- bg_data1d$sindex * 10000
      bg_data1d$soildul <- bg_data1d$soildul * 100
      
      #write object
      cat("write object\n")
      save(list=c("bg_data1d"),file=paste(bg_dir,"/",spp_name,"_bg_env_1dd.RData",sep=""))
    } else {
      cat("loading env data for 1dd mask\n")
      load(paste(bg_dir,"/",spp_name,"_bg_env_1dd.RData",sep=""))
    }
    
    ### remove unnecessary predictors in projection data.frame
    cat("remove unnecesary predictors\n")
    uniqvar <- unique(gsub("2","",varmodel))
    uniqvar <- unique(unlist(strsplit(uniqvar,"_",fixed=T)))
    remvar <- uniqvar[!uniqvar %in% names(bg_data)]
    if (length(remvar) > 0) {for (pred in remvar) {bg_data1d[,pred] <- NULL}}
    
    ### make required interaction terms
    cat("make needed interaction terms\n")
    for (pred in varmodel) {
      #pred <- varmodel[12]
      if (!pred %in% names(bg_data1d)) {
        cat("   making additional predictor",pred,"...\n")
        if (length(grep("_",pred)) > 0) {
          pred1 <- unlist(strsplit(pred,"_",fixed=T))[1]
          pred2 <- unlist(strsplit(pred,"_",fixed=T))[2]
          bg_data1d$value <- bg_data1d[,pred1] * bg_data1d[,pred2]
        } else {
          predlin <- gsub("2","",pred)
          bg_data1d$value <- bg_data1d[,predlin] ^ 2
        }
        names(bg_data1d)[ncol(bg_data1d)] <- pred
      }
    }
    
    #5. project the model
    cat("predicting over the whole area at 1dd\n")
    if (alg == "MAXENT") {
      cat("   few changes because maxent...\n")
      #for some reason biomod will treat -9999 as nodata for maxent
      bg_data1d$NAs <- apply(bg_data1d,1,FUN=function(x) {nac <- length(which(x == -9999)); return(nac)})
      if (length(which(bg_data1d$NAs > 0)) > 0) {
        for (i in which(bg_data1d$NAs > 0)) {
          bg_data1d[i,which(bg_data1d[i,] == -9999)] <- -9999.05
        }
      }
      bg_data1d$NAs <- NULL
    }
    prj_data1d <- as.numeric(predict(tmodel,bg_data1d[,3:ncol(bg_data1d)]))
    
    #6. assess the model in the same way that ecocrop was assessed
    #a. load mask to make a raster
    cat("make 1dd prediction raster \n")
    tcells <- cellFromXY(msk1d,bg_data1d[,c("x","y")])
    out_rs1d <- raster(msk1d)
    out_rs1d[tcells] <- prj_data1d
    
    #b. assess the model
    cat("evaluating the 1dd prediction\n")
    evrs1d <- raster(paste(base_dir,"/eval-msk/pa_coarse.tif",sep=""))
    
    #ath: thresholds (SSS: maximum sensitivity and specificity)
    cat("   sss threshold\n")
    eval_sss1d <- eval_sdm(rsl=out_rs1d,eval_rs=evrs1d,rocplot=T,plotdir=eval_dir,
                         filename="rocplot_1dd.jpg",thresh=ath_sss)
    evtable1d <- cbind(THRESH="SSS",TVAL=ath_sss,eval_sss1d$METRICS)
    
    #predict over presence points and evaluate over prevalence threshold
    cat("   prevalence threshold\n")
    eval_pre1d <- eval_sdm(rsl=out_rs1d,eval_rs=evrs1d,rocplot=F,thresh=ath_pre)
    evtable1d <- rbind(evtable1d,cbind(THRESH="PRE",TVAL=ath_pre,eval_pre1d$METRICS))
    
    #evaluate over 10% (of prediction) threshold
    cat("   ten per cent\n")
    ath_ten1d <- as.numeric(quantile(prj_data1d,probs=0.1))
    eval_fix1d <- eval_sdm(rsl=out_rs1d,eval_rs=evrs1d,rocplot=F,thresh=ath_ten1d)
    evtable1d <- rbind(evtable1d,cbind(THRESH="TEN",TVAL=ath_ten1d,eval_fix1d$METRICS))
    
    #here save evaluation object, 2.5min object, and 1dd object
    cat("making final objects\n")
    prj_2_5min <- out_rs; prj_1dd <- out_rs1d
    evaluation <- rbind(cbind(RESOL="2_5min",evtable),cbind(RESOL="1dd",evtable1d))
    details_2_5min <- list(SSS=eval_sss,PRE=eval_pre,TEN=eval_fix)
    details_1dd <- list(SSS=eval_sss1d,PRE=eval_pre1d,TEN=eval_fix1d)
    
    #write rasters
    cat("write rasters\n")
    prj_2_5min <- writeRaster(prj_2_5min,paste(proj_dir,"/proj_pd_2_5min.tif",sep=""),format="GTiff")
    prj_1dd <- writeRaster(prj_1dd,paste(proj_dir,"/proj_pd_1dd.tif",sep=""),format="GTiff")
    
    #write table
    cat("write evaluation table\n")
    write.csv(evaluation,paste(eval_dir,"/evaluation.csv",sep=""),row.names=F,quote=T)
    
    #save objects
    cat("write objects in RData files\n")
    save(list=c("prj_2_5min","prj_1dd"),file=paste(proj_dir,"/",spp_name,"_proj_pd.RData",sep=""))
    save(list=c("evaluation","details_2_5min","details_1dd"),
         file=paste(eval_dir,"/evaluation.RData",sep=""))
  }
}


#assess the accuracy of EcoCrop's spatial prediction using a gridded dataset
#of presence and absence
eval_sdm <- function(rsl,eval_rs,rocplot=F,plotdir="./img",filename="test.jpg",thresh=0) {
  pa_rsl <- rsl; pa_rsl[which(rsl[]>=thresh)] <- 1 #bin the prediction
  
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
  ab_p <- met$PRE_VAL[which(met$OBS == 0)] #model predictions at absence points
  pr_p <- met$PRE_VAL[which(met$OBS == 1)] #model predictions at presence points
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




