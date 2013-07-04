#JRV 2013
#CIAT / CCAFS

############################################################
### functions to fit models using BIOMOD2 library
############################################################

#function to run a model with provided configuration
proj_model <- function(bDir,sppName,seed,npa,alg,vset,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  #i/o dirs
  maxDir <- paste(bDir,"/bin",sep="")
  modDir <- paste(bDir,"/samplebias",sep="")
  prjDir <- paste(bDir,"/samplebias_prj",sep="")
  bgDir <- paste(bDir,"/bg-areas",sep="")
  envDir <- paste(bDir,"/env-data",sep="")
  topDir <- paste(envDir,"/topography",sep="")
  accDir <- paste(envDir,"/access_50k",sep="")
  if (!file.exists(prjDir)) {dir.create(prjDir)}
  
  #genus name
  genName <- unlist(strsplit(sppName,"_",fixed=T))[1]
  
  inp_dir <- paste(modDir,"/",alg,"/PA-",npa,sep="")
  out_dir <- paste(prjDir,"/",alg,"/PA-",npa,"/",genName,sep="")
  if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("projecting (b) a",alg,"model, with pa_seed=",npa,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  if (!file.exists(paste(out_dir,"/prjdata.RData",sep=""))) {
    #2. load model object
    cat("load model objects\n")
    fit_file <- paste(inp_dir,"/",gsub("\\_","\\.",genName),"/",gsub("\\_","\\.",genName),".",model_class,".models.out",sep="")
    setwd(inp_dir)
    sp_mOut <- get(load(fit_file)) #load model outputs
    tmodel <- get(BIOMOD_LoadModels(sp_mOut,models=alg)) #load specific model
    
    #3. load 30s mask
    msk <- raster(paste(bgDir,"/",sppName,"/",sppName,".tif",sep=""))
    
    #4. extract all data that is in the model
    if (!file.exists(paste(bgDir,"/",sppName,"/",sppName,"_sbias_bg_env_30s.RData",sep=""))) {
      cat("extract env data for mask\n")
      load(paste(bgDir,"/",sppName,"/",sppName,"_sbias_bg.RData",sep=""))
      
      bg_data <- bg_df
      pred_rs <- raster(paste(accDir,"/access.tif",sep="")); pred_rs <- readAll(pred_rs)
      bg_data <- cbind(bg_data,access=extract(pred_rs,bg_df)); rm(pred_rs); g=gc(); rm(g)
      pred_rs <- raster(paste(topDir,"/slope.tif",sep="")); pred_rs <- readAll(pred_rs)
      bg_data <- cbind(bg_data,slope=extract(pred_rs,bg_df)); rm(pred_rs); g=gc(); rm(g)
      pred_rs <- raster(paste(topDir,"/slope.tif",sep="")); pred_rs <- readAll(pred_rs)
      bg_data <- cbind(bg_data,aspect=extract(pred_rs,bg_df)); rm(pred_rs); g=gc(); rm(g)
      
      #check missing
      cat("final checks\n")
      bg_data$NAs <- apply(bg_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
      bg_data <- bg_data[which(bg_data$NAs == 0),]
      bg_data$NAs <- NULL
      rownames(bg_data) <- 1:nrow(bg_data)
      bg_data <- cbind(cell=cellFromXY(msk,bg_data[,c("x","y")]),bg_data)
      
      #scale other variables
      bg_data$slope <- bg_data$slope * 100
      
      #write object
      cat("write object\n")
      save(list=c("bg_data"),file=paste(bgDir,"/",sppName,"/",sppName,"_sbias_bg_env_30s.RData",sep=""))
    } else {
      cat("loading pre-existing env mask data\n")
      load(paste(bgDir,"/",sppName,"/",sppName,"_sbias_bg_env_30s.RData",sep=""))
    }
    
    
    #### divide to reduce memory use
    #determine maximum processing load
    bs <- blockSize(msk, n=2, minblocks=2)
    cat("processing in: ", bs$n, " chunks \n", sep="")
    
    #output raster
    outraster <- raster(msk)
    
    for (b in 1:bs$n) {
      #b <- 1
      cat(" ",round(b/bs$n*100,2),"%",sep="")
      
      iniCell <- 1+(bs$row[b]-1)*ncol(outraster)
      finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(outraster)
      allCells <- iniCell:finCell
      validCells <- allCells[which(!is.na(msk[allCells]))]
      validXY <- xyFromCell(msk,validCells)
      
      if (length(validCells) > 0) {
        rowVals <- extract(clm_stk,validCells)
        rowVals <- cbind(rowVals,sow=extract(sow_date,validXY))
        rowVals <- cbind(rowVals,har=extract(har_date,validXY))
        rowVals <- cbind(rowVals,validXY[,2])
        rasVals <- apply(rowVals, 1, this_fun, ...)
      } else {
        rasVals <- NA
      }
      outraster[validCells] <- rasVals
    }
    cat("\n")
    return(outraster)
    ### make required interaction terms
    
    
    
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



#function to run a model with provided configuration
run_bias_model <- function(bDir,sppName,npa,alg,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  #i/o dirs
  maxDir <- paste(bDir,"/bin",sep="")
  dataDir <- paste(bDir,"/occurrences",sep="")
  modDir <- paste(bDir,"/samplebias",sep="")
  bgDir <- paste(bDir,"/bg-areas",sep="")
  envDir <- paste(bDir,"/env-data",sep="")
  topDir <- paste(envDir,"/topography",sep="")
  accDir <- paste(envDir,"/access_50k",sep="")
  if (!file.exists(modDir)) {dir.create(modDir)}
  
  #genus name
  genName <- unlist(strsplit(sppName,"_",fixed=T))[1]
  
  #1. output model and config directory
  cat("loading presence and pseudo-absence data\n")
  outDir <- paste(modDir,"/",alg,"/PA-",npa,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  #2. load species data (from occurrences dir)
  if (!file.exists(paste(dataDir,"/",genName,"_sbias_occ.csv",sep=""))) {
    spp_data <- read.csv(paste(dataDir,"/",genName,".txt",sep=""))
    spp_data <- spp_data[,c("ID","LONGITUDE","LATITUDE")]
    
    #extract env data for spp data
    spp_data <- unique(spp_data[,2:3])
    rs <- raster(paste(accDir,"/access.tif",sep=""))
    spp_data <- cbind(spp_data,access=extract(rs,spp_data[,c("LONGITUDE","LATITUDE")]))
    rm(rs); g=gc(); rm(g)
    rs <- stack(paste(topDir,"/",c("slope","aspect"),".tif",sep=""))
    spp_data <- cbind(spp_data,extract(rs,spp_data[,c("LONGITUDE","LATITUDE")]))
    rm(rs); g=gc(); rm(g)
    spp_data$slope <- spp_data$slope * 100
    spp_data$NAS <- apply(spp_data,1,FUN=function(x) {length(which(is.na(x)))})
    spp_data <- spp_data[which(spp_data$NAS == 0),]
    spp_data$NAS <- NULL #removes field NAS
    rownames(spp_data) <- 1:nrow(spp_data)
    write.csv(spp_data,paste(dataDir,"/",genName,"_sbias_occ.csv",sep=""),quote=T,row.names=F)
  } else {
    spp_data <- read.csv(paste(dataDir,"/",genName,"_sbias_occ.csv",sep=""))
  }
  
  #3. load background data / create if needs be
  pab_data <- get_pa_sbias(spp_name=sppName,spp_data=spp_data,n_pa=npa,bg_dir=bgDir,
                           topo_dir=topDir,acc_dir=accDir)
  
  #run only if final stored objects do not exist
  if (!file.exists(paste(outDir,"/",gsub("\\_","\\.",genName),"/fitting.RData",sep=""))) {
    cat("\n-------------------------------------------------------\n")
    cat("---- processing genus", genName,"\n")
    cat("-------------------------------------------------------\n")
    
    #put interactions into data frames
    cat("making interactions\n")
    spp_data$access2 <- spp_data$access ^ 2; spp_data$access3 <- spp_data$access ^ 3
    spp_data$slope2 <- spp_data$slope ^ 2; spp_data$slope3 <- spp_data$slope ^ 3
    spp_data$aspect2 <- spp_data$aspect ^ 2; spp_data$aspect3 <- spp_data$aspect ^ 3
    spp_data$access_slope <- spp_data$access * spp_data$slope
    spp_data$access_aspect <- spp_data$access * spp_data$aspect
    spp_data$aspect_slope <- spp_data$slope * spp_data$aspect
    spp_data$access_aspect_slope <- spp_data$access * spp_data$slope * spp_data$aspect
    
    pab_data$access2 <- pab_data$access ^ 2; pab_data$access3 <- pab_data$access ^ 3
    pab_data$slope2 <- pab_data$slope ^ 2; pab_data$slope3 <- pab_data$slope ^ 3
    pab_data$aspect2 <- pab_data$aspect ^ 2; pab_data$aspect3 <- pab_data$aspect ^ 3
    pab_data$access_slope <- pab_data$access * pab_data$slope
    pab_data$access_aspect <- pab_data$access * pab_data$aspect
    pab_data$aspect_slope <- pab_data$slope * pab_data$aspect
    pab_data$access_aspect_slope <- pab_data$access * pab_data$slope * pab_data$aspect
    
    spp_tr <- spp_data; names(spp_tr)[1:2] <- c("x","y")
    pab_tr <- pab_data
    
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
                                     resp.name = genName, 
                                     PA.strategy="random",
                                     PA.nb.rep=1,PA.nb.absences=10000)
    
    #selecting model features (!change features as needed)
    sp_mOpt <- BIOMOD_ModelingOptions()
    sp_mOpt@MAXENT$path_to_maxent.jar <- maxDir
    sp_mOpt@MAXENT$maximumiterations <- 1000
    sp_mOpt@MAXENT$quadratic <- F
    sp_mOpt@MAXENT$product <- F
    sp_mOpt@MAXENT$threshold <- F
    sp_mOpt@MAXENT$hinge <- F
    sp_mOpt@GBM$n.trees <- 2000
    sp_mOpt@GLM$type <- "simple" #simple | quadratic | polynomial
    sp_mOpt@GLM$control$maxit <- 100
    sp_mOpt@GAM$k <- 3
    sp_mOpt@RF$ntree <- 100
    
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
    
    #save object with all necessary details
    cat("saving final objects\n")
    save(list=c("sp_bData","sp_mOut"),file=paste(outDir,"/",sp_bData@sp.name,"/fitting.RData",sep=""))
    
    #go back to base directory
    setwd(bDir)
  }
  #return object
  return(outDir)
}


### function to get a given number of pseudo-absences (with env data)
get_pa_sbias <- function(spp_name,spp_data,n_pa,bg_dir,topo_dir,acc_dir) {
  #check existence
  gen_name <- unlist(strsplit(spp_name,"_",fixed=T))[1]
  
  if (!file.exists(paste(bg_dir,"/",spp_name,"/sbias_bg_",n_pa,".RData",sep=""))) {
    cat("pseudo-absences don't exist yet, drawing pseudo absences",n_pa,"\n")
    #background area loading / creation
    bg_df <- load_bg_sbias(spp_name,bg_dir,topo_dir,acc_dir)
    
    #select PA from bg_df (without defining seed)
    cat("sampling\n")
    set.seed(n_pa); pa_sel <- sample(1:nrow(bg_df),11000)
    bg_sel <- bg_df[pa_sel,]; rm(bg_df); g=gc(); rm(g)
    rownames(bg_sel) <- 1:nrow(bg_sel)
    
    #5. extract climate data for pseudo absences
    cat("extracting climate\n")
    acc_stk <- stack(c(paste(acc_dir,"/access.tif",sep="")))
    bg_data <- as.data.frame(extract(acc_stk,bg_sel))
    bg_data <- cbind(bg_sel,bg_data)
    
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
    bg_data <- bg_data[1:10000,]
    rownames(bg_data) <- 1:nrow(bg_data)
    
    #write both objects into RData file
    cat("saving...\n")
    write.csv(bg_data,paste(bg_dir,"/",spp_name,"/sbias_bg_",n_pa,".csv",sep=""),row.names=F,quote=T)
    save(list=c("bg_data"),file=paste(bg_dir,"/",spp_name,"/sbias_bg_",n_pa,".RData",sep=""))
  } else {
    cat("pseudo-absences did exist, loading...\n")
    load(file=paste(bg_dir,"/",spp_name,"/sbias_bg_",n_pa,".RData",sep=""))
  }
  return(bg_data)
}


#### load background area / create if doesnt exist
load_bg_sbias <- function(spp_name,back_dir,topo_dir,acc_dir) {
  #filename
  bg_file <- paste(back_dir,"/",spp_name,"/",spp_name,"_sbias_bg.RData",sep="")
  if (!file.exists(bg_file)) {
    cat("creating background area as it didn't exist\n")
    
    #load verification files
    msk <- raster(paste(acc_dir,"/access.tif",sep=""))
    other_rs <- c(paste(topo_dir,"/aspect.tif",sep=""),
                  paste(topo_dir,"/slope.tif",sep=""))
    
    #rasterize the shapefile
    if (!file.exists(paste(back_dir,"/",spp_name,"/",spp_name,".tif",sep=""))) {
      cat("rasterize\n")
      #check existence of shapefile
      bg_sh <- paste(back_dir,"/",spp_name,"/",spp_name,".shp",sep="")
      if (!file.exists(bg_sh)) {stop("shapefile does not exist, please check")}
      #load input shapefile and mask
      bg_sh <- readShapePoly(bg_sh)
      #rasterize
      bg_rs <- rasterize(bg_sh,msk,silent=T,filename=paste(back_dir,"/",spp_name,"/",spp_name,".tif",sep=""),format="GTiff")
    } else {
      cat("load raster\n")
      bg_rs <- raster(paste(back_dir,"/",spp_name,"/",spp_name,".tif",sep=""))
    }
    
    #get xy from bg
    cat("make data frame\n")
    bg_rs[which(!is.na(bg_rs[]))] <- 1
    bg_df <- as.data.frame(xyFromCell(bg_rs,which(!is.na(bg_rs[]))))
    
    #removing grid cells that are NA
    cat("remove NAs from access\n")
    msk <- readAll(msk); rm(bg_rs); g=gc(); rm(g)
    bg_df$value <- extract(msk,cbind(x=bg_df$x,y=bg_df$y))
    bg_df <- bg_df[which(!is.na(bg_df$value)),]
    rm(msk); g=gc(); rm(g)
    bg_df$value <- NULL
    
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


