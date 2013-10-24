#JRV 2013
#CIAT / CCAFS

############################################################
### functions to project models using BIOMOD2 library
############################################################

#function to run a model with provided configuration
proj_model <- function(bDir,sppName,seed,npa,alg,vset,scenario,period,model,model_class="model_fit") {
  require(biomod2); require(raster); require(rgdal); require(maptools); require(dismo)
  
  #bDir <- bDir
  #sppName <- this_sppName
  #seed <- this_seed
  #npa <- this_npa
  #alg <- this_alg
  #vset <- this_vset
  #scenario <- tsce
  #period <- tper
  #model <- tmodel
  #model_class <- "model_fit"
  
  #i/o dirs
  maxDir <- paste(bDir,"/bin",sep="")
  modDir <- paste(bDir,"/models",sep="")
  prjDir <- paste(bDir,"/models_prj",sep="")
  envDir <- paste(bDir,"/env-data/proj_COL",sep="")
  bioDir <- paste(envDir,"/",scenario,"/Global_2_5min/",model,"/",period,"/_tiffs",sep="")
  topDir <- paste(envDir,"/topography",sep="")
  solDir <- paste(envDir,"/soil",sep="")
  if (!file.exists(prjDir)) {dir.create(prjDir)}
  
  sppName2 <- gsub("_",".",sppName,fixed=T)
  
  inp_dir <- paste(modDir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")
  out_dir <- paste(prjDir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,"/",sppName2,
                   "/",scenario,"-",period,"-",model,sep="")
  if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("projecting a",alg,"model, with pa_seed=",npa,"seed=",seed,"and variables=",vset,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  if (!file.exists(paste(out_dir,"/sdm_proj.tif",sep=""))) {
    #2. load model object
    cat("load model objects\n")
    fit_file <- paste(inp_dir,"/",sppName2,"/",sppName2,".",model_class,".models.out",sep="")
    setwd(paste(modDir,"/",alg,"/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")) #%#%#%#
    sp_mOut <- get(load(fit_file)) #load model outputs
    tmodel <- get(BIOMOD_LoadModels(sp_mOut,models=alg)) #load specific model
    
    #3. load 2.5 mask and get xy
    cat("load projection data\n")
    msk <- raster(paste(bioDir,"/bio_1.tif",sep=""))
    prj_data <- as.data.frame(xyFromCell(msk,which(!is.na(msk[]))))
    prj_data$CELL <- which(!is.na(msk[]))
    
    #put data into prj_data
    bio_stk <- stack(c(paste(bioDir,"/bio_",1:19,".tif",sep=""),paste(bioDir,"/",c("sind.tif","drymonths.tif"),sep="")))
    prj_data <- cbind(prj_data,extract(bio_stk,prj_data[,c("x","y")]))
    rm(bio_stk); g=gc(); rm(g)
    
    other_rs <- stack(c(paste(solDir,"/soildrain_final.tif",sep=""),
                        paste(topDir,"/",c("aspect.tif","slope.tif"),sep="")))
    prj_data <- cbind(prj_data,extract(other_rs,prj_data[,c("x","y")]))
    rm(other_rs); g=gc(); rm(g)
    
    #transform variables
    prj_data$sind <- prj_data$sind * 10000
    prj_data$slope <- prj_data$slope * 100
    
    prj_data$NAs <- apply(prj_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
    prj_data <- prj_data[which(prj_data$NAs == 0),]
    prj_data$NAs <- NULL
    
    #check for clamping
    cat("checking for clamping\n")
    clamp_data <- prj_data
    mxe_mod <- paste(modDir,"/MAXENT/PA-",npa,"_SD-",seed,"_VARSET-",vset,sep="")
    lambdas <- paste(mxe_mod,"/",sppName2,"/models/",model_class,"/",sppName2,"_PA1_Full_MAXENT_outputs/",sppName2,"_PA1_Full.lambdas",sep="")
    lambdas <- read.csv(lambdas,header=F)
    names(lambdas) <- c("term","lambda","min","max")
    lambdas <- lambdas[which(lambdas$term %in% names(clamp_data)),]
    
    for (tpr in paste(lambdas$term)) {
      #tpr <- paste(lambdas$term)[1]
      minval <- lambdas$min[which(lambdas$term == tpr)]
      maxval <- lambdas$max[which(lambdas$term == tpr)]
      lowmin <- which(clamp_data[,tpr] < minval)
      higmax <- which(clamp_data[,tpr] > maxval)
      if (length(lowmin) > 0) {
        cat("clamping",length(lowmin),"min values for",tpr,"\n")
        clamp_data[lowmin,tpr] <- minval
      }
      if (length(higmax) > 0) {
        cat("clamping",length(higmax),"max values for",tpr,"\n")
        clamp_data[higmax,tpr] <- maxval
      }
    }
    
    #4. projecting the model
    cat("project model\n")
    if (alg == "MAXENT") {tmodel@model_options$path_to_maxent.jar <- maxDir}
    
    #predict and check
    #setwd(out_dir)
    prjVect <- predict(tmodel,prj_data)
    if (length(prjVect) != nrow(prj_data)) {stop("error: projection not matching area")} 
    
    #5. putting back to raster
    prj_rs <- raster(msk)
    prj_rs[prj_data$CELL] <- prjVect
    
    #save objects
    cat("write output raster\n")
    prj_rs <- writeRaster(prj_rs,paste(out_dir,"/sdm_proj.tif",sep=""),format="GTiff")
  }
}


