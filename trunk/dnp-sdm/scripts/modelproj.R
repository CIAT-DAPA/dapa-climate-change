#JRV Jul 2013
#process Andean occurrence data
stop("dont run")

##############################
#Procedure to follow
##############################
#
#1. 

#loop cross-val 1:10 (with specified seed)
#5. run without cross-validation (but do the cross validation independently so as
#   to be able to use Hijmans 2012 Ecology ssb AUC correction)
#6. run a particular algorithm, evaluate, store eval output
#   configuration in Maxent: probably 10k PA)
#end loop cross-val

##############################
##############################

#load relevant package(s)
library(biomod2); library(raster); library(rgdal); library(maptools); library(dismo)

#base dir
bDir <- "/nfs/a102/eejarv/DNP-biodiversity"
#bDir <- "/mnt/a102/eejarv/DNP-biodiversity"
setwd(bDir)

#source functions
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/dnp-sdm"
#src.dir <- "~/Repositories/dapa-climate-change/trunk/dnp-sdm"
source(paste(src.dir,"/scripts/modelproj-fun.R",sep=""))

#lists of variables
varList <- data.frame(SET_ID=1:8,CLIM=c(rep("full",times=4),rep("subset",times=4)),
                      SOIL=c(F,T,F,T,F,T,F,T),TOPO=c(F,F,T,T,F,F,T,T))

#seeds to draw presences and pseudo absences (bootstraps)
seedList <- c(3379, 5728, 3781, 3590, 3266, 9121, 3441, 11667, 4484, 9559)

#list of number of PA selections
npaList <- c(3829, 1922, 1945, 5484, 2125, 8746, 2187, 1521, 9623, 1561)

#list of models
modList <- c('GLM','GAM','GBM','RF','ANN','MAXENT')

#experimental matrix
all_runs <- expand.grid(ALG=modList,NPA=npaList,VSET=varList$SET_ID,SEED=seedList)
all_runs <- cbind(ID=paste("SDM_",1:nrow(all_runs),sep=""),all_runs)

#projection scenarios
sceList <- c("baseline","sres_a1b","sres_a2","sres_b1")
mList <- data.frame()
for (i in 1:length(sceList)) {
  if (i == 1) {
    m1 <- expand.grid(SCE=sceList[i],PERIOD="1950_2000",
                      MODEL=list.files(paste(bDir,"/env-data/proj_COL/",sceList[i],"/Global_2_5min",sep="")))
  } else {
    m1 <- expand.grid(SCE=sceList[i],PERIOD=c("2020_2049","2040_2069","2070_2099"),
                      MODEL=list.files(paste(bDir,"/env-data/proj_COL/",sceList[i],"/Global_2_5min",sep="")))
  }
  mList <- rbind(mList,m1)
  rm(m1)
}
mList <- cbind(ID=paste("CLM_",1:nrow(mList),sep=""),mList)

#species name and configuration of run
this_sppName <- "Jaca_cauc" #species name

mod <- "MAXENT"
truns <- all_runs$ID[which(all_runs$ALG == mod)]

#actual model runs
for (run_i in truns) {
  #run_i <- truns[1] #23
  this_seed <- as.numeric(paste(all_runs$SEED[which(all_runs$ID == run_i)])) #seed for the cross validation
  this_npa <- as.numeric(paste(all_runs$NPA[which(all_runs$ID == run_i)])) #number of pseudo absences (from list)
  this_alg <- paste(all_runs$ALG[which(all_runs$ID == run_i)]) #modelling algorithm
  this_vset <- as.numeric(paste(all_runs$VSET[which(all_runs$ID == run_i)])) #set of variables to use
  
  for (prj in 1:nrow(mList)) {
    #prj <- 1
    tsce <- paste(mList$SCE[prj])
    tper <- paste(mList$PERIOD[prj])
    tmodel <- paste(mList$MODEL[prj])
    
    odir <- proj_model(bDir,sppName=this_sppName,seed=this_seed,npa=this_npa,alg=this_alg,
                       vset=this_vset,scenario=tsce,period=tper,model=tmodel)
  }
}


#######################################################################
#calculate average of prob of J caucana
prjDir <- paste(bDir,"/models_prj",sep="")
this_sppName2 <- gsub("_",".",this_sppName,fixed=T)
sresList <- expand.grid(SCE=sceList[2:4],PERIOD=c("2020_2049","2040_2069","2070_2099"))

bList <- list(); futList <- list()
#loop different sdm techniques
for (alg in modList) {
  #alg <- modList[1]
  truns <- all_runs$ID[which(all_runs$ALG == alg)]
  truns <- truns[1:7]
  
  #load and compute mean baseline
  for (run_i in truns) {
    #run_i <- truns[1]
    cat("...loading baseline run",run_i,"\n")
    this_seed <- as.numeric(paste(all_runs$SEED[which(all_runs$ID == run_i)])) #seed for the cross validation
    this_npa <- as.numeric(paste(all_runs$NPA[which(all_runs$ID == run_i)])) #number of pseudo absences (from list)
    this_alg <- paste(all_runs$ALG[which(all_runs$ID == run_i)]) #modelling algorithm
    this_vset <- as.numeric(paste(all_runs$VSET[which(all_runs$ID == run_i)])) #set of variables to use
    
    inp_dir <- paste(prjDir,"/",alg,"/PA-",this_npa,"_SD-",this_seed,"_VARSET-",this_vset,"/",this_sppName2,
                     "/baseline-1950_2000-worldclim",sep="")
    bList[[paste(alg,".",run_i,sep="")]] <- raster(paste(inp_dir,"/sdm_proj.tif",sep=""))
  }
  
  #load and compute each sres scenario and period
  for (srp in 1:nrow(sresList)) {
    #srp <- 1
    sres <- paste(sresList$SCE[srp])
    perd <- paste(sresList$PERIOD[srp])
    gcms <- paste(mList$MODEL[which(mList$SCE == sres & mList$PERIOD == perd)])
    if (is.null(futList[[paste(sres,".",perd,sep="")]])) {futList[[paste(sres,".",perd,sep="")]] <- list()}
    
    #load and compute mean baseline
    for (run_i in truns) {
      #run_i <- truns[1]
      this_seed <- as.numeric(paste(all_runs$SEED[which(all_runs$ID == run_i)])) #seed for the cross validation
      this_npa <- as.numeric(paste(all_runs$NPA[which(all_runs$ID == run_i)])) #number of pseudo absences (from list)
      this_alg <- paste(all_runs$ALG[which(all_runs$ID == run_i)]) #modelling algorithm
      this_vset <- as.numeric(paste(all_runs$VSET[which(all_runs$ID == run_i)])) #set of variables to use
      
      for (tgcm in gcms) {
        #tgcm <- gcms[1]
        cat("...loading",sres,"/",perd,"::: run",run_i,"/",tgcm,"\n")
        inp_dir <- paste(prjDir,"/",alg,"/PA-",this_npa,"_SD-",this_seed,"_VARSET-",this_vset,"/",this_sppName2,
                         "/",sres,"-",perd,"-",tgcm,sep="")
        futList[[paste(sres,".",perd,sep="")]][[paste(alg,".",run_i,".",tgcm,sep="")]] <- raster(paste(inp_dir,"/sdm_proj.tif",sep=""))
      }
    }
  }
}

#mean of baseline
b_mean <- lapply(bList,FUN=function(x) {mean(x[],na.rm=T)})
b_mean <- unlist(b_mean)
bx <- b_mean[order(b_mean,decreasing=T)]
b_mean <- b_mean[-grep("RF",names(b_mean))]
b_mean <- as.numeric(b_mean)

tiff(paste(bDir,"/ensemble_values.tiff",sep=""),res=300,pointsize=8,
     width=1900,height=1500,units="px",compression="lzw",type="cairo")
par(mar=c(10,4.5,1,1),cex=1,las=2)
barplot(bx,names.arg=names(bx),xlab=NA,ylab="Promedio probabilidad en Colombia",
        ylim=c(0,0.6))
grid()
abline(h=c(0.1,0.2),col="red",lty=2)
dev.off()

#mean of futures
f_mean <- list()
for (srp in 1:nrow(sresList)) {
  #srp <- 1
  sres <- paste(sresList$SCE[srp])
  perd <- paste(sresList$PERIOD[srp])
  cat("...computing",sres,"/",perd,"\n")
  
  gcms <- paste(mList$MODEL[which(mList$SCE == sres & mList$PERIOD == perd)])
  if (is.null(f_mean[[paste(sres,".",perd,sep="")]])) {f_mean[[paste(sres,".",perd,sep="")]] <- list()}
  f_mean[[paste(sres,".",perd,sep="")]] <- as.numeric(unlist(lapply(futList[[paste(sres,".",perd,sep="")]],FUN=function(x) {mean(x[],na.rm=T)})))
}

save(list=c("bList","futList","b_mean","f_mean"),file=paste(bDir,"/prelim_output_Jaca.cauc.RData",sep=""))


#sres_a2
tiff(paste(bDir,"/Jaca_cauc_sresa2.tiff",sep=""),res=300,pointsize=9,
     width=1900,height=1500,units="px",compression="lzw",type="cairo")
par(mar=c(5,4.5,1,1),cex=1)
hd <- hist(b_mean,breaks=c(seq(0,1,by=0.05),1),plot=F)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0,0.6),ylim=c(0,35),
     xlab="Mean suitability", ylab="pdf (%)",col="black")
grid()
hd <- hist(f_mean$sres_a2.2020_2049,breaks=c(seq(0,1,by=0.05),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=1)
hd <- hist(f_mean$sres_a2.2040_2069,breaks=c(seq(0,1,by=0.05),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=2)
hd <- hist(f_mean$sres_a2.2070_2099,breaks=c(seq(0,1,by=0.05),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=3)
legend(0.45,35,legend=c("Baseline","2030s","2050s","2080s"),lty=c(1,1,2,3),
       col=c("black","red","red","red"),cex=1)
dev.off()

#sres_a1b
tiff(paste(bDir,"/Jaca_cauc_sresa1b.tiff",sep=""),res=300,pointsize=8,
     width=1900,height=1500,units="px",compression="lzw",type="cairo")
par(mar=c(5,4.5,1,1),cex=1)
hd <- hist(b_mean,breaks=c(seq(0,1,by=0.05),1),plot=F)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0,0.6),ylim=c(0,35),
     xlab="Mean suitability", ylab="pdf (%)",col="black")
grid()
hd <- hist(f_mean$sres_a1b.2020_2049,breaks=c(seq(0,1,by=0.05),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=1)
hd <- hist(f_mean$sres_a1b.2040_2069,breaks=c(seq(0,1,by=0.075),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=2)
hd <- hist(f_mean$sres_a1b.2070_2099,breaks=c(seq(0,1,by=0.075),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=3)
dev.off()

#sres_b1
tiff(paste(bDir,"/Jaca_cauc_sresb1.tiff",sep=""),res=300,pointsize=8,
     width=1900,height=1500,units="px",compression="lzw",type="cairo")
par(mar=c(5,4.5,1,1),cex=1)
hd <- hist(b_mean,breaks=c(seq(0,1,by=0.05),1),plot=F)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0,0.6),ylim=c(0,35),
     xlab="Mean suitability", ylab="pdf (%)",col="black")
grid()
hd <- hist(f_mean$sres_b1.2020_2049,breaks=c(seq(0,1,by=0.075),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=1)
hd <- hist(f_mean$sres_b1.2040_2069,breaks=c(seq(0,1,by=0.075),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=2)
hd <- hist(f_mean$sres_b1.2070_2099,breaks=c(seq(0,1,by=0.075),1),plot=F)
lines(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",col="red",lty=3)
dev.off()

