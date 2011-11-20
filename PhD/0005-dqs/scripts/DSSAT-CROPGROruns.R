#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

####### WORKFLOW
#1. Create soil file (soil.sol) by modifying the existing one, and using a new SLPF, 
#   use step, max and min as arguments
#
#2. Go to C:\DSSAT45\Peanut folder and run the following line
#    C:\DSSAT45\DSCSM045.EXE CRGRO045 B DSSBatch.v45
#   This would use only one treatment assigned to the soil file soil.sol and will run the crop model
#
#3. Read Summary.out in the Peanut folder and get the 'HWAH', compare this with the observed yield. 
#   	a. Calculate RMSE
#     b. Calculate correlation coefficient
#   Store both in a file for further analysis, flagging the minimum RMSE
#
#4. To perform runs with other weather data, remove existing weather files from 
#   D:\CIAT_work\GLAM\PNAS-paper\DSSAT-PNUT\GJ-weather\WTH and put new files into that folder.
#   Then perform again from (1) to (3) and keep files for further analysis

########################################################################################
########## FUNCTIONS TO OPTIMISE BASED ON SHUFFLED OR PERTURBED YIELD DATA
########################################################################################
#1. Remove the data from ./DSSAT-PNUT/GJ-weather/WTH
#2. Copy the perturbed/shuffled data into that folder
#3. Call the CROPGRO wrapper and assess the model
#
#Note: use p, s, variable as function arguments to find the folder where the model is run
#
bd <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"
#bd <- "/media/DATA/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"
experiments <- list.files(paste(bd,"/GJ-weather/shuf-pert",sep=""))
type <- substr(experiments,1,1)
position <- gregexpr("_",experiments)
for (ex in 1:length(experiments)) {
  v <- substr(experiments[ex],position[[ex]][1]+1,position[[ex]][2]-1)
  k <- substr(experiments[ex],position[[ex]][2]+1,nchar(experiments[ex]))
  if (ex==1) {variable <- v;scale<-k} else {variable <- c(variable,v);scale<-c(scale,k)}
}
experiments <- data.frame(TYPE=type,VAR=variable,SCALE=scale)

################################################################################
#Create a function for using this data frame to parallelise the stuff
#control_list <- read.csv(paste(bd,"/bin/control/p_prec_climate_test.csv",sep="")) #load test control file

library(snowfall)
sfInit(parallel=T,cpus=5)

#export functions
sfExport("accuracy")
sfExport("optSLPF")
sfExport("runCROPGRO")
sfExport("runCROPGROPS")
sfExport("runPSModel")
sfExport("writeSoilFile")

#run the parallel function
bd <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"; sfExport("bd")
control_list <- read.csv(paste(bd,"/bin/control/p_prec_climate.csv",sep="")) #load control file
p_unique <- unique(control_list$P)
for (pval in p_unique) {
  cat("Process",pval,"\n")
  tp <- control_list$TYPE[1]; va <- control_list$VAR[1]; sca <- control_list$SCALE[1]
  reduced_list <- control_list[which(control_list$P==pval),]
  s_list <- reduced_list$SEED
  controlPS <- function(i) { #define a new function
    #cat(paste(x[1],x[2],x[3],x[4],x[5]),"\n")
    runPSModel(bd,ty=tp,v=va,sc=sca,s=i,p=pval)
    oFILE <- paste(bd,"/bin/control/",tp,"_",va,"_",sca,".ctr",sep="")
    system(paste("echo s=",i,"p=",pval,">>",oFILE))
  }
  sfExport("tp");sfExport("va");sfExport("sca");sfExport("pval")
  system.time(sfSapply(as.vector(s_list), controlPS))
}

#stop the cluster
sfStop()

#################
#create a data frame with all seeds and perturbations, this will be used as control file
createControlFiles <- function() {
  for (i in 1:nrow(experiments)) {
    ty <- experiments$TYPE[i]
    va <- experiments$VAR[i]
    sc <- experiments$SCALE[i]
    if (!file.exists(paste(bd,"/bin/control/",ty,"_",va,"_",sc,".csv",sep=""))) {
      dataFol <- paste(bd,"/GJ-weather/shuf-pert/",ty,"_",va,"_",sc,sep="")
      fList <- list.files(dataFol)
      for (f in fList) {
        cat(paste(ty),paste(va),paste(sc),paste(f),"\n")
        if (ty=="p") {
          sep <- unlist(strsplit(f,"_",fixed=T))
          p <- as.numeric(gsub("p-","",sep[1]))
          s <- as.numeric(gsub("s-","",sep[2]))
        } else {
          p <- NA
          s <- as.numeric(gsub("s-","",f))
        }
        outrow <- data.frame(TYPE=ty,VAR=va,SCALE=sc,P=p,SEED=s)
        if (f==fList[1]) {
          outMat <- outrow
        } else {
          outMat <- rbind(outrow,outMat)
        }
      }
      write.csv(outMat,paste(bd,"/bin/control/",ty,"_",va,"_",sc,".csv",sep=""),row.names=F,quote=F)
    }
  }
}


####################
#################### BELOW IS A TESTING EXPERIMENT TO SEE WHAT HAPPENS #############
testParallel <- function() {
  x <- 1 #select the experiment
  spIn <- paste(bd,"/GJ-weather/shuf-pert/",experiments$TYPE[x],"_",experiments$VAR[x],"_",experiments$SCALE[x],sep="") #folder where S|P occur
  
  if (experiments$TYPE[x]=="p") { #create list of perturbing values
    p_list <- seq(0,299,by=1)
    s_list <- list.files(spIn,pattern="p-0_s-")
    s_list <- as.numeric(gsub("p-0_s-","",s_list))
  } else {
    p_list <- NA
    s_list <- list.files(spIn,pattern="s-")
    s_list <- as.numeric(gsub("s-","",s_list))
  }
  if (!is.na(p_list[1])) {p <- p_list[1]} #define perturb ratio
  s <- s_list[1] #define seed
  
  #run model
  #opt_ps <- runPSModel(bd,ty=experiments$TYPE[x],v=experiments$VAR[x],sc=experiments$SCALE[x],s=1056,p=0)
  opt_ps <- runPSModel(bd,ty="p",v="prec",sc="climate",s=997,p=99)
  
  #parallelise
  #initialise
  library(snowfall)
  sfInit(parallel=T,cpus=3)
  
  #export functions
  sfExport("accuracy")
  sfExport("optSLPF")
  sfExport("runCROPGRO"); sfExport("runCROPGROPS")
  sfExport("runPSModel")
  sfExport("writeSoilFile")
  
  #testing mode
  p_list <- p_list[1:3]
  sfExport("bd"); sfExport("p_list"); sfExport("s_list"); sfExport("x"); sfExport("experiments")
  
  #here i need to create a function that makes use of a data.frame in which
  #the p and s values are specified for each experiment configuration so that this thing
  #can be run in parallel. Some type of progress track needs to be implemented, likely
  #via a file that is appended
  system.time(sfSapply(p_list, function(i) 
    runPSModel(bd,ty=experiments$TYPE[x],v=experiments$VAR[x],sc=experiments$SCALE[x],s=1056,p=i)))
  
  sfStop()
}

##################FUNCTION TO RUN THE PERTURBED/SHUFFLED CONFIG ##########################
#Function to run the CROPGRO optimiser for an specified perturbed/shuffled configuration
runPSModel <- function(bDir,ty,v,sc,s,p=NA) {
  cat("Cleaning and copying files \n")
  sp_folder <- paste(bDir,"/GJ-weather/shuf-pert/",ty,"_",v,"_",sc,sep="") #folder where S|P occur
  if (!is.na(p)) {
    if (v == "yield") {
      inpFolder <- paste(bDir,"/GJ-weather/WTH",sep="")
    } else {
      inpFolder <- paste(sp_folder,"/p-",p,"_s-",s,"/WTH",sep="")
    }
    inPSFolder <- paste(sp_folder,"/p-",p,"_s-",s,sep="")
  } else {
    if (v == "yield") {
      inpFolder <- paste(bDir,"/GJ-weather/WTH",sep="")
    } else {
      inpFolder <- paste(sp_folder,"/s-",s,"/WTH",sep="")
    }
    inPSFolder <- paste(sp_folder,"/s-",s,sep="")
  }
  if (!file.exists(paste(inPSFolder,"/proc.done",sep=""))) {
    zz <- file(paste(inPSFolder,"/proc.lock",sep=""),open="w");close(zz)
    
    #make copy of dssat here
    cat("Creating copy of DSSAT \n")
    bin_dir <- paste(bDir,"/bin",sep="")
    oCopy <- paste(bin_dir,"/dssat",sep="")
    copyNumber <- round(runif(1,0,9999),0)
    dCopy <- paste(bin_dir,"/dssat_c-",copyNumber,sep="")
    while (file.exists(dCopy)) {
      copyNumber <- round(runif(1,0,9999),0)
      dCopy <- paste(bin_dir,"/dssat_c-",copyNumber,sep="")
    }
    dir.create(dCopy)
    binList <- list.files(oCopy)
    binList <- lapply(binList,function(x,idir,odir) {k<-file.copy(paste(idir,"/",x,sep=""),paste(odir,"/",x,sep=""))},oCopy,dCopy)
    
    xDir <- dCopy #folder where CROPGRO looks for the stuff
    wthList <- list.files(xDir,pattern=".WTH")
    wthList <- lapply(wthList,function(x,idir) {k<-file.remove(paste(idir,"/",x,sep=""))},xDir)
    wthList <- list.files(inpFolder,pattern=".WTH")
    wthList <- lapply(wthList,function(x,idir,odir) {k<-file.copy(paste(idir,"/",x,sep=""),paste(odir,"/",x,sep=""))},inpFolder,xDir)
    
    #define folders for cropgro optim
    obs <- paste(inPSFolder,"/obsyield.txt",sep="")
    opt <- optSLPF(xDir,xDir,obs,slpfStep=0.05,perturbed=T) #optimise
    write.csv(opt[[1]],paste(sp_folder,"/p-",p,"_s-",s,"/optimisation.csv",sep=""),quote=T,row.names=F)
    write.csv(opt[[2]],paste(sp_folder,"/p-",p,"_s-",s,"/timeseries.csv",sep=""),quote=T,row.names=F)
    #control files
    file.remove(paste(inPSFolder,"/proc.lock",sep=""))
    zz <- file(paste(inPSFolder,"/proc.done",sep=""),open="w");close(zz)
    #remove dssat copy
    cat("Removing copy of DSSAT \n")
    binList <- list.files(dCopy)
    binList <- lapply(binList,function(x,idir) {k<-file.remove(paste(idir,"/",x,sep=""))},dCopy)
    setwd(oCopy); unlink(dCopy, recursive=TRUE)
    return(opt)
  }
}



########################################################################################
########## FIRST OPTIMISATION USING ORIGINAL INPUT WEATHER AND YIELD DATA
########################################################################################
firstOpt <- function() {
  pth <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-soil"
  xDir <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-exp"
  obs <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-yield/obsyield.txt"
  
  opt <- optSLPF(pth,xDir,obs,slpfStep=0.05,perturbed=F) #optimise
  write.csv(opt[[1]],"D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/optimisation.csv",quote=T,row.names=F)
  write.csv(opt[[2]],"D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/timeseries.csv",quote=T,row.names=F)
  
  tiff("D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/optim-SLPF.tiff",compression='lzw',res=300,
       pointsize=8,width=1500,height=1000)
  par(mar=c(4.5,4.5,1,1),cex=0.8)
  plot(opt[[1]]$SLPF,opt[[1]]$RMSE,type="p",pch=20,xlab="Soil fertility factor (SLPF)",ylab="RMSE (kg/ha)")
  lines(opt[[1]]$SLPF,opt[[1]]$RMSE)
  grid()
  dev.off()
  
  #run best prediction
  sof <- writeSoilFile(pth,slpf='0.80')
  runCROPGRO(xDir)
  values <- accuracy(xDir,obs)[[2]]
  mn <- min(values$HWAH,values$OBYL)
  mx <- max(values$HWAH,values$OBYL)
  
  #Plot the best prediction
  tiff("D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/best-pred.tiff",compression='lzw',res=300,
       pointsize=8,width=1500,height=1000)
  par(mar=c(4.5,4.5,1,1),cex=0.8)
  plot(values$YEAR,values$HWAH,type="p",pch=20,
       xlab="Year",ylab="Yield (kg/ha)",xlim=c(1966,1989),ylim=c(mn,mx))
  lines(values$YEAR,values$HWAH)
  points(values$YEAR,values$OBYL,type="p",pch=20,col='red')
  lines(values$YEAR,values$OBYL,col='red')
  grid()
  legend(1970,1800,legend=c("Observed","Modelled"),col=c("black","red"),lty=c(1,1),pch=c(20,20),cex=1.2)
  dev.off()
}

#############################################################################
#Optimise the SLPF
optSLPF <- function(sDir,expDir,obsY,slpfStep=0.05,perturbed=F) {
  #Create sequence of values
  slpfSeq <- seq(0,1,by=slpfStep); slpfSeq[1] <- 0.01
  #loop through sequence of fertility factor
  for (slpf in slpfSeq) {
    cat("\n")
    cat("Running for SLPF of",slpf,"\n")
    #Get it in the proper format
    slpfX <- paste(slpf)
    if (nchar(slpfX)!=4) {
      slpfX <- format(slpf,nsmall=2)
    }
    #Create the soil file
    cat("Creating soil file \n")
    sof <- writeSoilFile(sDir,slpf=slpfX)
    #CROPGRO run
    cat("Running crop model \n")
    if (!perturbed) {runCROPGRO(expDir)} else {runCROPGROPS(expDir)}
    #Assess prediction
    cat("Assessing prediction \n")
    accm <- accuracy(expDir,obsY)
    tims <- accm[[2]]
    tims <- cbind(SLPF=slpf,SLPFX=slpfX,tims)
    metx <- accm[[1]]
    metx <- cbind(SLPF=slpf,SLPFX=slpfX,metx)
    #Summarise output
    if (slpf==slpfSeq[1]) {
      metxRes <- metx
      timsRes <- tims
    } else {
      metxRes <- rbind(metxRes,metx)
      timsRes <- rbind(timsRes,tims)
    }
  }
  return(list(METRICS=metxRes,TIMESERIES=timsRes))
}

#############################################################################
#Read in summary and calculate performance metrics
accuracy <- function(expDir,obsYield) {
  #Check if file exists
  sFile <- paste(expDir,"/Summary.OUT",sep="")
  if (!file.exists(sFile)) {
    stop("CROPGRO Summary file does not exist")
  }
  
  if (!file.exists(obsYield)) {
    stop("Evaluation file does not exist")
  }
  
  #Reading in summary
  sData <- read.fortran(sFile,skip=4,format=c("A162","F8","A356"))
  names(sData) <- c("DUMM1","HWAH","DUMM2")
  HWAH <- sData$HWAH
  rm(sData)
  
  #Reading in obs. yield file
  yData <- read.fortran(obsYield,format=c("A12","F13"))
  names(yData) <- c("DUMM1","OBYL")
  OBYL <- yData$OBYL
  rm(yData)
  
  #Calculating metrics
  rmse <- sqrt(sum((HWAH-OBYL)^2)/length(OBYL))
  corr <- cor(HWAH,OBYL)
  metx <- data.frame(RMSE=rmse,CORR=corr)
  vals <- data.frame(YEAR=1966:1989,HWAH=HWAH,OBYL=OBYL)
  return(list(METRICS=metx,VALUES=vals))
}


#############################################################################
#Run crop model from installation folder of dssat
runCROPGRO <- function(expDir) {
  #This will run CROPGRO in the specified
  setwd(expDir)
  outList <- list.files(pattern=".OUT")
  outList <- lapply(outList,function(x,idir) {k<-file.remove(paste(idir,"/",x,sep=""))},expDir)
  system("C:\\DSSAT45\\DSCSM045.EXE CRGRO045 B DSSBatch.v45",show.output.on.console=F)
}

#Run crop model if it is located in X-file folder
runCROPGROPS <- function(expDir) {
  #This will run CROPGRO in the specified
  setwd(expDir)
  outList <- list.files(pattern=".OUT")
  outList <- lapply(outList,function(x,idir) {k<-file.remove(paste(idir,"/",x,sep=""))},expDir)
  system("DSCSM045.EXE CRGRO045 B DSSBatch.v45",show.output.on.console=F)
}

#############################################################################
#Write soil file (slpf needs to be character with nchar=4)
writeSoilFile <- function(path,slpf) {
  #Validate slpf
  if (!is.character(slpf)) {
    stop("SLPF needs to be a character of length 4")
  } else if (nchar(slpf)!=4) {
    stop("SLPF needs to be a character of length 4")
  }
  
  if (as.numeric(slpf)>1 | as.numeric(slpf)<0) {
    stop("SLPF must be between 0 and 1")
  }
  
  #copy .bak into actual file
  filePathName <- paste(path,"/soil.sol",sep="")
  if (file.exists(filePathName)) {
    st <- file.remove(filePathName)
  }
  st <- file.copy(paste(path,"/soil.sol.bak",sep=""),filePathName)
  
    #write the needed stuff
  sf <- file(filePathName,"a")
  writeLines(paste("   -99  0.09   6.8  0.60  76.0  1.00  ",slpf,' IB001 IB001 IB001',sep=""),sf)
  writeLines(paste("@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC",sep=""),sf)
  writeLines(paste("     5   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("    15   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("    28   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("    44   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("    65   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("    96   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("   122   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("   150   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("   178   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("   196   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("   200   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("   260   -99 0.350 0.450 0.480 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 ",sep=""),sf)
  writeLines(paste("",sep=""),sf)
	close(sf)
  
  return(filePathName)
}
