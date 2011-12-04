#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

####### WORKFLOW
#1. Get configurations (error type x variable x seed x| p%)
#2. Copy the original data of D:/CIAT_work/GLAM/PNAS-paper/GJ-weather into a dummy folder
#3. Pack (folder) the data and put it into DSSAT-PNUT/GJ-weather folder for further Linux 
#   translation use. Notes:
#      a. Naming should be: [p | s]_[prec | tmin | tmax | yield]_[day | climate | season]_[p-*_s-*.zip]
#      b. Read afterwards and then store in new folder named WTH/ inside each folder
#      c. The data is to be moved by the CROPGRO R wrapper
#
#
########################################################################################
########################################################################################
altClimDir <- "D:/CIAT_work/GLAM/PNAS-paper/GJ-weather/shuf-pert/dqs_data"
yieldData <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-yield"
weathData <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather"

#Perturbed experiments
#experiments <- expand.grid(EXP=c("climate","season"),VAR=c("tmin","tmax","prec","yield"))
experiments <- expand.grid(EXP=c("climate","season"),VAR=c("prec","yield"))
experiments <- rbind(experiments,data.frame(EXP="day",VAR="prec"))
allFixFiles <- c("mintemp.dat","maxtemp.dat","rainfall.dat","radiation.dat","obsyield.txt")

seed_list <- list.files(paste(altClimDir,"/mtemp/climate",sep=""),pattern="tmax_p-0_s-")
seed_list <- gsub("tmax_p-0_s-","",seed_list); seed_list <- as.numeric(gsub(".dat","",seed_list))

std_pert <- seq(0,299,by=1)
#dir.create(paste(outClimDir,"/dummy",sep="")) #create dummy dir to store stuff

for (i in 1:nrow(experiments)) {
  variable <- experiments$VAR[i] #get variable
  expt <- experiments$EXP[i] #get experiment type
   if (variable == "tmin" | variable == "tmax") { #define input folder
    mfol <- "mtemp"
  } else {
    mfol <- variable
  }
  if (variable == "tmin") { #file to copy
    fixFile <- "mintemp.dat"
  } else if (variable == "tmax") {
    fixFile <- "maxtemp.dat"
  } else if (variable == "prec") {
    fixFile <- "rainfall.dat"
  } else if (variable == "yield") {
    fixFile <- "obsyield.txt"
  }
  otherFiles <- allFixFiles[which(allFixFiles!=fixFile)] #other files to copy
  baseOut <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather"
  if (variable == "yield") { #define origin folder
    orgClimDir <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-yield"
  } else {
    orgClimDir <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather"
  }
  outClimDir <- paste(baseOut,"/shuf-pert",sep="") #output folder
  if (!file.exists(outClimDir)) {dir.create(outClimDir)}
  outFolder <- paste(outClimDir,"/p_",variable,"_",expt,sep="") #create dir to hold data
  if (!file.exists(outFolder)) {dir.create(outFolder)} #create dir to hold data
  inpFolder <- paste(altClimDir,"/",mfol,"/",expt,sep="") #input folder
  for (p in std_pert) { #loop through std perturbations
    for (s in seed_list) { #loop through seeds
      cat("Processing:",paste(variable),"/ expt = ",paste(expt),"/ s =",s,"/ p =",p,"%\n")
      #create sub-folders where data is stored following proposed structure
      outDir <- paste(outFolder,"/p-",p,"_s-",s,sep="") #create folder to compress
      if (!file.exists(outDir)) {dir.create(outDir)} #create dir with data
      if (variable == "tmin" | variable == "tmax") {
        oFile <- paste(inpFolder,"/",variable,"_p-",p,"_s-",s,".dat",sep="")
      } else {
        oFile <- paste(inpFolder,"/p-",p,"_s-",s,".dat",sep="")
      }
      dFile <- paste(outDir,"/",fixFile,sep="")
      status <- file.copy(oFile,dFile,overwrite=F) #copy file
      #copy other files
      for (oFil in otherFiles) {
        if (oFil != "obsyield.txt") { #copy data to data folder (weather)
          oFile <- paste(weathData,"/",oFil,sep="")
          dFile <- paste(outDir,"/",oFil,sep="")
          status <- file.copy(oFile,dFile,overwrite=F)
        } else { #copy data to data folder (yield)
          oFile <- paste(yieldData,"/",oFil,sep="")
          dFile <- paste(outDir,"/",oFil,sep="")
          status <- file.copy(oFile,dFile,overwrite=F)
        }
      }
      #now zip the folder
      #zip(paste(outDir,".zip",sep=""),outDir)
    }
  }
}


########################################################################################
########################################################################################
altClimDir <- "D:/CIAT_work/GLAM/PNAS-paper/GJ-weather/shuf-pert/dqs_data_shuffled"
yieldData <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-yield"
weathData <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather"

#Perturbed experiments
#experiments <- expand.grid(EXP=c("all","wyear","years"),VAR=c("tmin","tmax","prec"))
experiments <- expand.grid(EXP=c("all","wyear","years"),VAR=c("prec"))
experiments <- rbind(experiments,data.frame(EXP="years",VAR="yield"))
allFixFiles <- c("mintemp.dat","maxtemp.dat","rainfall.dat","radiation.dat","obsyield.txt")

for (i in 1:nrow(experiments)) {
  variable <- experiments$VAR[i] #get variable
  expt <- experiments$EXP[i] #get experiment type
  if (variable == "tmin" | variable == "tmax") { #define input folder
    mfol <- "temp"
    seed_list <- list.files(paste(altClimDir,"/",mfol,"/",expt,sep=""),pattern=paste(variable,"_s-",sep=""))
    seed_list <- gsub(paste(variable,"_s-",sep=""),"",seed_list); seed_list <- as.numeric(gsub(".dat","",seed_list))
  } else {
    mfol <- variable
    seed_list <- list.files(paste(altClimDir,"/",mfol,"/",expt,sep=""),pattern="s-")
    seed_list <- gsub("s-","",seed_list); seed_list <- as.numeric(gsub(".dat","",seed_list))
  }
  
  if (variable == "tmin") { #file to copy
    fixFile <- "mintemp.dat"
  } else if (variable == "tmax") {
    fixFile <- "maxtemp.dat"
  } else if (variable == "prec") {
    fixFile <- "rainfall.dat"
  } else if (variable == "yield") {
    fixFile <- "obsyield.txt"
  }
  otherFiles <- allFixFiles[which(allFixFiles!=fixFile)] #other files to copy
  baseOut <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather"
  if (variable == "yield") { #define origin folder
    orgClimDir <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-yield"
  } else {
    orgClimDir <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather"
  }
  outClimDir <- paste(baseOut,"/shuf-pert",sep="") #output folder
  if (!file.exists(outClimDir)) {dir.create(outClimDir)}
  outFolder <- paste(outClimDir,"/s_",variable,"_",expt,sep="") #create dir to hold data
  if (!file.exists(outFolder)) {dir.create(outFolder)} #create dir to hold data
  inpFolder <- paste(altClimDir,"/",mfol,"/",expt,sep="") #input folder
  for (s in seed_list) { #loop through seeds
    cat("Processing:",paste(variable),"/ expt =",paste(expt),"/ s =",s,"\n")
    #create sub-folders where data is stored following proposed structure
    outDir <- paste(outFolder,"/s-",s,sep="") #create folder to compress
    if (!file.exists(outDir)) {dir.create(outDir)} #create dir with data
    if (variable == "tmin" | variable == "tmax") {
      oFile <- paste(inpFolder,"/",variable,"_s-",s,".dat",sep="")
    } else {
      oFile <- paste(inpFolder,"/s-",s,".dat",sep="")
    }
    dFile <- paste(outDir,"/",fixFile,sep="")
    status <- file.copy(oFile,dFile,overwrite=F) #copy file
    #copy other files
    for (oFil in otherFiles) {
      if (oFil != "obsyield.txt") { #copy data to data folder (weather)
        oFile <- paste(weathData,"/",oFil,sep="")
        dFile <- paste(outDir,"/",oFil,sep="")
        status <- file.copy(oFile,dFile,overwrite=F)
      } else { #copy data to data folder (yield)
        oFile <- paste(yieldData,"/",oFil,sep="")
        dFile <- paste(outDir,"/",oFil,sep="")
        status <- file.copy(oFile,dFile,overwrite=F)
      }
    }
    #now zip the folder
    #zip(paste(outDir,".zip",sep=""),outDir)
  }
}
