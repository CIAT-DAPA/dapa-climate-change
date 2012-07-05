#Julian Ramirez-Villegas
#Script to remove extra glam run files to save drive space

#cal_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT/calib"
cal_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT/calib"

xpList <- list.files(cal_dir,pattern="rfd")
xpList <- c(xpList,list.files(cal_dir,pattern="fcal"))

#this was only needed once, before i modified the functions inside glam-optimise-functions.R
#to delete all GLAM inputs after GLAM is run

#loop through experiments
for (xpn in xpList) {
  xpDir <- paste(cal_dir,"/",xpn,sep="")
  
  #list and loop iterations
  itList <- list.files(xpDir,pattern="iter-")
  for (iter in itList) {
    cat("iteration",iter,"of",xpn,"\n")
    itDir <- paste(xpDir,"/",iter,sep="")
    
    #list and loop parameters
    pList <- list.files(itDir)
    pList <- pList[-grep("\\.RData",pList)]
    for (pn in pList) {
      pDir <- paste(itDir,"/",pn,sep="")
      
      #list and loop runs
      rList <- list.files(pDir,pattern="_run-")
      for (rn in rList) {
        rDir <- paste(pDir,"/",rn,sep="")
        
        #get to each of the folders and remove the input files
        setwd(paste(rDir,"/inputs/ascii/obs",sep=""))
        x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
        
        setwd(paste(rDir,"/inputs/ascii/soil",sep=""))
        x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
        
        setwd(paste(rDir,"/inputs/ascii/sow",sep=""))
        x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
        
        setwd(paste(rDir,"/inputs/ascii/wth",sep=""))
        x <- sapply(list.files(".",pattern="\\.7z"),FUN= function(x) {s <- file.remove(x)})
        
        setwd(cal_dir)
      }
      
    }
    
  }
  
}

