#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014

#cleanup junk in nodes
#works in both arc1 and arc2 systems

#define bin dir based on node
nname <- Sys.info()[["nodename"]]
if (length(grep("arc2",nname)) == 0) {
  tsys <- "arc1"
} else {
  tsys <- "arc2"
}

#directories
wd <- "~/quest-for-robustness"
rfil_dir <- paste(wd,"/scratch/runfiles",sep="")

#list of files
rfil_list <- list.files(rfil_dir, pattern="run.sh.o")

#loop files to construct node list
for (i in 1:length(rfil_list)) {
  #i <- 1
  #read in text from file
  rfil <- rfil_list[i]
  rtxt <- read.csv(rfil, sep="=", header=F)
  
  #determine line with "Running"
  rlin <- grep("Running",rtxt[,1])
  rlin <- paste(rtxt[rlin,1])
  
  #determine host
  rhost <- unlist(strsplit(rlin, " ", fixed=T))
  rhost <- rhost[length(rhost)]
  
  #if an arc2 host in arc2 then ssh
  if (tsys == "arc2") {
    if (length(grep("arc2",rhost)) != 0) {
      system(paste("ssh ",rhost, "'rm -rf /dev/shm/earjr && rm -rf /scratch/earjr'"))
      #system(paste("ssh ",rhost, "'ls -l /dev/shm/earjr'"))
    } else {
      cat("host",rhost,"does not belong to",tsys,"\n")
    }
  } else if (tsys == "arc1") {
    if (length(grep("arc2",rhost)) == 0) {
      system(paste("ssh ",rhost, "'rm -rf /dev/shm/earjr && rm -rf /scratch/earjr'"))
      #system(paste("ssh ",rhost, "'ls -l /dev/shm/earjr'"))
    } else {
      cat("host",rhost,"does not belong to",tsys,"\n")
    }
  }
}

