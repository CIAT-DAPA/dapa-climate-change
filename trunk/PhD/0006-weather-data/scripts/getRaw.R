#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

inDir <- "F:/PhD-work/crop-modelling/climate-data"
gsodDir <- paste(inDir,"/gsod-daily",sep="")

stations <- read.csv(paste(gsodDir,"/ish-history.csv",sep=""))
yr <- 1960
rg <- "afr"

yrDir <- paste(gsodDir,"/",yr,"_out-",rg,sep="")
stList <- list.files(yrDir)
stList <- gsub(paste("-",yr,".csv",sep=""),"",stList)
which.usaf <- seq(1,length(stList)*2-1,by=2)
usaf <- unlist(strsplit(stList,"-",fixed=T))[which.usaf]
wban <- unlist(strsplit(stList,"-",fixed=T))[-which.usaf]

#I = NA
#A,B,C = NA
#E = NA
dayMx <- as.data.frame(matrix(ncol=369,nrow=length(stList)))
names(dayMx) <- c("ID","USAF","WBAN",1:366)
dayMx$ID <- stList; dayMx$USAF <- usaf; dayMx$WBAN <- wban



