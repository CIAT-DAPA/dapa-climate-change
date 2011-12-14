setwd("C:/Users/USER/AppData/Local/Temp")
tmpList <- list.files(".",pattern="Rtmp")
tmpList <- tmpList[2:length(tmpList)]
wd <- getwd()
for (tmpF in tmpList) {
	fl <- list.files(tmpF)
	counter <- 1
	for (i in fl) {
		frm <- file.remove(paste(wd,"/",tmpF,"/",i,sep=""))
		cat(counter,":",frm)
		counter <- counter+1
	}
}
