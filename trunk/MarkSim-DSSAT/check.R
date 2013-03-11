#rootFolder <- "D:/CCAFS/MarkSimGCM/Workspace"

checkAll <- function(rootFolder) {
	cellList <- list.files(rootFolder)
	for (cell in cellList) {
		cat("Checking cell",cell,"\n")
		inFolder <- paste(rootFolder, "/", cell, sep="")
		
		cfOut <- checkFolder(inFolder)
		cfOut <- data.frame(CELL=cell, CHECK=cfOut)
		
		if (cell == cellList[1]) {
			checkAllFolders <- cfOut
		} else {
			checkAllFolders <- rbind(checkAllFolders, cfOut)
		}
	}
	
	write.csv(checkAllFolders, paste(rootFolder, "/FolderCheck.csv", sep=""), quote=F, row.names=F)
	return("Done!")
}

checkFolder <- function(WTGDir) {
	flist <- list.files(WTGDir,pattern=".WTG")
	
	if (length(flist) == 0) {
		folderResp <- "FAILED"
	} else {
		for (fname in flist) {
			checkRes <- checkWTG(paste(WTGDir,"/",fname,sep=""))
			
			if (fname == flist[1]) {
				allChecks <- checkRes
			} else {
				allChecks <- rbind(allChecks, checkRes)
			}
		}
		names(allChecks) <- c("SITE","LAT","LON","ALT","CHECK")
		write.csv(allChecks,paste(WTGDir, "/WTGCheck.csv", sep=""),row.names=F,quote=F)
		if (length(which(allChecks$CHECK == "FAILED")) > 0) {folderResp <- "FAILED"} else {folderResp <- "OK"}
	}
	
	return(folderResp)
}

checkWTG <- function(filename) {
	basicData <- read.fortran(file=filename, format=c("A6","2F9", "I6"),skip=2,n=1)
	names(basicData) <- c("SITE","LAT","LON","ALT")
	
	dailyData <- read.fortran(file=filename, format=c("A5", "4F6"), skip=4)
	names(dailyData) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
	
	dataCheck <- apply(dailyData[,2:5],1,checkRecords)
	if (length(which(dataCheck == F)) > 0) {checkResp <- "FAILED"} else {checkResp <- "OK"}
	
	result <- cbind(basicData, CHECK=checkResp)
	
	return(result)
}

checkRecords <- function(x) {
	isGood <- T
	
	#Doing the check
	if (!is.numeric(x[1]) | !is.numeric(x[2]) | !is.numeric(x[3]) | !is.numeric(x[4])) {isGood <- F}
	if (x[3] == x[2]) {isGood <- F}
	
	return(isGood)
}
