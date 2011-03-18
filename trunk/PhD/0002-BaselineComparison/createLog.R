#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

#Function to create a log file
createLog <- function(folder, name) {
  zz <- file(paste(folder, "/", name, sep=""), open="w")
  cat("Done on", date(), "\n", file=zz)
	cat("Platform", version$platform, "\n", file=zz)
	cat("Version", version$version.string, "\n", file=zz)
	close(zz)
}
