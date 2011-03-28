#Julian Ramirez
#March 2011
#CIAT / University of Leeds

#This will create a .cmd file that would perform the fit based on a number of 
#parameters that will be automatically tranferred from the input function to the run file

createRunFile <- function(inData, filename="", variable="tmean", 
varUnits="metres", ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=2, nsurf=12) {
  #Units matrix
  umx <- data.frame(ID=c(0:8), UNIT=c("undefined","metres","feet","kilometres","miles","degrees","radians","millimetres","mejajoules"))
  if (!tolower(varUnits) %in% umx$UNIT) {
    unit <- 0
    cat("Variable units not recognised, so using UNDEFINED \n")
  } else {
    unit <- umx$ID[which(umx$UNIT == tolower(varUnits))]
  }
  
  #Opening file in write mode
  fw <- file(paste(variable, "fit.cmd", sep=""), open="w")
  
  #Writing program details
  cat(variable, "\n",sep="",file=fw) #Variable name
  cat(unit, "\n", sep="",file=fw) #Units
  cat(ivars, "\n",sep="",file=fw) #Independent variables
  cat(icovars, "\n",sep="",file=fw) #Independent covariates
  cat(sivars, "\n",sep="",file=fw) #Surface independent variables
  cat(sicovars, "\n",sep="",file=fw) #Surface independent covariates
  cat(min(inData$LONG), " ", max(inData$LONG), " ", 0, " ", 5, "\n", sep="",file=fw) #Independent variable 1 limits transf. and unit code 
  cat(min(inData$LAT), " ", max(inData$LAT), " ", 0, " ", 5, "\n", sep="",file=fw) #Independent variable 2 limits transf. and unit code
  cat(min(inData$ALT), " ", max(inData$ALT), " ", 1, " ", 1, "\n", sep="",file=fw) #Independent variable 3 limits transf. and unit code
  cat("1000\n", sep="",file=fw)
  cat("0\n",sep="",file=fw) #Dependent variable transformation
  cat(sp.order, "\n",sep="",file=fw) #Order of spline
  cat(nsurf, "\n",sep="",file=fw) #Number of surfaces (one per month)
  cat("0\n",sep="",file=fw) #Error surface point weighting
  cat("1\n",sep="",file=fw) #Optimisation directive
  cat("1\n",sep="",file=fw) #Smoothing directive for each surface
  cat(filename, "\n", sep="",file=fw) #Filename
  cat(round((nrow(inData)+0.1*nrow(inData))), "\n", sep="", file=fw) #Maximum number of data points
  cat("10\n",sep="",file=fw) #Station label length
  cat("(a10,2f10.3,f8.2/12f9.2)\n",sep="",file=fw) #Datafile format
  cat(variable,".res\n", sep="", file=fw) #Residual data file
  cat(variable,".opt\n", sep="", file=fw) #Optimisation data file
  cat(variable,".sur\n", sep="", file=fw) #Surface coefficients file
  cat(variable,".lis\n", sep="", file=fw) #Data list file
  cat(variable,".cov\n", sep="", file=fw) #Error covariance file
  cat("\n",file=fw)
  
  #Closing file
  close(fw)
  return(paste(variable,".sur",sep=""))
}
