#Julian Ramirez
#March 2011
#CIAT / University of Leeds

#This would create a .cmd file that would perform the validation
#parameters that will be automatically tranferred from the input function to the run file

createValFile <- function(variable="prec", filename="", nsurf=0) {
  #Opening the file in writing mode
  fw <- file(paste(variable, "val.cmd", sep=""), open="w")
  
  #Writing all parameters
  cat(variable, ".sur\n", sep="", file=fw) #Surface coefficients file
  cat(nsurf, "\n", sep="", file=fw) #Surface members to calculate
  cat("1\n", sep="", file=fw) #Type of calculation (0 summary stats, 1 surface values)
  cat(variable, ".cov\n", sep="", file=fw) #Error covariance file
  cat("1\n", sep="", file=fw) #Type of error calculation (0 standard error, 1 model st errors, 2 pred. st. errors, 3 95% model CIs, 4 95% pred. CIs
  cat("\n", sep="", file=fw) #Maximum standard error to be calculated (not provided so blank)
  cat(filename, "\n", sep="",file=fw) #Validation points filename
  cat("10\n", sep="", file=fw) #Number of characters for station ID
  cat("(a10,2f10.3,f8.2/12f9.2)\n",sep="",file=fw) #Datafile format
  cat(variable, ".out\n", sep="", file=fw) #Output filename
  cat("1\n", sep="", file=fw) #Include position coordinates
  cat("(a10,2f10.3,f8.2,12f9.2,12f9.2)\n", sep="", file=fw) #Output data format
  cat("\n",file=fw)
  close(fw)
}
