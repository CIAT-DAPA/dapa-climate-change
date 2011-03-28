#Julian Ramirez
#March 2011
#CIAT / University of Leeds

#This would create a .cmd file that would perform the spline projection
#parameters that will be automatically tranferred from the input function to the run file

createPrjFile <- function(msk, variable="prec", nsurf=0, gridfiles=c("longitude.asc","latitude.asc","altitude.asc")) {
  #Opening the file in writing mode
  fw <- file(paste(variable, "prj.cmd", sep=""), open="w")
  
  #Creating surface list
  if (length(nsurf) != 1) {
    for (a in 1:length(nsurf)) {
      if (a == 1) {os <- paste(nsurf[a], sep="")} else {os <- paste(os, " ", nsurf[a], sep="")}
    }
  }
  
  #Writing all parameters
  cat(variable, ".sur\n", sep="", file=fw) #Surface coefficients file
  
  if (length(nsurf) != 1) {
    cat(os, "\n", sep="", file=fw) #Surface members to calculate
  } else {
    cat(nsurf, "\n", sep="", file=fw) #Surface members to calculate
  }
  cat("1\n", sep="", file=fw) #Type of calculation (0 for summary, 1 for actual values)
  cat("\n", sep="", file=fw) #Error covariance file (blank, as i do not want it)
  cat("\n", sep="", file=fw) #Maximum standard error to be calculated (not provided so blank)
  cat("1\n", sep="", file=fw) #Grid point position (0 for corners, 1 for centres)
  cat("0\n", sep="", file=fw) #Variable that increments in a row (longitude) (0 as i will provide file)
  cat(msk@extent@xmin, " ", (msk@extent@xmin+ncol(msk)*res(msk)[2]), " ", res(msk)[2], "\n", sep="", file=fw) #Limits and resolution of latitude
  cat("0\n", sep="", file=fw) #Variable that increments in a column (latitude) (0 as i will provide file)
  cat(msk@extent@ymin, " ", (msk@extent@ymin+nrow(msk)*res(msk)[1]), " ", res(msk)[1], "\n", sep="", file=fw) #Limits and resolution of longitude
  cat("0\n", sep="", file=fw) #Mode of mask grid (not supplied so 0)
  cat("2\n", sep="", file=fw) #Mode of first independent grid (2 means ESRI AAII)
  cat(gridfiles[1], "\n", sep="", file=fw) #Name of independent variable (longitude)
  cat("2\n", sep="", file=fw) #Mode of second independent grid (2 means ESRI AAII)
  cat(gridfiles[2],"\n", sep="", file=fw) #Name of independent variable (latitude)
  cat("2\n", sep="", file=fw) #Mode of third independent grid (2 means ESRI AAII)
  cat(gridfiles[3], "\n", sep="", file=fw) #Name of third independent variable (elevation)
  cat("2\n", sep="", file=fw) #Mode of output surface grids
  cat("-999\n", sep="", file=fw) #No data value in output grid
  
  #Looping output filenames
  #fix for zero
  if (length(nsurf) == 1 & nsurf == 0) {
    for (srf in 1:12) {
      cat(variable, "_", srf, ".asc\n", sep="", file=fw) #Output file name
    }
  } else {
    for (srf in nsurf) {
      cat(variable, "_", srf, ".asc\n", sep="", file=fw) #Output file name
    }
  }
  cat("(",ncol(msk), "f", 9.3, ")\n", sep="", file=fw) #Output file format
  
  #Last line
  cat("\n",file=fw)
  
  #Closing the file
  close(fw)
}
