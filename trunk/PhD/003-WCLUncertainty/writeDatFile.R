#Julian Ramirez
#March 2011
#CIAT / University of Leeds

#Script to write a formatted file ready to be used in anusplin
#Format in which the data is stored (a10,2f10.3,f8.2/12f9.2)
#a10 is the ID of the station (10 characters)
#2f10.3 is longitude and latitude (two 10 characters and three decimal places fields)
#f8.2 is altitude (8 characters and 2 decimal places)
#/12f9.2 is for monthly data (twelve 9 character and 2 decimal places fields) in the next line

#Input station data is specific to the WorldClim input station data format and column names
#ID,LONG,LAT,ALT,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC

writeDat <- function(stData, filename="dummy.dat") {
  #Creating line jump row
  jumpRow <- rep("\n", nrow(stData))
  
  stData$ID <- substr(stData$ID, 1, 10)
  
  #Extracting data into a matrix
  tk <- matrix(c(format(stData$ID,width=10),
    formatC(stData$LONG,width=10,digits=3,format="f"),
    formatC(stData$LAT,width=10,digits=3,format="f"),
    formatC(stData$ALT,width=8,digits=2,format="f"),
    jumpRow,
    formatC(stData$JAN,width=9,digits=2,format="f"),
    formatC(stData$FEB,width=9,digits=2,format="f"),
    formatC(stData$MAR,width=9,digits=2,format="f"),
    formatC(stData$APR,width=9,digits=2,format="f"),
    formatC(stData$MAY,width=9,digits=2,format="f"),
    formatC(stData$JUN,width=9,digits=2,format="f"),
    formatC(stData$JUL,width=9,digits=2,format="f"),
    formatC(stData$AUG,width=9,digits=2,format="f"),
    formatC(stData$SEP,width=9,digits=2,format="f"),
    formatC(stData$OCT,width=9,digits=2,format="f"),
    formatC(stData$NOV,width=9,digits=2,format="f"),
    formatC(stData$DEC,width=9,digits=2,format="f"),
    jumpRow),
    nrow=nrow(stData),ncol=18,byrow=F
  )
  
  #Writing the file
  fw <- file(filename, open="w")
  cat(t(tk), file=fw, sep="")
  close(fw)
  return(filename)
}
