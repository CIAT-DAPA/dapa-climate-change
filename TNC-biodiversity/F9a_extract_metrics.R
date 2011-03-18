#--------------------------------------------------------------------#
# Extract model parameters and write them to species.info file
# Johannes Signer (jmsigner@gmail.com)
# 18.3.2011
#--------------------------------------------------------------------#

extract.metrics <- function(path=".",log.file="log_extract_metric.txt"){

   # extract files *.lambda files
   system(paste("unzip - ",path,"/results.zip *.lambdas",sep=""))
   system(paste("mv ",path,"/*.lambdas ",path,"/train.lambdas",sep=""))

   # get metrics

   tryCatch({
   metrics <- read.csv(zip.file.extract("maxentResults.csv","results.zip"))
   write(paste(names(a), t(a[nrow(a),]),sep=":"),paste(path,"/info.txt", sep=""), append=T)
   report <- paste(path,"successfull")
   },error=funciton(x){report <- paste("error",x)})
   
   # write log
   write(report, log.file,append=T)

}
