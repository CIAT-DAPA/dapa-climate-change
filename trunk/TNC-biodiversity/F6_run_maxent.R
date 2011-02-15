######################################################################
# Function to run Maxent
# Johannes Signer (jmsigner@gmail.com)
# 3.2.2011
#--------------------------------------------------------------------#
run.maxent <- function(path,max.ram, dir.maxent, no.replicates, replicate.type,log.file="log.run.maxent")
{

   report <- paste(path,"maxent_startet:",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
   dir.create(paste(path,"/results", sep=""))
   dir.create(paste(path,"/cross", sep=""))

   tryCatch({

# train the model
# flags:
# -a autorun
# -z dont show gui
# -P dont do importance of env variables
   system(paste("java -mx",max.ram,"m -jar ", dir.maxent, "/maxent.jar nowarnings outputdirectory=", path, "/results samplesfile=",path,"/training/training_swd.csv environmentallayers=",path,"/training/bg.csv -a -z nopictures -P plots=false", sep=""),wait=T)
   
   report <- paste(report, "modelled_trained successfull")}, error=function(x){ report <- paste(report, "modell_trained error ", x)})


# crossvalidate
   
   tryCatch({
         system(paste("java -mx",max.ram,"m -jar ", dir.maxent, "/maxent.jar nowarnings outputdirectory=",path, "/cross samplesfile=",path,"/training/training_swd.csv environmentallayers=",path,"/training/bg.csv -P replicates=",no.replicates," replicatetype=",replicate.type," -a -z nopictures -P plots=false", sep=""),wait=T)
      report <- paste(report, "crossvalidation successfull")}, error=function(x){report <- paste(report, "crossvalidation error",x)})

## zip files
   tryCatch({
   system(paste("zip -jr ",path,"/cross.zip ", path,"/cross",sep=""))
   system(paste("zip -jr ",path,"/results.zip ", path,"/results",sep=""))
   system(paste("rm -r ",path,"/cross", sep=""))
   system(paste("rm -r ",path,"/results", sep=""))
   report <- paste(report, "successfull zip")}, error=function(x){
      report <- paste(report, "error zip", x)
   })

   write(paste(report, "finished:",format(Sys.time(), "%Y-%m-%d-%H-%M-%S")), log.file, append=T)
}

