######################################################################
# Function to project Maxent model
# Johannes Signer (jmsigner@gmail.com)
# 18.3.2011
#--------------------------------------------------------------------#
project.maxent <- function(path,max.ram, dir.maxent, dir.proj,type,log.file="log.run.maxent", proj.type="current")
{

   report <- paste(path,"projecting_",proj.type, ":",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
   dir.create(paste(path,"/",proj.type, sep=""))

   tryCatch({

# train the model
# flags:
# -a autorun
# -z dont show gui
   system(paste("java -mx",max.ram,"m -cp ", dir.maxent, "/maxent.jar density.Project ", path, "/train.lambdas ", dir.proj, " ", path, "/",proj.type,"/",proj.type," -r -a -z nowarnings",sep=""))
   
   report <- paste(report, "modelled_projected successfull")}, error=function(x){ report <- paste(report, "modell_projected error ", x)})
   
    system(paste("zip -jr ",path,"/",proj.type,".zip ", path,"/",proj.type,sep=""))
    system(paste("rm -r ", proj.type, sep=""))

   write(paste(report, "finished:",format(Sys.time(), "%Y-%m-%d-%H-%M-%S")), log.file, append=T)
}

