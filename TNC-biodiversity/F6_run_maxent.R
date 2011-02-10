######################################################################
# Function to run Maxent
# Johannes Signer (jmsigner@gmail.com)
# 3.2.2011
#--------------------------------------------------------------------#
run.maxent <- function(path,max.ram, dir.maxent, no.replicates, replicate.type,log.file="log.run.maxent")
{ 
t.start <- proc.time()[3]
dir.create(paste(dir.out,"/",sp_id,"/results", sep=""))
dir.create(paste(dir.out,"/",sp_id,"/cross", sep=""))

# train the model
# flags:
# -a autorun
# -z dont show gui
# -P dont do importance of env variables
system(paste("java -mx",max.ram,"m -jar ", dir.maxent, "/maxent.jar nowarnings outputdirectory=", dir.out,"/",sp_id, "/results samplesfile=",dir.out,"/",sp_id,"/training/species_swd.csv environmentallayers=",dir.out,"/",sp_id,"/training/background_swd.csv -a -z nopictures -P plots=false", sep=""),wait=T)
					
# crossvalidate
system(paste("java -mx",max.ram,"m -jar ", dir.maxent, "/maxent.jar nowarnings outputdirectory=",dir.out,"/", sp_id, "/cross samplesfile=",dir.out,"/",sp_id,"/training/species_swd.csv environmentallayers=",dir.out,"/",sp_id,"/training/background_swd.csv -P replicates=",no.replicates," replicatetype=",replicate.type," -a -z nopictures -P plots=false", sep=""),wait=T)

## zip files
system(paste("zip -r ",dir.out,"/",sp_id,".zip ", dir.out,"/",sp_id,sep=""))
system(paste("rm -r ",dir.out,"/",sp_id, sep=""))

tt <- proc.time()[3] - t.start
write(paste(sp_id, tt, Sys.time(), sep=";"), log.file, append=T)
}

