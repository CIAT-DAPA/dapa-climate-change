######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Johannes Signer
# Date: 24.1.2011
######################################################################

#--------------------------------------------------------------------#
# workflow:

# 1. Load the *.csv file with all global plant points
# 2. Split the the files and make for each species a folder
# 3. Create for each species a background file with 10000 background points
# 4. Create a presence swd file
# 5. Create a background swd file
# 6. Train and run Maxent

#--------------------------------------------------------------------#
# load libraries

library(raster)
library(snowfall)
library(jtools)
library(plyr)
library(stringr)

# clean workspace
rm(list=ls())

# Load parameters and setup working directory assuming R session was onpend from iabin root directory
source("./parameters/parameters.R")

#--------------------------------------------------------------------#
# 1. Read file with all presence points, split it and create a folder
#    for each species. The function is located in the file ./src/scripts/F1.R

sp <- apply(species.files.raw,1,write.species.csv,log.file=log.make.species.csv, min.points=pts.min,dir.out=dir.out)

#--------------------------------------------------------------------#
# Create SWD files 

# read the raster with biomes
biomes <- raster(biomes.path)
biomes.values <- getValues(biomes)

# extract backgrounds from wwf biomes task 10
### Export variables to workers
sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("get.background")
sfLibrary(raster)
sfExport("biomes")
sfExport("biomes.values")
sfExport("me.no.background")
sfExport("dir.out")
sfExport("dir.error.bg")
for (i in 1:length(sp))
system.time(sfSapply(sp[[i]], function(i) get.background(sp_id=i, biomes=biomes, v.all=biomes.values, no.background=me.no.background)))
 
sfStop()


## Make swd files 
# extract all unique points
system.time(get.full.swd(type="species", env.full=env.full, dir.out=dir.out))
system.time(get.full.swd(type="background", env.full=env.full, dir.out=dir.out))

# get points for each species sp
system.time(sapply_pb(sp, function(x) get.sp.swd(sp_id=x, type="species", pts=pts.full, swd=read.csv(paste(dir.out, "/species_swd.csv", sep="")))))

## run in parallel, because its to slow
sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("get.sp.swd")
sfExport("pts.full")
sfExport("dir.out")
system.time(sfSapply(sp, function(x) get.sp.swd(sp_id=x, type="background", pts=pts.full, swd=read.csv(paste(dir.out, "/background_swd.csv", sep="")))))
sfStop()



##### Run Maxent #####
### create the batch files for the servers 
split.list <- rep(1:sum(cores),each=ceiling(length(sp)/sum(cores)), length.out=length(sp))
per.core <- split(sp, split.list)
t.count <- 1 # tmp count
for (server in 1:length(servers))
{
  # save paramters
  if(!file.exists(paste(dir.out,servers[server],sep="/"))) dir.create(paste(dir.out,servers[server],sep="/"))
  for (core in 1:cores[server])
  {
    if(length(per.core)>=t.count)
    {
      # write species list    
      write.table(per.core[[t.count]], paste(dir.out,"/",servers[server],"/species_list_core",core,".txt",sep=""), row.names=F, col.names=F, quote=F)
      # write R batch file for each core
      write(paste("# load params\n",
        "load(\"",dir.out,"/parameters.RData\")\n",
        "# load data\n",
        "files <- read.table(\"",dir.out,"/",servers[server],"/species_list_core",core,".txt\")\n",
        "# prepare the log file\n",
        "log.file <- \"",log.run.maxent.prefix,"_", servers[server], "_core", core,".txt\"\n",
        "write(\"sp_id;proc_time;finished_at\", log.file, append=F)\n",
        "# run maxent\n",
        "sapply(files[,1], function(i) run.maxent(sp_id=i,max.ram=me.max.ram, dir.maxent=dir.maxent, dir.proj=dir.proj, no.replicates=me.no.replicates, replicate.type=me.replicate.type,log.file=log.file))",sep=""),
        paste(dir.out,"/",servers[server],"/runmaxent_core",core,".R",sep=""))
       t.count <- t.count+1
    }
  }
  # for each server make a shell script to start execute the R batches
  # call the shell scripts from the iabin root directory
  write(paste("for f in ",dir.out,"/", servers[server], "/ *.R\n",
    "do\n",
    "screen -S $f -m -d R CMD BATCH $f\n",
    # -S gives a meaningful session names, in this caste genomix*/runmanxent_core*.R
    # -d -m detach console
    "done", sep=""), paste(dir.out,"/", servers[server], "/run_batches.sh", sep=""))
  
}

  
###########################################
# Extract metrics from the model and 
# import rasters to grass (task 15)
###########################################

# most likely this will be continued after some time and parameters will need to be reloaded
# the name of the out.dir of the run that shall be continued need to be given here!
# This R session needs to be started from a GRASS shell in the right mapset with the right region!
parameters <- "results/20101216.4/parameters.RData"
load(parameters)
# load species ids
load(species.id.to.process)
files <- list.files(path=dir.out,pattern="^[0-9].*.zip")

# This script must be run within a grass shell
# check mapset and region are right
# setting up grass
#system(paste("g.mapset mapset=",mapset," -c", sep=""))
system("g.region sa")
system("g.region res=00:05")

# remove where there were erros predicting, when size is to small
files <- data.frame(files, stringsAsFactors=F)
files <- data.frame(files, size=apply(files,1,function(x) file.info(paste(dir.out,"/",x,sep=""))$size))

# only continue files with more than 1mb (1048576 bytes), write others to the redo file
files.error <- files[files[,2]<1048576,]
files <- files[files[,2]>=1048576,]

# prep out file
eval.stats <- data.frame(id=rep(NA, length(files[,1])), avg.auc_train=NA, avg.auc_test=NA, sd.auc=NA, prevalence=NA, ten.percentile=NA, roc=NA, bio1=NA, bio12=NA, bio15=NA, bio4=NA, bio5=NA, bio6=NA, bio16=NA, bio17=NA, max.contribution=NA)

write.table(files.error, paste(dir.out, "/species_where_maxent_failed.txt",sep=""))

st <- proc.time()
for (i in 1:length(files[,1])){
  system(paste("unzip -q ",dir.out,"/",files[i,1],sep="")) # extract the archives
  this.id=strsplit(files[i,1],"\\.")[[1]][1] # get the id of the species that is being procesed 
  
  # calculate roc threshold, min distance
  roc <- c()  
  for (j in 0:(me.no.replicates-1)){ ## some revision is needed here!    
    omission.rate <- try(read.csv(paste(dir.out,"/",this.id, "/cross/", this.id, "_",j,"_omission.csv", sep=""))) 
    if (class(omission.rate) != "try-error") { 
      spec <- omission.rate$Fractional.area
      sens <- 1-omission.rate$Training.omission
      log.vals <- omission.rate$Corresponding.logistic.value
          
      abs.dif <- abs(1-(sens+spec))

      if (length(log.vals[which(abs.dif== min(abs.dif))]) > 1) {
        roc <- c(roc, mean(log.vals[which(abs.dif==min(abs.dif))]))
      } else roc <- c(roc, log.vals[which(abs.dif==min(abs.dif))])
    }
  }

  me.res <- read.csv(paste(dir.out,"/",this.id, "/cross/maxentResults.csv", sep=""))

  eval.stats[i,'id'] <- this.id
  eval.stats[i,'avg.auc_train'] <- me.res[nrow(me.res),'Training.AUC']
  eval.stats[i,'avg.auc_test'] <- me.res[nrow(me.res),'Test.AUC']
  
  # if sd < 0, substitute for NA, that is because there fewer than 20 points and CV fails
  me.sd <- me.res[nrow(me.res),'AUC.Standard.Deviation']
  if (me.sd < 0) {eval.stats[i,'sd.auc'] <- NA} else eval.stats[i,'sd.auc'] <- me.sd

  eval.stats[i,'prevalence'] <- me.res[(me.no.replicates+1),'Prevalence..average.of.logistic.output.over.background.sites.']
  eval.stats[i,'ten.percentile'] <- me.res[(me.no.replicates+1),'X10.percentile.training.presence.logistic.threshold']
  eval.stats[i,'roc'] <- mean(roc)

  # bio variables
  eval.stats[i,'bio1'] <- me.res[nrow(me.res),'bio1.contribution']
  eval.stats[i,'bio12'] <- me.res[nrow(me.res),'bio12.contribution']
  eval.stats[i,'bio15'] <- me.res[nrow(me.res),'bio15.contribution']
  eval.stats[i,'bio4'] <- me.res[nrow(me.res),'bio4.contribution']

  # other four variales, only if a species has more than 10 records
  if(any(grepl("bio5", names(me.res)))) eval.stats[i,'bio5'] <- me.res[nrow(me.res),'bio5.contribution']
  if(any(grepl("bio6", names(me.res)))) eval.stats[i,'bio6'] <- me.res[nrow(me.res),'bio6.contribution']
  if(any(grepl("bio16", names(me.res)))) eval.stats[i,'bio16'] <- me.res[nrow(me.res),'bio16.contribution']
  if(any(grepl("bio17", names(me.res)))) eval.stats[i,'bio17'] <- me.res[nrow(me.res),'bio17.contribution']

  # get the bio with the max contribution
  eval.stats[i, 'max.contribution'] <- names(eval.stats)[which(eval.stats[i,]==max(eval.stats[i,grep("bio", names(eval.stats))], na.rm=T))][1]

  print(paste("finished ", i, "of", length(files[,1]), "species."))
  
  # import projection to grass
  system(paste("r.in.arc in=",dir.out,"/",this.id,"/proj/",this.id,".asc out=me.",this.id," --o  --q",sep=""),wait=T)
  # mport points to grass  
  system(paste("cat ",dir.out,"/",this.id,"/training/species.csv | awk -F',' 'NR > 1 {print $2 \"|\" $3}' | v.in.ascii out=occ_",this.id," --o --q",sep=""),wait=T)
  # delete all files except the info file, that have been extracted from the archive  
  system(paste("ls ",dir.out,"/",this.id," | grep -v info.txt |awk '{print\"",this.id,"/\" $0}' | xargs rm -rf",sep=""),wait=T)
  # move archive into the folder  
  system(paste("mv ",dir.out,"/",files[i,1]," ", dir.out, "/", this.id,sep=""))
  # delete all other folders 
  system(paste("rm -r ", dir.out,"/", this.id, "/cross ",dir.out,"/", this.id, "/proj ",dir.out,"/", this.id, "/results ",dir.out,"/", this.id, "/training",sep=""))
}

et <- proc.time()

# check for errors
missing.entries <- which(is.na(unlist(eval.stats)))
missing.values <- gsub("[0-9]", "", names(missing.entries))
missing.sp <- eval.stats[gsub("[^0-9]","",names(missing.entries)),1]
write.table(c("sp_id,missing"), log.extract.auc,row.names=F, col.names=F, quote=F, append=F)
write.table(cbind(missing.sp, missing.values), log.extract.auc, row.names=F, col.names=F, quote=F, append=T)

# write output
write.table(eval.stats, paste(dir.out, "/evaluation_statistics.csv",sep=""), col.names=T, row.names=F, sep=",", append=F, quote=F)

# write it to the species file
count <- 1
for (i in eval.stats[,1])
{
write(paste("avg.auc_train : ",eval.stats[eval.stats[,1]==i,'avg.auc_train'], 
              "\navg.auc_test : ",eval.stats[eval.stats[,1]==i,'avg.auc_test'],
              "\nsd.auc : ",eval.stats[eval.stats[,1]==i,'sd.auc'],
              "\nprevalence : ",eval.stats[eval.stats[,1]==i,'prevalence'],
              "\nten.percentile : ",eval.stats[eval.stats[,1]==i,'ten.percentile'], 
              "\nroc : ",  eval.stats[eval.stats[,1]==i,'roc'], 
              "\nbio1 : ", eval.stats[eval.stats[,1]==i,'bio1'],
              "\nbio4 : ", eval.stats[eval.stats[,1]==i,'bio4'],
              "\nbio12 : ", eval.stats[eval.stats[,1]==i,'bio12'],
              "\nbio15 : ", eval.stats[eval.stats[,1]==i,'bio15'],
              "\nbio5 : ", eval.stats[eval.stats[,1]==i,'bio5'],
              "\nbio6 : ", eval.stats[eval.stats[,1]==i,'bio6'],
              "\nbio16 : ", eval.stats[eval.stats[,1]==i,'bio16'],
              "\nbio17 : ", eval.stats[eval.stats[,1]==i,'bio17'],
              "\nmost.important.bio : ", eval.stats[eval.stats[,1]==i,'max.contribution'],sep=""), paste(dir.out,i,"info.txt", sep="/"), append=T)
              if(count %% 100 == 0) print(count)
              count <- count+1
}

###########################################
# Move all modells with an AUC of < 0.7 to
# a different folder

mkdir auc_below_07

for species in `ls | grep ^[0-9].*.[0-9]$`
do
 auc_species=`awk -F" : " '/avg.auc_test/{print $2}' $species/info.txt`
 if [  $(echo "$auc_species < 0.7" | bc) -gt 0 ] # check if the AUC is above the required threshold
 then
    mv -v $species ./auc_below_07/
 fi
done

###########################################
# Do CHull, export chull as buffer and cut 
# predictions to buffered chull (task 14)
###########################################

# description: cuts the predictions by a buffered convex hull of the occurence points and writes a *.csv with the coordinates of the convexhull and the buffered convex hull.

#### Execute in GRASS shell assuming the location and mapset are correct
#### cd into the working directory of this run eg. cd ./results/20101012.2

# load parameters 
. ../../parameters/parameters.sh

# initizialise
count=1
total=`ls | grep ^[0-9].*.[0-9]$ | wc -l` 

for i in `ls | grep ^[0-9].*.[0-9]$`
do
  # chull
  v.hull in=occ_$i out=occ_$i\_hull --o --q
  # buff chull  
  v.buffer in=occ_$i\_hull out=occ_$i\_hull_buffer distance=$buffer_distance type=area --o --q

  # export chull
  echo "lon,lat" >  $i/chull.csv
   # exports the coords of the polygon (B) and the centroid (C), awk is used to get extract only the points of the polyon. 
  v.out.ascii in=occ_$i\_hull format=standard | awk '/B/, /C/{if(!/B/ && !/C/) print $1","$2}' >> $i/chull.csv

  # export buffered chull
  echo "lon,lat" > $i/chull_buffer.csv
  # exports the coords of the polygon (B) and the centroid (C), awk is used to get extract only the points of the polyon. 
  v.out.ascii in=occ_$i\_hull_buffer format=standard | awk '/B/, /C/{if(!/B/ && !/C/) print $1","$2}' >> $i/chull_buffer.csv

  # cut predicitoins buffer
  v.to.rast in=occ_${i}_hull_buffer out=tmp use=val value=1 --q
  r.mapcalc "MASK=tmp"
  r.mapcalc "me.c.$i=me.$i"
  g.remove rast=tmp,MASK,me.$i --q
    
  # information on progress
  echo processed $count of $total
  count=$(( $count + 1 ))
done

#####################################################################################
# Species Richness per genus (task 16)
###########################################

# make a file where for each species the class, family, genus and species is written
# task 16 and 17 could be made a lot more efficient with the use of r.series, method=count

echo "class,fam_id,gen_id,sp_id" > tax.txt

for i in `ls | grep ^[0-9].*.[0-9]$`
do
  class=`awk -F' : ' '/class/ {print $2}' $i/info.txt`
  fam=`awk -F' : ' '/family_id/ {print $2}' $i/info.txt`
  gen=`awk -F' : ' '/genus_id/ {print $2}' $i/info.txt`
  sp=`awk -F' : ' '/species id/ {print $2}' $i/info.txt`
  echo "$class,$fam,$gen,$sp" >> tax.txt

done

# species richness per genus
# genus is the third column in the file tax.txt, hence take unique values

for genus in `awk -F, 'NR>1 {print $3}' tax.txt | sort -u`  # for each unique genus
do
  for species in `awk -F, '/'$genus'/{if(NR>1); print $4}' tax.txt` # for each species that belongs to the genus $genus
  do
    auc_species=`awk -F" : " '/avg.auc_test/{print $2}' $species/info.txt`
    if [  $(echo "$auc_species > $auc_th" | bc) -gt 0 ] # check if the AUC is above the required threshold
    then
      threshold=`awk -F" : " '/^'$occ_th'/{print $2}' $species/info.txt` # get the threshold from the species
      r.mapcalc "gen.sp.$genus.$species=if(isnull(me.c.${species}) ||| me.c.${species} < $threshold,0,1)" # create map for this species
    fi
  done
  g.mlist type=rast pattern="gen.sp.$genus.*" > tmp.sf # potentially problem, and an addtional loop will be needed for the mapcalc
  # adding the raster together
  r.mapcalc "A=0"
  r.mapcalc "B=0"
  out=A
  tmp=B

  while read line
  do
    r.mapcalc "$out=$tmp+$line"
    if [ $out == "A" ]; then
      out=B
      tmp=A
    else
      out=A
      tmp=B
    fi
  done < tmp.sf
  g.copy rast=$tmp,gen.$genus --o
  g.mremove rast="gen.sp.*" -f
done

# clean up
g.mremove rast="gen.sp.*" -f
g.remove rast=A,B
rm tmp.sf



###########################################
# Species Richness per class (task 17)
###########################################

# species richness per class

for class in `awk -F, 'NR>1 {print $1}' tax.txt | sort -u`  # for all classes
do
  for species in `awk -F, '/'$class'/{if(NR>1); print $4}' tax.txt` # get all species that belong to class a class
  do
    auc_species=`awk -F" : " '/avg.auc_test/{print $2}' $species/info.txt` # get the species auc
    if [  $(echo "$auc_species > $auc_th" | bc) -gt 0 ] # treat a species only if its AUC ia above the critical threshold
    then
      sleep 1
      threshold=`awk -F" : " '/^'"$occ_th"'/{print $2}' $species/info.txt`
      r.mapcalc "cla.sp.$class.$species=if(isnull(me.c.${species}) ||| me.c.${species} < $threshold,0,1)" # only consider area above the threshold
    fi
      
  done
  g.mlist type=rast pattern="cla.sp.$class.*" > tmp.sf

  # adding the raster together
  r.mapcalc "A=0"
  r.mapcalc "B=0"
  out=A
  tmp=B
  
  # progress
  # now adding all the rasters together

  while read line
  do
    r.mapcalc "$out=$tmp+$line"
    if [ $out == "A" ]; then
      out=B
      tmp=A
    else
      out=A
      tmp=B
    fi
  done < tmp.sf
  g.copy rast=$tmp,cla.$class
done

# clean up
g.mremove rast="cla.sp.*" -f
g.remove rast=A,B
rm tmp.sf

###########################################
# Threat for each species (task 18)
###########################################

###########################################
# R

###########################################
# Abrevatoins
#
# a. - stands for area in km2
# p. - percent
# m. - mean
# sd. - sd



ev <- read.csv("evaluation_statistics.csv")

 


###########################################
# load default

library(spgrass6)
library(raster)

sa <- raster(readRAST6("sa"))
sav <- getValues(sa)
a.sa <- getValues(area(sa))

t.agg <- raster(readRAST6("ta.aggregate"))
t.aggv <- getValues(t.agg)

# all the threats
threat.names <- c("aggregate", "access_pop", "conv_ag", "fires", "grazing", "infrastr", "oil_gas", "rec_conv")

threat.data <- list()

for (i in 1:length(threat.names)) {
  tmp <- list()
  tmp$name <- threat.names[i]
  tmp$data <- raster(readRAST6(paste("ta.",threat.names[i],sep="")))
  tmp$datav <- getValues(tmp$data)

  levels <- list()

  for (j in 1:4) {
    tmp2 <- list()
    tmp2$data <- raster(readRAST6(paste("tac.ta",threat.names[i],j,sep=".")))     
    tmp2$datav <- getValues(tmp2$data)
    
    tmp2v <- ifelse(is.na(tmp2$datav),0,1)
    tmp2$datav <- getValues(mask(setValues(sa,tmp2v),sa))

    levels[[j]] <- tmp2
  }
  tmp$levels <- levels
  threat.data[[i]] <- tmp
}


# code pixels that are lost already with 0, others with 1 and outside sa with NA
lost <- ifelse(is.na(t.aggv),0,1)
lost <- mask(setValues(sa,lost),sa)
lostv <- getValues(lost)

files <- list.files(pattern="^[0-9].*[0-9]$")
count <- 1
for (sp in files) {

  roc <- ev[ev$id==sp, 'roc']

  r <- raster(readRAST6(paste("me.c.",sp,sep="")))

  # total above threshold
  rv <- getValues(r)    
  a.total <- sum(a.sa[rv>=roc], na.rm=T)
  write(paste("area.above.th.in.km2 : ",a.total, sep=""), paste(sp,"/info.txt", sep=""), append=T)

  # area lost already
  a.lost <- sum(a.sa[rv>=roc & lostv == 0],na.rm=T)
  write(paste("area.lost.in.km2 : ",a.lost, sep=""), paste(sp,"/info.txt", sep=""), append=T)

  # area not lost
  a.nlost <- sum(a.sa[rv>=roc & lostv == 1],na.rm=T)

  # percent lost
  p.lost <- a.lost / a.total * 100
  write(paste("percent.lost : ",p.lost, sep=""), paste(sp,"/info.txt", sep=""), append=T)

  # mean occurrence probability in lost area
  m.occurrence.lost.at <- mean(rv[rv>=roc & lostv == 0],na.rm=T)
  write(paste("lost.mean.occ.probability : ",m.occurrence.lost.at, sep=""), paste(sp,"/info.txt", sep=""), append=T)

  # mean occurence probabilty in still available habitat
  m.occurrence.nlost.at <- mean(rv[rv>=roc & lostv == 1],na.rm=T)
  write(paste("not.lost.mean.occ.probability :",m.occurrence.nlost.at, sep=""), paste(sp,"/info.txt", sep=""), append=T)
  

  for (i in 1:length(threat.names)) {
    m.threat <- mean(threat.data[[i]]$datav[rv>=roc], na.rm=T)
    write(paste("ta.",threat.names[i],".mean : ",m.threat, sep=""), paste(sp,"/info.txt", sep=""), append=T)
    sd.threat <- sd(threat.data[[i]]$datav[rv>=roc], na.rm=T)
    write(paste("ta.",threat.names[i],".sd : ",sd.threat, sep=""), paste(sp,"/info.txt", sep=""), append=T)
    max.threat <- max(threat.data[[i]]$datav[rv>=roc], na.rm=T)
    write(paste("ta.",threat.names[i],".max : ",max.threat, sep=""), paste(sp,"/info.txt", sep=""), append=T)
    min.threat <- min(threat.data[[i]]$datav[rv>=roc], na.rm=T)
    write(paste("ta.",threat.names[i],".min : ",min.threat, sep=""), paste(sp,"/info.txt", sep=""), append=T)  

    for (j in 1:4) {
      
      # Percent under threat
      a.level <- sum(a.sa[threat.data[[i]]$level[[j]]$datav==1 & rv>=roc],na.rm=T) / a.nlost * 100
      
      if (threat.names[i]=="access_pop")      
        a.level <- sum(a.sa[threat.data[[i]]$level[[j]]$datav==1 & rv>=roc],na.rm=T) / a.total * 100

      write(paste("percent.under.",threat.names[i],".",j," : ",a.level, sep=""), paste(sp,"/info.txt", sep=""), append=T)
            
      # mean occurrence probability under threat
      m.ocp.level <- mean(rv[threat.data[[i]]$level[[j]]$datav==1 & rv>=roc],na.rm=T)
      write(paste(threat.names[i],".",j,"mean.occ.probability : ",m.ocp.level, sep=""), paste(sp,"/info.txt", sep=""), append=T)
      
      # mean occurenc eprobability not under threat
      m.ocp.nlevel <- mean(rv[threat.data[[i]]$level[[j]]$datav==0 & rv>=roc],na.rm=T)
      write(paste("not.",threat.names[i],".",j,"mean.occ.probability : ",m.ocp.nlevel, sep=""), paste(sp,"/info.txt", sep=""), append=T)
  
    }
  }

  print(paste(count, "of", length(files)))
  count <- count + 1
}



### Extract values

# all the fields from the info.txt file that will be included into the table
fields.to.extract <- c("species id ","percent.lost ","lost.mean.occ.probability ","not.lost.mean.occ.probability ","ta.aggregate.mean ","ta.aggregate.sd ","ta.aggregate.max ","ta.aggregate.min ","percent.under.aggregate.1 ","aggregate.1mean.occ.probability ","not.aggregate.1mean.occ.probability ","percent.under.aggregate.2 ","aggregate.2mean.occ.probability ","not.aggregate.2mean.occ.probability ","percent.under.aggregate.3 ","aggregate.3mean.occ.probability ","not.aggregate.3mean.occ.probability ","percent.under.aggregate.4 ","aggregate.4mean.occ.probability ","not.aggregate.4mean.occ.probability ","ta.access_pop.mean ","ta.access_pop.sd ","ta.access_pop.max ","ta.access_pop.min ","percent.under.access_pop.1 ","access_pop.1mean.occ.probability ","not.access_pop.1mean.occ.probability ","percent.under.access_pop.2 ","access_pop.2mean.occ.probability ","not.access_pop.2mean.occ.probability ","percent.under.access_pop.3 ","access_pop.3mean.occ.probability ","not.access_pop.3mean.occ.probability ","percent.under.access_pop.4 ","access_pop.4mean.occ.probability ","not.access_pop.4mean.occ.probability ","ta.conv_ag.mean ","ta.conv_ag.sd ","ta.conv_ag.max ","ta.conv_ag.min ","percent.under.conv_ag.1 ","conv_ag.1mean.occ.probability ","not.conv_ag.1mean.occ.probability ","percent.under.conv_ag.2 ","conv_ag.2mean.occ.probability ","not.conv_ag.2mean.occ.probability ","percent.under.conv_ag.3 ","conv_ag.3mean.occ.probability ","not.conv_ag.3mean.occ.probability ","percent.under.conv_ag.4 ","conv_ag.4mean.occ.probability ","not.conv_ag.4mean.occ.probability ","ta.fires.mean ","ta.fires.sd ","ta.fires.max ","ta.fires.min ","percent.under.fires.1 ","fires.1mean.occ.probability ","not.fires.1mean.occ.probability ","percent.under.fires.2 ","fires.2mean.occ.probability ","not.fires.2mean.occ.probability ","percent.under.fires.3 ","fires.3mean.occ.probability ","not.fires.3mean.occ.probability ","percent.under.fires.4 ","fires.4mean.occ.probability ","not.fires.4mean.occ.probability ","ta.grazing.mean ","ta.grazing.sd ","ta.grazing.max ","ta.grazing.min ","percent.under.grazing.1 ","grazing.1mean.occ.probability ","not.grazing.1mean.occ.probability ","percent.under.grazing.2 ","grazing.2mean.occ.probability ","not.grazing.2mean.occ.probability ","percent.under.grazing.3 ","grazing.3mean.occ.probability ","not.grazing.3mean.occ.probability ","percent.under.grazing.4 ","grazing.4mean.occ.probability ","not.grazing.4mean.occ.probability ","ta.infrastr.mean ","ta.infrastr.sd ","ta.infrastr.max ","ta.infrastr.min ","percent.under.infrastr.1 ","infrastr.1mean.occ.probability ","not.infrastr.1mean.occ.probability ","percent.under.infrastr.2 ","infrastr.2mean.occ.probability ","not.infrastr.2mean.occ.probability ","percent.under.infrastr.3 ","infrastr.3mean.occ.probability ","not.infrastr.3mean.occ.probability ","percent.under.infrastr.4 ","infrastr.4mean.occ.probability ","not.infrastr.4mean.occ.probability ","ta.oil_gas.mean ","ta.oil_gas.sd ","ta.oil_gas.max ","ta.oil_gas.min ","percent.under.oil_gas.1 ","oil_gas.1mean.occ.probability ","not.oil_gas.1mean.occ.probability ","percent.under.oil_gas.2 ","oil_gas.2mean.occ.probability ","not.oil_gas.2mean.occ.probability ","percent.under.oil_gas.3 ","oil_gas.3mean.occ.probability ","not.oil_gas.3mean.occ.probability ","percent.under.oil_gas.4 ","oil_gas.4mean.occ.probability ","not.oil_gas.4mean.occ.probability ","ta.rec_conv.mean ","ta.rec_conv.sd ","ta.rec_conv.max ","ta.rec_conv.min ","percent.under.rec_conv.1 ","rec_conv.1mean.occ.probability ","not.rec_conv.1mean.occ.probability ","percent.under.rec_conv.2 ","rec_conv.2mean.occ.probability ","not.rec_conv.2mean.occ.probability ","percent.under.rec_conv.3 ","rec_conv.3mean.occ.probability ","not.rec_conv.3mean.occ.probability ","percent.under.rec_conv.4 ","rec_conv.4mean.occ.probability ","not.rec_conv.4mean.occ.probability ","occ.prob.mean.in.pa ","occ.prob.sd.in.pa ","occ.prob.mean.outside.pa ","occ.prob.sd.outside.pa ","percent.area.protected ")

# create the tables
write(paste(fields.to.extract, collapse=","), "threats_table.csv", append=F)
write(paste(fields.to.extract, collapse=","), "error.threats_table.csv", append=F)

# get all files
f <- list.files(pattern="^[0-9].*[0-9]$")

# extract all values
system.time(lapply(f, rw.threat))

# function that extracts the values
rw.threat <- function(id, file="threats_table.csv", to.extract=fields.to.extract) {

  tmp <- read.table(paste(id, "/info.txt", sep=""), sep=":", stringsAsFactors=F)
  extracted <- tmp[tmp[,1] %in% to.extract,2]
  if(length(extracted)==length(to.extract)) {
    write(paste(extracted, collapse=","), file, append=T)
  } else write(paste(tmp[tmp[1,] %in% to.extract,2], collapse=","), paste("error.",file,sep=""), append=T)

}


## calculate the threat for each species

threats <- read.csv("threats_table.csv")

threats <- threats[!is.na(threats[,2]),]

# prep data.frame
for (danger in c("rec_conv", "infrastr", "grazing", "fires", "conv_ag", "access_pop", "oil_gas", "aggregate"))
 threats[1,paste("risk.", danger, sep="")] <- NA

col.risk <- grep("risk", names(threats))


for (i in 1:nrow(threats)) {
  t.index <- c()
  for (danger in c("rec_conv", "infrastr", "grazing", "fires", "conv_ag", "access_pop", "oil_gas", "aggregate"))
  {
    tmp.danger <- c()
    tmp.ocp <- c()
    for (level in 1:4) {
        a.threat <- threats[i,grep(paste("percent.under.",danger,".",level, sep=""), names(threats))]
        m.ocp <- threats[i,grep(paste("^",danger,".",level,"mean.occ.probability", sep=""), names(threats))]
        if (a.threat == 0 | is.na(a.threat)) {a.threat <- 0; m.ocp <- 1}      
        tmp.danger <- c(tmp.danger, a.threat/100 * m.ocp * level )
        tmp.ocp <- c(tmp.ocp, m.ocp*level)
    }
    t.index <- c(t.index, sum(tmp.danger)/sum(tmp.ocp))
  }
  threats[i,col.risk] <- t.index

if(i %% 100 == 0) print(i)

}



###########################################
# Maximum threat

# which is the highest risk
threats[, 'highest.risk'] <- apply(threats,1,get.highest.risk)

# function to the the highest risk, used in the apply above
get.highest.risk <- function(threats) 
  return(paste(names(threats)[which(threats==max(threats[grep("risk.*.[vgrpgs]$", names(threats))], na.rm=T))], collapse=","))

# what is the value of the highest risk
threats[,'value.highest.risk'] <- apply(threats[,grep("risk.*.[vgrpgs]$", names(threats))],1,max)
threats$group <- as.numeric(substr(threats$species.id,1,1))


# add a threat group 1 lowest quartile and 4 the highest
ss <- summary(threats$value.highest.risk)
threats$threat.group <- vreclass(threats$value.highest.risk, ss[c(2,3,5)])

# only use files AUC is > 0.7
f <- list.files(patter="^[0-9].*.[0-9]$")
threats <- threats[match(f,threats[,"species.id"]),]


write.csv(threats, "threats_complete.csv")


###########################################
# Mean % area protected in and outside 
# protected areas (task 19)
###########################################

# For each species calculate the mean and sd occurence probability within and outside protected areas
# For each species calcualte the % area within and outside protected areas

for species in `ls | grep ^[0-9]`
do
  g.remove rast="MASK"
  r.mapcalc "tmp.$species.1=if(me.c.${species} > 0,1,null())" # calculate temp grid only with the area above threshold
  area_total=`r.sum tmp.${species}.1 | awk -F= '{print $2}'` # sum the area
  r.mapcalc "MASK=if(protected_areas@PERMANENT)"
  eval `r.univar me.c.${species} -g`
  area_protected=`r.sum tmp.${species}.1 | awk -F= '{print $2}'` # sum the area
  percent_protected=`echo "scale=2; $area_protected / $area_total *100" | bc`  
  echo "occ.prob.mean.in.pa : $mean" >> $species/info.txt
  echo "occ.prob.sd.in.pa : $stddev" >> $species/info.txt
  g.remove rast="MASK"  
  r.mask -r
  r.mask -i -o protected_areas@PERMANENT
  eval `r.univar me.c.${species} -g`
  echo "occ.prob.mean.outside.pa : $mean" >> $species/info.txt
  echo "occ.prob.sd.outside.pa : $stddev" >> $species/info.txt
  echo "percent.area.protected : $percent_protected" >> $species/info.txt
done
g.remove rast="MASK"

###########################################
# Calculate for each park, takes about 2hrs (task 20)
###########################################

# for each protected area calculated the 

mkdir parks

for i in `r.category map=protected_areas_cat`
do 
  echo "cat : value" > parks/$i.txt
  # mask only this area
  r.mapcalc "tmp=if(protected_areas_cat==$i,1,null())"
  r.mapcalc "MASK=tmp"  
  for j in `g.mlist type=rast pattern="cla.*[a-z]$"`
  do
    eval `r.univar $j -g`
    echo "$j.max : $max" >> parks/$i.txt
    echo "$j.min : $min" >> parks/$i.txt
    echo "$j.sd : $stddev" >> parks/$i.txt
    echo "$j.mean : $mean" >> parks/$i.txt
  done

  for k in `g.mlist type=rast pattern="ta.*"`
  do
    eval `r.univar $k -g`
    echo "$k.max : $max" >> parks/$i.txt
    echo "$k.min : $min" >> parks/$i.txt
    echo "$k.sd : $stddev" >> parks/$i.txt
    echo "$k.mean : $mean" >> parks/$i.txt
  done
  g.remove rast=MASK
done
g.remove rast=tmp

## Join this values to the map of WDPA
# add columns
g.copy vect=protected_area@PERMANENT,pa
v.db.addcol map=pa columns="max_rep double precission"

for i in parks/*
do
  value=`cat $i | awk -F: '/cla.reptilia.max/{print $2}'`
  cat=`echo $i | cut -d/ -f2 | cut -d. -f1`
  v.db.update map=pa column=max_rep value="$value" where="cat=$cat"
  echo "done $i"
done

###########################################
# (task 21)
###########################################
mkdir summary

for i in `g.mlist type=rast pattern="cla.*[a-z]$"`
do
  > summary/$i.txt 
  r.mapcalc "tmp=if($i>0,1,null())"
  r.mapcalc "MASK=tmp"  
  for j in `g.mlist type=rast pattern="ta.*"`
  do
    eval `r.univar $j -g`
    echo "$j.max : $max" >> summary/$i.txt
    echo "$j.min : $min" >> summary/$i.txt
    echo "$j.sd : $stddev" >> summary/$i.txt
    echo "$j.mean : $mean" >> summary/$i.txt
  done
  g.remove rast=MASK
  area_total=`r.sum $i | awk -F= '{print $2}'`
  r.mapcalc "MASK=protected_areas"
  area_protected=`r.sum $i | awk -F= '{print $2}'` # sum the area that is protected
  percent_protected=`echo "scale=2; $area_protected / $area_total *100" | bc` 
  echo "percent.protected : $percent_protected" >> summary/$i.txt
done

##########################################################################################
#### End of Analysis 
##########################################################################################

###### Export convex hull grid, env variales and make everyhting ready for the web interface

for i in `ls | grep ^[0-9].*.[0-9]$` # do for all species
do
  r.out.arc in=me.c.$i out=$i/$i.asc
  
  echo "lon,lat" >  $i/points.csv
  # exports the coords of the polygon (B) and the centroid (C), awk is used to get extract only the points of the polyon. 
  v.out.ascii in=occ_$i format=point | awk -F'|' '{print $1","$2}' >> $i/points.csv	
  echo $i

done

## rename to make the dir ready for the web interface

mkdir ready

for i in `ls | grep ^[0-9].*.[0-9]$` # do for all species
do
  if [ `cat $i/chull.csv | wc -l` -gt 2 ] 
  then
    mkdir ready/$i
    cp $i/points.csv ready/$i/$i.csv
    cp $i/chull.csv ready/$i/${i}-chull.csv
    cp $i/chull_buffer.csv ready/$i/${i}-chull-buff.csv
    cp -v $i/info.txt ready/$i/${i}-info.txt
  fi
done

for i in `ls | grep ^[0-9].*.[0-9]$` # do for all species
do
  
  mkdir ready_web/$i
  cp $i/*.txt ready_web/$i/
  cp $i/*.csv ready_web/$i/
  cp -v $i/*.asc ready_web/$i/

done

