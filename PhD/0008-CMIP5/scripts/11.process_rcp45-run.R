#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2012

#Create daily weather data in the format of WTH files for CMIP5 baseline runs
#this script will first check if the GCM data are valid for the years that will be
#simulated. Similarly, this

#load packages
library(raster)

#load functions
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"

#sourcing needed functions
source(paste(src.dir,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
#source(paste(src.dir2,"/scripts/11.process_rcp45-run.R",sep=""))

#input directories
base_dir <- "/nfs/a102/eejarv"
gcm_dir <- paste(base_dir,"/CMIP5/rcp45",sep="")
scratch <- "/scratch/eejarv"

#output directories
out_dir <- paste(base_dir,"/CMIP5/rcp45",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}

#other details
yi <- 2020
yf <- 2049

#load GCM characteristics
gcm_chars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
gcm_list <- unique(gcm_chars$GCM)
gcm_list <- gcm_list[-7] #temporary because ichec_ec_earth does not have rsds

#processsing list of GCMs
for (i in 1:length(gcm_list)) {
#process_cmip5_rcp45_gcm <- function(i) {
  gcm <- gcm_list[i]
  
  gcm_odir <- paste(out_dir,"/",gcm,sep="")
  if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
  
  #reduce characteristics list for this GCM
  this_gcm <- gcm_chars[which(gcm_chars$GCM == paste(gcm)),]
  ens_list <- unique(this_gcm$Ensemble)
  
  #loop ensembles for that GCM
  for (ens in ens_list) {
    #ens <- ens_list[1]
    cat("Processing ensemble",paste(ens),"\n")
    this_ens <- this_gcm[which(this_gcm$Ensemble == ens),]
    
    #create directory of ensemble
    ens_odir <- paste(gcm_odir,"/",ens,sep="")
    if (!file.exists(ens_odir)) {dir.create(ens_odir)}
    
    #list of variables depends on number of nc files (i.e. tas is not always available)
    patn <- gsub("%var%","",this_ens$naming[1])
    ncf <- list.files(ens_odir,pattern=patn)
    
    #get the frequency of srad data
    srn <- unique(this_ens$srad_naming)[1]
    if (srn %in% ncf) {
      sr_freq <- "day"
    } else {
      sr_freq <- "mth"
      sr_patn <- unlist(strsplit(paste(srn),"_",fixed=T))[1:2]
      sr_patn <- paste(sr_patn,collapse="_")
      sr_file <- list.files(ens_odir,pattern=sr_patn)
      ncf <- c(ncf,sr_file)
    }
    
    if (length(list.files(ens_odir,pattern="\\.control")) != 5) {
      if (length(ncf) != 5) {stop("number of files not 5, check!")}
    }
    #produce vector with names of variables
    var_list <- unlist(lapply(ncf,function(x) unlist(strsplit(x,"_",fixed=T))[[1]][1]))
    
    #loop through variables
    for (vn in var_list) {
      #vn <- "pr" #tasmin, tasmax, rsds
      cat("variable:",vn,"\n")
      
      #define frequencies here
      if (vn == "rsds") {vn_freq <- sr_freq} else {vn_freq <- "day"}
      
      #here you need to create a workspace in /scratch
      work_dir <- paste(scratch,"/cmip5",sep="")
      if (!file.exists(work_dir)) {
        dir.create(work_dir) #create if doesnt exist
      } else {
        setwd(work_dir)
        system(paste("rm -f *")) #clean up if exists
      }
      
      #which is the control file
      ctrl_fil <- paste(ens_odir,"/",vn,"_",gcm,"_",ens,".control",sep="")
      if (!file.exists(ctrl_fil)) {
        for (year in yi:yf) {
          #year <- 2020
          cat("year:",year,"\n")
          
          if (vn == "rsds") {
            file_name <- unique(paste(this_ens$srad_naming[which(year >= this_ens$iYear & year <= this_ens$fYear)]))
          } else {
            file_name <- paste(this_ens$naming[which(year >= this_ens$iYear & year <= this_ens$fYear)])
            file_name <- gsub("%var%",vn,file_name)
          }
          
          if (length(file_name) != 0) {
            #leap condition
            wleap <- paste(this_ens$has_leap[which(year >= this_ens$iYear & year <= this_ens$fYear)][1])
            
            #create output year dir
            year_odir <- paste(ens_odir,"/",vn,"_",year,sep="")
            if (!file.exists(year_odir)) {dir.create(year_odir)}
            
            #if there are two files i need to read both, extract the first 11 months from the first 
            #one and the last month from the second file. As this only happens with UKMO models
            #i did not care about making it generic (i.e. it works only for a 360 day calendar)
            #although the above might not apply to the process done in this script
            if (length(file_name) > 1) {
              #check if year was already done
              nfil <- length(list.files(year_odir,pattern="\\.nc"))
              nd <- leap(year)
              if (wleap=="all30") {
                nd <- 360
              } else if (wleap == "no") {
                nd <- 365
              }
              
              if (vn_freq == "mth") {nd <- 12}
              
              if (nfil != nd) {
                #first part of the year
                nc_file <- paste(gcm_dir,"/",gcm,"/",ens,"/",file_name,sep="")[1]
                
                #copy to scratch if it does not exist already
                if (!file.exists(paste(work_dir,"/",file_name[1],sep=""))) {
                  setwd(work_dir)
                  system(paste("rm -f *"))
                  setwd(base_dir)
                  x <- file.copy(nc_file,work_dir)
                }
                
                if (vn_freq == "mth") {
                  setwd(work_dir)
                  temp_file <- paste(gcm,"_",ens,"_",year,".nc",sep="")
                  system(paste("cdo selyear,",year," ",file_name[1]," ",temp_file,sep=""))
                  
                  #now use splitmon to split the months, and remove tFile
                  mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
                  system(paste("cdo splitmon ",temp_file," ",mon_px,sep=""))
                  x <- file.remove(temp_file)
                } else {
                  #select the year I'm looking for using the CDO command "selyear"
                  setwd(work_dir)
                  temp_file <- paste(gcm,"_",ens,"_",year,".nc",sep="")
                  system(paste("cdo selyear,",year," ",file_name[1]," ",temp_file,sep=""))
                  
                  #now use splitmon to split the months, and remove tFile
                  mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
                  system(paste("cdo splitmon ",temp_file," ",mon_px,sep=""))
                  x <- file.remove(temp_file)
                }
                
                #second part of the year
                nc_file <- paste(gcm_dir,"/",gcm,"/",ens,"/",file_name,sep="")[2]
                
                #copy to scratch if it does not exist already
                if (!file.exists(paste(work_dir,"/",file_name[2],sep=""))) {
                  x <- file.copy(nc_file,work_dir)
                }
                
                if (vn_freq == "mth") {
                  setwd(work_dir)
                  temp_file <- paste(gcm,"_",ens,"_",year,".nc",sep="")
                  system(paste("cdo selyear,",year," ",file_name[2]," ",temp_file,sep=""))
                  
                  #now use splitmon to split the months, and remove tFile
                  mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
                  system(paste("cdo splitmon ",temp_file," ",mon_px,sep=""))
                  x <- file.remove(temp_file)
                } else {
                  #select the year I'm looking for using the CDO command "selyear"
                  setwd(work_dir)
                  temp_file <- paste(gcm,"_",ens,"_",year,".nc",sep="")
                  system(paste("cdo selyear,",year," ",file_name[2]," ",temp_file,sep=""))
                  
                  #now use splitmon to split the months, and remove tFile
                  mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
                  system(paste("cdo splitmon ",temp_file," ",mon_px,sep=""))
                  x <- file.remove(temp_file)
                  
                  #now loop the monthly files
                  mth_files <- list.files(".",pattern="_mth_")
                  for (mf in mth_files) {
                    #split the monthly file into daily files and remove monthly file
                    day_px <- paste(gsub("\\.nc","",mf),"_day_",sep="")
                    system(paste("cdo splitday ",mf," ",day_px,sep=""))
                    x <- file.remove(mf)
                  }
                }
                #move the daily files to the output folder
                system(paste("mv -f *_mth_* ",year_odir,sep=""))
              } else {
                cat("this year was already done!\n")
              }
              
            } else {
              #normal case: there was only one file with the year data that i need, 
              ###           so just do the process for that one this is when
              ###           the year is not split into two different files.
              
              #check if year was already done
              nfil <- length(list.files(year_odir,pattern="\\.nc"))
              nd <- leap(year)
              if (wleap=="all30") {
                nd <- 360
              } else if (wleap == "no") {
                nd <- 365
              }
              
              if (vn_freq == "mth") {nd <- 12}
              
              if (nfil != nd) {
                #name of netCDF file
                nc_file <- paste(gcm_dir,"/",gcm,"/",ens,"/",file_name,sep="")
                
                #copy to scratch if it does not exist already
                if (!file.exists(paste(work_dir,"/",file_name,sep=""))) {
                  setwd(work_dir)
                  system(paste("rm -f *"))
                  setwd(base_dir)
                  x <- file.copy(nc_file,work_dir)
                }
                
                #if the variable data is monthly
                if (vn_freq == "mth") {
                  setwd(work_dir)
                  temp_file <- paste(gcm,"_",ens,"_",year,".nc",sep="")
                  system(paste("cdo selyear,",year," ",file_name," ",temp_file,sep=""))
                  
                  #now use splitmon to split the months, and remove tFile
                  mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
                  system(paste("cdo splitmon ",temp_file," ",mon_px,sep=""))
                  x <- file.remove(temp_file)
                } else {
                  #select the year I'm looking for using the CDO command "selyear"
                  setwd(work_dir)
                  temp_file <- paste(gcm,"_",ens,"_",year,".nc",sep="")
                  system(paste("cdo selyear,",year," ",file_name," ",temp_file,sep=""))
                  
                  #now use splitmon to split the months, and remove tFile
                  mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
                  system(paste("cdo splitmon ",temp_file," ",mon_px,sep=""))
                  x <- file.remove(temp_file)
                  
                  #now loop the monthly files
                  mth_files <- list.files(".",pattern="_mth_")
                  for (mf in mth_files) {
                    #split the monthly file into daily files and remove monthly file
                    day_px <- paste(gsub("\\.nc","",mf),"_day_",sep="")
                    system(paste("cdo splitday ",mf," ",day_px,sep=""))
                    x <- file.remove(mf)
                  }
                }
                #move the daily files to the output folder
                system(paste("mv -f *_mth_* ",year_odir,sep=""))
              } else {
                cat("this year was already done!\n")
              }
            }
          }
        }
        #write control file
        cfo <- file(ctrl_fil,"w")
        cat("processed on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
            paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cfo)
        close(cfo)
      } else {
        cat("this job was already done!\n")
      }
    }
    
    #removing ensemble original big files only if the number of control files equals
    #the number of original nc files, else stop
    setwd(ens_odir)
    nnc <- list.files(".",pattern=patn)
    #nnc <- nnc[which(!nnc %in% paste(srn))]
    
    if (!srn %in% nnc) {
      sr_file <- list.files(".",pattern=sr_patn)
      nnc <- c(nnc,sr_file)
    }
    cnc <- length(nnc)
    cct <- length(list.files(".",pattern="\\.control"))
    
    ######
    #check this last part, and modify for processing of solar radiation
    if (cnc != 0) {
      if (cnc == cct) {
        #system("rm -f *.nc")
        anc <- list.files(".",pattern="\\.nc")
        #anc <- anc[which(!anc %in% paste(srn))]
        x <- sapply(anc,FUN= function(x) {s <- file.remove(x)})
      } else {
        stop("something weird happened, need to check before removing original files")
      }
    }
  }
}



