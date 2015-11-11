#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015
stop("!")

#get the entire NE soil database from IFPRI soil profiles

#directories
wd <- "~/Leeds-work/p4s-csa/hh-analyses"
ifprisoil_dir <- paste(wd,"/IFPRI_soil_profiles",sep="")

infile <- paste(ifprisoil_dir,"/NE.SOL",sep="")

#determine number of lines
nrows <- nrow(read.table(infile, sep=";"))

#first get the profile ID, LAT, LON, DEPTH, and general properties (SALB, SLU1, SLDR, SLRO)
#then read the layer stuff
if (!file.exists(paste(ifprisoil_dir,"/NE.RData",sep=""))) {
  soilprof_df <- data.frame()
  soilprof_ls <- list()
  ifil <- file(infile, open="r")
  all_lines <- readLines(ifil,n=nrows)
  close(ifil); rm(ifil)
  for (i in 1:nrows) {
    #i <- 1
    #read lines, in case of empty line read.table() will return error object
    tline <- all_lines[i]
    tline <- try(read.table(textConnection(tline)),silent=T)
    if (class(tline) != "try-error") {
      if (length(grep("\\*",tline$V1)) > 0) {
        cat("found a profile, reading all the stuff line=",i,"\n")
        sinfo1 <- all_lines[i+2]
        sinfo1 <- read.table(textConnection(sinfo1))
        sinfo2 <- all_lines[i+4]
        sinfo2 <- read.table(textConnection(sinfo2))
        out_row <- data.frame(PROF_ID=gsub("\\*","",paste(tline$V1)), ISO=paste(tline$V2),
                              LAT=sinfo1$V3, LONG=sinfo1$V4, FAMILY=paste(sinfo1$V5), SALB=sinfo2$V2,
                              SLU1=sinfo2$V3, SLDR=sinfo2$V4)
        soilprof_df <- rbind(soilprof_df, out_row)
        
        #now read all the profile details
        sprofdata <- data.frame()
        for (j in (i+6):(i+11)) {
          #j <- i+11
          trow <- all_lines[j]; trow <- read.table(textConnection(trow))
          sprofdata <- rbind(sprofdata,trow)
        }
        names(sprofdata) <- c("SLB","SLMH","SLLL","SDUL","SSAT","SRGF","SSKS","SBDM","SLOC",
                              "SLCL","SLSI","SLCF","SLNI","SLHW","SLHB","SCEC","SADC")
        soilprof_ls[[paste(out_row$PROF_ID[1])]] <- sprofdata
        rm(out_row)
      }
    }
  }
  save(list=c("soilprof_df","soilprof_ls"),file=paste(ifprisoil_dir,"/NE.RData",sep=""))
} else {
  load(file=paste(ifprisoil_dir,"/NE.RData",sep=""))
}

