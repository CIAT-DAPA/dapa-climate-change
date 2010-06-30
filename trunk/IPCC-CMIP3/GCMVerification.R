# Julian Ramirez, dawnpatrolmustaine@gmail.com
# gcmdata <- NcToAscii(scenario="20C3M")

#Script to create images from GCM data, run this after RcodeIPCC.R, and run GCMAveraging.aml after this script.

CreateGCMImages <- function(scenario='20C3M') {
  
  if (!toupper(scenario) %in% c("20C3M","SRES_A1B","SRES_A2","SRES_B1")) {
    stop('Scenario', scenario, ' is not supported')
  }
  
  imagedir <- paste("F://climate_change//_scripts//", scenario, sep="")
  if (!file.exists(imagedir)) {
    dir.create(imagedir)
  }
  
  basedir <- "F://climate_change//IPCC_CMIP3"
  scendir <- paste(basedir, "//", scenario, sep="")
  
  # Listing and looping the models
  
  modlist <- list.files(scendir, pattern="*_*")
  m <- 1
  
  for (modname in modlist) {
    
    outmoddir <- paste(imagedir, "//", modname, sep="")
    
    if (!file.exists(outmoddir)) {
      dir.create(outmoddir)
    }
    
    cat(paste("Processing model ", modname, "\n"))
    
    folder <- "multiyr_avgs"
    
    workdir <- paste(scendir, "//", modname, "//", folder, sep="")
    
    # Listing the periods to process
    
    perlist <- list.files(workdir, pattern="*_*")
    i <- 1
  
    for (period in perlist) {
      
      datadir <- paste(workdir, "//", period, sep="")
      filelist <- list.files(datadir, pattern="*.asc")
      
      toproc <- which(nchar(filelist) <= 12)
      filelist <- filelist[toproc]
      
      cat(paste("Processing period", period),"\n")
      setwd(datadir)
      
      outdir <- paste(outmoddir, "//", period, sep="")
      if (!file.exists(outdir)) {
        dir.create(outdir)
      }
      
      for (filename in filelist) {
        
        cat(paste("Processing file", filename),"\n")
  	
        # Creating an image per timeslice and model
        
        paste(unlist(strsplit(filename, ".", fixed=T))[1], ".asc", sep="")
        
        rs <- raster(filename)
        jpeg(paste(outdir, "//", unlist(strsplit(filename, ".", fixed=T))[1], ".jpg", sep=""))
        plot(rs)
        dev.off()
      }
      
      i <- i + 1
    }
    m <- m + 1
  }
  setwd("F://climate_change//_scripts")
  return("Done!")
}