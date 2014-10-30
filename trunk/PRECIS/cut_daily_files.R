################################
## Extract daily values PRECIS##
################################

# Required libraries
require(raster)
require(ncdf)
require(rgdal)
require(sp)
require(chron)

# Parameters
iDir <- "U:/rcm/precis/post_processed"
varLs <- c("Prec", "Tmax1_5", "Tmean1_5", "Tmin1_5")
oDir <- "G:/cenavarro/Request/request_mcarbajal"
reg <- extent(-72, -66, -19, -13)

cutDaily <- function(scen="baseline", gcm="hadam3p_3", iYr="1960", eYr="1990"){

  #scenLs <- c("baseline", "sres_a1b", "sres_a2")
  # for (scen in scenLs){
  
  # gcmLs <- list.dirs(paste(iDir, "/", scen, sep=""), recursive = FALSE, full.names=FALSE)
  
  # for (gcm in gcmLs){
  
  gcmDir <- paste(iDir, "/", scen, "/", gcm, "/daily_grids", sep="")
  
  if (file.exists(gcmDir)) {
    
    extDir <- paste(oDir, "/", scen, "/", gcm, sep="")
    
    for (var in varLs){
      
      mNc <- paste(oDir, "/", scen, "/", gcm, "/", var, "_", iYr, "-", eYr, ".nc", sep="")
      
      if (!file.exists(mNc)) {
        
        for (yr in iYr:eYr){
          
          # Remove temporal dir
          if (file.exists(paste(extDir, "/", yr, sep=""))) {
            unlink(paste(extDir, "/", yr, sep=""), recursive = TRUE)
          }
          
          oNc <- paste(oDir, "/", scen, "/", gcm, "/", tolower(var), "_", yr, ".nc", sep="")
          
          if (!file.exists(oNc)) {
            
            cat(" Extracting : ", scen, gcm, yr, var, " \n")
            
            # Extracting grid files 
            system(paste("7z x -mmt=4 ", gcmDir, "/", yr, ".zip ", yr, "/", var, "/ -o",  extDir, sep=""))
            
            #Remove innecesary files
            auxLs <- list.files(paste(extDir, "/", yr, "/", var, sep=""), full.names=T, pattern=".aux")
            do.call(unlink,list(auxLs))
                        
            # Listing grids
            listGr <- list.files(paste(extDir, "/", yr, "/", var, sep=""), full.names=T, pattern=tolower(var))
            
            cat(" Stacking and cut : ", scen, gcm, yr, var, " \n")
            
            # Stacking grids
            grStk <- stack(paste(listGr))
            
            # Cut Stack for region
            grCut <- crop(grStk, reg)
            grCutNc <- writeRaster(grCut, oNc, format="CDF", overwrite=T)
            
            # Set time to nc file
            if (gcm == "echam5" && leap.year(as.integer(yr)) == T){
              calendar <- "366_day"
            } else if (gcm == "echam5" && leap.year(as.integer(yr)) == F){
              calendar <- "365_day"
            } else{
              calendar <- "360_day"
            }
            
            system(paste("cdo setcalendar,", calendar, " ", oNc, " ", oDir, "/", scen, "/", gcm, "/", tolower(var), "_", yr, "_tmp.nc", sep=""))
            system(paste("cdo settaxis,", yr, "-01-01,12:00:00,1day ", oDir, "/", scen, "/", gcm, "/", tolower(var), "_", yr, "_tmp.nc ", " ", oNc, sep=""))
            
            unlink(paste(oDir, "/", scen, "/", gcm, "/", tolower(var), "_", yr, "_tmp.nc", sep=""))

            # Remove temporal extraction dir
            cat(" Removing extracted files : ", scen, gcm, yr, tolower(var), " \n")
            if (file.exists(paste(extDir, "/", yr, sep=""))) {unlink(paste(extDir, "/", yr, sep=""), recursive = TRUE)}
            
          }
          
          
        }
        
        cat(" Merging all years: ", scen, gcm, var, " \n")
        
        # Listing nc yearly files
        listNc <- list.files(paste(oDir, "/", scen, "/", gcm, sep=""), full.names=T, pattern=tolower(var))
        listNcMod <- paste(listNc, collapse = ' ')
        
        # Merging nc yearly files
        system(paste("cdo mergetime ", listNcMod, " ", mNc, sep=""))
        
        do.call(unlink,list(listNc))
                
        cat(" Done!: ", scen, gcm, var, " \n")
        
      }
      
    }
    
  }
  
  # }
  
  # }

}
