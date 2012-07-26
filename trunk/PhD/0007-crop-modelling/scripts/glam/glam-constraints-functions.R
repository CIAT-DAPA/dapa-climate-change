#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2012


#function to replace the weather files using a set of prescribed values
GLAM_chg_wth <- function(wth_dir,wth_root,yi,yf,target_var="TMIN",values=NA) {
  cat("transforming",target_var,"...\n")
  for (yr in yi:yf) {
    #yr <- yi
    #open the file
    wth_file <- paste(wth_dir,"/",wth_root,"001001",yr,".wth",sep="")
    wth_data <- read.fortran(wth_file,format=c("I5","F6","3F7"),skip=4)
    names(wth_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
    wth_data$YEAR <- as.numeric(substr(wth_data$DATE,1,2))
    wth_data$JDAY <- as.numeric(substr(wth_data$DATE,3,5))
    
    sdet_2 <- as.character(read.fortran(wth_file,n=1,format=c("A50")))
    sdet_2 <- strsplit(sdet_2," : ",fixed=T)[[1]][2]
    sdet_1 <- read.fortran(wth_file,skip=2,n=1,format=c("A6","2F9","5F6"))
    sdet_1$V1 <- gsub(" ","",sdet_1$V1)
    s_details <- data.frame(NAME=sdet_2,INSI=sdet_1$V1,LAT=sdet_1$V2,LONG=sdet_1$V3,
                            ELEV=sdet_1$V4,TAV=sdet_1$V5,AMP=sdet_1$V6,
                            REFHT=sdet_1$V7,WNDHT=sdet_1$V8)
    
    wth_data[,toupper(target_var)] <- values
    wth_file <- write_wth(wth_data,outfile=wth_file,site.details=s_details)
  }
  return(wth_dir)
}


