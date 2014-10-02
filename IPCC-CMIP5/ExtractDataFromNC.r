baseDir="G:/jetarapues/temp/-83.65-9.89"
yi <- '2040'
yf <- '2069'
otp <- get_loc_data(baseDir,yi,yf)
  
#extract data from netcdf files using cdo
get_loc_data <- function(baseDir,yi,yf) {

    nclist <- list.files(baseDir,full.names=T, recursive = FALSE,pattern='.tab')
    
      for (ncDir in nclist) {
        loc_hdata <- data.frame()
        
        var <- basename(ncDir)
        rcp <- sapply(strsplit(var, '[_]'), "[[", 1)
        model <- sapply(strsplit(var, '[_]'), "[[", 3)        
        years <- sapply(strsplit(var, '[_]'), "[[", 4)        
        varName <- sapply(strsplit(var, '[_]'), "[[", 2)        
        staYear <- sapply(strsplit(years, '[-]'), "[[", 1)
        endYear <- gsub(".tab","",substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 8))
        
        outfile = paste(baseDir,"/",rcp,"/",model,"/",yi,'-',yf,sep="")
        csvFile = paste(outfile,"/",varName,'.csv',sep='')
        
        if (!file.exists(csvFile)){
        
          if(staYear<=yi & endYear>=yf){        
          
          if (!file.exists(outfile)) {dir.create(outfile, recursive=T)}
          
          cat(paste("\t Extracting: ", rcp, " ", model, " ",varName,sep="")," \n")
        
          
          #organise table
          loc_data <- read.table(ncDir,header=F,sep="")
          if(nrow(loc_data) > 1){
            
            names(loc_data) <- c("YEAR","MONTH","DAY","LON","LAT","VALUE")
            #loc_data$LON <- loc_data$LON - 360
            loc_data <- subset(loc_data,loc_data$YEAR>=yi & loc_data$YEAR<=yf)         
            
            if(varName == "tas" || varName == "tasmax" || varName == "tasmin"){
              loc_data = aggregate(loc_data[,6],list(loc_data$MONTH),mean)
              names(loc_data) <- c("MONTH","VALUE")
              loc_data$VALUE <- loc_data$VALUE - 273.15
            }
            if(varName == "hur"){
              loc_data = aggregate(loc_data[,6],list(loc_data$MONTH),mean)
              names(loc_data) <- c("MONTH","VALUE")
            }         
            if(varName == "rsds"){
              loc_data = aggregate(loc_data[,6],list(loc_data$MONTH),mean)
              names(loc_data) <- c("MONTH","VALUE")
            }   
            if(varName == "sfcWind"){
              loc_data = aggregate(loc_data[,6],list(loc_data$MONTH),mean)
              names(loc_data) <- c("MONTH","VALUE")
            }          
            if (varName=="pr"){
              sumxyear=aggregate(loc_data[,6],list(loc_data$MONTH,loc_data$YEAR),sum)
              names(sumxyear) <- c("MONTH","YEAR","VALUE")
              loc_data=aggregate(sumxyear[,3],list(sumxyear$MONTH),mean)
              names(loc_data) <- c("MONTH","VALUE")
              loc_data$VALUE <- loc_data$VALUE * 86400
            }
  
            
            loc_hdata <- rbind(loc_hdata,loc_data)
            
            #x=aggregate(precip[,4],list(precip$month,precip$year),sum)
            #aggregate(x[,3],list(x$Group.1),mean)
            
            #write.csv(loc_hdata, file = paste(outfile,"/",gsub(".tab","",var),".csv",sep=''), row.names=F)
            #loc_data_cl <- data.frame(VALUE=tapply(loc_hdata$VALUE,loc_hdata$MONTH,FUN=mean))
            #loc_data_cl <- cbind(MONTH=row.names(loc_data_cl),loc_data_cl)    
            write.csv(loc_hdata, file = csvFile, row.names=F)          
          }
        }
          
        }
      }
 
  cat("Done!")
}