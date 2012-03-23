#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL

#to linearise monthly data
linearise <- function(input_vals) {
  day_mid <- c(-15,16,45,75,106,136,167,197,228,259,289,320,350,381)
  daily_vals <- rep(NA,times=(day_mid[14]-day_mid[1]+1))
  for (mth in 1:13) {
    deltawth <- (input_vals[mth+1]-input_vals[mth]) / (day_mid[mth+1]-day_mid[mth])
    
    for (d in day_mid[mth]:day_mid[mth+1]) {
      daily_vals[d+16] <- input_vals[mth] + deltawth*(d-day_mid[mth])
    }
  }
  return(daily_vals)
}


#function to create mask from yield adn met grids
maskCreate <- function(met,yld) {
  #create mask with pixels that are not NA in both
  met[which(!is.na(met[]))] <- 1
  yld[which(!is.na(yld[]))] <- 1
  
  #get coordinates and values of rasters
  cellMx <- data.frame(CELL=1:ncell(yld))
  cellMx$X <- xFromCell(yld,cellMx$CELL); cellMx$Y <- yFromCell(yld,cellMx$CELL)
  cellMx$VALS.YLD <- extract(yld,cbind(X=cellMx$X,Y=cellMx$Y))
  cellMx$VALS.MET <- extract(met,cbind(X=cellMx$X,Y=cellMx$Y))
  cellMx$VALS.OUT <- 1
  cellMx$VALS.OUT[which(is.na(cellMx$VALS.YLD) | is.na(cellMx$VALS.MET))] <- NA
  
  #assign mask data
  msk <- raster(yld)
  msk[cellMx$CELL] <- cellMx$VALS.OUT
  return(msk)
}


#find out what is the continous date that i refer to in the nc file
findNCPos <- function(thisYr,thisDay) {
  tropMet_iyr <- 1960
  counter <- 0
  
  for (yr in tropMet_iyr:thisYr) {
    if (yr<thisYr) {
      nd <- leap(yr)
      for (j in 1:nd) {
        counter <- counter+1
      }
    } else {
      for (j in 1:thisDay) {
        counter <- counter+1
      }
    }
  }
  return(counter)
}


extractDaily <- function(ncFile,x,y,year,nd) {
  cat("extracting daily data \n")
  #loop and extract days
  for (d in 1:nd) {
    #cat(d," ")
    ncPos <- findNCPos(year,d)
    rs <- raster(ncFile,band=ncPos)
    out_row <- data.frame(DAY=d,RAIN=extract(rs,cbind(X=x,Y=y)))
    if (d==1) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
  }
  return(out_all)
}
