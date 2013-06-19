### make a number of bioclimatic layers for modelling groundnut using SDMs
### this is to be done for the 30s fitting data

###
#various functions that are needed

#apply a function by blocks to enhance memory use
apply_by_blocks <- function(clm_stk,sow_date,har_date,this_fun) {
  #determine maximum processing load
  bs <- blockSize(clm_stk, n=20, minblocks=2)
  cat("\nprocessing in: ", bs$n, " chunks \n", sep="")
  
  #output raster
  outraster <- raster(clm_stk)
  
  for (b in 1:bs$n) {
    #b <- 1
    cat("\t",round(b/bs$n*100,2),"%",sep="")
    
    iniCell <- 1+(bs$row[b]-1)*ncol(outraster)
    finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(outraster)
    allCells <- iniCell:finCell
    validCells <- allCells[which(!is.na(clm_stk[[1]][allCells]))]
    validXY <- xyFromCell(clm_stk,validCells)
    
    if (length(validCells) > 0) {
      rowVals <- extract(clm_stk,validCells)
      rowVals <- cbind(rowVals,sow=extract(sow_date,validXY))
      rowVals <- cbind(rowVals,har=extract(har_date,validXY))
      rasVals <- apply(rowVals, 1, this_fun)
    } else {
      rasVals <- NA
    }
    outraster[validCells] <- rasVals
  }
  cat("\n")
  return(outraster)
}


#####
#function to calculate total seasonal rainfall
calc_totrain <- function(x) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  if (har > 365) {har <- har - 365}
  
  #growing season start and end
  Gi <- ceiling(sow/30); if (Gi > 12) {Gi <- 12}
  Gf <- ceiling(har/30); if (Gf>12) {Gf <- Gf-12}
  if (Gf < Gi) {gs <- c(Gf:12,1:Gi)} else {gs <- c(Gi:Gf)}
  
  #extract monthly climate
  monclim <- monclim[gs]
  
  #calculate and return
  seasrain <- sum(monclim)
  return(seasrain)
}
#####


