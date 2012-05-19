#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#May 2012


#########################################################
#Function to calculate soil hydrological characteristics
hydroChars <- function(sand,clay) {
  pwp <- 0; fc <- 0; sat <- 0
  acoef <- 0; bcoef <- 0
  
  acoef <- exp(-4.396 - 0.0715 * clay -  4.88e-4 * sand^2 - 4.285e-5 * sand^2 * clay)
  bcoef <- - 3.140 - 0.00222 * clay^2 - 3.484e-5 * sand^2 * clay
  sat <- 0.332 - 7.251e-4 * sand + 0.1276 * log10(clay);
  if ((acoef != 0.0) & (bcoef != 0.0)) {
    fc <- ((0.3333/ acoef)^(1.0 / bcoef))
    pwp <- ((15.0 / acoef)^(1.0 / bcoef))
  }
  return(data.frame(RLL=pwp,DUL=fc,SAT=sat))
}





