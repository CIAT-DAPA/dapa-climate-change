####################################################################################################
#         agmip_simple_mandv.R
#
#  This script adjusts a climate time series for climate scenarios.  This is designed for AgMIP
#   mean and variability scenarios using the 'stretched distribution' approach that is related to
#   quantile mapping.
#
#  This version aims for correct statistical parameters for distribution matching.
#
#  The .AgMIP files produced by this script change the eighth digit (or the 8 digit code) to F 
#   where F = Mean and daily variability change for Tmax, Tmin, and P.
#
#  THIS WAS FORMERLY acr_agmip100.R                   --  June 6, 2013
#    Updated to be used with the Guide for Running AgMIP Climate Scenario Generation Tools 
#    acr_agmip100.R was based on acr_giss250.m and acr_giss513.m
#    Updated for Version 2.0 of the Guide             --  July 25, 2013 by Nicholas Hudson
#    Updated to ensure Tmax is > Tmin                 --  October 24, 2013 by Nicholas Hudson
#    Updated to mean adjust P if can't fit on 3rd try --  March 3, 2014 by Nicholas Hudson
#
#
#     Author: Alex Ruane
#       						alexander.c.ruane@nasa.gov
#    	Created:	09/07/2012
# 		Translated to R by Nicholas Hudson: June 7, 2013
#   
#
#     Here's my minimal key for a scenario directory (e.g. FL051NXA)
#    
#     Seventh Digit is RCM:
#       X = no RCM used
#       0 = imposed values (sensitivity tests)
#       1 = crcm
#       2 = ecpc
#       3 = hrm3
#       4 = mm5i
#       5 = RegCM3
#       6 = WRF
#       7 = RegCM4
#       A = GFDL/ecpc
#       B = GFDL/hrm3
#       C = GFDL/RegCM3
#       D = CGCM3/crcm
#       E = CGCM3/RegCM3
#       F = CGCM3/WRF
#       G = HADCM3/ecpc
#       H = HADCM3/hrm3
#       I = HADCM3/mm5i
#       J = CCSM/crcm
#       K = CCSM/mm5i
#       L = CCSM/WRF
#
#     Eighth Digit is Type of Scenario:
#       X = Observations (no scenario)
#       A = Mean Change from GCM
#       B = Mean Change from RCM
#       C = Mean Change from GCM modified by RCM
#       D = Mean Temperature Changes Only
#       E = Mean Precipitation Changes Only
#       F = Mean and daily variability change for Tmax, Tmin, and P
#       G = P, Tmax and Tmin daily variability change only
#       H = Tmax and Tmin daily variability and mean change
#       I = P daily variability and mean change
#       J = CO2 only
#       K = P daily variability change only
#       L = Direct GCM
#       M = Direct RCM
#
#
#                                           -- Notes -- 
#  stdfactor is the fraction of the baseline standard deviation that is imposed in the future 
#    (12-element vector representing each month with 12 columns to represent DSSAT files Tmax(6),
#    Tmin(7)). 
#   If stdfactor is negative, then it is the negative of the new standard deviation to be imposed
#    (rather than a factor).
#   If stdfactor(col) == 1, no corrections are made.  
#
#  gamfactor provides the alpha changes for precipitation as a 12-element vector
#    Beta is determined according to the mean change (alpha*beta=mean)
#   If gamfactor is negative, then it is the negative of the new alpha to be imposed (rather than a
#    factor).
#     
#  wetfactor(mm) is the fraction of baseline wet days in the future  
#
#  meandelt(mm,col) are the mean change deltas to be imposed (fractional factor for precip)
#
####################################################################################################

agmip_simple_mandv  <- function(base, outfile, futloc, headerplus, baseinfo, stdfactor, gamfactor, wetfactor, meandelt) {
  
  ##  Begin debug
  #   rootDir     <- '*** your directory here ***\\R\\'         ##  <- Enter location here <-
  #   basefile    <- 'USAM0XXX'
  #   base        <- read.table(paste(rootDir, 'data\\Climate\\Historical\\', basefile, '.AgMIP', sep=''), skip=5, sep='')
  #   baseinfo    <- read.table(paste(rootDir, 'data\\Climate\\Historical\\', basefile, '.AgMIP', sep=''), skip=3, nrows=1)
  #   scencode    <- c('C','E','G','I','K','M')
  #   scen        <- 1
  #   gcmlist     <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T')
  #   thisgcm     <- 1
  #   outfile       <- paste(substr(basefile,1,4), scencode[scen], gcmlist[thisgcm], substr(basefile,7,7), 'F', sep ='')
  #   futloc      <- paste(rootDir, 'data\\Climate\\Simplescenario\\', sep='')
  #   
  #   ##  Test scenario #1
  #   headerplus      <- 'Ames, Iowa, USA with: mean(Tmax)*0; mean(Tmin)*0; mean(P)*0.5; std(Tmax)*1.5; std(Tmin)*1.5; #wet*0.75; alpha*0.75'                     ##  Additional header information
  #   meandelt        <- matrix(1,12,12)
  #   meandelt[,6]    <- 0                                      ##  Mean change on Tmax
  #   meandelt[,7]    <- 0                                      ##  Mean change on Tmin
  #   meandelt[,8]    <- 0.5                                    ##  Mean change on P
  #   stdfactor       <- matrix(1,12,12)
  #   stdfactor[,6]   <- 1.5                                    ##  Deviation imposed on Tmax
  #   stdfactor[,7]   <- 1.5                                    ##  Deviation imposed on Tmin
  #   wetfactor       <- rep(0.75,12)                           ##  Fraction of baseline wet days
  #   gamfactor       <- rep(0.75,12)                           ##  Alpha change for precipitation
  #   
  # #   ##  Test scenario #2
  # #   headerplus      <- 'Ames, Iowa, USA with: mean(Tmax)*2; mean(Tmin)*2; mean(P)*1; std(Tmax)*0.75; std(Tmin)*1.25; #wet*1; alpha*1'                           ##  Additional header information
  # #   meandelt        <- matrix(1,12,12)
  # #   meandelt[,6]    <- 2                                      ##  Mean change on Tmax
  # #   meandelt[,7]    <- 2                                      ##  Mean change on Tmin
  # #   meandelt[,8]    <- 1                                      ##  Mean change on P
  # #   stdfactor       <- matrix(1,12,12)
  # #   stdfactor[,6]   <- 0.75                                   ##  Deviation imposed on Tmax
  # #   stdfactor[,7]   <- 1.25                                   ##  Deviation imposed on Tmin
  # #   wetfactor       <- rep(1,12)                              ##  Fraction of baseline wet days
  # #   gamfactor       <- rep(1,12)                              ##  Alpha change for precipitation
  #     
  #   ## Load required packages
  #   library <- c('R.matlab', 'R.utils', 'MASS')
  #   lapply(library, require, character.only = T)
  #   rm(library)
  #   
  ##  End debug
  
  ##  Load data and set variables
  ddate       <- base[,1] %% 1000
  loop.test   <- rep(0,5)
  mmtick      <- c(0,31,28,31,30,31,30,31,31,30,31,30,31)
  mmtickleap  <- c(0,31,29,31,30,31,30,31,31,30,31,30,31)
  mmcum       <- cumsum(mmtick)
  mmcumleap   <- cumsum(mmtickleap)
  
  ##  Initialize month vectors;
  Jan <- rep(1,0)
  Feb <- rep(1,0)
  Mar <- rep(1,0)
  Apr <- rep(1,0)
  May <- rep(1,0)
  Jun <- rep(1,0)
  Jul <- rep(1,0)
  Aug <- rep(1,0)
  Sep <- rep(1,0)
  Oct <- rep(1,0)
  Nov <- rep(1,0)
  Dec <- rep(1,0)
  
  ##  Classify 30 years of daily record into monthly catgories
  for(dd in 1:nrow(base)){
    if (base[dd,3]==1)    Jan <- c(Jan,dd)
    if (base[dd,3]==2)    Feb <- c(Feb,dd)
    if (base[dd,3]==3)    Mar <- c(Mar,dd)
    if (base[dd,3]==4)    Apr <- c(Apr,dd)
    if (base[dd,3]==5)    May <- c(May,dd)
    if (base[dd,3]==6)    Jun <- c(Jun,dd)
    if (base[dd,3]==7)    Jul <- c(Jul,dd)
    if (base[dd,3]==8)    Aug <- c(Aug,dd)
    if (base[dd,3]==9)    Sep <- c(Sep,dd)
    if (base[dd,3]==10)   Oct <- c(Oct,dd)
    if (base[dd,3]==11)   Nov <- c(Nov,dd)
    if (base[dd,3]==12)   Dec <- c(Dec,dd)
  }
  
  ##  Set wide range for expected temperature coverage
  tcov <- seq(-80,80,0.1)
  
  ##  Standard deviation and mean increase = newscen
  newscen <- base
  
  ##  Create NaN matrices for Tmax, Tmin, and Precip for loops
  newmu             <- matrix(NaN,12,12)
  newstd            <- matrix(NaN,12,12)
  newalpha          <- matrix(NaN,12)
  newbeta           <- matrix(NaN,12)
  nnewwetdays       <- matrix(NaN,12)
  intermscen        <- matrix(0,nrow(base))
  meanfitstandard   <- matrix(NaN,12,12)
  stdfitstandard    <- matrix(NaN,12,12)
  alphafitstandard  <- matrix(NaN,12)
  betafitstandard   <- matrix(NaN,12)
  
  ##  Turn on diagnostics for Tmax, Tmin and Precipitation Loops
  ##   0 = No diagnostics, 1 = January, 2 = February, 3 = ..., 13 = all months
  rundiag <- 0 
  
  ###--------------------------------------------------------------------------------------------###
  ####################################### Tmax and Tmin Loop #######################################
  ###--------------------------------------------------------------------------------------------###
  while (loop.test[1]<3 && (loop.test[2] != 1 || loop.test[3] != 1)) {
    #     cat('\n',loop.test,'\n')
    loop.test[1]    <- loop.test[1] + 1
    loop.test[2:3]  <- 0
    loop.test[5]    <- 0
    
    ########################################### Tmax Loop ##########################################
    col     <- 6
    #     rundiag <- 0  ##  Turn on diagnostic, 0 = No diagnostic, 1 = January, 2 = ..., 13 = all months
    
    for(mm in 1:12){
      #       cat('Month = ', month.name[mm], '\n')
      monthfailed <- 0
      thismonth   <- eval(parse(text = month.abb[mm])) ##  Vector of month days over 30 years
      
      ##  Increase standard deviation for Tmax of this month by calculating the normal statistics
      basestd         <- sd(base[thismonth,col])
      basemu          <- mean(base[thismonth,col])
      
      ##  Calculate new mean with meandelt and new standard deviation with stdfactor
      newmu[mm,col]   <- basemu + meandelt[mm,col]
      newstd[mm,col]  <- basestd * stdfactor[mm,col]
      if (stdfactor[mm,col]<0)        newstd[mm,col]  <- (-stdfactor[mm,col])
      
      ##  Rank baseline monthly series and find significant limits of baseline and new cdf
      ranklist        <- sort(base[thismonth,col], decreasing = TRUE)
      
      basecdf         <- pnorm(tcov,basemu,basestd)
      newcdf          <- pnorm(tcov,newmu[mm,col],newstd[mm,col])
      basecdflims     <- c(tcov[min(which(basecdf > 1e-6))],tcov[max(which(basecdf < (1-1e-4)))])
      newcdflims      <- c(tcov[min(which(newcdf  > 1e-6))],tcov[max(which(newcdf  < (1-1e-4)))])
      cdflims         <- c(min(c(basecdflims[1],newcdflims[1])), 
                           max(c(basecdflims[2],newcdflims[2])))
      
      ##  R hist bins handles outliers differently than Matlab, widen cdflims by +/-20 for cdfvect
      #       cdfvect         <- seq(cdflims[1],cdflims[2],0.1)
      cdfvect2        <- seq(cdflims[1]-20,cdflims[2]+20,0.1)
      cdfvect3        <- seq(min(cdfvect2)-0.05,max(cdfvect2)+0.05,0.1)
      
      ##  Now regenerate only significant portion
      basecdf <- pnorm(cdfvect2,basemu,basestd)
      newcdf  <- pnorm(cdfvect2,newmu[mm,col],newstd[mm,col])
      
      ##  Check for goodness of fit in baseline period as standard
      truebasecdf <- 1/length(ranklist)*cumsum(hist(base[thismonth,col], cdfvect3,
                                                    plot = FALSE)$counts)
      
      ##  Check for initial goodness of fit in future period (newscen begins = base)
      truenewcdf  <- 1/length(ranklist)*cumsum(hist(newscen[thismonth,col], cdfvect3,
                                                    plot = FALSE)$counts)
      
      ##  Continue until future distribution, in comparison to theoretical future distribution,
      ##    looks like baseline distribution.
      
      ##  Initially move directly with no spread (1:1)
      spread  <-  0
      meanerr <- abs(mean(newscen[thismonth,col])-newmu[mm,col])
      stderr  <- abs(sd(newscen[thismonth,col])/newstd[mm,col]-1)
      
      ##  Good fit if mean is within 0.1K and standard deviation is within 0.2% of desired
      meanfitstandard[mm,col] <- 0.1
      stdfitstandard[mm,col]  <- 0.002
      
      while (((meanerr > meanfitstandard[mm,col]) || (stderr > stdfitstandard[mm,col])) && (monthfailed == 0)) {
        #         cat('meanerr <- ', as.character(format(meanerr, width=6, digits=3, trim=TRUE)),
        #             '\t\t\t\tstderr <- ', as.character(format(stderr*100, width=6, digits=3, trim=TRUE)),
        #             '\t\t\t\tmeanfs <- ', as.character(format(meanfitstandard[mm,col], width=5)),
        #             '\t\t\t\tstdfs <- ' , as.character(format(stdfitstandard[mm,col], width=5)),
        #             '\t\t\t\tspread <- ', as.character(format(spread, width=3)), '\n')
        
        ##  Find new value in ranked list
        ii <- 1
        while (ii < length(ranklist) + 1) {
          thisval <- ranklist[ii]
          
          ##  Find shift in value corresponding to this quantile in both cdfs
          baseprctile <- basecdf[which.min(abs(cdfvect2-thisval))]
          newvalue    <- cdfvect2[which.min(abs(newcdf-baseprctile))]
          delt        <- newvalue-thisval
          
          ##  Find all occurrences with same value
          while((ii < length(ranklist)+1) && (ranklist[ii] == thisval))     ii  <- ii + 1
          
          thischunk <- which(base[thismonth,col] == thisval)
          
          ##  Now adjust each of these members randomly to a new location assign according to a
          ##    random sequence and from center out in the spread.
          sequence  <- sample(length(thischunk))
          
          for(jj in 1:length(thischunk)){
            origloc   <- thismonth[thischunk[sequence[jj]]]
            
            ##  If stdfactor is 1, only change mean
            if (stdfactor[mm,col] == 1) {
              newscen[origloc,col] <- thisval + delt
            } else {
              #               newscen[origloc,col] <- thisval + delt + spread*rnorm(1)
              rand.num  <- rnorm(1)
              
              if (abs(rand.num) > 2)   rand.num  <- abs(rand.num*2)/rand.num
              
              newscen[origloc,col] <- thisval + delt + spread*rand.num
              #               cat('\nthisval =', as.character(thisval),'\t\t\t\tdelt =',
              #                   as.character(round(delt*100)/100),'\t\t\t\trand =',
              #                   as.character(round(100*rand.num)/100),'\t\t\t\tspread =',
              #                   as.character(round(100*spread)/100),'\t\t\t\tnewval =',
              #                   as.character(round(100*newscen[origloc,col])/100))
            }
          }
        }
        
        ##  Check for initial goodness of fit in future period (newscen begins = base)
        truenewcdf  <- 1/length(ranklist)*cumsum(hist(newscen[thismonth,col], cdfvect3,
                                                      plot = FALSE)$counts)
        
        meanerr     <- abs(mean(newscen[thismonth,col])-newmu[mm,col])
        stderr      <- abs(sd(newscen[thismonth,col])/newstd[mm,col]-1)
        spread      <- spread + 0.1       ## Increase spread for next round of cdf casting
        
        if (spread > 1.5) {
          spread <- 0
          
          if (meanerr > meanfitstandard[mm,col]) {
            meanfitstandard[mm,col] <- meanfitstandard[mm,col] + 0.05
            #             cat('meanfitstandard for month', as.character(mm), 'set to',
            #                 as.character(meanfitstandard[mm,col]),'column =',as.character(col),'\n')
          }
          
          if (stderr > stdfitstandard[mm,col])  {
            stdfitstandard[mm,col]  <- stdfitstandard[mm,col] + 0.002
            #             cat('stdfitstandard for month', as.character(mm), 'set to',
            #                 as.character(100*stdfitstandard[mm,col]),'column =',as.character(col),'\n')
          }
          
          if ((meanfitstandard[mm,col] > 0.3) || (stdfitstandard[mm,col] > 0.1))  {
            monthfailed             <- 1
            if (loop.test[1] == 2) newscen[thismonth,col]  <- -99
            cat('POOR FIT FOR ', month.name[mm], ' (', as.character(mm),') in column = ',
                as.character(col),' (Tmax)\n', sep='')
            
            cat('Meanerr =', as.character(meanerr), '     Stderr =', as.character(stderr),'\n')
            
            newscen[,6] <- base[,6]
            
            loop.test[5]  <- 1
            
            if (loop.test[5] == 1) break
          }
          meanerr <- 1234             ## Make sure you give a chance for tighter spread to be
          stderr  <- 1234             ##   successful at new fit standard
        }
      }                                                 ##  Large while loop for Tmax
      
      if (loop.test[5] == 1) break
      
      ##  Tmax Diagnostics
      if (rundiag == mm || rundiag == 13) {
        
        ##  Plot specified month time series
        plot(newscen[thismonth,col], col = 'red', type = 'l',
             xlim = c(0,ceiling(length(newscen[thismonth,col])/100)*100),
             ylim = c(floor(min(newscen[thismonth,col])/10)*10,
                      ceiling(max(newscen[thismonth,col])/10)*10),
             xlab = paste('Days in',month.name[mm]),
             ylab = expression(paste('Tmax in  ', degree,'C')))
        lines(base[thismonth,col], col = 'blue')
        title(main = paste('Time Series of', month.name[mm], 'Maximum Temperatures (Tmax)'))
        
        ##  CDF of Tmax Stretch
        ##   Uncomment lines setEPS(), postscript(...), and dev.off() to export .eps
        ##   Uncomment lines jpeg(...) and dev.off() to export as .jpeg
        #         setEPS()
        #         postscript('TmaxStretch_December.eps', horizontal = FALSE, onefile = FALSE)
        #         jpeg(filename = 'TmaxStretch_December.jpeg', quality = 100)
        
        plot(cdfvect2, truebasecdf, col = 'blue', type = 'l', lwd = 2,
             xlim = c(floor(min(cdfvect2)/10)*10,ceiling(max(cdfvect2)/10)*10),
             ylim = c(-0.05,1.05), xlab = expression(paste('Tmax in  ', degree,'C')), ylab = ' ')
        lines(cdfvect2, basecdf, col = 'cyan', lwd = 2)
        lines(cdfvect2, truenewcdf, col = 'red', lwd = 2)
        lines(cdfvect2, newcdf, col = 'green', lwd = 2)
        lines(cdfvect2, truebasecdf, col = 'blue', lwd = 2)
        par(ps = 15)
        title(main = paste('CDF of', month.name[mm], 'Maximum Temperatures (Tmax)'))
        legend(floor(min(cdfvect2)/10)*10,1.05, legend = c('1980-2010 Baseline Observations \n', 'Baseline Theoretical Distribution \n', 'Imposed Scenario \n', 'Imposed Theoretical Distribution \n'),col = c('blue', 'cyan', 'red', 'green'), lty = 1, cex = 0.7, bty = 'n', y.intersp = 0.5, seg.len = 1)
        
        #         dev.off()
      }           ##  Diagnostic loop
      if (mm == 12)  loop.test[2] <- 1                    ##  Completion validation
    }                      ##  Month loop for Tmax
    
    ########################################### Tmin Loop ##########################################
    col     <- 7
    #     cat('\n',loop.test,'\n')
    #     rundiag <- 0  ##  Turn on diagnostic, 0 = No diagnostic, 1 = January, 2 = ..., 13 = all months
    
    for(mm in 1:12){
      #       cat('Month = ', month.name[mm], '\n')
      monthfailed <- 0
      thismonth   <- eval(parse(text = month.abb[mm])) ##  Vector of month days over 30 years
      
      ##  First impose diurnal temperature range according to new Tmaxs as intermediate scenario
      intermscen[thismonth]   <- newscen[thismonth,col-1] - (base[thismonth,col-1] - base[thismonth,col])
      newscen[thismonth,col]  <- intermscen[thismonth]
      
      ##  Increase standard deviation for Tmin of this month by calculating the normal statistics
      basestd       <- sd(base[thismonth,col])
      basemu        <- mean(base[thismonth,col])
      
      ##  Calculate intermediate standard deviation and mean
      intermstd     <- sd(intermscen[thismonth])
      intermmu      <- mean(intermscen[thismonth])
      
      ##  Calculate new mean with meandelt and new standard deviation with stdfactor
      newmu[mm,col]   <- basemu + meandelt[mm,col]
      newstd[mm,col]  <- basestd * stdfactor[mm,col]
      if (stdfactor[mm,col]<0)        newstd[mm,col]  <- (-stdfactor[mm,col])
      
      ##  Rank baseline monthly series and find significant limits of baseline and new CDF
      ranklist      <- sort(intermscen[thismonth], decreasing = TRUE)
      
      basecdf       <- pnorm(tcov,basemu,basestd)
      intermcdf     <- pnorm(tcov,intermmu,intermstd)
      newcdf        <- pnorm(tcov,newmu[mm,col],newstd[mm,col])
      basecdflims   <- c(tcov[min(which(basecdf > 1e-6))],tcov[max(which(basecdf < (1-1e-4)))])
      intermcdflims <- c(tcov[min(which(intermcdf > 1e-6))],tcov[max(which(intermcdf < (1-1e-4)))])
      newcdflims    <- c(tcov[min(which(newcdf > 1e-6))],tcov[max(which(newcdf < (1-1e-4)))])
      cdflims       <- c(min(c(basecdflims[1],newcdflims[1],intermcdflims[1])),
                         max(c(basecdflims[2],newcdflims[2],intermcdflims[2])))
      
      ##  R hist bins handles outliers differently than Matlab, widen cdflims by +/-5 for cdfvect
      #       cdfvect       <- seq(cdflims[1],cdflims[2],0.1)
      cdfvect2      <- seq(cdflims[1]-20,cdflims[2]+20,0.1)
      cdfvect3      <- seq(min(cdfvect2)-0.05,max(cdfvect2)+0.05,0.1)
      
      ##  Now regenerate only significant portion
      basecdf       <- pnorm(cdfvect2,basemu,basestd)
      intermcdf     <- pnorm(cdfvect2,intermmu,intermstd)
      newcdf        <- pnorm(cdfvect2,newmu[mm,col],newstd[mm,col])
      
      ##  Check for goodness of fit in baseline period as standard
      truebasecdf   <- 1/length(ranklist)*cumsum(hist(base[thismonth,col], cdfvect3,
                                                      plot = FALSE)$counts)
      
      ##  Check for goodness of fit in intermediate scenario
      trueintermcdf <- 1/length(ranklist)*cumsum(hist(intermscen[thismonth], cdfvect3,
                                                      plot = FALSE)$counts)
      
      ##  Check for initial goodness of fit in future period (newscen begins = base)
      truenewcdf    <- 1/length(ranklist)*cumsum(hist(newscen[thismonth,col], cdfvect3,
                                                      plot = FALSE)$counts)
      
      ##  Continue until future distribution, in comparison to theoretical future distribution, 
      ##    looks like  baseline distribution (MSE does not increase by more than 5%).
      
      ##  Initially move directly with no spread (1:1)
      spread  <- 0
      meanerr <- abs(mean(newscen[thismonth,col])-newmu[mm,col])
      stderr  <- abs(sd(newscen[thismonth,col])/newstd[mm,col]-1)
      
      ##  Good fit if mean is within 0.1K and standard deviation is within 0.2% of desired
      meanfitstandard[mm,col] <- 0.1
      stdfitstandard[mm,col]  <- 0.002
      
      while (((meanerr > meanfitstandard[mm,col]) || (stderr > stdfitstandard[mm,col])) 
             && (monthfailed == 0)) {
        #         cat('meanerr <- ', as.character(format(meanerr, width=6, digits=3, trim=TRUE)),
        #             '\t\t\t\tstderr <- ', as.character(format(stderr*100, width=6, digits=3, trim=TRUE)),
        #             '\t\t\t\tmeanfs <- ', as.character(format(meanfitstandard[mm,col], width=5)),
        #             '\t\t\t\tstdfs <- ' , as.character(format(stdfitstandard[mm,col], width=5)),
        #             '\t\t\t\tspread <- ', as.character(format(spread, width=3)), '\n')
        
        ##  Find new value in ranked list
        ii <- 1
        while (ii < length(ranklist) + 1){
          thisval <- ranklist[ii]
          
          ##  Find shift in value corresponding to this quantile in both cdfs
          intermprctile <- intermcdf[which.min(abs(cdfvect2-thisval))]
          newvalue      <- cdfvect2[which.min(abs(newcdf-intermprctile))]
          delt          <- newvalue-thisval
          
          ##  Find all occurrences with same value    -- this could be more efficient
          while((ii < length(ranklist)+1) && (ranklist[ii] == thisval))     ii  <- ii + 1
          
          thischunk <- which(intermscen[thismonth] == thisval)
          
          ##  Now adjust each of these members randomly to a new location
          ##  Assign according to a random sequence and from center out in the spread
          
          sequence    <- sample(length(thischunk))
          for(jj in 1:length(thischunk)){
            origloc   <- thismonth[thischunk[sequence[jj]]]
            
            ##  If stdfactor is 1, only change mean
            if (stdfactor[mm,col] == 1) {
              newscen[origloc,col]    <- thisval + delt
            } else {
              #               newscen[origloc,col] <- thisval + delt + spread*rnorm(1)
              rand.num  <- rnorm(1)
              
              if (abs(rand.num) > 2)   rand.num  <- abs(rand.num*2)/rand.num
              
              #               newscen[origloc,col] <- thisval + delt + spread*rand.num
              newscen[origloc,col] <- thisval + delt + spread*rand.num*0.5
              #               cat('\nthisval =', as.character(thisval),'\t\t\t\tdelt =',
              #                   as.character(round(delt*100)/100),'\t\t\t\trand =',
              #                   as.character(round(100*rand.num)/100),'\t\t\t\tspread =',
              #                   as.character(round(100*spread)/100),'\t\t\t\tnewval =',
              #                   as.character(round(100*newscen[origloc,col])/100))
            }
          }
        }
        
        if (loop.test[5] == 1) break
        
        ##  Check for initial goodness of fit in future period (newscen begins = base)
        truenewcdf  <- 1/length(ranklist)*cumsum(hist(newscen[thismonth,col], cdfvect3,
                                                      plot = FALSE)$counts)
        meanerr     <- abs(mean(newscen[thismonth,col])-newmu[mm,col])
        stderr      <- abs(sd(newscen[thismonth,col])/newstd[mm,col]-1)
        
        spread      <- spread + 0.1       ## Increase spread for next round of CDF casting
        if (spread > 1.5) {
          spread <- 0
          
          if (meanerr > meanfitstandard[mm,col]) {
            meanfitstandard[mm,col] <- meanfitstandard[mm,col] + 0.05
            #             cat('meanfitstandard for month', as.character(mm), 'set to',
            #                 as.character(meanfitstandard[mm,col]),'column =',as.character(col),'\n')
          }
          
          if (stderr > stdfitstandard[mm,col])  {
            stdfitstandard[mm,col] <- stdfitstandard[mm,col] + 0.002
            #             cat('stdfitstandard for month', as.character(mm), 'set to', 
            #                 as.character(100*stdfitstandard[mm,col]),'column =',as.character(col),'\n')
          }
          
          if ((meanfitstandard[mm,col] > 0.3) || (stdfitstandard[mm,col] > 0.1))  {
            monthfailed <- 1
            if (loop.test[1] == 2) newscen[thismonth,col]  <- -99
            cat('POOR FIT FOR ', month.name[mm], ' (', as.character(mm),') in column = ', as.character(col),' (Tmin)\n', sep='')
            
            cat('Meanerr =', as.character(meanerr), '     Stderr =', as.character(stderr),'\n')
            
            newscen[,7] <- base[,7]
            
            loop.test[5]  <- 1
            
            if (loop.test[5] == 1) break
          }
          meanerr <- 1234             ##  Make sure you give a chance for tighter spread to be
          stderr  <- 1234             ##   successful at new fit standard
        }
      }                                            ##  Large while loop for Tmin
      
      if (loop.test[5] == 1) break
      
      ##  Tmin Diagnostics
      if (rundiag == mm || rundiag == 13) {
        
        ##  Plot specified month time series
        plot(newscen[thismonth,col], col = 'red', type = 'l',
             xlim = c(0,ceiling(length(newscen[thismonth,col])/100)*100),
             ylim = c(floor(min(newscen[thismonth,col])/10)*10,
                      ceiling(max(newscen[thismonth,col])/10)*10),
             xlab = paste('Days in',month.name[mm]),
             ylab = expression(paste('Tmin in  ', degree,'C')))
        lines(base[thismonth,col], col = 'blue')
        title(main = paste('Time Series of', month.name[mm], 'Minimum Temperatures (Tmin)'))
        
        ##  CDF of Tmin Stretch
        ##   Uncomment lines setEPS(), postscript(...), and dev.off() to export .eps
        ##   Uncomment lines jpeg(...) and dev.off() to export as .jpeg
        #         setEPS()
        #         postscript(paste('TminStretch_', month.name[mm], '.eps', sep=''), horizontal = FALSE, onefile = FALSE)
        #         jpeg(filename = paste('TminStretch_', month.name[mm], '.jpeg', sep=''), quality = 100)
        
        plot(cdfvect2,  truebasecdf, col = 'blue',  lwd = 2, type = 'l', 
             xlim = c(floor(min(cdfvect2)/10)*10,ceiling(max(cdfvect2)/10)*10),
             ylim = c(-0.05,1.05), xlab = expression(paste('Tmin in  ', degree,'C')), ylab = ' ')
        lines(cdfvect2, basecdf,       col = 'cyan',    lwd = 2)
        lines(cdfvect2, trueintermcdf, col = 'black',   lwd = 2)
        lines(cdfvect2, intermcdf,     col = 'magenta', lwd = 2)
        lines(cdfvect2, truenewcdf,    col = 'red',     lwd = 2)
        lines(cdfvect2, newcdf,        col = 'green',   lwd = 2)
        lines(cdfvect2, truebasecdf,   col = 'blue',    lwd = 2)
        par(ps = 15)
        title(main = paste('CDF of', month.name[mm], 'Minimum Temperatures (Tmin)'))
        legend(floor(min(cdfvect2)/10)*10,1.05, legend = c('1980-2010 Baseline Observations \n', 'Baseline Theoretical Distribution \n', 'Imposed Intermediate Scenario \n', 'Imposed Intermediate Theoretical Distribution','Imposed Scenario \n', 'Imposed Theoretical Distribution \n'),col = c('blue', 'cyan', 'black', 'magenta', 'red', 'green'), lty = 1, cex = 0.7, bty = 'n', y.intersp = 0.5, seg.len = 1)
        
        #         dev.off()
      }           ##  Diagnostic loop
      if (mm == 12)  loop.test[3] <- 1                    ##  Completion validation
    }                      ##  Month loop for Tmin
    
    ##  Test to ensure Tmax > Tmin
    aa <- newscen[,6]-newscen[,7]
    bb <- which(aa <= 0)
    #     cat('Number of Tmin > Tmax  =  ', as.character(length(bb)), '\n Mean error = ', as.character(mean(aa[bb])), '\n')
    
    if (length(bb) > 0){
      for(ii in 1:length(bb)){
        newscen[bb[ii],6] <- mean(as.numeric(newscen[bb[ii],6:7])) + 0.1
        newscen[bb[ii],7] <- newscen[bb[ii],6] - 0.2
      }
    }
  }   ##  Loop to rerun (3x) if does not converge
  
  ##  Test for successful completion of Tmax and Tmin loop
  if (loop.test[2] == 0) {
    stop(paste('Failed to shift mean and variability of ', as.character(outfile), ' in ', month.name[mm], ' for Tmin.\n\t\t\t\t\t\t\tDid not produce ',as.character(outfile), '.AgMIP.', sep=''))
  }
  if (loop.test[3] == 0) {
    stop(paste('Failed to shift mean and variability of ', as.character(outfile), ' in ', month.name[mm], ' for Tmax.\n\t\t\t\t\t\t\tDid not produce ',as.character(outfile), '.AgMIP.', sep=''))
  }
  
  ###--------------------------------------------------------------------------------------------###
  ####################################### Precipitation Loop #######################################
  ###--------------------------------------------------------------------------------------------###
  ##  The while loop that attempts to fit the new precipitation distribution to the same parameters 
  ##   as the baseline distribution relies on loop.test[4] = mm.
  ##  This while loop is given 3 opportunities to succeed, as is counted by loop.test[1].
  ##  When the fit is successful, loop.test[4] = loop.test[4] + 1, to move the calculation to the
  ##   next month.  After completing all months successfully, loop.test[4] will be set to 13.
  ##  If the while loop is not successful in adjusting the distribution, only a mean change is
  ##   applied to that month without maintaining the distribution parameters from the baseline.
  
  col     <- 8
  #   rundiag <- 0  ##  Turn on diagnostic, 0 = No diagnostic, 1 = January, 2 = ..., 13 = all months
  loop.test[4]      <- 1                      ##  Set loop.test[4] for mm = 1 
  loop.test[c(1,5)] <- 0                      ##  Reset while loop counter
  
  for(mm in 1:12) {
    
    if (loop.test[1] == 0) {
      #       cat('\nMonth = ', month.name[mm])
      #       cat('\n', loop.test)
      thismonth       <- eval(parse(text = month.abb[mm]))    ## Vector of month days over 30 years
      
      ##  First calculate baseline distribution of precipitation
      basemu        <- mean(base[thismonth,col])
      basewetdays   <- thismonth[base[thismonth,col] > 0]
      nbasewetdays  <- length(basewetdays)
      basealpha     <- NaN
      basebeta      <- NaN
      
      ##  Calculate statistics for rainfall and new mean for each month
      newmu[mm,col]   <- basemu*meandelt[mm,col]
      
      ##  Adjust newmu to be no less than 0.0005 - Added July 23, 2013 NIH
      ##    0.0005 would reduce a month with 930 wet days to ~0 wet days
      if (newmu[mm,col] < 0.001) newmu[mm,col] <- 0.001
      
      ##  Test for nonzero monthly precipitation
      if (nbasewetdays < 0)   next
      
      ##  Test for > 31 total days of precipitation to perform gamma fit
      if (nbasewetdays > 31) {
        gammadistwork <- fitdistr(base[basewetdays,col],dgamma, start=list(shape = 1, rate = 0.1),
                                  lower=0.001)
        basealpha     <- as.numeric(gammadistwork$estimate[1])
        basebeta      <- 1/as.numeric(gammadistwork$estimate[2])
      }
      
      ##  Directly impose alpha for months with > 31 total days of precipitation
      if (!is.nan(basealpha)) {
        newalpha[mm] <- basealpha*gamfactor[mm]
        if (gamfactor[mm] < 0) newalpha[mm] <- -gamfactor[mm]
      }
      
      ##  Cannot have more wet days than days, workaround to round 0.5 to 1
      posneg            <- sign(nbasewetdays*wetfactor[mm])
      nnewwetdays[mm]   <- min(c((trunc(abs(nbasewetdays*wetfactor[mm]) + 0.5)*posneg),
                                 length(thismonth)))
      
      ##  Must have at least 1 rainy if baseline month had at least 1 rainy day
      if (nnewwetdays[mm] < 1)  nnewwetdays[mm]   <- max(nnewwetdays[mm],1)
      
      ##  Set beta so mean change is imposed (mean of wet days = alpha*beta) and accounts for
      ##   changing number of wet days
      if (!is.nan(basealpha)) newbeta[mm] <- newmu[mm,col]*
        length(thismonth)/nnewwetdays[mm]/newalpha[mm]
      
    }
    
    while (loop.test[1] < 3 && loop.test[4] == mm) {
      
      ##  Reset newscen on subsequent loops
      if (loop.test[1] > 0)   newscen[thismonth,c(5,col)] <- base[thismonth,c(5,col)]
      
      ##  Reset loop.test and count loop
      loop.test[1]  <- loop.test[1] + 1
      loop.test[5]  <- 0
      
      ##  Set number of rainy days in future scenario
      newwetdays    <- thismonth[newscen[thismonth,col] > 0]
      
      ##  Set new dry days according to smallest rainfall totals
      while (nnewwetdays[mm] < length(newwetdays)) {
        
        wetrank   <- sort(newscen[newwetdays,col])
        newdry    <- length(newwetdays)-nnewwetdays[mm]
        drizdays  <- which(newscen[newwetdays,col] == wetrank[1])
        
        if (length(drizdays) < newdry) {
          newscen[newwetdays[drizdays],col] <- 0
          
          ##  Increase radiation on the new dry days by 10% following Mearns et al., 1996
          newscen[newwetdays[drizdays],5]   <- newscen[newwetdays[drizdays],5] * 1.1
          
        } else {
          sequence  <- sample(length(drizdays))
          newscen[newwetdays[drizdays[sequence[1:newdry]]],col] <- 0
          
          ##  Increase radiation on the new dry days by 10% following Mearns et al., 1996
          newscen[newwetdays[drizdays[sequence[1:newdry]]],5]   <- 
            newscen[newwetdays[drizdays[sequence[1:newdry]]],5] * 1.1
        }
        newwetdays  <- thismonth[newscen[thismonth,col] > 0]
      }
      
      ##  Set new wet days according to smallest solar radiation (cloudiness)
      if (nnewwetdays[mm] > length(newwetdays)) {
        cloudyrank    <- sort(newscen[thismonth,5])
        newwet        <- nnewwetdays[mm] - length(newwetdays)
        ncloudyrank   <- 0
        
        while (newwet > 0) {
          cloudydays  <- which(newscen[thismonth,5] == cloudyrank[ncloudyrank + 1])
          ncloudyrank <- ncloudyrank + length(cloudydays)
          newwetcandidates  <- which(newscen[thismonth[cloudydays],col] == 0)
          
          if (length(newwetcandidates) <= newwet) {
            
            if (length(newwetcandidates) != 0) {
              newscen[thismonth[cloudydays[newwetcandidates]],col]  <- 0.3
              
              ##  Reduce radiation on new rain days by 10% following Mearns et al., 1996
              newscen[thismonth[cloudydays[newwetcandidates]],5]  <- 
                newscen[thismonth[cloudydays[newwetcandidates]],5] * 0.9
            }
          } else {
            
            if (length(newwetcandidates) > 0) {
              sequence  <- sample(length(newwetcandidates))
              newscen[thismonth[cloudydays[newwetcandidates[sequence[1:newwet]]]],col]  <- 0.3
              
              ##  Reduce radiation on new rain days by 10% following Mearns et al., 1996
              newscen[thismonth[cloudydays[newwetcandidates[sequence[1:newwet]]]],5]    <- 
                newscen[thismonth[cloudydays[newwetcandidates[sequence[1:newwet]]]],5] * 0.9
            }
          }
          newwetdays  <- thismonth[newscen[thismonth,col] > 0]
          newwet      <- nnewwetdays[mm] - length(newwetdays)
        }
      }
      
      ##  Calculate theoretical distributions for use in shifting
      cdfvect       <- seq(0,1000,0.1)
      cdfvect2      <- seq(-0.05,1000.05,0.1)
      startscen     <- newscen
      truestartcdf  <- 1/nnewwetdays[mm] * cumsum(hist(startscen[newwetdays,col], cdfvect2,
                                                       plot = FALSE)$counts)
      truebasecdf   <- 1/nbasewetdays    * cumsum(hist(base[basewetdays,col], cdfvect2,
                                                       plot = FALSE)$counts)
      truenewcdf    <- 1/nnewwetdays[mm] * cumsum(hist(newscen[newwetdays,col],cdfvect2,
                                                       plot = FALSE)$counts)
      
      ##  Initialize as missing for months too dry for gamma distribution
      startcdf      <- cdfvect*NaN
      basecdf       <- cdfvect*NaN
      newcdf        <- cdfvect*NaN
      
      if (!is.nan(basealpha)) {
        
        ##  Error handling optimization
        test.fit1 <- try(suppressWarnings(gammastart  <- fitdistr(startscen[newwetdays,col], dgamma, start=list(shape = startalpha, rate = 1/startbeta), lower=0.001)), silent = TRUE)
        if (class(test.fit1) == 'try-error') {
          test.fit2 <- try(suppressWarnings(gammastart  <- fitdistr(startscen[newwetdays,col],dgamma, start=list(shape = 1, rate = 0.1), lower=0.001)), silent = TRUE)
          if (class(test.fit2) == 'try-error') {
            test.fit3 <- try(suppressWarnings(gammastart  <- fitdistr(startscen[newwetdays,col], 'gamma')), silent = TRUE)
            if (class(test.fit3) == 'try-error') loop.test[5]  <- 1
          }
        }
        
        if (loop.test[5] == 1) next
        
        ##  Re-calculate base cdf with new starting point now that the # of wet days has changed
        startalpha  <- as.numeric(gammastart$estimate[1])
        startbeta   <- 1/as.numeric(gammastart$estimate[2])
        startcdf    <- pgamma(cdfvect, startalpha,    1/startbeta)
        basecdf     <- pgamma(cdfvect, basealpha,     1/basebeta)
        newcdf      <- pgamma(cdfvect, newalpha[mm],  1/newbeta[mm])
        
        ##  Sort rainy days by rainfall from starting scenario after wet day adjustment
        ranklist    <- sort(startscen[newwetdays,col], decreasing = TRUE)
      }
      
      ##  Make sure no days are rainier than 999 mm
      if (length(which(newscen[thismonth,col]>999))>0) {
        newscen[thismonth[newscen[thismonth,col]>999],col]  <- 999
      }
      
      ##  Make sure future period monthly means are correct
      ##  This is the only thing done when gamma distribution cannot be fit because there aren't
      ##   enough wet days
      meanerrfact   <- newmu[mm,col]/mean(newscen[thismonth,col])
      
      #     cat('Target: ', as.character(newmu[mm,col]),
      #         '    Start: ',   as.character(mean(newscen[thismonth,col])),
      #         '    Revised: ', as.character(mean(newscen[thismonth,col]*meanerrfact)), '\n')
      
      newscen[thismonth,col]  <- newscen[thismonth,col]*meanerrfact
      
      meanerr       <- abs(mean(newscen[thismonth,col]) - newmu[mm,col])
      alphaerr      <- 0        ##  No gamma error if distribution can't be fit
      betaerr       <- 0        ##  No gamma error if distribution can't be fit
      
      ##  Check for initial goodness of fit in future period
      if (!is.nan(basealpha)) {
        
        ##  Error handling optimization          
        test.fit1 <- try(gg  <- suppressWarnings(fitdistr(newscen[newwetdays,col],dgamma, start=list(shape = startalpha, rate = 1/startbeta), lower=0.001)), silent = TRUE)
        if (class(test.fit1) == 'try-error') {
          test.fit2 <- try(gg  <- suppressWarnings(fitdistr(newscen[newwetdays,col],dgamma, start=list(shape = 1, rate = 0.1), lower=0.001)), silent = TRUE)
          if (class(test.fit2) == 'try-error') {
            test.fit3 <- try(suppressWarnings(gg  <- fitdistr(newscen[newwetdays,col], 'gamma')), silent = TRUE)
            if (class(test.fit3) == 'try-error') loop.test[5]  <- 1
          }
        }
        
        if (loop.test[5] == 1) next
        
        alphaerr    <- abs(as.numeric(gg$estimate[1])/newalpha[mm] - 1)
        betaerr     <- abs((1/as.numeric(gg$estimate[2]))/newbeta[mm] - 1)
      }
      
      ##  Initially move directly with no spread (1:1)
      ##  Spread for rainfall is a percentage of the imposed value
      spread      <- 0
      
      ##  Reset loop counter for while loop
      loop.test[1]  <- 0
      
      while (loop.test[1] < 3 && loop.test[4] == mm) {
        loop.test[1]  <- loop.test[1] + 1
        loop.test[5]  <- 0
        #         cat('\n', loop.test)
        
        ##  Good fit if mean is within 0.1 mm/day and gamma parameters are within 0.2% of desired
        meanfitstandard [mm,col]  <- 0.05
        alphafitstandard[mm]      <- 0.002
        betafitstandard [mm]      <- 0.002
        
        ##  Reset newscen
        newscen[thismonth,col] <- startscen[thismonth,col]
        
        while (( (meanerr  > meanfitstandard[mm,col]) ||
                 (alphaerr > alphafitstandard[mm])  ||
                 (betaerr  > betafitstandard[mm]))  ) {
          
          ##  Find new value in ranked list
          ii  <- 1
          
          while (ii < (length(ranklist)+1)) {
            thisval <- ranklist[ii]
            
            ##  Find shift in value corresponding to this percentile in both cdfs
            startprctile  <- startcdf[which.min(abs(cdfvect-thisval))]
            newvalue      <- cdfvect[which.min(abs(newcdf-startprctile))]
            delt          <- newvalue-thisval
            
            ##  Find all occurrances with same value
            while ((ii < length(ranklist)+1) && (ranklist[ii] == thisval))    ii   <- ii + 1
            
            thischunk     <- which(startscen[thismonth,col] == thisval)
            
            ##  Now adjust each of these members randomly to new location
            ##  Assign according to random sequence and from center out in the spread
            sequence      <- sample(length(thischunk))
            
            if (length(thischunk) > 0) {
              for (jj in 1:length(thischunk)) {
                origloc               <- thismonth[thischunk[sequence[jj]]]
                #                 newscen[origloc,col]  <- (thisval+delt) + (thisval+delt)*spread*rnorm(1)
                rand.num              <- rnorm(1)
                
                if (abs(rand.num) > 2)   rand.num  <- abs(rand.num*2)/rand.num
                
                newscen[origloc,col]  <- thisval + delt + (delt*spread*rand.num)
                
                if (newscen[origloc,col] < 0.1)       newscen[origloc,col]  <- 0.1
                
                #                 cat('\nthisval = ', as.character(thisval),'\t\t\t\tdelt =',
                #                     as.character(round(delt*100)/100),'\t\t\t\trand =',
                #                     as.character(round(100*rand.num)/100),'\t\t\t\tspread =',
                #                     as.character(round(100*spread)/100),'\t\t\t\tnewval =',
                #                     as.character(round(100*newscen[origloc,col])/100))
              }
            }
          }
          
          ##  Make sure future period monthly means are correct
          meanerrfact     <- newmu[mm,col]/mean(newscen[thismonth,col])
          
          #           cat('Target: ',  as.character(newmu[mm,col]),
          #               '    Start: ', as.character(mean(newscen[thismonth,col])),
          #               '    Revised: ', as.character(mean(newscen[thismonth,col]*meanerrfact)), '\n')
          
          newscen[thismonth,col]  <- newscen[thismonth,col]*meanerrfact
          
          ##  Check for initial goodness of fit in future period (newscen begins = startcdf != base)
          truenewcdf  <- 1/nnewwetdays[mm] * cumsum(hist(newscen[newwetdays,col],cdfvect2,
                                                         plot = FALSE)$counts)
          meanerr     <- abs(mean(newscen[thismonth,col]) - newmu[mm,col])
          alphaerr    <- 0          ## no gamma error if distribution can't be fit
          betaerr     <- 0          ## no gamma error if distribution can't be fit
          
          if (!is.nan(basealpha)) {
            
            ##  Error handling optimization
            test.fit1 <- try(suppressWarnings(gg  <- fitdistr(newscen[newwetdays,col],dgamma, start=list(shape = startalpha, rate = 1/startbeta), lower=0.001)), silent = TRUE)
            if (class(test.fit1) == 'try-error') {
              test.fit2 <- try(suppressWarnings(gg  <- fitdistr(newscen[newwetdays,col],dgamma, start=list(shape = 1, rate = 0.1), lower=0.001)), silent = TRUE)
              if (class(test.fit2) == 'try-error') {
                test.fit3 <- try(suppressWarnings(gg  <- fitdistr(newscen[newwetdays,col], 'gamma')), silent = TRUE)
                if (class(test.fit3) == 'try-error') loop.test[5]  <- 1
              }
            }
            
            if (loop.test[5] == 1) next
            
            alphaerr  <- abs(as.numeric(gg$estimate[1])/newalpha[mm] - 1)
            betaerr   <- abs((1/as.numeric(gg$estimate[2]))/newbeta[mm] - 1)
          }
          
          #           cat(sprintf('%10s%5.2f%30s%7.2f%25s%7.2f%25s%7.2f%30s%7.2f%25s%7.2f%25s%7.2f%s',
          #                       '\nspread = ', spread,
          #                       '\n     Mean Error         = ',   meanerr,
          #                       'Alpha Error        = ',  alphaerr*100,
          #                       'Beta Error         = ',   betaerr*100,
          #                       '\n     Mean Fit Standard  = ',   meanfitstandard[mm,col],
          #                       'Alpha Fit Standard = ',  alphafitstandard[mm]*100,
          #                       'Beta Fit Standard  = ',   betafitstandard[mm]*100, '\n'))
          
          
          spread    <- spread + 0.05  ##  Increase spread for next round of cdf casting
          
          if (spread > 0.2) {         ##  Revise standard for fit (if >20% shift/random standard 
            ##    deviation is necessary)
            spread  <- 0
            
            if (meanerr  > meanfitstandard[mm,col]) {
              meanfitstandard[mm,col] <- meanfitstandard[mm,col] + 0.05
              
              #               cat('\n  Mean Fit Standard for', month.name[mm], 'set to',
              #                   as.character(meanfitstandard[mm,col]))
            }
            
            if (alphaerr > alphafitstandard[mm]) {
              alphafitstandard[mm] <- alphafitstandard[mm] + 0.005
              #                 cat('\n Alpha Fit Standard for ', month.name[mm], ' set to ',
              #                     as.character(100*alphafitstandard[mm]), sep='')
            }
            
            if (betaerr  > betafitstandard[mm]) {
              betafitstandard[mm] <- betafitstandard[mm] + 0.005
              #               cat('\n  Beta Fit Standard for ', month.name[mm], ' set to ',
              #                   as.character(100*betafitstandard[mm]), sep='')
            }
            
            if((meanfitstandard[mm,col] > 0.3) ||
               (alphafitstandard[mm]  > 0.075) ||
               (betafitstandard[mm]   > 0.075)) {
              
              #               if (loop.test[1] == 1) cat('\n  Fit Standard exceeded on first attempt in month of ', month.name[mm],'.\n\n', sep='')
              #               
              #               if (loop.test[1] == 2) cat('\n  Fit Standard exceeded on second attempt in month of ', month.name[mm],'.\n\n', sep='')
              
              
              ##  Reset newscen
              newscen[thismonth,col] <- startscen[thismonth,col]
              
              loop.test[5]  <- 1
            }
            
            if (loop.test[5] == 1) break
            
            ##  To be successful at new fit standard, make sure you give a chance for tighter spread
            meanerr   <- 1234 
            alphaerr  <- 1234
            betaerr   <- 1234
          }
          
        }                       ##  Large while loop for Precip
        
        if (loop.test[5] != 1) {
          loop.test[4] <- loop.test[4] + 1
          loop.test[1] <- 0
          
          #           cat('\n', month.name[mm], ' complete\n\n', sep='')
          
          next
          
        }
        
        if (loop.test[1]>2 && loop.test[5] != 0) {
          newscen[thismonth,8] <- startscen[thismonth,col]*(newmu[mm,col]/
                                                              mean(startscen[thismonth,col]))
          
          cat(paste('Failed to shift mean and variability of ', as.character(outfile), ' in ', month.name[mm], ' for Precipitation.  Applied mean shift to precipitation future scenario without maintaining parameters of baseline distribution.\n', sep=''))
          
          loop.test[4] <- loop.test[4] + 1
          loop.test[1] <- 0
          
          next
          
        }
        
      }  ##  Loop to rerun (3x) if does not converge
      
    }  ##  Loop to rerun (3x) if does not converge
    
    ##  Precipitation Diagnostics
    if (rundiag == mm || rundiag == 13) {
      
      ##  Plot specified month time series
      plot(newscen[thismonth,col], col = 'red', type = 'l', xlim = c(0,1000), ylim = c(-5,(ceiling(max(newscen[thismonth,col])/10)*10)))
      lines(base[thismonth,col], col = 'blue')
      title(main = paste('Time Series of', month.name[mm], 'Precipitation (Rain)'))
      
      ##  CDF of Precipitation Stretch
      ##   Uncomment lines setEPS(), postscript(...), and dev.off() to export .eps
      ##   Uncomment lines jpeg(...) and dev.off() to export as .jpeg
      #     setEPS()
      #     postscript(paste('PStretch_', month.name[mm], '.eps', sep=''), horizontal = FALSE, onefile = FALSE)
      #     jpeg(filename = paste('PStretch_', month.name[mm], '.jpeg', sep=''), quality = 100)
      
      plot (cdfvect, truebasecdf, col = 'cyan', type = 'l', lwd = 2, xlim = c(-5,220), ylim = c(-0.05,1.05), xlab = expression(~degree~C), ylab = ' ')
      lines(cdfvect, basecdf, col = 'blue', lwd = 2)
      lines(cdfvect, truenewcdf, col = 'magenta', lwd = 2)
      lines(cdfvect, newcdf, col = 'red', lwd = 2)
      #     lines(cdfvect, startcdf, col = 'green', lwd = 2)
      #     lines(cdfvect, truestartcdf, col = 'black', lwd = 2)
      par(ps = 15)
      title(main = paste('CDF of', month.name[mm], 'Precipitation Events (Rain)'))
      legend(50,0.5, legend = c('1980-2010 Baseline Observations \n', 'Baseline Theoretical Distribution \n', 'A2 Mid-Century Scenario \n', 'A2 Mid-Century Theoretical Distribution \n'),col = c('cyan', 'blue', 'magenta', 'red'), lty = 1, cex = 0.7, bty = 'n', y.intersp = 0.5, seg.len = 1)
      
      #     dev.off()
    }               ##  Diagnostic loop
    
  }  ##  Month loop for Precip
  
  ##  Test for successful completion of Precipitation Loop
  if (loop.test[4] != 13) {
    stop(paste('Failed to shift mean and variability of ', as.character(outfile), ' in ', month.name[mm], ' for Precipitation.\n\t\t\t\t\t\t\tDid not produce ',as.character(outfile), '.AgMIP.', sep=''))   
  }  
  
  ###--------------------------------------------------------------------------------------------###
  ########################################  All Diagnostics  ####################################### 
  ###--------------------------------------------------------------------------------------------###
  #   rundiag <- 0  ##  Turn on diagnostic, 0 = No diagnostics, 1 = January, 2 = ..., 13 = all months
  
  if (rundiag > 0) {
    pctdiff <- 1                ##  Set Percent Difference
    colhead <- c('','','','','','Tmax','Tmin','Rain')
    
    if (rundiag == 13)  test.mm <- 1:12        else  test.mm <- rundiag
    
    for (col in 6:7) {
      cat('\n\nColumn', as.character(col), ' - ', colhead[col])
      
      for (mm in test.mm) {
        thismonth       <- eval(parse(text = month.abb[mm]))
        test1 <- 100 - (100*mean(newscen[thismonth,col])/newmu [mm,col])
        test2 <- 100 - (100*sd  (newscen[thismonth,col])/newstd[mm,col])
        
        if (abs(test1) > abs(pctdiff) || abs(test2) > abs(pctdiff)) {
          cat('\n',month.name[mm], '\n\t\tDesired Mean =', as.character(format(newmu[mm,col], digits = 4, nsmall = 4, width = 10)), '\t\t\t\t\tActual Mean =', as.character(format(mean(newscen[thismonth,col]), digits = 4, nsmall = 4, width = 10)), '\t\t\t\t\t% Difference =', as.character(format(test1, digits = 4, nsmall = 4, width = 10)), '%', sep='')
          
          cat('\n\t\tDesired Std  = ', as.character(format(newstd[mm,col], digits = 4, nsmall = 4, width = 9)), '\t\t\t\t\tActual Std  = ', as.character(format(sd(newscen[thismonth,col]), digits = 4, nsmall = 4, width = 9)), '\t\t\t\t\t% Difference = ', as.character(format(test2, digits = 4, nsmall = 4, width = 9)), '%', sep='')
        }
      }
    }
    
    col   <- 8
    cat('\n\nColumn', as.character(col), ' - ', colhead[col])
    
    for (mm in test.mm) {
      thismonth   <- eval(parse(text = month.abb[mm]))
      wet         <- length(which(newscen[thismonth,col] > 0.01))
      newwetdays  <- thismonth[which(newscen[thismonth,col] > 0)]
      if (nbasewetdays > 31) {
        gg          <- fitdistr(newscen[newwetdays,col],'gamma', lower=c(0.01,0.01))
        test3       <- 100-(100*mean(newscen[thismonth,col])/newmu[mm,col])
        test4       <- 100-(100*(gg$estimate[1])/newalpha[mm])
        test5       <- 100-(100*(1/gg$estimate[2])/newbeta[mm])
        
        if (abs(test3) > abs(pctdiff) || abs(test4) > abs(pctdiff) || abs(test5) > abs(pctdiff)) {
          cat('\n',month.name[mm], '\n\t\t\t\tDesired Wet Days = ', as.character(nnewwetdays[mm]), '\t\t\t\t\tActual Wet Days = ', as.character(wet))
          
          cat('\n\t\t\t\tDesired Mean  = ', as.character(format(newmu[mm,col], digits = 4, nsmall = 4, width = 8)), '\t\t\t\t\tActual Mean  = ', as.character(format(mean(newscen[thismonth,col]), digits = 4, nsmall = 4, width = 8)), '\t\t\t\t\t% Difference = ', as.character(format(test3, digits = 4, nsmall = 4, width = 10)), '%', sep='')
          
          cat('\n\t\t\t\tDesired Alpha = ', as.character(format(newalpha[mm], digits = 4, nsmall = 4, width = 8)), '\t\t\t\t\tActual Alpha = ', as.character(format(gg$estimate[1], digits = 4, nsmall = 4, width = 8)), '\t\t\t\t\t% Difference = ', as.character(format(test4, digits = 4, nsmall = 4, width = 10)), '%', sep='')
          
          cat('\n\t\t\t\tDesired Beta  = ', as.character(format(newbeta[mm], digits = 4, nsmall = 4, width = 8)), '\t\t\t\t\tActual Beta  = ', as.character(format(1/gg$estimate[2], digits = 4, nsmall = 4, width = 8)), '\t\t\t\t\t% Difference = ', as.character(format(test5, digits = 4, nsmall = 4, width = 10)), '%', sep='')
        }
      }      
    }    
  }
  
  ###--------------------------------------------------------------------------------------------###
  ######################################  Print .AgMIP file  #######################################  
  ###--------------------------------------------------------------------------------------------###
  
  if (loop.test[2] == 1 && loop.test[3] == 1 && loop.test[4] == 13) { ## Necessary?
    
    ##  Remove all variables except those needed to print file
    rm(list=setdiff(ls(), c('base', 'outfile', 'futloc','headerplus', 'baseinfo', 'newscen',
                            'ddate')))
    invisible(gc())
    
    ##  Calculate Tave and Tamp
    Tave    <- mean(c(newscen[,6],newscen[,7]))
    Tmonth  <- matrix(NaN,12)
    
    for (thismonth in 1:12){
      Tmonth[thismonth] <- mean(c(newscen[which(newscen[,3] == thismonth),6], 
                                  newscen[which(newscen[,3]==thismonth),7]))
    }
    
    Tamp    <- (max(Tmonth)-min(Tmonth))/2
    
    ##  Treat missing values appropriately, even if they've been moved a little
    if (length(newscen[which(newscen[,5] <   0),5]) > 0)  newscen[which(newscen[,5] <   0),5] <- -99
    if (length(newscen[which(newscen[,6] < -70),6]) > 0)  newscen[which(newscen[,6] < -70),6] <- -99
    if (length(newscen[which(newscen[,7] < -70),7]) > 0)  newscen[which(newscen[,7] < -70),7] <- -99
    if (length(newscen[which(newscen[,8] <   0),8]) > 0)  newscen[which(newscen[,8] <   0),8] <- -99
    
    ## Write it all out with proper station code in basic AgMIP format 
    outfile2 <- paste(futloc, outfile, '.AgMIP', sep='')
    
    sink(outfile2)
    
    cat('*WEATHER DATA : ', headerplus,'\n',sep='')
    cat('\n')
    cat(sprintf('%54s', '@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT'),'\n')
    
    ##  Don't forget to adjust reference height for temperature and winds
    cat(sprintf('%6s%9.3f%9.3f%6.0f%6.1f%6.1f%6.1f%6.1f', baseinfo$V1, baseinfo$V2,baseinfo$V3,
                baseinfo$V4,Tave,Tamp,baseinfo$V7,baseinfo$V8),'\n')
    cat(sprintf('%s', '@DATE    YYYY  MM  DD  SRAD  TMAX  TMIN  RAIN  WIND  DEWP  VPRS  RHUM'),'\n')
    
    ##  And add the newly created data...
    for (dd in 1:length(ddate)){
      cat(sprintf('%7s%6s%4s%4s%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.0f\n',
                  as.character(newscen[dd,1]),
                  as.character(newscen[dd,2]),
                  as.character(newscen[dd,3]),
                  as.character(newscen[dd,4]),
                  newscen[dd,5],
                  newscen[dd,6],
                  newscen[dd,7],
                  newscen[dd,8],
                  newscen[dd,9],
                  newscen[dd,10],
                  newscen[dd,11],
                  newscen[dd,12]))
    }
    
    sink()
  }
  ##  Remove all variables
  rm(list = ls(all = TRUE))
  invisible(gc())
}