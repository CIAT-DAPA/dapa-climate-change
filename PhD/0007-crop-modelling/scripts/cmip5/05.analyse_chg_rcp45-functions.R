#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#September 2012

#make a map wrapping spplot
make_spplot <- function(prs,pcols,pbrks,ht=1000,tiffName="test.tif") {
  #required package
  require(maptools); data(wrld_simpl)
  require(raster); require(sp)
  
  #some plot details
  wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
  grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))
  grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)
  layt <- list(wld,grli)
  
  #plot summaries. Make a nice map out from all this
  brks.lab <- round(pbrks,1)
  
  #dimensions of plot
  #ht <- 1000
  fct <- (prs@extent@xmin-prs@extent@xmax)/(prs@extent@ymin-prs@extent@ymax)
  wt <- ht*(fct+.1)
  
  this_plot <- spplot(prs,sp.layout=layt,col.regions=pcols,
                      par.settings=list(fontsize=list(text=12)),
                      at=pbrks,pretty=brks)
  
  #final plotting
  tiff(tiffName,res=300,compression="lzw",height=ht,width=wt)
  print(this_plot)
  dev.off()
}


#calculate summaries
calc_summary <- function(vn_stk) {
  vn_sum <- list()
  vn_sum$MEAN <- calc(vn_stk,fun=mean)
  vn_sum$SD <- calc(vn_stk,fun=sd)
  vn_sum$CV <- abs(vn_sum$SD/vn_sum$MEAN*100)
  vn_sum$CV[which(vn_sum$CV[] > 100)] <- 100
  vn_sum$AG <- calc(vn_stk,fun=ag_fun)
  vn_sum$Q1 <- calc(vn_stk,fun=calc_q1)
  vn_sum$Q4 <- calc(vn_stk,fun=calc_q4)
  return(vn_sum)
}

#functions to calculate quantiles of a raster stack
calc_q1 <- function(x) {
  q_val <- as.numeric(quantile(x,probs=0.25))
  q_m <- mean(x[which(x<=q_val)])
  return(q_m)
}

calc_q4 <- function(x) {
  q_val <- as.numeric(quantile(x,probs=0.75))
  q_m <- mean(x[which(x>=q_val)])
  return(q_m)
}

#function to calculate model agreement. This is percent of models that predict
#changes in the same direction as the average change
ag_fun <- function(x) {
  me <- mean(x)
  if (me == 0) {
    nag <- length(which(x == me))
  } else if (me < 0) {
    nag <- length(which(x < 0))
  } else if (me > 0) {
    nag <- length(which(x > 0))
  }
  pag <- nag/length(x)*100
  return(pag)
}



#function to load chg data from a given gcm run
loadData <- function(hist_gcmDir,rcp_gcmDir,months=c(1:12),vars=c("pr","tas"),
                     chg_type=c("p","a"),calc_type=c("sum","mean"),ext=".tif",msk=NA) {
  #months <- c(6:8)
  #vars <- c("pr","tas")
  #chg_type <- c("p","a")
  #calc_type <- c("sum","mean")
  
  #add few checks here as per lengths and contents of vars, chg_type, calc_type
  if (length(chg_type) != length(vars)) {
    warning("types of changes not specified for all variables. Using abs for all ")
    chg_type <- rep("a",times=length(vars))
  }
  
  if (length(calc_type) != length(vars)) {
    warning("types of calculations not specified for all variables. Using sum for all ")
    calc_type <- rep("sum",times=length(vars))
  }
  
  #correct months to strings with "0" for values <10
  months[which(months < 10)] <- paste("0",months[which(months < 10)],sep="")
  
  #loop variables
  out_data <- list()
  for (i in 1:length(vars)) {
    #i <- 1
    vn <- vars[i]
    chtyp <- chg_type[i]
    fun_type <- calc_type[i]
    
    cat("variable",vn,"\n")
    
    if (!chtyp %in% c("p","a")) {
      warning("change type misspecified, assuming abs")
      chtyp <- "a"
    }
    
    if (!fun_type %in% c("sum","mean","min","max","sd")) {
      warning("function type misspecified, assuming sum")
      fun_type <- "sum"
    }
    
    #loading the data
    hist_stk <- stack(paste(hist_gcmDir,"/",vn,"_",months,ext,sep=""))
    rcp_stk <- stack(paste(rcp_gcmDir,"/",vn,"_",months,ext,sep=""))
    
    #calculating over the data
    hist_rs <- calc(hist_stk,fun=get(fun_type))
    rcp_rs <- calc(rcp_stk,fun=get(fun_type))
    
    #create function to calculate change change
    if (chtyp == "p") {
      chg_fun <- function(x) {
        if (x[1] == 0) {x[1] <- 0.00001}
        z <- (x[2]-x[1])/x[1]*100
      }
    } else if (chtyp == "a") {
      chg_fun <- function(x) {z <- x[2]-x[1]}
    }
    
    #calculate change according to function
    chg_rs <- calc(stack(c(hist_rs,rcp_rs)),chg_fun)
    
    #at the end just cut and resample if mask is not NA
    if (class(msk)[1] == "RasterLayer") {
      chg_rs <- resample(chg_rs,msk,method="ngb")
      #chg_rs <- mask(chg_rs,msk) #mask the raster
    }
    
    out_data[[vn]] <- chg_rs
  }
  return(out_data)
}


#function to get the data
get_wth <- function(wthFil,century=1900) {
  wth <- read.fortran(wthFil,format=c("I5","F6","3F7"),skip=4)
  names(wth) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
  wth <- cbind(YEAR=(as.numeric(substr(wth$DATE,1,2))+century),JDAY=as.numeric(substr(wth$DATE,3,5)),wth)
  return(wth)
}

#function to wrap the watbal function
do_wbal <- function(wth) {
  wth$ETMAX <- NA; wth$AVAIL <- NA; wth$ERATIO <- NA
  wth$CUM_RAIN <- NA; wth$RUNOFF <- NA; wth$DEMAND <- NA
  wth <- watbal_wrapper(wth)
  wth$TMEAN <- (wth$TMAX + wth$TMIN)/2
  return(wth)
}

#function to get the metrics for a particular gs
do_metrics <- function(wth,gs_i,gs_f) {
  #select growing season
  wth <- wth[which(wth$JDAY >= gs_i & wth$JDAY <= gs_f),]
  row.names(wth) <- 1:nrow(wth)
  yr <- wth$YEAR[1]
  
  #calculate each metric
  #a. rainfall during groundnut growing season
  rain <- sum(wth$RAIN)
  
  #b. mean temperature during groundnut growing season
  tmen <- mean(wth$TMEAN)
  
  #c. number of days with rain > 0mm, 2mm, 5mm, 10mm, 15mm, 20mm
  rd_0 <- length(which(wth$RAIN>0))
  rd_2 <- length(which(wth$RAIN>2))
  rd_5 <- length(which(wth$RAIN>5))
  rd_10 <- length(which(wth$RAIN>10))
  rd_15 <- length(which(wth$RAIN>15))
  rd_20 <- length(which(wth$RAIN>20))
  
  #d. rainfall std, rainfall c.v.
  rstd <- sd(wth$RAIN)
  if (mean(wth$RAIN) == 0) {
    rcov <- rstd/1
  } else {
    rcov <- rstd/mean(wth$RAIN)
  }
  
  #e. number of days TMAX>34 (HTS)
  hts_34 <- length(which(wth$TMAX>34))
  
  #f. number of days TMAX>40 (HTS)
  hts_40 <- length(which(wth$TMAX>40))
  
  #g. number of days TMEAN>35 (TETRS)
  tetr_35 <- length(which(wth$TMEAN>35))
  
  #h. number of days TMEAN>47 (TETRS)
  tetr_47 <- length(which(wth$TMEAN>47))
  
  #i. tmean std, tmean c.v.
  tstd <- sd(wth$TMEAN)
  tcov <- tstd/tmen
  
  #j. number of days with Ea/Ep ratio < 0.25, 0.5, 0.75
  eratio_25 <- length(which(wth$ERATIO<0.25))
  eratio_50 <- length(which(wth$ERATIO<0.5))
  eratio_75 <- length(which(wth$ERATIO<0.75))
  
  #from Trnka et al. (2011) GCB
  #k. sum of global radiation of days with daily mean temperature >8, 
  #   daily minimum temperature >0, and ETRATIO>0.5
  #   (sum of effective global radiation)
  effsrad <- sum(wth$SRAD[which(wth$TMEAN > 8 & wth$TMIN > 0 & wth$ERATIO > 0.5)])
  
  #l. number of days with daily mean temperature >8, daily minimum
  #   temperature >0 and ERATIO>0.5
  #   (sum of effective growing days)
  effgd <- length(which(wth$TMEAN > 8 & wth$TMIN > 0 & wth$ERATIO > 0.5))
  
  #output row
  orow <- data.frame(YEAR=yr,SOW=gs_i,HAR=gs_f,RAIN=rain,RSTD=rstd,RCOV=rcov,RD.0=rd_0,
                     RD.2=rd_2,RD.5=rd_5,RD.10=rd_10,RD.15=rd_15,RD.20=rd_20,
                     HTS1=hts_34,HTS2=hts_40,TETR1=tetr_35,TETR2=tetr_47,
                     ERATIO.25=eratio_25,ERATIO.50=eratio_50,ERATIO.75=eratio_75,
                     TSTD=tstd,TMEN=tmen,TCOV=tcov,EFF.SRAD=effsrad,EFF.GD=effgd)
  
  return(orow)
}
