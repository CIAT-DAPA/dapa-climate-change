#----------------------------------------------------------------------------------------------#
# funcitons to calculate dissimilarity
#----------------------------------------------------------------------------------------------#

calc.dis <- function(roll, params,cdata,base,project) {
  # create result 
  res_return <- list()
  
  # for each combination (lag) loop calc dissimilartiy
  if (params$method=="ccafs") {
      res_all <- apply(roll, 1, function(x) calc.ccafs(x,params,cdata,delta))
  } else if (params$method=="ccafs.generic") {
      res_all <- apply(roll, 1, function(x) calc.ccafs.generic(x,params,cdata,base,project))
  } else if (params$method=="hallegate" | params$method=="hal") {
      res_all <- apply(roll,1,function(x) calc.hal(lag=x, params=params,cdata, delta))
  }
    
  if (params$keep.lag & params$across.year) {
      # create stack with lagged 
      res_return$res_stacked <- stack(res_all)
  
  } else if (params$sumarize.lag & params$across.year) {
      # take the minimum of each each month
      res_sum <- stack(res_all)
      res_return$res_sum <- stackApply(res_sum,rep(1,nlayers(res_sum)),min)
  } else if(!params$across.year) {
      res_return <- res_all
  }
  
  return(res_return)
}

#----------------------------------------------------------------------------------------------#
# Functions to calculate dissimilarity
#----------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------#
# ccafs dissimilarity

calc.ccafs <- function(lag, # lag is the arrangemnt of months
  params,                   # all parameters
  cdata,                    # base/reference value to find dissimilarity
  base,                     # second scenario against which dissimilarity should be looked for
  project
  ) {
  # calc dissimilartiy
  cat(str_c("calculating dissimilarity (ccafs) for: ",delta," starting with ",lag[1], ". \n"))
  # substract ref mean tmp
  t1 <- cdata$tmean_b - params$ref_value[[str_c(delta,"_tmean")]][lag]
    
  # substract ref prec
  p1 <- cdata$prec_b - params$ref_value[[str_c(delta,"_prec")]][lag]
    
  # substract ref dtr
  d1 <- cdata$dtr_b - params$ref_value[[str_c(delta,"_dtr")]][lag]
    
  t2 <- (t1*t1)
  p2 <- p1*p1
    
  v1 <- stackApply(t2,rep(1,nlayers(t2)),sum)
  v2 <- stackApply(p2,rep(1,nlayers(p2)),sum)
    
  res <- sqrt(v1+v2)
  return(res)    
}

#----------------------------------------------------------------------------------------------#
# ccafs generic
calc.ccafs.generic <- function(lag, # lag is the arrangemnt of months
  params,                   # all parameters
  cdata,                   # base/reference value to find dissimilarity
  base,                     # second scenario against which dissimilarity should be looked for
  project
  ) {
  # calc dissimilartiy
  cat(str_c("calculating dissimilarity (ccafs.generic) projecting ",params$gcms[base]," to ",params$gcms[project]," starting with ",lag[1], ". \n"))
  # substract ref mean tmp
  
  # substract reference value
  ll <- lapply(1:length(params$vars), 
      function(x) cdata$training[[which(params$idx.g==base & params$idx.v == x)]] - params$ref_value[[which(params$idx.g==base & params$idx.v == x)]][lag])
  
  # substrack reference values for weights
  ww <- lapply(1:length(params$weights), function(x) { 
    if (class(cdata$weights[[x]]) == "RasterLayer" | class(cdata$weights[[x]]) == "RasterStack") {
      return(cdata$weights[[which(params$idx.g==base & params$idx.v == x)]] - params$ref_weights[[which(params$idx.g==base & params$idx.v == x)]][lag])
      } else {return(params$ref_weights[[x]])}
    })
  
  # multiply by weights
  ll <- lapply(1:length(ll), function(x) ll[[x]] * ww[[x]])
  
  # square
  ll <- lapply(ll, function(x) x*x)
    
  # sum division ups
  ll <- lapply(ll, function(x) stackApply(x,rep(1,nlayers(x)),sum))

  # sum over all lists
  res <- 0
  for (i in ll) res <- res + i

  # take sqrt 
  res <- sqrt(res)

  return(res)    
}

#----------------------------------------------------------------------------------------------#
# hal method

calc.hal <- function(lag,params,base.v,delta) {
 
 # progress
 cat(str_c("calculating dissimilarity for (hal): ",delta," starting with ",lag[1], ". \n"))
 
 # rasters for which dissimilarity should be found
 pre <- base.v[['pre_b.v']] # thats where i want to project to
 tmp <- base.v[['tmean_b.v']]
 
 # reference values for rain and tmp (vector of length 12)
 pre.ref <- params$ref_value[[str_c(delta,"_prec")]][lag]
 tmp.ref <- params$ref_value[[str_c(delta,"_tmean")]][lag]
 
 
 # Eq. 1
 p_sum <- sum(pre.ref) # sum(ref_values[13:24])
 eq1 <- abs(apply(pre,1,sum) - p_sum) / p_sum <= params$hal.prec.month # abs(apply(c1[,13:24], 1, sum) - p_sum) / p_sum <= 0.15 # change the 0.15, 
 
 # Eq. 2
 c3_prec <- abs(scale(pre,center=pre.ref,scale=ifelse(pre.ref==0,1,pre.ref))) # abs(scale(c1[,13:24], center=ref_values[13:24], scale=ifelse(ref_values[13:24]==0, 1, ref_values[13:24])))
 eq2 <- apply(c3_prec, 1, sum)/12 <= params$hal.prec.annual
 
 # Eq. 3
 c5 <- abs(scale(tmp, center=tmp.ref, scale=F))
 c6 <- apply(c5, 1, sum)/12
 eq3 <- c6 <= params$hal.tmp
 
 # find site that match all three requirements
 hal <- ifelse(eq1 & eq2 & eq3, c6, NA)
 
 return(hal)
}

sumarize.lag <- function(x,method="min") {
   apply(res_all, 1, function(x) {
   if (sum(is.na(x))==length(months)) {
     return(c(NA, NA)) }
   else {
     r1 <- which.min(x)
     r2 <- x[r1]
     return(c(r1, r2)) } } )
}
