#----------------------------------------------------------------------------------------------#
# funcitons to calculate dissimilarity
#----------------------------------------------------------------------------------------------#

calc.dis <- function(roll, params,base.v,delta,cdata) {
  # create result 
  res_return <- list()
  
  # for each combination (lag) loop calc dissimilartiy
  delta <- delta
  if (params$method=="ccafs") {
      res_all <- apply(roll, 1, function(x) calc.ccafs(x,params,base.v,delta))
  } else if (params$method=="ccafs.generic") {
      res_all <- apply(roll, 1, function(x) calc.ccafs.generic(params.base.v,delta))
  } else if (params$method=="hallegate" | params$method=="hal") {
      res_all <- apply(roll,1,function(x) calc.hal(lag=x, params=params,base.v, delta))
  }
    
  if (params$keep.lag) {
      # create stack with lagged 
      res_return$res_stacked <- do.call(stack, apply(res_all,2,function(x) setValues(cdata$tmean_b[[1]],x)))
  
  } else if (params$sumarize.lag) {
      # take the minimum of each each month
      res_sum <- sumarize.lag(res_all)
      res_return$res_sum <- setValues(cdata$tmean_b[[1]],res_sum[2,])
  }
    
  return(res_return)
}

#----------------------------------------------------------------------------------------------#
# Functions to calculate dissimilarity
#----------------------------------------------------------------------------------------------#

# ccafs dissimilarity

calc.ccafs <- function(lag, # lag is the arrangemnt of months
  params,                   # all parameters
  base.v,                   # base/reference value to find dissimilarity
  delta                     # second scenario against which dissimilarity should be looked for
  ) {
  # calc dissimilartiy
  cat(str_c("calculating dissimilarity (ccafs) for: ",delta," starting with ",lag[1], ". \n"))
  # substract ref mean tmp
  t1 <- t(base.v[['tmean_b.v']]) - params$ref_value[[str_c(delta,"_tmean")]][lag]
    
  # substract ref prec
  p1 <- t(base.v[['pre_b.v']]) - params$ref_value[[str_c(delta,"_prec")]][lag]
    
  # substract ref dtr
  d1 <- t(base.v[['dtr_b.v']]) - params$ref_value[[str_c(delta,"_dtr")]][lag]
    
  t2 <- d1*t1*t1
  p2 <- p1*p1
    
  v1 <- colSums(t2)
  v2 <- colSums(p2)
    
  res <- sqrt(v1+v2)
  return(res)    
}

# ccafs generic
calc.ccafs.generic <- function(lag, # lag is the arrangemnt of months
  params,                   # all parameters
  base.v,                   # base/reference value to find dissimilarity
  delta                     # second scenario against which dissimilarity should be looked for
  ) {
  # calc dissimilartiy
  cat(str_c("calculating dissimilarity (ccafs) for: ",delta," starting with ",lag[1], ". \n"))
  # substract ref mean tmp
  
  # substract reference value
  ll <- lapply(str_c(params$var,"_b.v"), function(x) (t(base.v[[x]]) - params$ref_value[[str_c(delta,"_",x)]][lag])^2)
  
  # square
  ll <- lapply(ll, function(x) x*x)
    
  # sum division ups
  ll <- lapply(ll, colSums)

  # take square rot
  ll <- lapply(ll, sqrt)
  
  # sum over all lists
  res <- c()
  for (i in ll) res <- ress + i
  
  return(res)    
}

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
