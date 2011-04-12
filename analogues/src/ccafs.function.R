ccafs.function <- function(lag, params,base.v,delta="current") {
  
  t1 <- t(base.v[['tmp_b.v']]) - params$ref_value[[str_c(delta,"_tmean")]][lag]
  
  p1 <- t(base.v[['pre_b.v']]) - params$ref_value[[str_c(delta,"_prec")]][lag]
  
  d1 <- t(base.v[['dtr_b.v']]) - params$ref_value[[str_c(delta,"_dtr")]][lag]
  
  t2 <- d1*t1*t1
  p2 <- p1*p1
  
  v1 <- colSums(t2)
  v2 <- colSums(p2)
  
  res <- sqrt(v1+v2)
  return(res) 
}