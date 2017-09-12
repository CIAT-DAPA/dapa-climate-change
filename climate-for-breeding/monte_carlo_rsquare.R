#monte-carlo estimate of r-squared significance
n <- 82 #46 82
rsqall <- c()
for (i in 1:1000) {
  #i <- 1
  yvals <- runif(n,-100,100)
  x1 <- runif(n,-100,100)
  fit <- lm(yvals~x1)
  rsq <- summary(fit)$r.squared
  rsqall <- c(rsqall, rsq)
}
rsq_thresh <- quantile(rsqall, probs=0.95)
