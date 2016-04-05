require(raster)
iDir <- "/direntrada"
stk <- stack(paste0(iDir, "/prec_", 1:12, ".asc"))

Q10 <- calc(stk, fun = stats::quantile,probs = 0.10,na.rm=TRUE)
writeRaster(Q10[[1]], paste0(dirOut, "/q10_25.tif"))

Q90 <- calc(stk, fun = stats::quantile,probs = 0.90,na.rm=TRUE)
writeRaster(Q10[[1]], paste0(dirOut, "/q90_25.tif"))

