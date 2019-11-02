require(raster)
require(stats)

countryLs <- c("sin")
bDir <- "D:/Workspace/csa_profiles"

mt <- c()

for (country in countryLs){
  
  iDir <- paste0(bDir, "/", country, "/by_model/anomalies/rcp45")
  
  models <- list.dirs(iDir, recursive = FALSE, full.names = TRUE)
  periods <- c("2020_2049", "2040_2069", "2060_2089")
  
  for (period in periods){
    
    bio_1s <- stack(paste0(models, "/", period, "/bio_1"))
    bio_12s <- stack(paste0(models, "/", period, "/bio_12"))
    
    fun <- function(x) { sd(x, na.rm = T) }
    quan <- function(x) { quantile(x,probs = c(.05,.95),na.rm=TRUE) }
    
    # calc(s, fun = function(x) {} )
    # bio_12std <- calc(bio_12s, quan)
    
    bio_1avg <- cellStats(calc(bio_1s, mean), "mean")
    bio_12avg <- cellStats(calc(bio_12s, mean), "mean")
    bio_1std <- cellStats(calc(bio_1s, fun), "mean")
    bio_12std <- cellStats(calc(bio_12s, fun), "mean")
    
    mt <- rbind(mt, cbind(country, period, bio_1avg, bio_12avg, bio_1std, bio_12std))
    
  }

  write.csv(mt, paste0(bDir, "/", country, "/country_stats.csv"))
  
}


