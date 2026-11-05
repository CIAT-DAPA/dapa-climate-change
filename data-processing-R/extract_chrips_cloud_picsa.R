## Descarga de datos CHIRPS
## chirps: API Client for CHIRPS and CHIRTS
## More info https://github.com/ropensci/chirps

## Install CHIRPS package
# install.packages("chirps")
# install.packages("terra")

# Load chirps library
library(chirps)
library(terra)
	
# Set lon/lat of points
lonlat <- data.frame(lon = c(-89.86686),
                     lat = c(16.91609))
site <- "flores"

# Set dates
period <- "2019_2025"
dates <- c("2019-01-01", "2025-12-31")

#For a faster download of few datapoints (~ 10 datapoints), the argument server = "ClimateSERV" can be used
datP <- get_chirps(lonlat, dates, server = "ClimateSERV", as.matrix = FALSE)
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirps_prec_", period, ".csv")
write.csv(datP, outfile, row.names = F)

datTx <- get_chirts(lonlat, dates, server = "ClimateSERV", var = "Tmax")
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirts_tmax_", period, ".csv")
write.csv(datTx, outfile, row.names = F)

datTm <- get_chirts(lonlat, dates, server = "ClimateSERV", var = "Tmin")
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirts_tmin_", period, ".csv")
write.csv(datTm, outfile, row.names = F)


# Set dates
period <- "2010_2023"
dates <- c("2010-01-01", "2023-11-30")

#For a faster download of few datapoints (~ 10 datapoints), the argument server = "ClimateSERV" can be used
datP <- get_chirps(lonlat, dates, server = "ClimateSERV", as.matrix = FALSE)
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirps_prec_", period, ".csv")
write.csv(datP, outfile, row.names = F)

datTx <- get_chirts(lonlat, dates, server = "ClimateSERV", var = "Tmax")
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirts_tmax_", period, ".csv")
write.csv(datTx, outfile, row.names = F)

datTm <- get_chirts(lonlat, dates, server = "ClimateSERV", var = "Tmin")
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirts_tmin_", period, ".csv")
write.csv(datTm, outfile, row.names = F)