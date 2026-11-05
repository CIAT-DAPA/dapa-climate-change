## Descarga de datos CHIRPS
## chirps: API Client for CHIRPS and CHIRTS
## More info https://github.com/ropensci/chirps

## Install CHIRPS package
install.packages("chirps")
install.packages("terra")

# Load chirps library
library(chirps)
library(terra)

# Set lon/lat of points
lonlat <- data.frame(lon = c(-91.634722),
                     lat = c(15.311111))
site <- "santabarabara"

# Set dates
period <- "1990_2009"
dates <- c("1990-01-01", "2009-12-31")
lonlat <- data.frame(lon = c(-76.087),
                     lat = c(1.948))

# Set dates
dates <- c("1982-01-01", "2024-12-31")

## Function to get CHIRPS data
#dat <- get_chirps(lonlat, dates, server = "CHC", as.matrix = FALSE)

#For a faster download of few datapoints (~ 10 datapoints), the argument server = "ClimateSERV" can be used
datP <- get_chirps(lonlat, dates, server = "ClimateSERV", as.matrix = FALSE)
datTx <- get_chirts(lonlat, dates, server = "ClimateSERV", var = "Tmax")
datTm <- get_chirts(lonlat, dates, server = "ClimateSERV", var = "Tmin")

# Write data
outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirps_prec_", period, ".csv")
outfile <- "D:/cenavarro/request/request_ee/01_baseline/col_salvajina_chirps.csv"
write.csv(datP, outfile, row.names = F)

outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirts_tmax_", period, ".csv")
write.csv(datTx, outfile, row.names = F)

outfile <- paste0("/Users/cenavarro/Workspace/", site, "_chirts_tmin_", period, ".csv")
write.csv(datTm, outfile, row.names = F)


