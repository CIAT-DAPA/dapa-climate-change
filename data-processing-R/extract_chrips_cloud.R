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
lonlat <- data.frame(lon = c(-76.087),
                     lat = c(1.948))

# Set dates
dates <- c("1982-01-01", "2024-12-31")

## Function to get CHIRPS data
#dat <- get_chirps(lonlat, dates, server = "CHC", as.matrix = FALSE)

#For a faster download of few datapoints (~ 10 datapoints), the argument server = "ClimateSERV" can be used
datP <- get_chirps(lonlat, dates, server = "ClimateSERV", as.matrix = FALSE)

# Write data
outfile <- "D:/cenavarro/request/request_ee/01_baseline/col_salvajina_chirps.csv"
write.csv(datP, outfile, row.names = F)


## Function to get CHIRTS data
# Data is currently available from 1983 to 2016
outfile <- "/Users/cenavarro/Workspace/test_chirts_tx.csv"
datTx <- get_chirts(lonlat, dates, var = "Tmax")
write.csv(datTx, outfile, row.names = F)

outfile <- "/Users/cenavarro/Workspace/test_chirts_tn.csv"
datTx <- get_chirts(lonlat, dates, var = "Tmin")
write.csv(datTx, outfile, row.names = F)
