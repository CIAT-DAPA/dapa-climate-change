source("readGSODData.R")
work.dir <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/gsod-weather-stations"

#Station locations
#stloc <- readWSLocation(wd=work.dir)

#Individual year processing
for (yr in 1961:1990) {
  cat("\n")
  cat("Year", yr, "\n")
  rest <- readStations(wd=work.dir, yr, ow=T)
}

#Joining all years ()
jy <- joinYears(work.dir, 1961, 1990, variable="tmean")
jy <- joinYears(work.dir, 1961, 1990, variable="tmax")
jy <- joinYears(work.dir, 1961, 1990, variable="tmin")
jy <- joinYears(work.dir, 1961, 1990, variable="rain")

#Calculate baseline (1961-1990) average
bm <- baselineMean(outvn="tmean", wd=work.dir)
