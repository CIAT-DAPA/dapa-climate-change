
#Average and uncertainties

gcmDir <- "F:/Final_Course/day_6/_Final_modelling/2040_2069/analyses/runs-future"
outFolder <- "F:/Final_Course/day_6/_Final_modelling/ensemble/2040_2069"
rsn <- "moz_sorghum_suitability"

if (!file.exists(outFolder)) {dir.create(outFolder)}

#Creating the stack
gcmList <- list.files(gcmDir)

for (gcm in gcmList) {
  
  cat(gcm, "/n")
  rDir <- paste(gcmDir, "/", gcm, sep="")
  rs <- raster(paste(rDir, "/", rsn, ".tif", sep=""))

  if (gcm == gcmList[1]) {
    gcmstack <- c(rs)
  } else {
    gcmstack <- c(gcmstack, rs)
  }
}

gcmstack <- stack(gcmstack)

#Stack averages
avg <- mean(gcmstack)

#Stack standar deviation
#fun <- function(x) { sd(x) }
#std <- calc(gcmstack, fun)


avg <- writeRaster(avg, paste(outFolder, "/", rsn, "_avg.tif", sep=""), overwrite=T)
#std <- writeRaster(stdv, paste(outFolder, "/", rsn, "_std.asc", sep=""), format="ascii", overwrite=T)

