library(raster)

rs_dir <- "D:/CIAT/Projects/ecu-hidroelectrica/02_baseline/monthly-averages"
setwd(rs_dir)
rs_list <- list.files(pattern = ".asc")

st_list <- read.csv("D:/CIAT/Projects/ecu-hidroelectrica/01_station_data/st_data_processed/st_selection.csv", header = T)
coordinates <- st_list[5:6]


value <- extract(stack(rs_list), coordinates)

write.table(value, paste0("mth_clim.txt"), sep="\t", row.names=F, quote=F)
