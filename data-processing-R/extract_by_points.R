library(raster)

rs_dir <- "W:/04_interpolation/outputs_yearly_v2/average/tif"
setwd(rs_dir)

for (var in c("prec", "tmean"))
rs_stack <- stack(paste0("prec_", expand.grid(1:12, 1981:2014)[,2], "_", expand.grid(1:12, 1981:2014)[,1], ".tif"))

st_list <- read.csv("W:/04_interpolation/extractions/points.csv", header = T)
coordinates <- st_list[3:4]

value <- extract(rs_stack, coordinates)
value_t <- t(value)


write.table(t(value), paste0("W:/04_interpolation/extractions/mth_yearly_prec.txt"), sep="\t", row.names=T, quote=F)

