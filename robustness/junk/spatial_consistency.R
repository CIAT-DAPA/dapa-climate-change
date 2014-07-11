#check why there is something weird going on between yield and meteorology data
#answer: CDO shifted meteorology by 1.125/2 = 0.5625 to the left. 
#This was presumably when cutting the data to African domain. 
#Hence just a shift to the right will fix it. 
#Make sure the displacement is accounted for in any data extraction

wd <- "~/Leeds-work/quest-for-robustness/data"
setwd(wd)

#obs
rs1 <- raster("./meteorology/baseline_climate/Rainf_daily_WFDEI_GPCC/afr_Rainf_daily_WFDEI_GPCC_197901.nc")
rs2 <- raster("./meteorology/baseline_climate/Rainf_daily_WFD_GPCC/afr_Rainf_daily_WFD_GPCC_195001.nc")

#gcms
rs3 <- raster("./meteorology/future_climate/gfdl-esm2m/hist/afr_pr_bced_1960_1999_gfdl-esm2m_hist_1950.nc")

#sowing date
rs4 <- raster("./crop_calendar_sacks/major_maize_harvest.end.tif")

#yield data
rs5 <- raster("./yield_data_maize/descriptive_stats/cv_ModelYld500.tif")

xyFromCell(rs1,1)
xyFromCell(rs2,cellFromXY(rs2,c(-20.25,19.6875)))
xyFromCell(rs3,cellFromXY(rs3,c(-20.25,19.6875)))
xyFromCell(rs4,cellFromXY(rs4,c(-20.25,19.6875)))
xyFromCell(rs5,cellFromXY(rs5,c(-20.25,19.6875)))

rs1_1 <- shift(rs1,x=0.5625,y=0)
rs2_1 <- shift(rs2,x=0.5625,y=0)
rs3_1 <- shift(rs3,x=0.5625,y=0)

xyFromCell(rs1_1,1)
xyFromCell(rs2_1,cellFromXY(rs2_1,c(-19.6875,19.6875)))
xyFromCell(rs3_1,cellFromXY(rs3_1,c(-19.6875,19.6875)))
xyFromCell(rs4,cellFromXY(rs4,c(-19.6875,19.6875)))
xyFromCell(rs5,cellFromXY(rs5,c(-19.6875,19.6875)))

writeRaster(rs1,"wfdei.tif",format="GTiff")
writeRaster(rs2,"wfd.tif",format="GTiff")
writeRaster(rs3,"gcm.tif",format="GTiff")
writeRaster(rs4,"sow.tif",format="GTiff")
writeRaster(rs5,"yield.tif",format="GTiff")

writeRaster(rs1_1,"wfdei_1.tif",format="GTiff")
writeRaster(rs2_1,"wfd_1.tif",format="GTiff")
writeRaster(rs3_1,"gcm_1.tif",format="GTiff")

rs4_1 <- shift(rs4,x=-0.5625,y=0)
writeRaster(rs4_1,"sow_1.tif",format="GTiff")

rs4_2 <- resample(rs4,rs1,method="ngb")
writeRaster(rs4_2,"sow_2.tif",format="GTiff")

rs5_1 <- shift(rs5,x=-0.5625,y=0)
writeRaster(rs5_1,"yield_1.tif",format="GTiff")


